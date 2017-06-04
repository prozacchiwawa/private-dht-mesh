module Broadcast

open Util
open Buffer
open RBTree
open Return

(*
 * We must have a list of connected network members.
 * Each stream has an outbound datagram queue, a received seq number
 * and a send seq number, as well as a last recv time.
 *
 * On Tick, we push a ping if we haven't done a send on the broadcast
 * previously since the last tick.
 * 
 * A conversation that hasn't had a recv in the timeout time is removed.
 * On Recv, we bump the recv time and last recv seq number and send on
 * any datagrams.  We send a ping to acknowledge if there was a datagram.
 *
 * In order to reduce combinatorial explosions in conversations, we use
 * hubs to do broadcasting.  Each broadcast has n masters, which are the n
 * nodes closest to the hash of the broadcast name.
 * We maintain a conversation per broadcast with all broadcast masters.
 * If a node is connected to a non-master for a broadcast, the broadcast
 * messages are forwarded to this node's masters and vice versa.
 *
 * At any given time, the master list is the list of peers from state.alive
 * that begins at lower_bound(hash(channel)) and goes to
 * (lower_bound(hash(channel)) + n), wrapping around.
 * 
 * We must at least ping our chosen masters to ensure that we're in their
 * peer set.  Any node designated as a master must forward broadcast
 * packets away from itself.  Any node not designated a master must forward
 * packets toward masters.
 *)

type Message =
  { channel : string
  ; data : string option
  }
   
type Msg<'peer when 'peer : comparison> =
  | TimeTick
  | SetId of 'peer
  | InMessage of ('peer * Message)
  | OutMessage of Message
  | JoinBroadcast of string
  | LeaveBroadcast of string
  | AddNode of 'peer

type PeerState<'peer when 'peer : comparison> =
  { name : 'peer ;
    lastSeen : int
  }

type BroadcastPeer<'peer> =
  { name : 'peer
  }

type SideEffect<'peer> =
  | OutPacket of ('peer * Serialize.Json)

type BroadcastInstance<'peer when 'peer : comparison> =
  { masters : Set<'peer> ;
    lastSeen : int ;
    seq : int ;
    refs : int ;
    peers : Map<'peer, BroadcastPeer<'peer>>
  }

type State<'peer when 'peer : comparison> =
  { curTick : int ;
    myId : 'peer option ;
    numMasters : int ;
    inactiveTimeout : int ;
    alive : rbtree.Tree<'peer,PeerState<'peer>> ;
    broadcasts : Map<string, BroadcastInstance<'peer>>
  }

let init inactiveTimeout =
  { curTick = 0 ;
    myId = None
    inactiveTimeout = inactiveTimeout ;
    numMasters = 3 ;
    alive = RBTree.createTree Util.strcmp ;
    broadcasts = Map.empty
  }

let doExpireBroadcasts state =
  let expired =
    state.broadcasts
    |> Map.toSeq
    |> Seq.filter
         (fun (k,b) ->
           b.lastSeen + state.inactiveTimeout < state.curTick ||
             b.refs <> 0
         )
    |> Map.ofSeq
  in
  { state with broadcasts = expired }
  |> Return.singleton

let stringKey channel =
  let h = Crypto.createHash "md5" in
  let _ = Crypto.updateBuffer (Buffer.fromString channel "binary") h in
  (Crypto.digestHex h)
  |> Seq.map Util.charCode
  |> Util.windowed 4
  |> (fun ss ->
       Seq.concat
         [ss 
          |> Seq.truncate 1
          |> Seq.map
               (fun ss -> 
                 ss
                 |> Seq.map Util.stringFromCharCode
                 |> String.concat ""
                 |> parseHex
                 |> Option.map (fun x -> x ||| 0xfc00)
                 |> optionDefault 0xfc00
                 |> toHex
                 |> zeropad 4
               ) ;
          ss
          |> Seq.skip 1
          |> Seq.map (fun ss -> Seq.map Util.stringFromCharCode ss)
          |> Seq.map (String.concat "")
         ]
     )
  |> String.concat ":"

let encodePacketForChannel channel buf =
  Serialize.jsonObject
    (List.concat
       [ [("c", Serialize.jsonString channel)]
       ; buf
         |> Option.map (fun buf -> ("m", Serialize.jsonString buf))
         |> optionToList
       ] |> Array.ofSeq
    )
  
let doMastersForChannel channel (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  state.broadcasts
  |> Map.tryFind channel
  |> Option.map
       (fun br ->
         let wantKey = stringKey channel in
         let masters =
           [state.alive.lt wantKey |> RBTree.iterSeq ;
            RBTree.head state.alive |> RBTree.iterSeq]
           |> Seq.concat
           |> Seq.toArray
           |> Seq.truncate state.numMasters
           |> Seq.map fst
           |> Set.ofSeq
         in
         let _ = printfn "Masters for %A: %A" channel masters in
         let peers =
           masters
           |> Seq.filter (fun m -> Map.tryFind m br.peers = None)
           |> Seq.fold (fun p m -> Map.add m { name = m } p) br.peers
         in
         let broadcast =
           { br with 
               masters = masters ;
               peers = peers
           }
         in
         { state with
             broadcasts = Map.add channel broadcast state.broadcasts
         }
       )
  |> optionDefault state
  |> Return.singleton

let doPingPeers
      (state : State<'peer>) :
      (State<'peer> * SideEffect<'peer> list) =
  state.broadcasts
  |> Map.toSeq
  |> Seq.map
       (fun (channel,br) ->
         let packet = encodePacketForChannel channel None in
         br.peers
         |> Map.toSeq
         |> Seq.map
              (fun (m,_) ->
                if Some m <> state.myId then
                  [OutPacket (m, packet)]
                else
                  []
              )
         |> Seq.toList
         |> Seq.concat
       )
  |> Seq.concat
  |> Seq.toList
  |> tupleSndWith state

let doTick state =
  (doExpireBroadcasts state)
  |> Return.andThen doPingPeers

let indicatePeerAlive (p : 'peer) (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  (if Some p <> state.myId then
     { state with
         alive =
           (state.alive.find p).node
           |> Option.map
                (fun node ->
                  ((state.alive.remove p).insert)
                    p
                    { node.value with lastSeen = state.curTick }
                )
           |> optionDefault
                (((state.alive.remove p).insert)
                   p
                   { name = p ;
                     lastSeen = state.curTick
                   }
                )
     }
   else
     state
  ) |> Return.singleton

(* Using the broadcast state for the indicated channel, do one of these
 * things:
 * 1) If we have an id and it's a member of br.masters, then we forward
 *    the broadcast packet to every peer.
 * 2) Else, forward the broadcast packet to all masters.
 *)
let forwardBroadcast (p : 'peer) (channel : string) (data : string option) (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let makePacket p =
    [OutPacket (p,encodePacketForChannel channel data)]
  in
  state.broadcasts
  |> Map.tryFind channel
  |> Option.map
       (fun br ->
         let iAmMaster =
           state.myId
           |> Option.filter ((flip Set.contains) br.masters)
           |> optionEither true false
         in
         (if iAmMaster then
            br.peers
            |> Map.toSeq
            |> Seq.map fst
          else if state.myId <> None then
            br.masters
            |> Set.toSeq
            |> Seq.filter (fun m -> Some m <> state.myId)
          else
            Seq.empty
         )
         |> Seq.filter (fun m -> m <> p)
         |> Seq.map makePacket
         |> Seq.concat
         |> Seq.toList
       )
  |> Option.map (tupleSndWith state)
  |> optionDefault (state, [])

let handlePacket p msg state =
  state.broadcasts
  |> Map.tryFind msg.channel
  |> Option.map
       (fun br ->
         br.peers
         |> Map.tryFind p
         |> Option.map (fun p -> (p,br))
         |> optionDefault ({ name = p }, br)
       )
  |> Option.map
       (fun (p,br) ->
         { br with
             lastSeen = state.curTick ;
             peers = Map.add p.name p br.peers
         }
       )
  |> Option.map
       (fun br ->
         { state with broadcasts = Map.add msg.channel br state.broadcasts }
       )
  |> optionDefault
       (let br =
          { masters = Set.empty ;
            lastSeen = state.curTick ;
            refs = 0 ;
            seq = -1 ;
            peers = Map.add p { name = p } Map.empty
          }
        in
        { state with broadcasts = Map.add msg.channel br state.broadcasts }
       )
  |> (doMastersForChannel msg.channel)
  |> Return.andThen (indicatePeerAlive p)
  |> Return.andThen (forwardBroadcast p msg.channel msg.data)

let update
      (msg : Msg<'peer>)
      (state : State<'peer>) :
      (State<'peer> * SideEffect<'peer> list) =
  match Util.log "b" msg with
  | SetId p -> { state with myId = Some p } |> Return.singleton
  | JoinBroadcast b ->
     state.broadcasts
     |> Map.tryFind b
     |> Option.map (fun br -> { br with refs = br.refs + 1 })
     |> optionDefault
          { masters = Set.empty ;
            lastSeen = state.curTick ;
            refs = 1 ;
            seq = -1 ;
            peers = Map.empty
          }
     |> (fun br -> { state with broadcasts = Map.add b br state.broadcasts })
     |> doMastersForChannel b
     |> Return.andThen
          (fun state ->
            state.myId
            |> Option.map
                 (fun myid ->
                   forwardBroadcast myid b None state
                 )
            |> optionDefault (state, [])
          )
  | LeaveBroadcast b ->
     state.broadcasts
     |> Map.tryFind b
     |> Option.map
          (fun br ->
            { state with
                broadcasts =
                  Map.add
                    b
                    { br with refs = Util.max (br.refs - 1) 0 }
                    state.broadcasts
            }
          )
     |> optionDefault state
     |> Return.singleton
  | OutMessage b ->
     state.myId
     |> Option.map (fun id -> handlePacket id b state)
     |> optionDefault (Return.singleton state)
  | InMessage (p,b) -> handlePacket p b state
  | AddNode p -> indicatePeerAlive p state
  | TimeTick -> doTick state
