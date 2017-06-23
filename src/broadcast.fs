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
  ; seq : int
  ; data : string option
  }
   
type Msg<'peer when 'peer : comparison> =
  | TimeTick
  | SetId of 'peer
  | InMessage of ('peer * Message)
  | ForwardedMessage of ('peer * Message)
  | OutMessage of Message
  | JoinBroadcast of string
  | LeaveBroadcast of string
  | AddNode of 'peer
  | Success of ('peer * string * int)
  | Failure of 'peer

type PeerState<'peer when 'peer : comparison> =
  { name : 'peer ;
    lastSeen : int
  }

type BroadcastPeer<'peer> =
  { name : 'peer
  ; inseq : int
  ; outseq : int
  ; outqueue : Map<int, Message>
  ; lastping : int
  }

type SideEffect<'peer> =
  | OutPacket of ('peer * Serialize.Json)
  | UserMessage of Message

type BroadcastInstance<'peer when 'peer : comparison> =
  { masters : Set<'peer>
  ; lastSeen : int
  ; refs : int
  ; peers : Map<'peer, BroadcastPeer<'peer>>
  }

type State<'peer when 'peer : comparison> =
  { curTick : int
  ; myId : 'peer option
  ; numMasters : int
  ; inactiveTimeout : int
  ; alive : rbtree.Tree<'peer,PeerState<'peer>>
  ; broadcasts : Map<string, BroadcastInstance<'peer>>
  }

let init inactiveTimeout =
  { curTick = 0 ;
    myId = None ;
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
  let h = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString channel "utf-8") h in
  Crypto.digestHex h

let encodePacketForChannel msg forwarded =
  Serialize.jsonObject
    (List.concat
       [ [ ("c", Serialize.jsonString msg.channel)
         ; ("s", Serialize.jsonInt msg.seq)
         ]
       ; msg.data
         |> Option.map (fun buf -> ("m", Serialize.jsonString buf))
         |> optionToList
       ; forwarded
         |> Option.map (fun from -> ("f", Serialize.jsonString from))
         |> optionToList
       ] |> Array.ofSeq
    )
  
let doMastersForChannel channel (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let peerlist = state.alive |> RBTree.head |> RBTree.iterSeq |> List.ofSeq in
  state.broadcasts
  |> Map.tryFind channel
  |> Option.map
       (fun br ->
         let wantKey = stringKey channel in
         let masterCandidates =
           RBTree.head state.alive
           |> RBTree.iterSeq
         in
         let masters =
           [ state.alive.lt wantKey |> RBTree.iterSeq
           ; RBTree.head state.alive |> RBTree.iterSeq
           ]
           |> Seq.concat
           |> Seq.toArray
           |> Seq.truncate state.numMasters
           |> Seq.map fst
           |> Set.ofSeq
         in
         let peers =
           masters
           |> Seq.filter (fun m -> Map.tryFind m br.peers = None)
           |> Seq.fold (fun p m -> Map.add m { name = m ; outseq = 0 ; outqueue = Map.empty ; inseq = -1 ; lastping = -1 } p) br.peers
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
         br.peers
         |> Map.toSeq
         |> Seq.map
              (fun (m,p) ->
                let packet =
                  if Seq.length p.outqueue = 0 then
                    encodePacketForChannel
                      { channel = channel
                      ; seq = p.outseq
                      ; data = None
                      } None
                  else
                    encodePacketForChannel
                      (Seq.head p.outqueue |> (fun kv -> kv.Value)) None
                [OutPacket (m, packet)]
              )
         |> Seq.toList
         |> Seq.concat
       )
  |> Seq.concat
  |> Seq.toList
  |> tupleSndWith state

let doTick state =
  (doExpireBroadcasts { state with curTick = state.curTick + 1 })
  |> Return.andThen doPingPeers

let indicatePeerAlive (p : 'peer) (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let peerlist = state.alive |> RBTree.head |> RBTree.iterSeq |> List.ofSeq in
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
  )
  |> (fun state ->
       let peerlist = state.alive |> RBTree.head |> RBTree.iterSeq |> List.ofSeq in
       state
     )
  |> Return.singleton

let makePacket msg p forward = [OutPacket (p,encodePacketForChannel msg forward)]

let forwardFirstMessage channel peer forward state =
  Map.tryFind channel state.broadcasts
  |> Option.bind
       (fun br ->
         Map.tryFind peer br.peers
         |> Option.map
              (fun p ->
                if Seq.length p.outqueue = 1 then
                  makePacket
                    (Map.toSeq p.outqueue
                     |> Seq.map snd
                     |> Seq.head
                    ) p.name forward
                else
                  []
              )
       )
  |> optionDefault []
  |> List.ofSeq
  |> tupleSndWith state

(* Using the broadcast state for the indicated channel, do one of these
 * things:
 * 1) If we have an id and it's a member of br.masters, then we forward
 *    the broadcast packet to every peer.
 * 2) Else, forward the broadcast packet to all masters.
 *)
let forwardBroadcast
      (p : 'peer)
      (f : bool)
      (msg : Message)
      (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let forward = if f then Some p else None in
  let internMsgToBroadcast msg state =
    state.broadcasts
    |> Map.tryFind msg.channel
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
           |> Seq.map (fun p -> Map.tryFind p br.peers |> optionToList)
           |> Seq.concat
           |> Seq.fold
                (fun (send,br) peer ->
                  let addMsg peer =
                    msg.data <> None || peer.lastping <> state.curTick
                  in
                  let redundant = Map.tryFind msg.seq peer.outqueue <> None in
                  let nseq peer = peer.outseq + 1 in
                  ((if redundant then send else (peer.name :: send)),
                   { br with
                       peers =
                         Map.add
                           peer.name
                           { peer with
                               outseq = nseq peer ;
                               outqueue =
                                 (if addMsg peer then
                                    Map.add
                                      (nseq peer)
                                      { msg with seq = nseq peer }
                                      peer.outqueue
                                  else
                                    peer.outqueue
                                 ) ;
                               lastping =
                                 if addMsg peer then
                                   state.curTick
                                 else
                                   peer.lastping
                           }
                           br.peers
                   }
                  )
                )
                ([],br)
           |> (fun (send,br) ->
                ( send
                , { state with
                      broadcasts = Map.add msg.channel br state.broadcasts
                  }
                )
              )
         )
    |> optionDefault ([],state)
  in
  internMsgToBroadcast msg state
  |> (fun (send,state) ->
       if send <> [] then
         forwardFirstMessage msg.channel p forward state
       else
         (state,[])
     )

let isRetransmission p msg state =
  state.broadcasts
  |> Map.tryFind msg.channel
  |> Option.bind
       (fun br ->
         Map.tryFind p br.peers |> Option.map (fun p -> p.inseq >= msg.seq)
       )
  |> optionDefault false
  
let handlePacket
      (p : 'peer)
      (forwarded : bool)
      (msg : Message)
      (state : State<'peer>) =
  let retx = isRetransmission p msg state in
  state.broadcasts
  |> Map.tryFind msg.channel
  |> Option.map
       (fun br ->
         br.peers
         |> Map.tryFind p
         |> Option.map
              (fun p ->
                if retx then (p,br) else ({ p with inseq = msg.seq }, br)
              )
         |> optionDefault
              ({ name = p
               ; outseq = 0
               ; outqueue = Map.empty
               ; inseq = msg.seq
               ; lastping = state.curTick
               }, br)
       )
  |> Option.map
       (fun (peer,br) ->
         { br with
             lastSeen = state.curTick ;
             peers = Map.add peer.name peer br.peers
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
            peers =
              Map.add
                p
                { name = p
                ; outseq = 0
                ; outqueue = Map.empty
                ; inseq = -1
                ; lastping = state.curTick
                }
                Map.empty
          }
        in
        { state with broadcasts = Map.add msg.channel br state.broadcasts }
       )
  |> doMastersForChannel msg.channel
  |> Return.andThen (indicatePeerAlive p)
  |> (fun ret ->
       if not retx then
         Return.andThen (forwardBroadcast p forwarded msg) ret
         |> Return.command [UserMessage msg]
       else if Some p = state.myId then
         Return.command [UserMessage msg] ret
       else
         ret
     )

let nextPacket p c s state =
  state.broadcasts
  |> Map.tryFind c
  |> Option.map
       (fun br ->
         br.peers
         |> Map.tryFind p
         |> Option.map
              (fun peer ->
                let newPeer =
                  { peer with outqueue = Map.remove s peer.outqueue }
                in
                { br with
                    peers =
                      Map.add
                        p
                        newPeer
                        br.peers
                }
              )
         |> optionDefault br
       )
  |> Option.map
       (fun br -> { state with broadcasts = Map.add c br state.broadcasts })
  |> optionDefault state
  |> forwardFirstMessage c p None
  
let update
      (msg : Msg<'peer>)
      (state : State<'peer>) :
      (State<'peer> * SideEffect<'peer> list) =
  let _ = printfn "b %A" msg in
  match msg with
  | SetId p -> { state with myId = Some p } |> Return.singleton
  | JoinBroadcast b ->
     state.broadcasts
     |> Map.tryFind b
     |> Option.map (fun br -> { br with refs = br.refs + 1 })
     |> optionDefault
          { masters = Set.empty ;
            lastSeen = state.curTick ;
            refs = 1 ;
            peers = Map.empty
          }
     |> (fun br -> { state with broadcasts = Map.add b br state.broadcasts })
     |> doMastersForChannel b
     |> Return.andThen
          (fun state ->
            state.myId
            |> Option.map (fun myid -> forwardBroadcast myid false { channel = b ; seq = 0 ; data = None } state)
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
     |> Option.map (fun id -> handlePacket id false b state)
     |> optionDefault (Return.singleton state)
  | InMessage (p,b) -> handlePacket p false b state
  | ForwardedMessage (p,b) -> handlePacket p true b state 
  | AddNode p -> indicatePeerAlive p state
  | Success (p,c,s) -> nextPacket p c s state
  | Failure p -> (state, [])
  | TimeTick -> doTick state
