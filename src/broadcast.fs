module Broadcast

open Util
open Buffer
open RBTree
open Return
open BroadcastData
open BroadcastInstance

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
   
type Msg<'peer when 'peer : comparison> =
  | TimeTick
  | SetId of 'peer
  | InMessage of ('peer * Message)
  | ForwardedMessage of ('peer * 'peer * Message)
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

let withChannel channel f state =
  let br =
    Map.tryFind channel state.broadcasts
    |> optionDefault
         { channel = channel ;
           masters = Set.empty ;
           lastSeen = state.curTick ;
           refs = 0 ;
           peers = Map.empty
         }
  in
  let state = { state with broadcasts = Map.add channel br state.broadcasts } in
  match state.myId with
  | Some id ->
     f id state br
     |> Return.map
          (fun br ->
            { state with broadcasts = Map.add br.channel br state.broadcasts }
          )
  | None -> Return.singleton state
  
let doMastersForChannel
      (channel : string)
      (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let peerlist = state.alive |> RBTree.head |> RBTree.iterSeq |> List.ofSeq in
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
  withChannel
    channel
    (fun id state br -> BroadcastInstance.doMasters masters state.curTick br)
    state

let doPingPeers
      (state : State<'peer>) :
      (State<'peer> * SideEffect<'peer> list) =
  state.broadcasts
  |> Map.toSeq
  |> Seq.fold
       (fun st (ch,_) ->
         Return.andThen
           (withChannel ch (fun id st br -> pingPeers br)) st
       )
       (state,[])

let doTick state =
  (doExpireBroadcasts { state with curTick = state.curTick + 1 })
  |> Return.andThen doPingPeers

let indicatePeerAlive (p : string) (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  let peerlist = state.alive |> RBTree.head |> RBTree.iterSeq |> List.ofSeq in
  if Some p <> state.myId then
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
    } |> Return.singleton
  else
    (state, [])
  
let isRetransmission
      (forwarded : ForwardKind)
      (p : string)
      (msg : Message)
      (state : State<'peer>) =
  state.broadcasts
  |> Map.tryFind msg.channel
  |> Option.map (BroadcastInstance.isRetransmission forwarded p msg)
  |> optionDefault false
  
let handlePacket
      (p : string)
      (forwarded : ForwardKind)
      (msg : Message)
      (state : State<'peer>) =
  let retx = isRetransmission forwarded p msg state in
  state |> withChannel
    msg.channel
    (fun id state br ->
      if retx then
        BroadcastInstance.handleRetransmission state.curTick br
      else
        BroadcastInstance.handlePacket id p forwarded state.curTick msg br
    )
  |> Return.andThen (doMastersForChannel msg.channel)
  |> Return.andThen (indicatePeerAlive p)
  |> (fun ret ->
       if not retx || Some p = state.myId then
         Return.command [UserMessage msg] ret
       else
         ret
     )

let nextPacket
      (p : string)
      (c : string)
      (s : int)
      (state : State<'peer>) : (State<'peer> * SideEffect<'peer> list) =
  withChannel
    c
    (fun id st br ->
      BroadcastInstance.withPeer
        Originated st.curTick p { channel = c ; seq = s ; data = None }
        (fun peer br ->
          let newPeer = { peer with outqueue = Map.remove s peer.outqueue } in
          let effects =
            newPeer.outqueue |> Map.toSeq |> Seq.truncate 1
            |> Seq.map
                 (fun (_,msg) ->
                   let m = { msg with seq = newPeer.outseq } in
                   OutPacket (newPeer.name, encodePacketForChannel m None)
                 )
            |> List.ofSeq
          in
          (newPeer, effects)
        )
        br
    )
    state
  
let update
      (msg : Msg<'peer>)
      (state : State<'peer>) :
      (State<'peer> * SideEffect<'peer> list) =
  match msg with
  | SetId p -> { state with myId = Some p } |> Return.singleton
  | JoinBroadcast b ->
     withChannel
       b
       (fun id st br ->
         ({ br with refs = br.refs + 1 }, [])
       )
       state
     |> Return.andThen (doMastersForChannel b)
  | LeaveBroadcast b ->
     withChannel
       b
       (fun id st br ->
         { br with refs = Util.max (br.refs - 1) 0 }
         |> Return.singleton
       )
       state
  | OutMessage b ->
     state.myId
     |> Option.map (fun id -> handlePacket id Originated b state)
     |> optionDefault (Return.singleton state)
  | InMessage (p,b) -> handlePacket p Received b state
  | ForwardedMessage (p,f,b) -> handlePacket p (Forwarded f) b state 
  | AddNode p -> indicatePeerAlive p state
  | Success (p,c,s) -> nextPacket p c s state
  | Failure p -> (state, [])
  | TimeTick -> doTick state
