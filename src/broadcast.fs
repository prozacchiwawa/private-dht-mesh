module Broadcast

open Util
open Buffer
open RBTree
open Return
open KBucket
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
   
type IDAndBuckets<'peer> =
  { id : 'peer
  ; peers : KBucket<Buffer,Buffer>
  }

type State<'peer when 'peer : comparison> =
  { curTick : int
  ; myId : IDAndBuckets<'peer> option
  ; inactiveTimeout : int
  ; broadcasts : Map<string, BroadcastInstance<'peer>>
  }

let kbOps =
  { distance = KBucket.defaultDistance
  ; nodeId = id
  ; arbiter = fun a b -> a
  ; keyLength = Buffer.length
  ; keyNth = Buffer.at
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let numMasters = 4
  
let init inactiveTimeout =
  { curTick = 0 ;
    myId = None ;
    inactiveTimeout = inactiveTimeout ;
    broadcasts = Map.empty ;
  }

let peersOfChannel channel state =
  match Map.tryFind channel state.broadcasts with
  | Some br ->
     br.peers |> Map.toSeq |> Seq.map fst |> List.ofSeq
  | None -> []
  
let doExpireBroadcasts state =
  let expired =
    state.broadcasts
    |> Map.toSeq
    |> Seq.filter
         (fun (k,b) ->
           b.lastseen + state.inactiveTimeout < state.curTick ||
             b.refs <> 0
         )
    |> Map.ofSeq
  in
  { state with broadcasts = expired }
  |> Return.singleton

let withChannel channel f state =
  let haveChannel = Map.containsKey channel state.broadcasts in
  let br =
    Map.tryFind channel state.broadcasts
    |> optionDefault (defChannel channel state.curTick)
  in
  let state = { state with broadcasts = Map.add channel br state.broadcasts } in
  match state.myId with
  | Some id ->
     f id.id state br
     |> Return.andThen
          (fun br ->
            if haveChannel then
              (br,[])
            else
              let peerBuckets = id.peers in
              let channelId = stringKey br.channel in
              let peers =
                KBucket.closest
                  kbOps
                  peerBuckets
                  (Buffer.fromString channelId "hex")
                  numMasters
                  None
                |> Array.map (Buffer.toString "hex")
              in
              let _ = printfn "Computed masters: %A" peers in
              BroadcastInstance.doMasters id.id peers br
          )
     |> Return.map
          (fun br ->
            { state with broadcasts = Map.add br.channel br state.broadcasts }
          )
  | None -> Return.singleton state

let doMasters state =
  state.myId
  |> Option.map
       (fun id ->
         let peerBuckets = id.peers in
         state.broadcasts
         |> Map.toSeq
         |> Seq.map snd
         |> Seq.fold
              (fun st br ->
                let channelId = stringKey br.channel in
                let peers =
                  KBucket.closest
                    kbOps
                    peerBuckets
                    (Buffer.fromString channelId "hex")
                    numMasters
                    None
                  |> Array.map (Buffer.toString "hex")
                in
                Return.andThen
                  (fun st ->
                    BroadcastInstance.doMasters id.id peers br
                    |> Return.map
                         (fun br ->
                           { st with
                               broadcasts = Map.add br.channel br st.broadcasts
                           }
                         )
                  )
                  st
              )
              (Return.singleton state)
       )
  |> optionDefault (state, [])

let removeNode rid state =
  state.myId
  |> Option.map
       (fun id ->
         let peerBuckets = id.peers in
         state.broadcasts
         |> Map.toSeq
         |> Seq.map snd
         |> Seq.fold
              (fun st br ->
                Return.andThen
                  (fun st ->
                    BroadcastInstance.removeNode rid br
                    |> Return.map
                         (fun br ->
                           { st with
                               broadcasts = Map.add br.channel br st.broadcasts
                           }
                         )
                  )
                  st
              )
              (Return.singleton state)
       )
  |> optionDefault (state, [])
  
let rec update msg state =
  let _ = printfn "b %A" msg in
  match msg with
  | SetId id ->
     ({ state with myId = Some { id = id ; peers = KBucket.init (Buffer.fromString id "hex") } }, [])
     |> Return.andThen (update (AddNode id))
  | JoinBroadcast c ->
     withChannel
       c (fun id st br -> BroadcastInstance.doIntroduction id st.curTick br) state
  | SetMasters (c,m) ->
     withChannel
       c (fun id st br -> BroadcastInstance.setMasters id st.curTick m br) state
  | InUserMessage (c,text) ->
     withChannel
       c (fun id st br -> BroadcastInstance.doPublish id st.curTick text br) state
  | InPacket (peer,body) ->
     decodePacket peer body
     |> Option.bind
          (fun msg ->
            Serialize.field "c" body
            |> Option.map Serialize.asString
            |> Option.map (fun c -> (c,msg))
          )
     |> Option.map
          (fun (c,msg) ->
            withChannel
              c
              (fun id st br ->
                BroadcastInstance.receivePacket id st.curTick msg br
              )
              state
          )
     |> optionDefault (state, [])
  | AddNode newNode ->
     state.myId
     |> Option.map
          (fun id ->
            let (kb,eff) =
              KBucket.add
                kbOps id.peers (Buffer.fromString newNode "hex") None
            in
            let newId = { id with peers = kb } in
            ({ state with myId = Some newId }, [])
            |> Return.andThen doMasters
          )
     |> optionDefault (state, [])
  | RemoveNode oldNode ->
     state.myId
     |> Option.map
          (fun id ->
            let (kb,eff) =
              KBucket.remove
                kbOps id.peers (Buffer.fromString oldNode "hex") None
            in
            let newId = { id with peers = kb } in
            ({ state with myId = Some newId }, [])
            |> Return.andThen (removeNode oldNode)
            |> Return.andThen doMasters
          )
     |> optionDefault (state, [])
  | _ -> (state, [])
