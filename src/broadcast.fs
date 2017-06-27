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
   
type State<'peer when 'peer : comparison> =
  { curTick : int
  ; myId : 'peer option
  ; inactiveTimeout : int
  ; broadcasts : Map<string, BroadcastInstance<'peer>>
  }

let init inactiveTimeout =
  { curTick = 0 ;
    myId = None ;
    inactiveTimeout = inactiveTimeout ;
    broadcasts = Map.empty
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
  let br =
    Map.tryFind channel state.broadcasts
    |> optionDefault (defChannel channel state.curTick)
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
  
let update msg state =
  match msg with
  | SetId id -> ({ state with myId = Some id }, [])
  (state, [])
