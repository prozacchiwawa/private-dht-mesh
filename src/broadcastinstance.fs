module BroadcastInstance

open Util
open BroadcastData
     
type BroadcastPeer<'peer> =
  { name : 'peer
  (* The sequence number of the most recent datagram sent from the indicated peer
   * and received by this peer. *)
  ; bseq : int
  (* The last known contact with the indicated peer in Broadcast ticks. *)
  ; lastping : int
  }

type BroadcastInstance<'peer when 'peer : comparison> =
  { channel : string
  (* The masters of a broadcast are responsible for forwarding broadcast datagrams
   * to all broadcast peers.  Their peer lists are supposed to be close to
   * authoritative.
   *)
  ; masters : Set<'peer>
  ; lastseen : int
  ; seq : int
  ; refs : int
  ; peers : Map<'peer, BroadcastPeer<'peer>>
  ; lastintroduced : int
  }

let initPeer tick p =
  { name = p
  ; bseq = -1
  ; lastping = tick
  }
  
let defChannel channel tick =
  { channel = channel ;
    masters = Set.empty ;
    lastseen = tick ;
    refs = 0 ;
    peers = Map.empty ;
    lastintroduced = -1 ;
    seq = 0
  }

(* Given the sequence m of masters, ensure that br knows about those masters.
 * As a side effect, emit pings to masters that were not previously in the
 * broadcast's master set.
 *)
let setMasters id tick m br =
  let effects =
    if br.lastintroduced <> tick && (Seq.length m) <> 0 then
      m |> List.map
             (fun p ->
               let encoded =
                 encodePacket (Ping { op = id ; peer = id ; channel = br.channel })
               in
               OutPacket (p,encoded)
             )
    else
      []
  in
  let _ = printfn "setMasters %A" m in
  ({ br with lastintroduced = tick ; masters = m |> Set.ofSeq }, effects)

(* Given a tick and a peer name p, lookup or create the indicated peer indicating
 * a last contact instant of tick if created.
 *
 * Apply f peer and broadcast, and update the peer in br.
 *
 * Transforms a peer in br by f and resets the peer in br's collection.
 *)
let withPeer
      tick
      p
      (f : BroadcastPeer<'peer> ->
           BroadcastInstance<'peer> ->
           (BroadcastPeer<'peer> * SideEffect<'peer> list))
      (br : BroadcastInstance<'peer>) =
  let defaultPeer = (initPeer tick p) in
  let peer = Map.tryFind p br.peers |> optionDefault defaultPeer in
  let br = { br with peers = Map.add peer.name peer br.peers } in
  f peer br
  |> Return.map
       (fun newPeer -> { br with peers = Map.add newPeer.name newPeer br.peers })

(* Emit introductions to new broadcasts.  Signal ready if we're communicating with
 * something.
 *)
let doIntroduction id tick br =
  let effects = 
    Seq.map
      (fun p ->
        OutPacket
          (p,encodePacket (Ping { op = id ; channel = br.channel ; peer = id }))
      )
      br.masters
    |> List.ofSeq
  in
  if List.length effects <> 0 then
    ( { br with lastintroduced = tick }
    , List.concat [[BroadcastReady br.channel];effects]
    )
  else
    (br, [])

(* Publish a message to the channel.
 *)
let doPublish id tick text br =
  let s = br.seq in
  let toOwnSubs = [UserMessage { channel = br.channel ; data = text }] in
  let toMasters =
    br.masters
    |> Seq.filter (fun m -> m <> id)
    |> Seq.map
         (fun m ->
           [ OutPacket
               ( m
               , encodePacket
                   (Primary
                      { seq = s
                      ; peer = id
                      ; channel = br.channel
                      ; data = text
                      }
                   )
               )
           ]
         )
    |> Seq.concat
    |> List.ofSeq
  in
  let toPeers =
    if Set.contains id br.masters then
      br.peers
      |> Map.toSeq
      |> Seq.map (fun (k,_) -> k)
      |> Seq.filter (fun m -> m <> id)
      |> Seq.map
           (fun m ->
             OutPacket
               ( m
               , encodePacket
                   (Delivery
                      { seq = s
                      ; peer = id
                      ; channel = br.channel
                      ; data = text
                      ; op = id
                      }
                   )
               )
           )
      |> List.ofSeq
    else
      []
  in
  withPeer
    tick id
    (fun peer br ->
      ({ peer with bseq = s }, List.concat [toMasters;toPeers;toOwnSubs])
    )
    { br with seq = br.seq + 1 }

let doMasters (id : string) (peers : string array) br =
  let newMasters = Array.filter (fun p -> not (Set.contains p br.masters)) peers in
  let pings = 
    Seq.map
      (fun p ->
        OutPacket
          (p,encodePacket (Ping { op = id ; channel = br.channel ; peer = id }))
      )
      newMasters
    |> List.ofSeq
  in
  ({br with masters = Set.ofSeq peers }, pings)
  
let removeNode nid br =
  ({ br with
       masters = Set.remove nid br.masters ;
       peers = Map.remove nid br.peers
   }, [])

let receivePacket
      id tick msg br : (BroadcastInstance<'peer> * SideEffect<'peer> list) =
  match msg with
  | Ping msg ->
     (* Ping from this host *)
     withPeer
       tick msg.op
       (fun peer br ->
         let _ = printfn "%A ping from %A" id peer in
         if Set.contains id br.masters then
           ({ peer with lastping = tick }, [])
         else if peer.lastping <> tick then
           let effects =
             br.masters
             |> Seq.filter (fun m -> m <> id)
             |> Seq.map
                  (fun m ->
                    OutPacket
                      ( m
                      , encodePacket
                          (Ping
                             { op = msg.op
                             ; peer = id
                             ; channel = br.channel
                             }
                          )
                      )
                  )
             |> List.ofSeq
           in
           ({ peer with lastping = tick }, effects)
         else
           (peer, [])
       )
       br
  | Primary msg ->
     (* I am a master *)
     let ctor = if Set.contains id br.masters then M2M else P2M in
     let msg =
       { seq = msg.seq
       ; channel = br.channel
       ; peer = id
       ; data = msg.data
       ; op = msg.peer
       }
     in
     withPeer
       tick msg.op
       (fun peer br ->
         let _ = printfn "primary msg %A peer %A" msg peer in
         if msg.seq > peer.bseq then
           let toMasters =
             br.masters
             |> Seq.filter (fun m -> m <> id)
             |> Seq.map (fun m -> OutPacket (m, encodePacket (ctor msg)))
             |> List.ofSeq
           in
           let toPeers =
             if (Set.contains id br.masters) then
               br.peers
               |> Map.toSeq
               |> Seq.map (fun (k,_) -> k)
               |> Seq.filter (fun m -> m <> id)
               |> Seq.map (fun m -> OutPacket (m, encodePacket (Delivery msg)))
               |> List.ofSeq
             else
               []
           in
           let toOwnSubs =
             [UserMessage { channel = br.channel ; data = msg.data }]
           in
           ({ peer with
                bseq = msg.seq ;
                lastping = tick
            }, List.concat [toMasters;toPeers;toOwnSubs])
         else
           ({ peer with lastping = tick }, [])
       )
       br
  | P2M msg ->
     let msg =
       { seq = msg.seq
       ; channel = br.channel
       ; peer = id
       ; data = msg.data
       ; op = msg.op
       }
     in
     withPeer
       tick msg.op
       (fun peer br ->
         let effects =
           if msg.seq > peer.bseq then
             if Set.contains id br.masters then
               br.peers
               |> Map.toSeq
               |> Seq.map (fun (k,_) -> k)
               |> Seq.filter (fun m -> m <> id)
               |> Seq.map (fun m -> OutPacket (m, encodePacket (Delivery msg)))
               |> List.ofSeq               
             else
               br.masters
               |> Seq.filter (fun m -> m <> id)
               |> Seq.map (fun m -> OutPacket (m, encodePacket (P2M msg)))
               |> List.ofSeq
           else
             []
         in
         ({ peer with
              bseq = msg.seq ;
              lastping = tick
          }, effects)
       )
       br
  | M2M msg ->
     withPeer
       tick msg.op
       (fun peer br ->
         if msg.seq > peer.bseq then
           let effects =
             if Set.contains id br.masters then
               br.peers
               |> Map.toSeq
               |> Seq.map
                    (fun (k,_) ->
                      OutPacket (k,encodePacket (Delivery msg))
                    )
             else
               br.masters
               |> Seq.filter (fun m -> id <> m)
               |> Seq.map
                    (fun m ->
                      OutPacket (m,encodePacket (P2M { msg with peer = id }))
                    )
           in
           ({ peer with
                bseq = msg.seq ;
                lastping = tick
            }, effects |> List.ofSeq)
         else
           (peer, [])
       )
       br
  | Delivery msg ->
     withPeer
       tick msg.op
       (fun peer br ->
         let _ = printfn "delivery %A" peer in
         if msg.seq > peer.bseq then
           let (body : UserMsg) =
             { channel = br.channel ; data = msg.data }
           in
           let (effects : SideEffect<'peer> list) = [UserMessage body]
           in
           ({ peer with
                bseq = msg.seq ;
                lastping = tick
            }, effects)           
         else
           (peer, [])
       )
       br
