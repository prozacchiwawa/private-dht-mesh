module BroadcastInstance

open Util
open BroadcastData
     
type BroadcastPeer<'peer> =
  { name : 'peer
  (* The sequence number of the most recent datagram sent to this peer *)
  ; bseq : int
  ; lastping : int
  }

type BroadcastInstance<'peer when 'peer : comparison> =
  { channel : string
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

let setMasters tick m br =
  if br.lastintroduced <> tick && (Seq.length m) <> 0 then
    let effects =
      m |> List.map
             (fun p ->
               let encoded =
                 encodePacket (Ping { op = p ; peer = p ; channel = br.channel })
               in
               OutPacket (p,encoded)
             )
    in
    let _ = printfn "setMasters %A" m in
    ({ br with lastintroduced = tick ; masters = m |> Set.ofSeq }, effects)
  else
    (br, [])
  
let withPeer tick p f br =
  let defaultPeer = (initPeer tick p) in
  let peer = Map.tryFind p br.peers |> optionDefault defaultPeer in
  let br = { br with peers = Map.add peer.name peer br.peers } in
  f peer br
  |> Return.map
       (fun newPeer -> { br with peers = Map.add newPeer.name newPeer br.peers })
  |> Return.map
       (fun br ->
         let _ = printfn "withPeer after %A" (br.peers |> Map.toSeq |> Seq.map fst) in
         br
       )

let doIntroduction tick br =
  let effects = 
    Seq.map
      (fun p ->
        OutPacket (p,encodePacket (Ping { op = p ; channel = br.channel ; peer = p }))
      )
      br.masters
    |> List.ofSeq
  in
  if List.length effects <> 0 then
    ({ br with lastintroduced = tick }, List.concat [[BroadcastReady br.channel];effects])
  else
    (br, [])

let doPublish id tick text br =
  let s = br.seq in
  let toOwnSubs = [UserMessage { channel = br.channel ; data = text }] in
  let toMasters =
    br.masters
    |> Seq.filter (fun m -> m <> id)
    |> Seq.map
         (fun m ->
           [ OutPacket (m, encodePacket (Primary { seq = s ; peer = id ; channel = br.channel ; data = text }))
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
             OutPacket (m, encodePacket (Delivery { seq = s ; peer = id ; channel = br.channel ; data = text ; op = id }))
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
  
let receivePacket id tick msg br : (BroadcastInstance<'peer> * SideEffect<'peer> list) =
  match msg with
  | Ping msg ->
     (* Ping from this host *)
     withPeer
       tick msg.peer
       (fun peer br ->
         if Set.contains id br.masters then
           ({ peer with lastping = tick }, [])
         else
           let effects =
             br.masters
             |> Seq.filter (fun m -> m <> id)
             |> Seq.map
                  (fun m ->
                    OutPacket (m, encodePacket (Ping { op = msg.op ; peer = id ; channel = br.channel }))
                  )
             |> List.ofSeq
           in
           ({ peer with lastping = tick }, effects)
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
