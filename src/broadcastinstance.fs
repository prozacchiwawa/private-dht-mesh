module BroadcastInstance

open Util
open BroadcastData
     
type BroadcastPeer<'peer> =
  { name : 'peer
  ; inseq : int
  ; outseq : int
  ; outqueue : Map<int, Message>
  ; lastping : int
  }

type BroadcastInstance<'peer when 'peer : comparison> =
  { channel : string
  ; masters : Set<'peer>
  ; lastSeen : int
  ; refs : int
  ; peers : Map<'peer, BroadcastPeer<'peer>>
  }

let initPeer forwarded tick p msg =
  { name = p
  ; outseq = 0
  ; outqueue = Map.empty
  ; inseq = if forwarded <> Originated then msg.seq else -1
  ; lastping = tick
  }  

let isRetransmission
      (forwarded : ForwardKind)
      (p : 'peer)
      (msg : Message)
      (br : BroadcastInstance<'peer>) =
  Map.tryFind p br.peers
  |> Option.map (fun p -> forwarded <> Originated && p.inseq >= msg.seq)
  |> optionDefault false

let pingPeers br =
  Map.toSeq br.peers
  |> Seq.map
       (fun (_,p) ->
         let fromQueue =
           p.outqueue |> Map.toSeq |> Seq.truncate 1
           |> Seq.map (fun (_,o) -> (p,o)) |> List.ofSeq
         in
         [fromQueue; [(p,{ channel = br.channel ; seq = -1 ; data = None })]]
         |> Seq.concat |> Seq.truncate 1
       )
  |> Seq.concat             
  |> Seq.fold
       (fun br (p,msg) ->
         Return.andThen
           (fun br ->
             (br, [OutPacket (p.name,encodePacketForChannel { msg with seq = p.outseq } None)])
           )
           br
       )
       (br,[])
  
let withPeer fwd tick p msg f br =
  let defaultPeer = (initPeer fwd tick p msg) in
  let peer = Map.tryFind p br.peers |> optionDefault defaultPeer in
  let br = { br with peers = Map.add peer.name peer br.peers } in
  f peer br
  |> Return.map
       (fun newPeer -> { br with peers = Map.add newPeer.name newPeer br.peers })
  
let doMasters masters tick br =
  masters
  |> Seq.filter (fun m -> Map.tryFind m br.peers = None)
  |> Seq.fold
       (fun brret m ->
         let msg = { channel = br.channel ; seq = -1 ; data = None } in
         Return.map
           (fun br ->
             { br with
                 peers =
                   Map.add
                     m
                     (initPeer Originated tick m msg)
                     br.peers
             }
           )
           brret
       )
       ({ br with masters = masters }, [])
  
let internMsg
      (myId : 'peer)
      (tick : int)
      (peerFrom : string)
      (f : ForwardKind)
      (msg : Message)
      (br : BroadcastInstance<string>) =
  let iAmMaster = Set.contains myId br.masters in
  let (forwardToPeers : seq<string>) =
    (if iAmMaster then
       br.peers
       |> Map.toSeq
       |> Seq.map fst
     else
       br.masters
       |> Set.toSeq
       |> Seq.filter (fun m -> m <> myId)
    )
  in
  forwardToPeers
  |> Seq.filter (fun m -> m <> peerFrom)
  |> Seq.fold
       (fun brret peer ->
         let addMsg peer =
           msg.data <> None || (f <> Originated && f <> Received)
         in
         let retx peer = peer.inseq >= msg.seq && (f <> Originated) in
         let nseq peer = peer.outseq + 1 in
         Return.andThen
           (withPeer
              f tick peer msg
              (fun peer br ->
                let wasEmpty = Map.isEmpty peer.outqueue in
                let outmsg = { msg with seq = nseq peer } in
                let newElts =
                  if (addMsg peer) && msg.data <> None then [ outmsg ] else []
                in
                let newQueue =
                  Seq.fold
                    (fun oq e -> Map.add outmsg.seq e oq)
                    peer.outqueue
                    newElts
                in
                let newPeer =
                  { peer with
                      inseq =
                        (if f <> Originated && msg.seq <> -1 then
                           msg.seq
                         else
                           peer.inseq
                        ) ;
                      outseq = outmsg.seq ;
                      outqueue = newQueue ;
                      lastping = if addMsg peer then tick else peer.lastping
                  }
                in
                let fromOpt =
                  match f with
                  | Forwarded f -> Some f
                  | _ -> None
                in
                let effects =
                  newPeer.outqueue
                  |> Seq.truncate (if retx newPeer then 0 else 1)
                  |> Seq.map
                       (fun q ->
                         OutPacket
                           (newPeer.name,encodePacketForChannel outmsg fromOpt)
                       )
                  |> List.ofSeq
                in
                (newPeer, effects)
              )
           )
           brret
       )
       (Return.singleton br)
  
let handlePacket
      (id : 'peer)
      (p : 'peer)
      (forwarded : ForwardKind)
      (tick : int)
      (msg : Message)
      (br : BroadcastInstance<'peer>) =
  { br with lastSeen = tick }
  |> withPeer
       forwarded tick p msg
       (fun p br ->
         let newPeer =
           { p with
               inseq =
                 if forwarded <> Originated then msg.seq else p.inseq
           }
         in
         (newPeer, [])
       )
  |> Return.andThen (internMsg id tick p forwarded msg)

let handleRetransmission
      (tick : int)
      (br : BroadcastInstance<string>) =
  Return.singleton { br with lastSeen = tick }
  
