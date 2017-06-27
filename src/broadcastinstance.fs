module BroadcastInstance

open Util
open BroadcastData
     
type SideEffect<'peer> =
  | UserMessage of (string * string)
  | OutDatagram of (string * Serialize.Json)
                
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
  ; refs : int
  ; peers : Map<'peer, BroadcastPeer<'peer>>
  }

let initPeer tick p =
  { name = p
  ; bseq = 0
  ; lastping = tick
  }
  
let defChannel channel tick =
  { channel = channel ;
    masters = Set.empty ;
    lastseen = tick ;
    refs = 0 ;
    peers = Map.empty
  }
  
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
