module DHTRpcTest

open Util
open Buffer
open MochaTest
open DHTData
open KBucket
open DHTRPC
       
let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

type FakeNode =
  { id : Buffer
  ; host : string
  ; port : int
  }

type FakeDhtEvent =
  | Ready
  | Datagram of Serialize.Json * NodeIdent
  | Payload of Serialize.Json * NodeIdent
  | FindNode of Buffer * NodeIdent array

type FakeDht =
  { node : FakeNode
  ; nodes : KBucket<Buffer,FakeNode>
  ; events : FakeDhtEvent list
  }

type FakeSystem =
  { dhts : Map<string, FakeDht>
  ; iam : string
  }

let fakeKBucketOps =
  { distance = KBucket.defaultDistance
  ; nodeId = fun n -> n.id
  ; arbiter = fun a b -> a
  ; keyNth = Buffer.at
  ; keyLength = Buffer.length
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let addNode n system =
  let newId = (string n) in
  let id = DHT.hashId newId in
  let idString = Buffer.toString "binary" id in
  let fakeNode =
    { id = id
    ; host = newId
    ; port = 9999
    ; 
    }
  in
  let fakeDht =
    { node = fakeNode
    ; nodes = KBucket.init id
    ; events = [Ready]
    }
  in
  let dhtsWithNode =
    let addNodePlease v =
      let (newNodes,events) =
        KBucket.add
          fakeKBucketOps
          v.nodes
          fakeNode
          None
      in
      newNodes
    in
    Map.map
      (fun k v -> { v with nodes = addNodePlease v })
      system.dhts
  in
  { system with
      dhts = Map.add idString fakeDht dhtsWithNode ;
      iam = idString
  }

let allDhts =
  let xid = DHT.hashId (string 0) in
  let idString = Buffer.toString "binary" xid in
  Seq.fold
    (fun s n -> addNode n s)
    { iam = idString
    ; dhts = Map.empty
    }
    (Seq.init 100 id)

let harvest system =
  Map.tryFind system.iam system.dhts
  |> optionMap
       (fun dht ->
         dht.events
         |> Seq.map
              (fun evt ->
                match evt with
                | Datagram (json,tgt) -> [DHTRPC.Datagram (json,tgt)]
                | Payload (json,tgt) -> [DHTRPC.Payload (json,tgt)]
                | Ready -> [DHTRPC.Bootstrapped]
                | _ -> []
              )
         |> Seq.concat
         |> List.ofSeq
        ,{ system with
             dhts =
               Map.add
                 (Buffer.toString "binary" dht.node.id)
                 { dht with events = [] }
                 system.dhts
         }
       )
  |> optionDefault ([], system)
       
let dhtOps qreply : DHTRPC.DHTOps<FakeSystem> =
  { findnode =
      fun what target system ->
        let which =
          target
          |> optionMap (Buffer.toString "binary")
          |> optionDefault system.iam
        in
        Map.tryFind which system.dhts
        |> optionMap
             (fun dht ->
               let closest =
                 KBucket.closest
                   fakeKBucketOps
                   dht.nodes
                   what
                   8
                   None
               in
               let idArray =
                 Array.map
                   (fun n ->
                     { NodeIdent.id = n.id
                     ; NodeIdent.host = n.host
                     ; NodeIdent.port = n.port
                     }
                   )
                   closest
               in
               { system with
                   dhts =
                     Map.add
                       (Buffer.toString "binary" dht.node.id)
                       { dht with events = FindNode (what, idArray) :: dht.events }
                       system.dhts
               }
             )
        |> optionDefault system
  ; query =
      fun inFlight target query system ->
        let exists =
          Map.tryFind (Buffer.toString "binary" target) system.dhts <> None
        in
        Map.tryFind system.iam system.dhts
        |> optionMap
             (fun dht ->
               let reply = qreply target query system in
               let replyFrom =
                 { NodeIdent.id = dht.node.id
                 ; NodeIdent.host = dht.node.host
                 ; NodeIdent.port = dht.node.port
                 }
               in
               { system with
                   dhts =
                     Map.add
                       (Buffer.toString "binary" dht.node.id)
                       { dht with
                           events = (Payload (reply,replyFrom)) :: dht.events
                       }
                       system.dhts
               }
             )
        |> optionDefault system
  ; closest =
      fun n what system ->
        Map.tryFind system.iam system.dhts
        |> optionMap
             (fun dht ->
               KBucket.closest fakeKBucketOps dht.nodes what n None
               |> Array.map
                    (fun n ->
                      { NodeIdent.id = n.id
                      ; NodeIdent.host = n.host
                      ; NodeIdent.port = n.port
                      }
                    )
             )
        |> optionDefault [| |]
  ; harvest = harvest
  ; tick = id
  }
                                                   
let (tests : (string * ((unit -> unit) -> unit)) list) =
  [ "query sends a query to a target node" =>
      fun donef ->
        let q = ref false in
        let gotQuery target query dht =
          let _ = q := true in
          Serialize.jsonObject [| |]
        in
        let ops = dhtOps gotQuery in
        let (rpc : DHTRPC.DHTWithQueryProcessing<FakeSystem>) =
          DHTRPC.init allDhts
        in
        let targetIdStr = string 1 in
        let target = DHT.hashId targetIdStr in
        let query = Serialize.jsonObject [| |] in
        let (initdq : DHTRPC.DHTWithQueryProcessing<FakeSystem>) =
          DHTRPC.directQuery ops target query rpc
        in
        let _ = massert.ok !q in
        donef ()
  ]
