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
  let previousNodes n system =
    Seq.init
      (n - 1)
      (fun nn ->
        let id = DHT.hashId (string (system.dhts.Count - nn - 1)) in
        let idString = Buffer.toString "binary" id in
        (Map.find idString system.dhts).node
      )
  in
  let newId = string n in
  let id = DHT.hashId newId in
  let idString = Buffer.toString "binary" id in
  let fakeNode =
    { id = id
    ; host = newId
    ; port = 9999
    }
  in
  let fakeDht =
    let nodes =
      Seq.fold
        (fun nodes node ->
          let (nodes,events) =
            KBucket.add
              fakeKBucketOps
              nodes
              node
              None
          in
          nodes
        )
        (KBucket.init id)
        (previousNodes n system)
    in
    { node = fakeNode
    ; nodes = nodes
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
    (Seq.init 200 id)

let harvest system =
  Map.tryFind system.iam system.dhts
  |> optionMap
       (fun dht ->
         dht.events
         |> Seq.map
              (fun evt ->
                match evt with
                | FindNode (id,nodes) -> [DHTRPC.Find (id,nodes)]
                | Datagram (json,tgt) -> [DHTRPC.Datagram (json,tgt)]
                | Payload (json,tgt) -> [DHTRPC.Payload ((dump "r" json),tgt)]
                | Ready -> [DHTRPC.Bootstrapped]
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
               Array.map
                 (fun n ->
                   { NodeIdent.id = n.id
                   ; NodeIdent.host = n.host
                   ; NodeIdent.port = n.port
                   }
                 )
                 closest
             )
        |> optionThen
             (fun idArray ->
               Map.tryFind system.iam system.dhts
               |> optionMap (fun dht -> (idArray,dht))
             )
        |> optionMap
             (fun (idArray,dht) ->
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
        let qid = ShortId.generate () in
        let query =
          query
          |> Serialize.addField "qid" (Serialize.jsonString qid)
          |> Serialize.addField "id" (Serialize.jsonString system.iam)
        in
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
  ; dhtId =
      fun system ->
        Map.find system.iam system.dhts
        |> (fun dht -> dht.node.id)
  }
                                                   
let (tests : (string * ((unit -> unit) -> unit)) list) =
  [ "closest?" =>
      fun donef ->
        let a = Buffer.fromArray [|0xaa;0x55|] in
        let b = Buffer.fromArray [|0xaa;0x5a|] in
        let c = Buffer.fromArray [|0xa5;0x55|] in
        let nodes =
          [|a;b;c|]
          |> Array.map
               (fun a ->
                 { NodeIdent.id = a
                 ; NodeIdent.host = Buffer.toString "binary" a
                 ; NodeIdent.port = 0
                 }
               )
        in
        let sorted = DHTRPC.sortClosest b nodes in
        let _ = massert.ok (sorted = [|nodes.[1];nodes.[0];nodes.[2]|]) in
        donef ()
  ; "distance?" =>
      fun donef ->
        let a = Buffer.fromArray [|0x55;0xaa|] in
        let b = Buffer.fromArray [|0x56;0xff|] in
        let dist = KBucket.defaultDistance fakeKBucketOps a b in
        let _ = massert.ok (dist = [|3;0x55|]) in
        donef ()
  ; "determineBucket" =>
      fun donef ->
        let a = Buffer.fromArray [|0x55;0xaa|] in
        let which = [|-1;1;-1;1;-1;1;-1;1;1;-1;1;-1;1;-1;1;-1|] in
        let determined =
          Seq.map
            (fun i ->
              KBucket.determineBucket
                fakeKBucketOps
                a
                (Some i)
            )
            (Seq.init (Array.length which) id)
          |> Array.ofSeq
        in
        let _ = massert.ok (determined = which) in
        donef ()
  ; "query sends a query to a target node" =>
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
        let rdq =
          Seq.fold
            (fun rpc _ -> DHTRPC.tick ops rpc)
            initdq
            (Seq.init 16 id)
        in
        let _ = massert.ok !q in
        donef ()
  ; "We can make a query and receive a brief reply" =>
      fun donef ->
        let gotQuery target query dht =
          query
          |> dump "request"
          |> Serialize.addField
               "target"
               (Serialize.field "id" query
                |> optionDefault (Serialize.jsonNull ())
               )
          |> Serialize.addField
               "rid"
               (Serialize.field "qid" query
                |> optionDefault (Serialize.jsonNull ())
               )
          |> Serialize.addField
               "response"
               (Serialize.jsonBool true)
          |> dump "response"
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
        let (events,rdq) =
          Seq.fold
            (fun (events,rpc) _ ->
              let rpc = DHTRPC.tick ops rpc in
              let (nevents,rpc) = DHTRPC.harvest rpc in
              (nevents @ events, rpc) 
            )
            ([], initdq)
            (Seq.init 16 id)
        in
        let response =
          List.fold
            (fun response evt ->
              match evt with
              | DHTRPC.QueryReply (id,target,resp) ->
                 Serialize.field "response" resp
                 |> optionMap Serialize.truthy
                 |> optionDefault response
              | _ -> response
            )
            false
            events
        in
        let _ = massert.ok response in
        donef ()
  ]
