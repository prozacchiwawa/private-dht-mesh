module DHTRpcTest

open Util
open Buffer
open MochaTest
open DHTData
open KBucket
open DHTRPC
       
let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

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
        let dist = KBucket.defaultDistance SimpleFakeDHT.fakeKBucketOps a b in
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
                SimpleFakeDHT.fakeKBucketOps
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
        let ops = SimpleFakeDHT.dhtOps gotQuery in
        let (rpc : DHTRPC.DHTWithQueryProcessing<SimpleFakeDHT.FakeSystem>) =
          DHTRPC.init SimpleFakeDHT.allDhts
        in
        let targetIdStr = string 1 in
        let target = DHT.hashId targetIdStr in
        let query = Serialize.jsonObject [| |] in
        let (initdq : DHTRPC.DHTWithQueryProcessing<SimpleFakeDHT.FakeSystem>) =
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
        let ops = SimpleFakeDHT.dhtOps gotQuery in
        let (rpc : DHTRPC.DHTWithQueryProcessing<SimpleFakeDHT.FakeSystem>) =
          DHTRPC.init SimpleFakeDHT.allDhts
        in
        let targetIdStr = string 1 in
        let target = DHT.hashId targetIdStr in
        let query = Serialize.jsonObject [| |] in
        let (initdq : DHTRPC.DHTWithQueryProcessing<SimpleFakeDHT.FakeSystem>) =
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
  ; "We can make a query and receive a brief reply with the testnet" =>
      fun donef ->
        
        donef ()
  ]
