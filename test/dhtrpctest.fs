module DHTRpcTest

open Util
open Buffer
open MochaTest
open DHTData
open KBucket
open DHTRPC
       
let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let (tests : (string * ((unit -> unit) -> unit)) list) =
  [ "We can make a query and receive a brief reply with the testnet" =>
      fun donef ->
        let ops = TestNetDHT.dhtOps in
        let testnet = TestNetDHT.init () in
        let targetIdStr = TestNetDHT.hostname 1 in
        let target = DHT.hashId targetIdStr in
        let sourceIdStr = TestNetDHT.hostname 0 in
        let source = DHT.hashId sourceIdStr in
        let query n =
          Serialize.jsonObject
            [| ("qtick", Serialize.jsonInt n) |]
        in
        let (events,testnet) =
          Seq.fold
            (fun ((events,testnet) : ((string * DHTRPC.DWQAction) list * TestNetDHT.TestSystem)) n ->
              let testnet =
                if n = 100 then
                  let txid = ShortId.generate () in
                  TestNetDHT.map
                    (DHTRPC.directQuery ops txid target (query n))
                    (Buffer.toString "binary" source)
                    testnet
                else
                  testnet
              in
              let testnet = TestNetDHT.tick testnet in
              let (nevents,testnet) = TestNetDHT.harvest testnet in
              let testnet =
                Seq.fold
                  (fun testnet evt ->
                    match evt with
                    | (who,QueryRequest (txid,peer,req)) ->
                       let reply =
                         Serialize.addField
                           "reply"
                           (Serialize.jsonBool true)
                           req
                       in
                       TestNetDHT.map
                         (DHTRPC.shortReply ops txid peer reply)
                         (Buffer.toString "binary" target)
                         testnet
                    | _ -> testnet
                  )
                  testnet
                  nevents
              in
              (nevents @ events, testnet)
            )
            ([], testnet)
            (Seq.init 500 id)
        in
        let response =
          List.fold
            (fun response evt ->
              match (response,evt) with
              | (_,(who,DHTRPC.QueryError e)) -> Some false
              | (None,(who,DHTRPC.QueryReply (id,target,resp))) ->
                 Serialize.field "reply" resp
                 |> Option.map (Serialize.truthy >> Some)
                 |> optionDefault response
              | _ -> response
            )
            None
            events
        in
        let _ = massert.ok (response = Some true) in
        donef () ;
    "Two nodes that can communicate should stay connected for a long time" =>
      fun donef ->
        let ops = TestNetDHT.dhtOps in
        let testnet = TestNetDHT.init () in
        let targetIdStr = TestNetDHT.hostname 1 in
        let target = DHT.hashId targetIdStr in
        let sourceIdStr = TestNetDHT.hostname 0 in
        let source = DHT.hashId sourceIdStr in
        let query n =
          Serialize.jsonObject
            [| ("qtick", Serialize.jsonInt n) |]
        in
        let (events,testnet) =
          Seq.fold
            (fun ((events,testnet) : ((string * DHTRPC.DWQAction) list * TestNetDHT.TestSystem)) n ->
              let testnet =
                if n >= 100 && n % 10 = 0 && n < 400 then
                  let txid = ShortId.generate () in
                  TestNetDHT.map
                    (DHTRPC.directQuery ops txid target (query n))
                    (Buffer.toString "binary" source)
                    testnet
                else
                  testnet
              in
              let testnet = TestNetDHT.tick testnet in
              let (nevents,testnet) = TestNetDHT.harvest testnet in
              let testnet =
                Seq.fold
                  (fun testnet evt ->
                    match evt with
                    | (who,QueryRequest (txid,peer,req)) ->
                       let reply =
                         Serialize.addField
                           "reply"
                           (Serialize.jsonBool true)
                           req
                       in
                       TestNetDHT.map
                         (DHTRPC.shortReply ops txid peer reply)
                         (Buffer.toString "binary" target)
                         testnet
                    | _ -> testnet
                  )
                  testnet
                  nevents
              in
              (nevents @ events, testnet)
            )
            ([], testnet)
            (Seq.init 500 id)
        in
        let response =
          List.fold
            (fun response evt ->
              match evt with
              | (who,DHTRPC.QueryError e) -> -1000
              | (who,DHTRPC.QueryReply (id,target,resp)) ->
                 if Serialize.field "reply" resp <> None then
                   response + 1
                 else
                   response
              | _ -> response
            )
            0
            events
        in
        let _ = massert.ok (response = 30) in
        donef ()
  ]
