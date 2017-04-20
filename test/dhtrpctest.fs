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
        let ops = TestNetDHT.dhtOps in
        let testnet = TestNetDHT.init () in
        let targetIdStr = TestNetDHT.hostname 1 in
        let target = DHT.hashId targetIdStr in
        let sourceIdStr = TestNetDHT.hostname 0 in
        let source = DHT.hashId sourceIdStr in
        let query = Serialize.jsonObject [| |] in
        let (events,testnet) =
          Seq.fold
            (fun ((events,testnet) : ((string * DHTRPC.DWQAction) list * TestNetDHT.TestSystem)) n ->
              let _ =
                if n = 100 then
                  for i = 0 to 7 do
                    begin
                      let dhtrpc =
                        Map.find
                          (Buffer.toString
                             "binary"
                             (DHT.hashId (TestNetDHT.hostname i))
                          )
                          testnet.testnet.nodes
                      in
                      DHTVis.writeFile
                        (fun (n : DHTData.Node) -> n.id)
                        (fun (n : DHTData.Node) ->
                          String.concat
                            " "
                            [string dhtrpc.dht._tick;n.host]
                        )
                        (TestNetDHT.hostname i)
                        dhtrpc.dht.nodes
                    end
              in
              let testnet =
                if n = 100 then
                  let txid = ShortId.generate () in
                  TestNetDHT.map
                    (DHTRPC.directQuery ops txid target query)
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
                 |> optionMap (Serialize.truthy >> Some)
                 |> optionDefault response
              | _ -> response
            )
            None
            events
        in
        let _ = massert.ok (response = Some true) in
        donef ()
  ]
