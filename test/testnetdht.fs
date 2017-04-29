module TestNetDHT

open Util
open Buffer
open DHT
open DHTData
open KBucket
open TestNet

type TestDhtEvent = (string * DHTRPC.DWQAction)

type TestSystem =
  { numnodes : int
  ; testnet : TestNet<DHTRPC.DWQAction, DHTRPC.DHTWithQueryProcessing<DHT.DHT>>
  ; idtonet : Map<string, (string * int)>
  ; events : TestDhtEvent list
  ; tick : int
  }

let hostname n =
  sprintf "0.0.0.%d" n
    
(* 
 * each node has a host name of (0.0.0.n), port 1, id hashId (0.0.0.n)
 *)
let init () =
  let numnodes = 20 in
  let bootstrapHost = hostname 7 in
  let bootstrapId = DHT.hashId bootstrapHost in
  let nodes =
    Seq.fold
      (fun nodes n ->
        let host = hostname n in
        let id = DHT.hashId host in
        let sid = Buffer.toString "binary" id in
        let dht =
          DHT.init
            { DHT.defaultOpts with
                id = Some id ;
                bootstrap =
                  [| { id = bootstrapId ; host = bootstrapHost ; port = 1 } |]
            }
          |> DHT.bootstrap
        in
        let rpc = DHTRPC.init dht in
        Map.add
          sid
          rpc
          nodes
      )
      Map.empty
      (Seq.init numnodes id)
  in
  let endpoints =
    Seq.fold
      (fun endpoints n ->
        let host = hostname n in
        let id = DHT.hashId host in
        let sid = Buffer.toString "binary" id in
        Map.add
          (host, 1)
          sid
          endpoints
      )
      Map.empty
      (Seq.init numnodes id)
  in
  let (idtonet : Map<string, (string * int)>) =
    endpoints
    |> Map.toSeq
    |> Seq.map (fun (k,v) -> (v,k))
    |> Map.ofSeq
  in
  { numnodes = numnodes
  ; idtonet = idtonet
  ; testnet = TestNet.init nodes endpoints Map.empty
  ; events = []
  ; tick = 0
  }

let harvestDHTRPC self id rpc =
  let isDatagram e =
    match e with
    | DHTRPC.SendDatagram (j,n) -> true
    | _ -> false
  in
  let txDatagram e =
    match e with
    | DHTRPC.SendDatagram (j,n) ->
       let source = Map.find id self.idtonet in
       let dest = (n.host,n.port) in
       let body = Buffer.fromString (Serialize.stringify j) "utf-8" in
       [ { source = source ; dest = dest ; body = body } ]
    | _ -> []
  in
  let (events,rpc) = DHTRPC.harvest rpc in
  let (datagrams,events) = List.partition isDatagram events in
  ( events |> List.map (fun e -> (id,e))
  , datagrams |> List.map txDatagram |> List.concat
  , rpc
  )
    
let map f nid self =
  { self with
      testnet =
        TestNet.map
          (harvestDHTRPC self)
          f
          nid
          self.testnet
  }

let dhtOps : DHTRPC.DHTOps<DHT.DHT> =
  { findnode = DHT._findnode
  ; query = DHT.query
  ; getClosest = DHT.closest
  ; harvest = DHTRPC.harvestDHT
  ; tick = DHT.tick
  ; dhtId = fun dht -> dht.id
  ; recv = DHT._onrequest
  }

let tick self =
  let self = { self with tick = self.tick + 1 } in
  let self =
    if self.tick % 16 = 1 then
      { self with
          testnet =
            TestNet.mapall 
              (harvestDHTRPC self)
              (DHTRPC.tick dhtOps)
              self.testnet
      }
    else
      self
  in
  { self with
      testnet =
        TestNet.tick
          (fun (host,port) body node ->
            Map.tryFind (host,port) self.testnet.endpoints
            |> Option.bind
                 (fun id ->
                   let bodyStr = Buffer.toString "utf-8" body in
                   bodyStr
                   |> Serialize.parse
                   |> Option.map
                        (fun body ->
                          ({ NodeIdent.id = Buffer.fromString id "binary"
                           ; NodeIdent.host = host
                           ; NodeIdent.port = port
                           }
                          , body
                          )
                        )
                 )
            |> Option.map
                 (fun (source,body) ->
                   DHTRPC.recv
                     dhtOps
                     body
                     source
                     node
                 )
            |> optionDefault node
          )
          (harvestDHTRPC self)
          self.testnet
  }

let harvest self =
  let (events, testnet) = TestNet.harvest self.testnet in
  (events, { self with testnet = testnet })
