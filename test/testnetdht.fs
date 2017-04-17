module TestNetDHT

open Util
open Buffer
open DHT
open DHTData
open KBucket
open TestNet

type TestDhtEvent =
  | Ready
  | Datagram of Serialize.Json * NodeIdent
  | Payload of Serialize.Json * NodeIdent
  | FindNode of Buffer * NodeIdent array

type TestSystem =
  { testnet : TestNet<DHTRPC.DWQAction, DHTRPC.DHTWithQueryProcessing<DHT.DHT>>
  ; iam : string
  }

(* 
 * each node has a host name of (string n), port 1, id hashId (string n)
 *)
let init =
  let nodes =
    Seq.fold
      (fun nodes n ->
        let host = string n in
        let id = DHT.hashId host in
        let sid = Buffer.toString "binary" id in
        let dht = DHT.init { DHT.defaultOpts with id = Some id } in
        let rpc = DHTRPC.init dht in
        Map.add
          sid
          rpc
          nodes
      )
      Map.empty
      (Seq.init 200 id)
  in
  let endpoints =
    Seq.fold
      (fun endpoints n ->
        let host = string n in
        let id = DHT.hashId host in
        let sid = Buffer.toString "binary" id in
        Map.add
          (host, 1)
          sid
          endpoints
      )
      Map.empty
      (Seq.init 200 id)
  in
  { iam = string 0
  ; testnet = TestNet.init nodes endpoints Map.empty
  }
