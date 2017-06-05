module DHTRunner

open Util
open Buffer
open Bacon
open Network
open NodeSocket
open DHTData
open DHTRPC

type InputEventDHT =
  | NoOp
  (* Tick timers *)
  | Tick
  (* Start *)
  | Start
  (* A datagram on the socket (incoming) *)
  | Datagram of UDPMessage
  (* Send a request to a foreign DHT *)
  | QueryStart of (string * Buffer * Serialize.Json)
  (* Send a reply corresponding to a request *)
  | QueryReply of (string * NodeIdent * Serialize.Json)
  (* Add a node from an external source *)
  | AddNode of NodeIdent
  (* Write out the state *)
  | Save of string
     
type OutputEventDHT =
  (* Report a request from a foreign DHT *)
  | QueryPerform of (string * NodeIdent * Serialize.Json)
  (* Report a reply from a foreign DHT *)
  | QueryComplete of (string * NodeIdent * Serialize.Json)
  (* Report a query failure *)
  | QueryError of (string * Buffer * string)
  (* Send datagram *)
  | SendDatagram of (Serialize.Json * NodeIdent)
  (* Exception *)
  | Error of string
  (* Done with save operation *)
  | SaveComplete of string
  (* Report a new node *)
  | NodeAdded of NodeIdent

(* Set up the DHT system as an autonomous actor in our system *)
let runDHT
      (args : string array)
      (macs : string list)
      (inputBus : bacon.Observable<InputEventDHT, unit>)
      (key : string) : (Buffer * bacon.Observable<OutputEventDHT, unit>) =
  let macsStr = Buffer.toString "binary" (DHT.hashId (String.concat "|" macs)) in
  let dhtid = DHT.hashId (String.concat "|" [macsStr;key]) in
  let _ = printfn "DHTID %s" (Buffer.toString "hex" dhtid) in
  let dhtkick =
    { DHT.defaultOpts with
        id = Some dhtid ;
        bootstrap =
          Array.map
            (fun a ->
              let values = stringSplit "@" a in
              { NodeIdent.id = Buffer.fromString values.[0] "hex"
              ; NodeIdent.host = values.[1]
              ; NodeIdent.port = Ports.udpport
              }
            )
            args
    }
  in
  let dht = DHT.init dhtkick in
  let dhtOps : DHTRPC.DHTOps<DHT.DHT> =
    { findnode = DHT._findnode
    ; query = DHT.query
    ; getClosest = DHT.closest
    ; harvest = DHTRPC.harvestDHT
    ; tick = DHT.tick
    ; dhtId = fun dht -> dht.id
    ; recv = DHT._onrequest
    ; cancel = DHT._cancelRequest
    ; addNode = DHT._addNode
    }
  in
  let dhtrpc = ref (DHTRPC.init dht) in
  let (resultBus : bacon.Bus<OutputEventDHT,unit>) = Bacon.newBus () in
  let _ =
    inputBus.onValue
      (fun v ->
        let newDhtrpc =
          match v with
          | NoOp -> !dhtrpc
          | Start ->
             DHTRPC.map dhtOps DHT.bootstrap !dhtrpc
          | Tick ->
             DHTRPC.tick dhtOps !dhtrpc
          | Datagram msg ->
             (Buffer.toString "binary" msg.msg)
             |> Serialize.parse
             |> Option.map (fun p -> printfn "datagram %A" p ; p)
             |> Option.bind
                  (fun p ->
                    Serialize.field "id" p
                    |> Option.map (fun id -> (p,id))
                  )
             |> Option.map (fun (p,id) -> (p,Serialize.asString id))
             |> Option.map
                  (fun (p,id) ->
                    DHTRPC.recv
                      dhtOps
                      p
                      { NodeIdent.id = Buffer.fromString id "base64"
                      ; NodeIdent.host = msg.rinfo.address
                      ; NodeIdent.port = msg.rinfo.port
                      }
                      !dhtrpc
                  )
             |> optionDefault !dhtrpc
          | QueryStart (txid, id, body) ->
             DHTRPC.directQuery
               dhtOps
               txid
               id
               body
               !dhtrpc
          | QueryReply (qid, otherNode, body) ->
             DHTRPC.shortReply
               dhtOps
               qid
               otherNode
               body
               !dhtrpc
          | Save v ->
             (* XXX Reminder -- implement me *)
             (resultBus.push (SaveComplete v) ; !dhtrpc)
          | AddNode nid -> DHTRPC.addNode dhtOps nid !dhtrpc
        in
        let (events, newDhtrpc) = DHTRPC.harvest newDhtrpc in
        let _ = dhtrpc := newDhtrpc in
        List.map
          (fun e ->
            match e with
            | DHTRPC.QueryRequest (txid,nid,body) ->
               resultBus.push (QueryPerform (txid,nid,body))
            | DHTRPC.QueryReply (txid,nid,body) ->
               resultBus.push (QueryComplete (txid,nid,body))
            | DHTRPC.QueryError (txid,nid,error) ->
               resultBus.push (QueryError (txid,nid,error))
            | DHTRPC.SendDatagram (json,nid) ->
               resultBus.push (SendDatagram (json,nid))
            | DHTRPC.NodeAdded nid ->
               resultBus.push (NodeAdded nid)
            | DHTRPC.NodeRemoved nid -> ()
          )
          events
        |> ignore
      )
  in
  (dhtid, Bacon.busObservable resultBus)
