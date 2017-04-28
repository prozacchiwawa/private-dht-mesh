#r "../node_modules/fable-core/Fable.Core.dll"

#load "./wrap/util.fs"
#load "./wrap/serialize.fs"
#load "./wrap/buffer.fs"
#load "./wrap/q.fs"
#load "./wrap/dns.fs"
#load "./wrap/ipaddr.fs"
#load "./wrap/network.fs"
#load "./wrap/bonjour.fs"
#load "./wrap/nodesocket.fs"
#load "./wrap/crypto.fs"
#load "./wrap/shortid.fs"
#load "./wrap/bacon.fs"
#load "./wrap/websocket.fs"
#load "./wrap/express.fs"
#load "./dhtdata.fs"
#load "./kbucket.fs"
#load "./dht.fs"
#load "./dhtrpc.fs"
 
open Util
open Buffer
open Bacon
open Network
open NodeSocket
open DHTData
open DHTRPC
   
(* Things to be done (kind of hard) 

 - Route collection
 - Local master election
 - dtls wrapper

 General plan:

 - We use the DHT library mostly unmodified but provide a socket object we control.
   - Our socket object doesn't even represent a network connection so much as a
   - delivery contract.  We don't use real IP addresses.
 - In the presence of a local peer, with nothing else, we bootstrap to them.
 - We store our known peers set at intervals.
 - We load them when we start up, trying to contact a subset.

Every node contains a full network map for now.  It could be improved.

*)

type InputEventDHT =
  | NoOp
  (* Tick timers *)
  | Tick
  (* A datagram on the socket (incoming) *)
  | Datagram of UDPMessage
  (* Send a request to a foreign DHT *)
  | QueryStart of (string * Buffer * Serialize.Json)
  (* Send a reply corresponding to a request *)
  | QueryReply of (string * NodeIdent * Serialize.Json)
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
             
let main args : unit =
  (* Get key from the environment *)
  let key = getenv "KEY" |> optionDefault "" in

  (* Our socket *)
  let udpsocket = dgram.Socket("udp4") in

  (* Set up the DHT system as an autonomous actor in our system *)
  let runDHT
        (macs : string list)
        (inputBus : bacon.Observable<InputEventDHT, unit>)
        (key : string) : bacon.Observable<OutputEventDHT, unit> =
    let macsStr = Buffer.toString "binary" (DHT.hashId (String.concat "|" macs)) in
    let dhtid = DHT.hashId (String.concat "|" [macsStr;key]) in
    let dhtkick =
      { DHT.defaultOpts with
          id = Some dhtid ;
          bootstrap =
            Array.map
              (fun a ->
                { NodeIdent.id = dhtid
                ; NodeIdent.host = a
                ; NodeIdent.port = 3327
                }
              )
              args
      }
    in
    let dht = DHT.init dhtkick in
    let dhtOps : DHTRPC.DHTOps<DHT.DHT> =
      { findnode = DHT._findnode
      ; query = DHT.query
      ; closest = DHT.closest
      ; harvest = DHTRPC.harvestDHT
      ; tick = DHT.tick
      ; dhtId = fun dht -> dht.id
      ; recv = DHT._onrequest
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
            | Tick ->
               DHTRPC.tick dhtOps !dhtrpc
            | Datagram msg ->
               msg.msg
               |> Buffer.toString "utf-8"
               |> Serialize.parse
               |> optionThen
                    (fun p ->
                      Serialize.field "id" p
                      |> optionMap (fun id -> (p,id))
                    )
               |> optionMap (fun (p,id) -> (p,Serialize.asString id))
               |> optionMap
                    (fun (p,id) ->
                      DHTRPC.recv
                        dhtOps
                        p
                        { NodeIdent.id = Buffer.fromString id "binary"
                        ; NodeIdent.host = msg.rinfo.address
                        ; NodeIdent.port = msg.rinfo.port
                        }
                        !dhtrpc
                    )
               |> optionDefault !dhtrpc
            | QueryStart (txid, id, body) ->
               let shortId = String.concat "|" [txid;ShortId.generate ()] in
               DHTRPC.directQuery
                 dhtOps
                 shortId
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
               (resultBus.push (SaveComplete v) ; !dhtrpc)
          in
          let (events, newDhtrpc) = DHTRPC.harvest !dhtrpc in
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
            )
            events
          |> ignore
        )
    in
    Bacon.busObservable resultBus

  let bonjour = Bonjour.newBonjour () in
  let _ =
    udpsocket.bind
      { NodeSocket.addr = "0.0.0.0" ;
        NodeSocket.port = 3327 ;
        NodeSocket.exclusive = true
      }
  in
  let (inputBus : bacon.Bus<InputEventDHT,unit>) = Bacon.newBus () in
  Network.get_interfaces_list ()
  |> Q.map
       (fun fid ->
         let _ = printfn "Starting DHT" in
         let macs =
           fid
           |> Seq.map (fun i -> i.desc)
           |> Seq.concat
           |> Seq.map (fun i -> i.mac)
           |> List.ofSeq
         in
         let outputBus = runDHT macs (Bacon.busObservable inputBus) key in
         let tickBus = Bacon.repeatedly 1000 [| Tick |] in
         let _ = inputBus.plug tickBus in
         let dgramBus = Bacon.newBus () in
         let _ =
           udpsocket.onMessage
             (fun dgram -> dgramBus.push (Datagram dgram) |> ignore)
         in
         let _ = inputBus.plug (Bacon.busObservable dgramBus) in
         let requestBus = Bacon.newBus () in
         let _ = inputBus.plug (Bacon.busObservable requestBus) in
         let _ =
           outputBus.onValue
             (fun v ->
               match v with
               | SendDatagram (body, nodeid) ->
                  udpsocket.sendString
                    (Serialize.stringify body)
                    { SendAddr.address = nodeid.host
                    ; SendAddr.port = nodeid.port
                    }
                  |> ignore
               | _ -> ()
             )
         in
         (requestBus, outputBus)
       )
  |> Q.andThen
       (fun ((requestBus, outputBus) : (bacon.Bus<InputEventDHT,unit> * bacon.Observable<OutputEventDHT,unit>)) ->
         let _ = printfn "Starting web server" in
         let doReply
               (requestBus : bacon.Bus<InputEventDHT,unit>)
               (arr : Serialize.Json array) ws =
           let nodeId =
             arr.[2]
             |> Serialize.field "id"
             |> optionMap Serialize.asString
             |> optionDefault ""
           in
           let nodeHost =
             arr.[2]
             |> Serialize.field "host"
             |> optionMap Serialize.asString
             |> optionDefault ""
           in
           let nodePort =
             arr.[2]
             |> Serialize.field "port"
             |> optionThen Serialize.asFloat
             |> optionDefault 0.0
             |> int
           in
           let nodeIdent =
             { NodeIdent.id =
                 Buffer.fromString nodeId "binary"
             ; NodeIdent.host = nodeHost
             ; NodeIdent.port = nodePort
             }
           in
           if String.length nodeId = 0 ||
                nodeHost = "" ||
                  nodePort = 0
           then
             let errResponse =
               Serialize.jsonObject
                 [| ("error", Serialize.jsonArray arr) |]
             in
             Express.wsSend
               (WebSocket.String (Serialize.stringify errResponse))
               ws
           else
             requestBus.push
               (QueryReply
                  ( Serialize.asString arr.[1]
                  , nodeIdent
                  , arr.[3]
                  )
               )
         in
         let doOutputMsg ws v =
           match v with
           | QueryPerform (txid, nid, body) ->
              Express.wsSend
                (WebSocket.String
                   (Serialize.stringify
                      (Serialize.jsonArray
                         [| Serialize.jsonString "REQUEST"
                          ; Serialize.jsonString txid
                          ; Serialize.jsonObject
                              [| ("id",
                                  Serialize.jsonString
                                    (Buffer.toString "binary" nid.id)
                                 )
                               ; ("host", Serialize.jsonString nid.host)
                               ; ("port", Serialize.jsonInt nid.port)
                              |]
                          ; body
                         |]
                      )
                   )
                )
                ws
           | QueryComplete (txid, nid, body) ->
              Express.wsSend
                (WebSocket.String
                   (Serialize.stringify
                      (Serialize.jsonArray
                         [| Serialize.jsonString "COMPLETE"
                          ; Serialize.jsonString txid
                          ; Serialize.jsonObject
                              [| ("id",
                                  Serialize.jsonString
                                    (Buffer.toString "binary" nid.id)
                                 )
                               ; ("host", Serialize.jsonString nid.host)
                               ; ("port", Serialize.jsonInt nid.port)
                              |]
                          ; body
                         |]
                      )
                   )
                )
                ws
           | QueryError (txid, id, error) ->
              Express.wsSend
                (WebSocket.String
                   (Serialize.stringify
                      (Serialize.jsonArray
                         [| Serialize.jsonString "ERROR"
                          ; Serialize.jsonString txid
                          ; Serialize.jsonString (Buffer.toString "binary" id)
                          ; Serialize.jsonString error
                         |]
                      )
                   )
                )
                ws
           | _ -> ()
         in
         let doWebSocketMsgInner ws arr =
           if (Array.length arr) > 0 then
             if Serialize.asString arr.[0] = "POST" &&
                  (Array.length arr) = 4
             then
               requestBus.push
                 (QueryStart
                    ( Serialize.asString arr.[1]
                    , Buffer.fromString
                        (Serialize.asString arr.[2])
                        "binary"
                    , arr.[3]
                    )
                 )
             else if Serialize.asString arr.[0] = "REPLY" &&
                       (Array.length arr) = 4
             then
               doReply requestBus arr ws
             else
               let errResponse =
                 Serialize.jsonObject
                   [| ("error", Serialize.jsonArray arr) |]
               in
               Express.wsSend
                 (WebSocket.String (Serialize.stringify errResponse))
                 ws
         in
         let doWebSocketMsg ws msg =
           (match msg with
            | WebSocket.String msg -> msg
            | WebSocket.Buffer msg -> Buffer.toString "binary" msg
           )
           |> Serialize.parse
           |> optionThen (Serialize.arrayMap id)
           |> optionMap (doWebSocketMsgInner ws)
           |> optionDefault ()
         in
         let app = Express.newApp () in
         app
         |> Express.ws
              "/v1/ep"
              (fun ws req ->
                Express.wsOnMessage
                  (doWebSocketMsg ws)
                  ws ;
                outputBus.onValue (doOutputMsg ws)
              )
         |> Express.listen 3000
       )
  |> Q.map
       (fun app ->
         bonjour.publish
           (Bonjour.serviceDesc "com.euso.DHTRPC" "com.euso.DHTRPC" 3327) ;
         Bonjour.find
           (Bonjour.serviceQueryByType "com.euso.DHTRPC")
           (printfn "Service %A")
           bonjour
       )
  |> Q.errThen (fun e -> dump "error" e |> ignore ; Q.value ())
  |> Q.fin
