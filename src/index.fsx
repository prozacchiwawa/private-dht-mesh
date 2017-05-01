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
  (* Start *)
  | Start
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
             
let main argv : unit =
  (* Get key from the environment *)
  let key = getenv "KEY" |> optionDefault "" in

  (* Our socket *)
  let udpsocket = datagram.Socket("udp4") in

  let args = argv |> Seq.skip 1 |> Array.ofSeq in

  (* Set up the DHT system as an autonomous actor in our system *)
  let runDHT
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
      ; getClosest = DHT.closest
      ; harvest = DHTRPC.harvestDHT
      ; tick = DHT.tick
      ; dhtId = fun dht -> dht.id
      ; recv = DHT._onrequest
      ; cancel = DHT._cancelRequest
      }
    in
    let dhtrpc = ref (DHTRPC.init dht) in
    let (resultBus : bacon.Bus<OutputEventDHT,unit>) = Bacon.newBus () in
    let _ =
      inputBus.onValue
        (fun v ->
          let newDhtrpc =
            match dump "inbus" v with
            | NoOp -> !dhtrpc
            | Start ->
               DHTRPC.map dhtOps DHT.bootstrap !dhtrpc
            | Tick ->
               DHTRPC.tick dhtOps !dhtrpc
            | Datagram msg ->
               (Buffer.toString "binary" msg.msg)
               |> (fun txt -> printfn "raw %A" txt ; txt)
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
                     let _ = printfn "recv from %s" id in
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
               (resultBus.push (SaveComplete v) ; !dhtrpc)
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
            )
            events
          |> ignore
        )
    in
    (dhtid, Bacon.busObservable resultBus)

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
         let ((dhtid,outputBus) :
                (Buffer * bacon.Observable<OutputEventDHT,unit>)) =
           runDHT macs (Bacon.busObservable inputBus) key
         in
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
               match dump "outbus" v with
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
         (dhtid, requestBus, outputBus)
       )
  |> Q.andThen
       (fun ((dhtid, requestBus, outputBus) : (Buffer * bacon.Bus<InputEventDHT,unit> * bacon.Observable<OutputEventDHT,unit>)) ->
         let _ = printfn "Starting web server" in
         let doReply
               (requestBus : bacon.Bus<InputEventDHT,unit>)
               (arr : Serialize.Json array) ws =
           let nodeId =
             arr.[2]
             |> Serialize.field "id"
             |> Option.map Serialize.asString
             |> optionDefault ""
           in
           let nodeHost =
             arr.[2]
             |> Serialize.field "host"
             |> Option.map Serialize.asString
             |> optionDefault ""
           in
           let nodePort =
             arr.[2]
             |> Serialize.field "port"
             |> Option.bind Serialize.asFloat
             |> optionDefault 0.0
             |> int
           in
           let nodeIdent =
             { NodeIdent.id =
                 Buffer.fromString nodeId "hex"
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
         let wsSend (wss : Map<string,Express.WebSocket>) (m : Serialize.Json) :
               unit =
           wss
           |> Map.iter
                (fun wsid ws ->
                  Express.wsSend (WebSocket.String (Serialize.stringify m)) ws
                )
         in
         let doOutputMsg wsSend v =
           match dump "doOutputMsg" v with
           | QueryPerform (txid, nid, body) ->
              if txid = "test" then
                requestBus.push (QueryReply (txid,nid,body))
              else
                wsSend
                  (Serialize.jsonArray
                     [| Serialize.jsonString "REQUEST"
                     ; Serialize.jsonString txid
                     ; Serialize.jsonObject
                       [| ("id",
                           Serialize.jsonString
                             (Buffer.toString "base64" nid.id)
                        )
                       ; ("host", Serialize.jsonString nid.host)
                       ; ("port", Serialize.jsonInt nid.port)
                       |]
                     ; body
                     |]
                  )
           | QueryComplete (txid, nid, body) ->
              wsSend
                (Serialize.jsonArray
                   [| Serialize.jsonString "COMPLETE"
                    ; Serialize.jsonString txid
                    ; Serialize.jsonObject
                        [| ("id",
                            Serialize.jsonString
                              (Buffer.toString "base64" nid.id)
                           )
                         ; ("host", Serialize.jsonString nid.host)
                         ; ("port", Serialize.jsonInt nid.port)
                        |]
                    ; body
                   |]
                )
           | QueryError (txid, id, error) ->
              wsSend
                (Serialize.jsonArray
                   [| Serialize.jsonString "ERROR"
                    ; Serialize.jsonString txid
                    ; Serialize.jsonString (Buffer.toString "base64" id)
                    ; Serialize.jsonString error
                   |]
                )
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
                        "hex"
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
           let _ = printfn "Web Socket Msg %A" msg in
           (match msg with
            | WebSocket.String msg -> msg
            | WebSocket.Buffer msg -> Buffer.toString "utf-8" msg
           )
           |> Serialize.parse
           |> Option.bind (Serialize.arrayMap id)
           |> Option.map (doWebSocketMsgInner ws)
           |> optionDefault ()
         in
         let app = Express.newApp () in
         let (allSockets : Map<string,Express.WebSocket> ref) = ref Map.empty in
         let _ =
           outputBus.onValue (doOutputMsg (fun m -> wsSend !allSockets m))
         in
         app
         |> Express.ws
              "/v1/ep"
              (fun ws req ->
                let _ = printfn "New Websocket Connection!" in
                let wsid = ShortId.generate () in
                allSockets := Map.add wsid ws !allSockets ;
                Express.wsOnMessage
                  (doWebSocketMsg ws)
                  ws ;
                Express.wsOnClose
                  (fun _ -> allSockets := Map.remove wsid !allSockets)
                  ws
              )
         |> Express.listen 3000
         |> Q.map (fun app -> (dhtid, requestBus, app))
       )
  |> Q.map
       (fun (dhtid,requestBus,app) ->
         let dhtidArray = Buffer.toArray dhtid in
         let dhtidStr = String.concat "" (Array.map (sprintf "%02x") dhtidArray) in
         let serviceName = String.concat "." ["com.euso.DHTRPC";dhtidStr] in
         bonjour.publish
           (Bonjour.serviceDesc serviceName "com.euso.DHTRPC" 3327) ;
         Bonjour.find
           (Bonjour.serviceQueryByType "com.euso.DHTRPC")
           (printfn "Service %A")
           bonjour ;
         requestBus.push Start
       )
  |> Q.errThen (fun e -> (dump "error" (toString e)) |> ignore ; Q.value ())
  |> Q.fin
