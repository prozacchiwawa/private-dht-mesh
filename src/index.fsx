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
#load "./wrap/rbtree.fs"
#load "./ports.fs"
#load "./dhtdata.fs"
#load "./kbucket.fs"
#load "./dht.fs"
#load "./dhtrpc.fs"
#load "./elm-return/respond.fs"
#load "./elm-return/return.fs"
#load "./broadcast.fs"
#load "./dhtrunner.fs"
#load "./basicwsservice.fs"
#load "./bonjourservice.fs"
#load "./bcastrunner.fs"
#load "./broadcastservice.fs"
 
open Util
open Buffer
open Bacon
open Network
open NodeSocket
open DHTData
open DHTRPC
open DHTRunner
   
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

let main argv : unit =
  (* Get key from the environment *)
  let key = getenv "KEY" |> optionDefault "" in

  (* Our socket *)
  let udpsocket = datagram.Socket("udp4") in

  let args = argv |> Seq.skip 1 |> Array.ofSeq in

  let app = Express.newApp () in

  let _ =
    udpsocket.bind
      { NodeSocket.addr = "0.0.0.0" ;
        NodeSocket.port = Ports.udpport ;
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
           runDHT args macs (Bacon.busObservable inputBus) key
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
         (dhtid, requestBus, outputBus)
       )
  |> Q.map
       (fun (dhtid, requestBus, outputBus) ->
         BasicWSService.serve dhtid requestBus outputBus app ;
         (dhtid, requestBus, outputBus)
       )
  |> Q.map
       (fun (dhtid, toDHTBus, fromDHTBus) ->
         let bserviceBus = Bacon.newBus () in
         let brequestBus = Bacon.newBus () in
         let _ =
           fromDHTBus.onValue
             (fun evt ->
               match evt with
               | NodeAdded nid ->
                  bserviceBus.push (BroadcastService.AddNode nid)
               | QueryPerform (qid, nid, json) ->
                  let _ = printfn "QueryPerform %A" json in
                  let body = Serialize.jsonObject [| |] in
                  let _ = toDHTBus.push (QueryReply (qid, nid, body)) in
                  bserviceBus.push
                    (BroadcastService.RpcRequestIn
                       (Buffer.toString "hex" nid.id, json)
                    )
               | _ -> ()
             )
         in
         let _ =
           (Bacon.busObservable brequestBus).onValue
             (fun (peer,body) ->
               let qid = ShortId.generate () in
               toDHTBus.push
                 (QueryStart (qid, Buffer.fromString peer "hex", body))
             )
         in
         BroadcastService.serve
           (Buffer.toString "hex" dhtid)
           (Bacon.busObservable bserviceBus)
           brequestBus
           app ;
         (dhtid, toDHTBus, fromDHTBus)
       )
  |> Q.map (fun (dhtid,requestBus,outputBus) ->
         BonjourService.serve dhtid requestBus ;
         (dhtid, requestBus, outputBus)
       )
  |> Q.map (fun _ -> Express.listen Ports.webport app)
  |> Q.errThen (fun e -> (dump "error" (toString e)) |> ignore ; Q.value ())
  |> Q.fin
