module BroadcastService

open Util
open Buffer
open Bacon
open Broadcast
open DHTData
open BCastRunner

type InputMsg =
  | NoOp
  | AddNode of string
  | RemoveNode of string
  | RpcRequestIn of (string * Serialize.Json)
  | RpcRequestDone of (string * Serialize.Json)
  | RpcRequestFail of string
   
let passOnEffect
      (outputBus : bacon.Bus<(string * Serialize.Json),unit>) e s =
  match e with
  | RpcRequest (peer,body) -> (outputBus.push (peer,body) ; s)
  | WSSend (wsid,msg) ->
     match Map.tryFind wsid s.clients with
     | Some client -> (Express.wsSend (WebSocket.String msg) client.websocket ; s)
     | None -> s
                               
(* Must use SetId message on broadcast to set a local peerid *)
let serve
      (dhtid : string)
      (inputBus : bacon.Observable<InputMsg,unit>)
      (outputBus : bacon.Bus<(string * Serialize.Json),unit>)
      app =
  let _ = printfn "Serving nats" in
  let tickbus = Bacon.repeatedly 5000 [| () |] in
  let rbus = Bacon.newBus () in
  let rob = Bacon.busObservable rbus in
  let state = ref (init ()) in
  let _ =
    rob.onValue
      (fun v ->
        let newState = update v !state in
        state :=
          List.fold
            (fun s e -> passOnEffect outputBus e s)
            { newState with events = [] }
            (List.rev newState.events)
      )
  in
  let _ = tickbus.onValue (fun _ -> rbus.push (BCastRunner.Tick)) in
  let _ =
    inputBus.onValue
      (fun msg ->
        match msg with
        | AddNode nid -> rbus.push (BCastRunner.AddNode nid)
        | RpcRequestIn (peer,body) ->
           rbus.push (BCastRunner.RpcRequestIn (peer,body))
        | RpcRequestDone (peer,body) ->
           rbus.push (BCastRunner.RpcSuccess (peer,body))
        | RpcRequestFail peer ->
           rbus.push (BCastRunner.RpcFailure peer)
        | NoOp -> ()
      )
  in
  let _ = rbus.push (SetId dhtid) in
  Express.ws
    "/nats"
    (fun ws req ->
      let wsid = ShortId.generate () in
      let _ = rbus.push (NewSocket (wsid, ws)) in
      Express.wsOnMessage
        (fun msg ->
          match msg with
          | WebSocket.Buffer b -> rbus.push (WSReceive (wsid,b))
          | WebSocket.String s ->
             rbus.push (WSReceive (wsid,Buffer.fromString s "utf-8"))
        )
        ws ;
      Express.wsOnClose (fun _ -> rbus.push (SocketClosed wsid)) ws ;
    )
    app ;
  Express.files "/nats-demo" "./nats" app
