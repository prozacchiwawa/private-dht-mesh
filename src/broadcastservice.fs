module BroadcastService

open Broadcast
open NodeSocket
open DHTData

type InputMsg =
  | NoOp
  | AddNode of NodeIdent
  | RpcRequest of Serialize.Json

let doWebSocketMsg ws msg =
  let msgString =
    match msg with
    | WebSocket.String s -> s
    | WebSocket.Buffer b -> Buffer.toString "utf-8" b
  in
  Express.wsSend (WebSocket.String msgString) ws
  
let serve dhtid requestBus outputBus app =
  let _ = printfn "Serving nats" in
  let (allSockets : Map<string,Express.WebSocket> ref) = ref Map.empty in
  let bsys = Broadcast.init 15000 in
  let bInput = Bacon.newBus () in
  Express.ws
    "/v1/nats"
    (fun ws req ->
      let wsid = ShortId.generate () in
      allSockets := Map.add wsid ws !allSockets ;
      Express.wsOnMessage (doWebSocketMsg ws) ws ;
      Express.wsOnClose (fun _ -> allSockets := Map.remove wsid !allSockets) ws
    )
    app
  |> ignore
