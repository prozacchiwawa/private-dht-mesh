module BasicWSService

open Util
open Bacon
open DHTRunner
open DHTData
open DHTRPC

let serve
      dhtid
      (requestBus : bacon.Bus<InputEventDHT,unit>)
      (outputBus : bacon.Observable<OutputEventDHT,unit>)
      app =
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
        (DHTRunner.QueryReply
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
         requestBus.push (DHTRunner.QueryReply (txid,nid,body))
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
    | DHTRunner.QueryError (txid, id, error) ->
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
  |> ignore
