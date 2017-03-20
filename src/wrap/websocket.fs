module WebSocket

open Fable.Core

open Buffer

type WSModule = Unused0
type WebSocket = Unused1
type WSConnection = Unused2
type WSMessage =
  | String of string
  | Buffer of Buffer

[<Emit("(function() { var ws = require('websocket'); return ws; })()")>]
let ws_ : unit -> WSModule = fun _ -> failwith "JS"

let ws_module = ws_ ()

[<Emit("new $0.client()")>]
let webSocketClient_ : WSModule -> WebSocket = fun wsm -> failwith "JS"

let webSocketClient _ = webSocketClient_ ws_module

[<Emit("$2.connect($0,$1)")>]
let connect : string -> string -> WebSocket -> unit = fun url proto ws -> failwith "JS"

[<Emit("$0.close()")>]
let close : WSConnection -> unit = fun ws -> failwith "JS"

[<Emit("$1.send($0)")>]
let sendString : string -> WSConnection -> unit = fun s ws -> failwith "JS"

[<Emit("$1.send($0)")>]
let sendBuffer : Buffer -> WSConnection -> unit = fun s ws -> failwith "JS"

[<Emit("$2.on($0,$1)")>]
let wsOn_ : string -> 'a -> 'b -> unit = fun evt f ws -> failwith "JS"

let onConnectFailed (f : string -> unit) (ws : WebSocket) =
  wsOn_ "connectFailed" f ws

let onConnect (f : WSConnection -> unit) (ws : WebSocket) =
  wsOn_ "connect" f ws

let onError (f : string -> unit) (ws : WSConnection) =
  wsOn_ "error" f ws

let onClose (f : unit -> unit) (ws : WSConnection) =
  wsOn_ "close" f ws

[<Emit("(function() { $2.on('message', function(msg) { if (msg.type === 'utf8') { $0(msg.utf8Data); } else { $1(msg.binaryData); } }); })()")>]
let onMessage_ : (string -> unit) -> (Buffer -> unit) -> WSConnection -> unit = fun ms mb ws -> failwith "JS"

let onMessage f ws =
  onMessage_
    (fun s -> f (String s))
    (fun b -> f (Buffer b))
    ws
