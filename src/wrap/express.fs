module Express

open Fable.Core

open Buffer
open Q
open WebSocket

type App = Unused0

type Request = Unused1
type Response = Unused2
type Next = Unused3
type WebSocket = Unused4

[<Emit("(function() { var express = require('express'); var app = express(); app.use(function(req,res,nxt) { req.rawBody = ''; req.setEncoding('binary'); req.on('data', function(chunk) { req.rawBody += chunk; }); req.on('end',nxt); }); var expressWs = require('express-ws'); expressWs(app); return {app: app}; })()")>]
let newApp : unit -> App = fun _ -> failwith "JS"

[<Emit("(function(app,ep,path) { var express = require('express'); app.app.use(ep, express.static(path)); return app; })($2,$0,$1)")>]
let files : string -> string -> App -> App = fun ep path app -> failwith "JS"

[<Emit("(function() { $1.app.use(function (req,res,next) { $0(req,res,next); }); return $1; })()")>]
let filter : (Request -> Response -> Next -> unit) -> App -> App = fun tx app -> failwith "JS"

[<Emit("(function() { $2.app.get($0,function (req,res,next) { $1(req,res,next); }); return $2; })()")>]
let get : string -> (Request -> Response -> Next -> unit) -> App -> App = fun ep hdlr app -> failwith "JS"

[<Emit("(function() { $2.app.post($0,function (req,res,next) { $1(req,res,next); }); return $2; })()")>]
let post : string -> (Request -> Response -> Next -> unit) -> App -> App = fun ep hdlr app -> failwith "JS"

[<Emit("(function() { $2.close = $2.app.listen($0,$1); })()")>]
let listen_ : int -> (unit -> unit) -> App -> unit = fun port cb app -> failwith "JS"

let listen (n : int) (app : App) : Q.Promise<unit,string> =
  let f : Q.Future<unit,string> = Q.defer () in
  let _ = app |> listen_ n (fun _ -> Q.resolve () f) in
  f.promise

[<Emit("$0.close.close()")>]
let close : App -> unit = fun app -> failwith "JS"

[<Emit("$3.on('message',function(msg) { if (typeof(msg) === 'string') { $2($1(msg)); } else { $2($0(msg)); } })")>]
let wsOnMessage_ : (Buffer -> WSMessage) -> (string -> WSMessage) -> (WSMessage -> unit) -> WebSocket -> unit = fun mb ms hdlr ws -> failwith "JS"

let wsOnMessage = wsOnMessage_ (fun b -> Buffer b) (fun s -> String s)

[<Emit("$1.on('close',function() { $0(); })")>]
let wsOnClose : (unit -> unit) -> WebSocket -> unit = fun f ws -> failwith "JS"

[<Emit("$0.close()")>]
let wsClose : WebSocket -> unit = fun _ -> failwith "JS"

[<Emit("$1.send($0.Fields[0])")>]
let wsSend : WSMessage -> WebSocket -> unit = fun m ws -> failwith "JS"

[<Emit("$1.getHeader($0)")>]
let reqGetHeader : string -> Request -> string = fun h r -> failwith "JS"

[<Emit("$0.headers")>]
let reqGetHeaders : Request -> Serialize.Json = fun r -> failwith "JS"

[<Emit("$0.method")>]
let reqGetMethod : Request -> string = fun r -> failwith "JS"

[<Emit("(function () { return Buffer.from($0.rawBody, 'binary'); })()")>]
let reqGetBody : Request -> Buffer = fun r -> failwith "JS"

[<Emit("$0.url")>]
let reqGetUrl : Request -> string = fun r -> failwith "JS"

[<Emit("$0.path")>]
let reqGetPath : Request -> string = fun r -> failwith "JS"

[<Emit("$0.params")>]
let reqGetParams : Request -> Serialize.Json = fun r -> failwith "JS"

[<Emit("(function(header,val,response) { response.append(header,val); return response; })($0,$1,$2)")>]
let resSetHeader : string -> string -> Response -> Response = fun h v r -> failwith "JS"

[<Emit("(function() { $1.end($0); })()")>]
let resEndString : string -> Response -> unit = fun d r -> failwith "JS"

[<Emit("$1.end($0)")>]
let resEndBuffer : Buffer -> Response -> unit = fun d r -> failwith "JS"

[<Emit("(function() { $2.app.ws($0,function(ws,req) { $1(ws)(req); }); return $2; })()")>]
let ws : string -> (WebSocket -> Request -> unit) -> App -> App = fun ep hldr app -> failwith "JS"
