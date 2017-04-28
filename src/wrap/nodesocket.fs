module NodeSocket

open Util
open Buffer
open Q
open Dns
open IPAddr
open Fable.Core
open Fable.Core.JsInterop

type BindArgs = { addr : string; port : int; exclusive : bool }
type SendAddr = { address : string; port : int }

type UDPMessage = { msg : Buffer; rinfo : SendAddr }
  
[<Import("*", "net")>]
module net_ =
  type Socket =
    abstract close : unit -> unit
    abstract write : Buffer -> string -> unit
    abstract on : string -> 'a -> unit
  type UnixServer =
    abstract close : unit -> unit
    abstract on : string -> 'a -> unit

type NetModule = Unused0

[<Emit("$0.end($1)")>]
let endSocketBuffer_ : net_.Socket -> Buffer -> unit = fun s b -> failwith "JS"

[<Emit("(function () { var nm = require('net'); return nm; })()")>]
let net_module_ : unit -> NetModule = fun _ -> failwith "JS"

let nm_ = net_module_ ()

module net__ =
  type Socket (sock_ : net_.Socket) =
    let socket = sock_ in
    member self.close () =
      socket.close ()
    member self.endBuffer buf =
      endSocketBuffer_ socket buf
    member self.sendBuffer buf =
      try
        (socket.write buf "binary" ; true)
      with (e : exn) ->
        (log "Error" e |> ignore ; false)
    member self.onClose (hdlr : (unit -> unit)) =
        socket.on "close" hdlr
    member self.onEnd (hdlr : (unit -> unit)) =
        socket.on "end" hdlr
    member self.onError (hdlr : (string -> unit)) =
        socket.on "error" (fun exn -> hdlr ("" + exn))
    member self.onDrain (hdlr : (unit -> unit)) =
        socket.on "drain" hdlr
    member self.onData (hdlr : (Buffer -> unit)) =
        socket.on "data" hdlr

  type UnixServer (serv_ : net_.UnixServer) =
    let server = serv_ in
    member self.close () =
      server.close ()
    member self.onConnection (hdlr : (Socket -> unit)) =
      server.on "connection" (fun c -> hdlr (new Socket(c)))


[<Emit("(function () { try { var s = $0.createServer(); s.on('listening', function () { $3(s) }); s.on('err', function(e) { $2(''+e); }); s.listen($1); } catch(e) { $2(''+e); } })()")>]
let createUnixServer_ : NetModule -> string -> (string -> unit) -> (net_.UnixServer -> unit) -> unit = fun nm ad er cb -> failwith "JS"

let createUnixServer addr er cb = 
  try
    createUnixServer_
      nm_
      addr
      er
      (fun c -> cb (new net__.UnixServer(c)))
  with _ ->
    ()

[<Emit("(function () { try { var conn = $0.createConnection($1); conn.on('connect', function() { $3(conn); }); conn.on('err', function(e) { $2('' + e); }); } catch (e) { $2('' + e); } })()")>]
let connectUnixServer_ : NetModule -> string -> (string -> unit) -> (net_.Socket -> unit) -> unit = fun nm p er cb -> failwith "JS"

let connectUnixServer path er cb =
  try
    connectUnixServer_
      nm_
      path
      er
      (fun c -> cb (new net__.Socket(c)))
  with _ -> ()

[<Import("*", "dgram")>]
module dgram_ =
  type Socket =
    abstract close : unit -> unit
    abstract bind : BindArgs -> unit
    abstract send : 'a -> int -> int -> int -> string -> unit
    abstract on : string -> 'a -> unit
  let createSocket : string -> Socket = failwith "JS only"

module datagram =
  type Socket (ty : string) =
    let lastName = ref None in
    let lastAddr = ref None in
    let lookupType = if ty = "udp4" then 4 else 6 in
    let socket : dgram_.Socket = dgram_.createSocket ty in

    member self._startLookup str =
      Dns.lookup str lookupType
      |> Q.map 
           (fun res ->
             !lastName = Some str |> ignore ;
             !lastAddr = Some res |> ignore ;
             res
           )

    member self._lookup str =
      if IPAddr.isValid str then
        Q.value str
      else
        match (!lastName,!lastAddr) with
        | (Some a, Some r) ->
           if a = str then Q.value r else self._startLookup str
        | _ ->
           self._startLookup str

    member self.close () =
        socket.close ()
    member self.bind bindArgs =
        try
          (socket.bind bindArgs ; true)
        with _ ->
          false
    member self.sendString str sendAddr =
        try
          self._lookup sendAddr.address
          |> Q.map
               (fun addr ->
                 socket.send str 0 (String.length str) sendAddr.port addr
               )
          |> Q.handle (fun e -> log "Could not send dgram" e |> ignore)
          |> Q.fin ;
          true
        with e ->
          begin
            log "with error" e |> ignore ;
            false
          end
    member self.sendBuffer (buf : Buffer) (sendAddr : SendAddr) =
        try
          self._lookup sendAddr.address
          |> Q.map
               (fun addr ->
                 socket.send buf 0 buf.length sendAddr.port addr
               )
          |> Q.handle (fun e -> log "Could not send dgram" e |> ignore)
          |> Q.fin ;
          true
        with e ->
          begin
            log "with error" e |> ignore ;
            false
          end
    member self.onClose (hdlr : (unit -> unit)) =
        socket.on "close" hdlr
    member self.onError (hdlr : (string -> unit)) =
        socket.on "error" (fun exn -> hdlr ("" + exn))
    member self.onListening (hdlr : (unit -> unit)) =
        socket.on "listening" hdlr
    member self.onMessage (hdlr : (UDPMessage -> unit)) =
        socket.on "message"
          (System.Func<_,_,_>
            (fun msg rinfo ->
              hdlr
                { msg = msg
                ; rinfo =
                    { address = rinfo.address
                    ; port = rinfo.port
                    }
                }
            )
          )
  let createSocket ty = new Socket(ty)
