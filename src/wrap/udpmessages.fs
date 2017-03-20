module UdpMessages

open Fable.Core

open Util
open Buffer
open NodeSocket

module dgramprovider =
  type DgramProvider =
    abstract incoming : Buffer -> SendAddr -> unit

[<Import("*","udp-messenger")>]
module udpmessages =
  type Messenger =
    abstract close : unit -> unit
    abstract send : Buffer -> string -> int -> unit

[<Emit("(function() { var aprov = require('./dgramprovider'); return new aprov.DgramProvider($0); })()")>]
let newDgramProvider : (UDPMessage -> unit) -> dgramprovider.DgramProvider = fun f -> failwith "JS"

[<Emit("(function() { var um = require('udp-messenger'); return new um($0,$1,$2); })()")>]
let newMessenger : int -> dgramprovider.DgramProvider -> int -> udpmessages.Messenger = fun timeout provider mtu -> failwith "JS"

[<Emit("(function() { $1.on('message', $0); })()")>]
let onMessage : (Buffer -> unit) -> udpmessages.Messenger -> unit = fun f m -> failwith "JS"

[<Emit("(function() { $1.on('dropped', $0); })()")>]
let onDropped : (string -> unit) -> udpmessages.Messenger -> unit = fun f m -> failwith "JS"

[<Emit("(function() { $1.on('sending', $0); })()")>]
let onSending : (string -> unit) -> udpmessages.Messenger -> unit = fun f m -> failwith "JS"

[<Emit("(function() { $1.on('sent', $0); })()")>]
let onSent : (string -> unit) -> udpmessages.Messenger -> unit = fun f m -> failwith "JS"
