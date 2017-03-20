module Bonjour

open Fable.Core

open Q
open Buffer
open Util

type ServiceDesc = Unused0

[<Emit("(function (name, type, port) { return { name: name, type: type, port: port } })($0,$1,$2)")>]
let serviceDesc : string -> string -> int -> ServiceDesc = fun n t p -> failwith "JS"
[<Emit("(Object.assign({}, $1)).subtypes = $0")>]
let serviceDescAddSubtypes : string array option -> ServiceDesc -> ServiceDesc = fun a d -> failwith "JS"
[<Emit("(Object.assign({}, $1)).protocol = $0")>]
let serviceDescAddProtocol : string option -> ServiceDesc -> ServiceDesc = fun a d -> failwith "JS"
[<Emit("(Object.assign({}, $1)).txt = $0")>]
let serviceDescAddTxt : Serialize.Json -> ServiceDesc -> ServiceDesc = fun a d -> failwith "JS"

type ServiceQuery = Unused1

[<Emit("(function(name) { return { name: name }; })($0)")>]
let serviceQueryByName : string -> ServiceQuery = fun n -> failwith "JS"
[<Emit("(function(v) { return { type: v }; })($0)")>]
let serviceQueryByType : string -> ServiceQuery = fun t -> failwith "JS"

type ServiceReferer =
  { address : string
  ; family : string
  ; port : int
  ; size : int
  }

type ServiceEntry =
  { name : string
  ; fqdn : string
  ; host : string
  ; referer : ServiceReferer
  ; protocol : string
  ; subtypes : string array
  ; rawTxt : Buffer
  ; txt : Serialize.Json
  }

type Event<'a> = Unused2

[<Emit("$1.addEventListener($0)")>]
let addEventListener : ('a -> unit) -> Event<'a> -> unit = fun f e -> failwith "JS"
[<Emit("$1.removeEventListener($0)")>]
let removeEventListener : ('a -> unit) -> Event<'a> -> unit = fun f e -> failwith "JS"

[<Import("*", "bonjour")>]
module bonjour =
  type Bonjour =
    abstract publish : ServiceDesc -> unit
    abstract unpublishAll : (unit -> unit) option -> unit
    abstract destroy : unit -> unit
    abstract up : Event<ServiceEntry>
    abstract down : Event<ServiceEntry>
    abstract start : unit -> unit
    abstract stop : unit -> unit
    abstract update : unit -> unit

[<Emit("(function() { var bonjour = require('bonjour'); return bonjour(); })()")>]
let newBonjour : unit -> bonjour.Bonjour = fun _ -> failwith "JS"
[<Emit("(function(q,cb,b) { return b.find(q,cb); })($0,$1,$2)")>]
let find : ServiceQuery -> (ServiceEntry -> unit) -> bonjour.Bonjour -> unit = fun q cb b -> failwith "JS"
