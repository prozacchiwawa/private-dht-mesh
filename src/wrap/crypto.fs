module Crypto

open Fable.Core

open Buffer

type CryptoModule = Unused0

[<Emit("(function() { var c = require('crypto'); return c; })()")>]
let crypto_ : unit -> CryptoModule = fun _ -> failwith "JS"

let crypto_module = crypto_ ()

type Hasher = Unused1

[<Emit("$0.createHash($1)")>]
let createHash_ : CryptoModule -> string -> Hasher = fun cm ty -> failwith "JS"

let createHash ty = createHash_ crypto_module ty

[<Emit("$1.update($0)")>]
let updateBuffer : Buffer -> Hasher -> unit = fun b h -> failwith "JS"

[<Emit("$0.digest()")>]
let digestBuffer : Hasher -> Buffer = fun h -> failwith "JS"

[<Emit("$0.digest('hex')")>]
let digestHex : Hasher -> string = fun h -> failwith "JS"

[<Emit("$0.randomBytes($1)")>]
let randomBytes_ : CryptoModule -> int -> Buffer = fun cm n -> failwith "JS"
let randomBytes = randomBytes_ crypto_module
