module IPAddr

open Fable.Core
open Buffer

type IPAddrModule = Unused0
type IPAddrObject = Unused1

[<Emit("(function() { var ipa = require('ipaddr.js'); return ipa; })()")>]
let ipa_mod_ : unit -> IPAddrModule = fun _ -> failwith "JS"
let ipa_mod = ipa_mod_ ()

[<Emit("(function(m,s) { return m.parse(s); })($0,$1)")>]
let parse_ : IPAddrModule -> string -> IPAddrObject = fun m s -> failwith "JS"
let parse = parse_ ipa_mod

[<Emit("(function(m,a) { return m.fromByteArray(a); })($0,$1)")>]
let fromByteArray_ : IPAddrModule -> int array -> IPAddrObject = fun m a -> failwith "JS"
let fromByteArray = fromByteArray_ ipa_mod

[<Emit("(function(m,s) { return m.isValid(s); })($0,$1)")>]
let isValid_ : IPAddrModule -> string -> bool = fun m s -> failwith "JS"
let isValid = isValid_ ipa_mod

[<Emit("$0.toByteArray()")>]
let toByteArray : IPAddrObject -> int array = fun o -> failwith "JS"

[<Emit("$0.kind()")>]
let kind : IPAddrObject -> string = fun o -> failwith "JS"

[<Emit("$0.toString()")>]
let toString : IPAddrObject -> string = fun o -> failwith "JS"
