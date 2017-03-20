module ShortId

open Fable.Core

type ShortIdModule = Unused0
[<Emit("(function() { var sid = require('shortid'); return sid; })()")>]
let shortid_module_ : unit -> ShortIdModule = fun _ -> failwith "JS"

let shortid_module = shortid_module_ ()

[<Emit("$0.generate()")>]
let generate_ : ShortIdModule -> string = fun _ -> failwith "JS"

let generate _ = generate_ shortid_module
