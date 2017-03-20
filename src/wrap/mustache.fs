module Mustache

open Fable.Core
open Serialize

[<Emit("(function(template,p) { var mustache = require('mustache'); return mustache.render(template,p); })($0,$1)")>]
let render : string -> Json -> string = fun _ _ -> failwith "JS"
