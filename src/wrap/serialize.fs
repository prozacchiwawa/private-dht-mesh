module Serialize

open Fable.Core

open Util

type Json = Unused0

[<Emit("{ try { return JSON.parse($0); } catch(e) { return null; } }")>]
let parse : string -> Json option = fun s -> failwith "JS"

[<Emit("JSON.stringify($0)")>]
let stringify : Json -> string = fun s -> failwith "JS"

[<Emit("null")>]
let jsonNull : unit -> Json = fun s -> failwith "JS"

[<Emit("('' + $0)")>]
let jsonString : string -> Json = fun s -> failwith "JS"

[<Emit("$0")>]
let jsonFloat : float -> Json = fun f -> failwith "JS"

[<Emit("$0")>]
let jsonBool : bool -> Json = fun b -> failwith "JS"

[<Emit("$0")>]
let jsonInt : int -> Json = fun f -> failwith "JS"

[<Emit("$0")>]
let jsonArray : Json array -> Json = fun a -> failwith "JS"

[<Emit("(function(kva) { var r = {}; for (var i = 0; i < kva.length; i++) { var k = kva[i][0]; var v = kva[i][1]; r[k] = v; } return r; })($0)")>]
let jsonObject : (string * Json) array -> Json = fun a -> failwith "JS"

[<Emit("(!!v)")>]
let truthy : Json -> bool = fun v -> failwith "JS"

[<Emit("('' + $0)")>]
let asString : Json -> string = fun j -> failwith "JS"

[<Emit("(function(obj) { return Object.keys(obj).map(function(k) { return [k,obj[k]]; }); })($0)")>]
let explode : Json -> (string * Json) array = fun o -> failwith "JS"

[<Emit("(function(n,o) { if (o[n]) { return o[n]; } else { return null; } })($0,$1)")>]
let field : string -> Json -> Json option = fun n o -> failwith "JS"

[<Emit("(function(n,o) { while (n.length != 0) { if (o && o[n[0]]) { o = o[n[0]]; } else { o = null; } n = n.slice(1); } })($0,$1)")>]
let at : string array -> Json -> Json option = fun n o -> failwith "JS"

[<Emit("(function(n,v,o) { var no = Object.assign({},o); no[n] = v; return no; })($0,$1,$2)")>]
let addField : string -> Json -> Json -> Json = fun n v o -> failwith "JS"

[<Emit("(function(obj) { if (typeof(obj) === 'number') { return obj; } else { return null; } })($0)")>]
let asFloat : Json -> float option = fun f -> failwith "JS"

[<Emit("(function(obj) { if (typeof(obj) === 'number') { return Math.floor(obj); } else { return null; } })($0)")>]
let floor : Json -> int option = fun f -> failwith "JS"

[<Emit("(function(tx,obj) { if (obj instanceof Array) { return obj.map(tx); } else { return null; } })($0,$1)")>]
let arrayMap : (Json -> 'a) -> Json -> 'a array option = fun f o -> failwith "JS"
