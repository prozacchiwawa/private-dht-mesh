module Request

open Fable.Core

open Q
open Serialize
  
type RequestPromiseModule = Unused0
  
[<Emit("(function () { var rp = require('request-promise'); return rp; })()")>]
let rp_ : unit -> RequestPromiseModule = fun _ -> failwith "JS"

let rp_module = rp_ ()

[<Emit("$0($1)")>]
let get_ : RequestPromiseModule -> string -> Promise<string,string> = fun q u -> failwith "JS"

let get url = get_ rp_module url

[<Emit("$0({method:'POST', url:$1, body:$2})")>]
let post_ : RequestPromiseModule -> string -> string -> Promise<string,string> = fun q u b -> failwith "JS"

let post url body = post_ rp_module url body

[<Emit("$0({method:'PUT', url:$1, body: $2})")>]
let put_ : RequestPromiseModule -> string -> string -> Promise<string,string> = fun q u b -> failwith "JS"

let put url body = put_ rp_module url body
