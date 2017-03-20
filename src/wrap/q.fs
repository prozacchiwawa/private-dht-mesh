module Q

open Fable.Core
  
type QModule = Unused0
  
[<Emit("(function() { var q = require('q'); return q; })()")>]
let q_ : unit -> QModule = fun _ -> failwith "JS"

let q_module = q_ ()

type ('a,'f) Promise = Unused1

[<Emit("$0($1)")>]
let value_ : QModule -> 'a -> Promise<'a,'f> = fun q a -> failwith "JS"

let value v = value_ q_module v
  
type Future<'a,'f> = { promise : Promise<'a,'f> }
  
[<Emit("$0.defer()")>]
let defer_ : QModule -> Future<'a,'f> = fun q -> failwith "JS"

let defer _ = defer_ q_module
  
[<Emit("$1.resolve($0)")>]
let resolve : 'a -> Future<'a,'f> -> unit = fun a f -> failwith "JS"  

[<Emit("$1.reject($0)")>]
let reject : 'f -> Future<'a,'f> -> unit = fun e f -> failwith "JS"

[<Emit("$0.all($1)")>]
let all_ : QModule -> Promise<'a,'f> array -> Promise<'a array,'f> = fun q a -> failwith "JS"

let all va = all_ q_module va

[<Emit("$0.all([$1,$2])")>]
let tuple_ : QModule -> Promise<'a,'f> -> Promise<'b,'f> -> Promise<('a*'b),'f> = fun q a b -> failwith "JS"

let tuple a b = tuple_ q_module a b
  
[<Emit("$1.then($0)")>]
let map : ('a -> 'b) -> Promise<'a,'f> -> Promise<'b,'f> = fun f a -> failwith "JS"

[<Emit("$1.then($0)")>]
let andThen : ('a -> Promise<'b,'f>) -> Promise<'a,'f> -> Promise<'b,'f> = fun f a -> failwith "JS"

[<Emit("$1.catch(function (a) { throw $0(a); });")>]
let errmap : ('f -> 'g) -> Promise<'a,'f> -> Promise<'a,'g> = fun f a -> failwith "JS"

[<Emit("$1.catch(function (a) { return $0(a); })")>]
let errThen : ('f -> Promise<'g,'e>) -> Promise<'a,'f> -> Promise<'g,'e> = fun f a -> failwith "JS"

let eat p = errmap (fun _ -> ()) p
  
[<Emit("$0($2).catch($1)")>]
let handle_ : QModule -> ('f -> 'a) -> Promise<'a,'f> -> Promise<'a,unit> = fun q f a -> failwith "JS"

let handle f p = handle_ q_module f p

[<Emit("$0.delay($1)")>]
let delay_ : QModule -> int -> Promise<unit,'e> = fun q t -> failwith "JS"

let delay t = delay_ q_module t
  
[<Emit("$0($1).catch(function() {}).done()")>]
let fin_ : QModule -> Promise<'a, unit> -> unit = fun q p -> failwith "JS"

let fin p = fin_ q_module p

[<Emit("$0($2).fin($1)")>]
let ending_ : QModule -> (unit -> unit) -> Promise<'a,unit> -> unit = fun q f p -> failwith "JS"

let ending f p = ending_ q_module f p

let fail (err : 'f) : Promise<'a,'f> =
  let d = defer () in
  let _ = reject err d in
  d.promise

           
