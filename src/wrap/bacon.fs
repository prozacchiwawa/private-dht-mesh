module Bacon

open Util
open Fable.Core
open Fable.Core.JsInterop

type Event<'a,'e> = Unused0
type Baconjs = Unused1

[<Import("*", "baconjs")>]
module bacon =
  type Observable<'a,'e> =
    abstract subscribe : (Event<'a,'e> -> unit) -> (unit -> unit)
    abstract onValue : ('a -> unit) -> unit
    abstract onError : ('e -> unit) -> unit
    abstract onEnd : (unit -> unit) -> unit  
    abstract map : ('a -> 'b) -> Observable<'b,'e>
    abstract mapError : ('e -> 'f) -> Observable<'a,'f>
  type ('a,'e) Bus =
    abstract push : 'a -> unit
    abstract error : 'e -> unit
    abstract plug : Observable<'a,'e> -> unit

[<Emit("(function() { var b = require('baconjs'); return b; })()")>]
let baconjs_module_ : unit -> Baconjs = fun u -> failwith "JS only"

let baconjs_module = baconjs_module_ ()
      
[<Emit("(function() { return new ($0).Bus(); })()")>]
let newBus_ : Baconjs -> bacon.Bus<'a,'e> = fun u -> failwith "JS only"

let newBus () = newBus_ baconjs_module

[<Emit("$0")>]
let busObservable : bacon.Bus<'a,'e> -> bacon.Observable<'a,'e> = fun b -> failwith "JS only"

[<Emit("$0.repeatedly($1,$2)")>]
let repeatedly_ : Baconjs -> int -> 'a array -> bacon.Observable<'a,unit> = fun b t v -> failwith "JS only"

let repeatedly t v = repeatedly_ baconjs_module t v
  
[<Emit("$0.end()")>]
let conclude : bacon.Bus<'a,'e> -> unit = fun b -> failwith "JS only"
