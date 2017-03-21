module MochaTest

open Fable.Core

[<Emit("describe($0,$1)")>]
let describe : string -> (unit -> unit) -> unit = fun s f -> failwith "JS only"
[<Emit("it($0,$1)")>]
let it : string -> ((unit -> unit) -> unit) -> unit = fun s f -> failwith "JS only"

[<Import("*","assert")>]
module massert =
  let equal : 'a -> 'a -> unit = fun a b -> failwith "JS only"
  let deepEqual : 'a -> 'a -> unit = fun a b -> failwith "JS only"
  let ok : 'a -> unit = fun a -> failwith "JS only"
  let fail : 'a -> unit = fun a -> failwith "JS only"
