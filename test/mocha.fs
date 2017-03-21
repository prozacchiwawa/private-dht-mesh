module MochaTest

open Fable.Core

[<Emit("describe($0,$1)")>]
let describe : string -> (unit -> unit) -> unit = fun s f -> failwith "JS only"
[<Emit("it($0,$1)")>]
let it : string -> ((unit -> unit) -> unit) -> unit = fun s f -> failwith "JS only"

type ChaiModule = Unused0

[<Emit("(function() { var chai = require('chai'); return chai; })()")>]
let chai_module_ : unit -> ChaiModule = fun _ -> failwith "JS"
let chai_module = chai_module_ ()

type ChaiAssert<'a> =
  { equal : 'a -> 'a -> unit
  ; deepEqual : 'a -> 'a -> unit
  ; ok : bool -> unit
  ; fail : bool -> unit
  }

[<Emit("(function(chai) { return chai.assert; })($0)")>]
let chai_assert_ : ChaiModule -> ChaiAssert<'a> = fun chai -> failwith "JS"
let massert : ChaiAssert<obj> = chai_assert_ chai_module
