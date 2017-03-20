module Moment

open Fable.Core

type MomentModule = Unused0

[<Emit("(function() { var m = require('moment'); return m; })()")>]
let moment_ : unit -> MomentModule = fun _ -> failwith "JS"

let moment = moment_ ()

[<Emit("(function(moment,outfmt) { return moment().format(outfmt); })($0,$1)")>]
let now_ : MomentModule -> string -> string = fun _ _ -> failwith "JS"
let now fmt = now_ moment fmt

[<Emit("(function(moment,fmt,outfmt,date) { return moment(date,fmt).format(outfmt); })($0,$1,$2,$3)")>]
let dateConvert_ : MomentModule -> string -> string -> string -> string option = fun _ _ _ _ -> failwith "JS"

let dateConvert = dateConvert_ moment
