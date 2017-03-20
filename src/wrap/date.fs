module Date

open Fable.Core

open Serialize

type Date =
  abstract year : int
  abstract month : int
  abstract date : int
  abstract hours : int
  abstract minutes : int
  abstract seconds : int
  abstract milliseconds : int
  abstract now : unit -> Date
  abstract parse : string -> Date
  abstract UTC : unit -> Date
  abstract getDate : unit -> int
  abstract getDay : unit -> int
  abstract getFullYear : unit -> int
  abstract getHours : unit -> int
  abstract getMilliseconds : unit -> int
  abstract getMinutes : unit -> int
  abstract getMonth : unit -> int
  abstract getSeconds : unit -> int
  abstract getTime : unit -> int
  abstract getTimezoneOffset : unit -> int
  abstract getUTCDate : unit -> int
  abstract getUTCDay : unit -> int
  abstract getUTCFullYear : unit -> int
  abstract getUTCHours : unit -> int
  abstract getUTCMilliseconds : unit -> int
  abstract getUTCMinutes : unit -> int
  abstract getUTCMonth : unit -> int
  abstract getUTCSeconds : unit -> int
  abstract getYear : unit -> int
  abstract toDateString : unit -> string
  abstract toJSON : unit -> Json
  abstract toISOString : unit -> string
  abstract toGMTString : unit -> string
  abstract toLocalDateString : unit -> string
  abstract toString : unit -> string
  abstract toTimeString : unit -> string
  abstract toUTCString : unit -> string

[<Emit("(function () { var d = new Date(); d.setTime($0); return d; })()")>]
let dateFromTime : int -> Date = fun t -> failwith "JS"

[<Emit("Date.now()")>]
let now : unit -> Date = fun _ -> failwith "JS"

