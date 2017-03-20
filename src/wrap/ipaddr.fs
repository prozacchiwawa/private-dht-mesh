module IPAddr

open Fable.Core
open Buffer

[<Import("*","ipaddr.js")>]
module ipaddr =
  type Address =
    abstract kind : unit -> string
    abstract toString : unit -> string
    abstract toByteArray : unit -> int array
  let parse : string -> Address = fun a -> failwith "JS only"
  let fromByteArray : int array -> Address = fun a -> failwith "JS only"
  let isValid : string -> bool = fun a -> failwith "JS only"

let parse = ipaddr.parse

let fromByteArray = ipaddr.fromByteArray

let isValid a = not (not (ipaddr.isValid a))
