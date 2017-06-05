module BroadcastTest

open Util
open Buffer
open MochaTest
open DHTData
open Broadcast

type DoneF = unit -> unit
type Test = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let tests : Test list =
  [ "should be creatable" =>
      fun donef -> let i = Broadcast.init 3 in donef ()
  ]
