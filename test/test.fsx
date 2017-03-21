#r "../node_modules/fable-core/Fable.Core.dll"

#load "../src/wrap/util.fs"
#load "../src/wrap/serialize.fs"
#load "../src/wrap/buffer.fs"
#load "../src/wrap/q.fs"
#load "../src/wrap/dns.fs"
#load "../src/wrap/ipaddr.fs"
#load "../src/wrap/network.fs"
#load "../src/wrap/bonjour.fs"
#load "../src/forward.fs"
#load "./mocha.fs"

open Util
open MochaTest

type DoneF = unit -> unit
type It = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let tests : (string * (It list)) list =
  [ "forward" => 
      [ "should be creatable" =>
          fun donef -> let i = Forward.init () in donef ()
      ]
  ]

let _ =
  List.map
    (fun (n,t) ->
      describe
        n
        (fun () ->
          List.map (fun (itName,itTest) -> it itName itTest) t
          |> ignore
        )
    )
    tests
