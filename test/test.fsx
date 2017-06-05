#r "../node_modules/fable-core/Fable.Core.dll"

#load "../src/wrap/util.fs"
#load "../src/wrap/serialize.fs"
#load "../src/wrap/buffer.fs"
#load "../src/wrap/shortid.fs"
#load "../src/wrap/q.fs"
#load "../src/wrap/q-io.fs"
#load "../src/wrap/dns.fs"
#load "../src/wrap/ipaddr.fs"
#load "../src/wrap/network.fs"
#load "../src/wrap/bonjour.fs"
#load "../src/wrap/crypto.fs"
#load "../src/wrap/rbtree.fs"
#load "../src/ports.fs"
#load "../src/queue.fs"
#load "../src/forward.fs"
#load "../src/kbucket.fs"
#load "../src/dhtdata.fs"
#load "../src/dht.fs"
#load "../src/dhtrpc.fs"
#load "../src/elm-return/respond.fs"
#load "../src/elm-return/return.fs"
#load "../src/broadcast.fs"
#load "./mocha.fs"
#load "./kbuckettest.fs"
#load "./testnet.fs"
#load "./dhttest.fs"
#load "./dhtvis.fs"
#load "./testnetdht.fs"
#load "./dhtrpctest.fs"
#load "./broadcasttest.fs"

open Util
open Buffer
open MochaTest
open Forward
open KBucket

type DoneF = unit -> unit
type It = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let basicNodeA = { local = true ; target = "169.254.1.2" ; id = "testA" }
let basicNodeB = { local = true ; target = "169.254.1.3" ; id = "testB" }

let final f _ = f ()

let tests : (string * (It list)) list =
  [ "forward" => 
      [ "should be creatable" =>
          fun donef -> let i = Forward.init () in donef ()
      ; "should allow addition of nodes" =>
          fun donef ->
            let i = Forward.init () in
            begin
              i |> Forward.addNode basicNodeA |> final donef
            end
      ; "should allow addition of edges" =>
          fun donef ->
            let i = Forward.init () in
            begin
              i
              |> Forward.addNode basicNodeA
              |> Forward.addNode basicNodeB
              |> Forward.addEdge basicNodeA.id basicNodeB.id
              |> final donef 
            end
      ; "should know if two nodes are connected" =>
          fun donef ->
            let i =
              Forward.init ()
              |> Forward.addNode basicNodeA
              |> Forward.addNode basicNodeB
              |> Forward.addEdge basicNodeA.id basicNodeB.id
            in
            begin
              massert.ok (i |> Forward.connected basicNodeA.id basicNodeB.id) ;
              massert.ok
                (i 
                 |> Forward.removeEdge basicNodeA.id basicNodeB.id
                 |> Forward.connected basicNodeA.id basicNodeB.id
                 |> not
                ) ;
              donef ()
            end
      ]
  ; "k-bucket" => KBucketTest.tests
  ; "dht" => DHTTest.tests
  ; "dhtrpc" => DHTRpcTest.tests
  ; "broadcast" => BroadcastTest.tests
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
