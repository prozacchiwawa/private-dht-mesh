module DHTTest

open MochaTest
open Buffer

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let (tests : (string * ((unit -> unit) -> unit)) list) = 
  [ "should attempt to bootstrap" =>
      fun donef ->
        let dht1 = DHT.init { DHT.defaultOpts with bootstrap = [| { host = "4.3.2.1" ; port = 9999 ; id = Buffer.zero 32 } |] } in
        let dht2 = DHT.bootstrap dht1 in
        let dht3 =
          List.fold
            (fun dht t ->
              DHT.tick 0 dht
            )
            dht2
            [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]
        in
        let _ = massert.ok ((Util.dump "events" dht3.events) <> []) in
        donef ()
  ]
