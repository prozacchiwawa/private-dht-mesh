module DHTTest

open MochaTest
open Util
open Buffer

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let (tests : (string * ((unit -> unit) -> unit)) list) = 
  [ "should attempt to bootstrap" =>
      fun donef ->
        let dht1 =
          DHT.init
            { DHT.defaultOpts with
                bootstrap =
                  [| { host = "4.3.2.1"
                     ; port = 9999
                     ; id = DHT.hashId "dht" 
                     } 
                  |] 
            }
        in
        let dht2 = DHT.bootstrap dht1 in
        let dht3 =
          List.fold
            (fun dht t ->
              DHT.tick 0 dht
            )
            dht2
            [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]
        in
        let _ = massert.ok (dht3.events <> []) in
        donef () ;
    "should take a bootstrap reply" =>
      fun donef ->
        let pushPacket (d : DHT.DHT) (l : DHT.Action) : DHT.DHT =
          match l with
          | DHT.Request (b,n) ->
             match Serialize.field "id" b with
             | Some id ->
                let (nid : DHTData.NodeIdent) =
                  { id = Buffer.fromString (Serialize.asString id) "binary" 
                  ; host = n.host
                  ; port = n.port
                  }
                in
                DHT._onrequest b nid d
             | None -> d
          | DHT.Response (b,n) -> DHT._onresponse 0 b n d
          | _ -> d
        in
        let pushFrom ((b,d) : (DHT.DHT * DHT.DHT)) : (DHT.DHT * DHT.DHT) =
          let events = List.rev b.events in
          let _ = printfn "events %A" events in
          let newb = { b with events = [] } in
          (newb, List.fold pushPacket d events)
        in
        let bsid = DHT.hashId "dht" in
        let bst1 = DHT.init { DHT.defaultOpts with bootstrap = [| |] } in
        let dht1 =
          DHT.init
            { DHT.defaultOpts with
                bootstrap =
                  [| { host = "4.3.2.1"
                     ; port = 9999
                     ; id = bsid
                     } 
                  |] 
            }
        in
        let dht2 = DHT.bootstrap dht1 in
        let dht3 =
          Array.fold
            (fun (dht,bst) t ->
              let d = DHT.tick 0 dht in
              let b = DHT.tick 0 bst in
              let (b1,d1) = pushFrom (b,d) in
              pushFrom (d,b)
            )
            (dht2,bst1)
            (Array.init 50 id)
        in
        massert.ok true ;
        donef ()
  ]
