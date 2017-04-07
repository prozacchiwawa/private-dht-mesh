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
        let pushPacket
              ((host,port) : (string * int))
              (d : DHT.DHT)
              (l : DHT.Action) : DHT.DHT =
          let getIdFromRequest b =
            let ids =
              Serialize.field "id" b
              |> optionMap Serialize.asString
              |> optionDefault ""
            in
            Buffer.fromString ids "binary"
          in
          let _ = printfn "event %A" l in
          match l with
          | DHT.Request (b,_) ->
             let id = getIdFromRequest b in
             DHT._onrequest 0 b { id = id ; host = host ; port = port } d
          | DHT.Response (b,_) ->
             let id = getIdFromRequest b in
             DHT._onresponse 0 b { id = id ; host = host ; port = port } d
          | _ -> d
        in
        let pushFrom ((b,d) : (DHT.DHT * DHT.DHT)) : (DHT.DHT * DHT.DHT) =
          let events = List.rev b.events in
          let newb = { b with events = [] } in
          (newb,
           List.fold
             (pushPacket ("127.0.0.1", 9999))
             d
             events
          )
        in
        let bsid = DHT.hashId "dht" in
        let bst1 = DHT.init { DHT.defaultOpts with bootstrap = [| |] } in
        let dht1 =
          DHT.init
            { DHT.defaultOpts with
                id = Some (DHT.hashId "1") ;
                bootstrap =
                  [| { host = "4.3.2.1"
                     ; port = 9999
                     ; id = bsid
                     } 
                  |]
            }
        in
        let dhta =
          DHT.init
            { DHT.defaultOpts with
                id = Some (DHT.hashId "a") ;
                bootstrap =
                  [| { host = "9.8.7.6"
                     ; port = 9999
                     ; id = bsid
                     }
                  |]
            }
        in
        let dht2 = DHT.bootstrap dht1 in
        let dhtb = DHT.bootstrap dhta in
        let (dht3,bst2) =
          Array.fold
            (fun (dht,bst) t ->
              let d = DHT.tick 0 dht in
              let b = DHT.tick 0 bst in
              let (b1,d1) = pushFrom (b,d) in
              pushFrom (d1,b1)
            )
            (dht2,bst1)
            (Array.init 50 id)
        in
        let (dhtc,bst3) =
          Array.fold
            (fun (dht,bst) t ->
              let d = DHT.tick 0 dht in
              let b = DHT.tick 0 bst in
              let (b1,d1) = pushFrom (b,d) in
              pushFrom (d1,b1)
            )
            (dhtb,bst2)
            (Array.init 50 id)
        in
        let nc = KBucket.toArray dhtc.nodes in
        let _ = dump "c-nodes" nc in
        let n2 = KBucket.toArray bst3.nodes in
        let _ = dump "2-nodes" n2 in
        massert.ok (Array.length nc > 1) ;
        donef ()
  ]
