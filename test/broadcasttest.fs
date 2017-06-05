module BroadcastTest

open Util
open Buffer
open MochaTest
open DHTData
open Broadcast

type DoneF = unit -> unit
type Test = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

type IterateAction =
  | Wait of int
  | Do of Broadcast.Msg<string>

type BIter =
  { broadcast : Broadcast.State<string>
  ; events : Broadcast.SideEffect<string> list
  }
            
let rec runIter l b =
  match l with
  | [] -> b
  | (Wait 0) :: tl -> runIter tl b
  | (Wait n) :: tl ->
     let (newb,beff) = Broadcast.update (Broadcast.TimeTick) b.broadcast in
     runIter
       ((Wait (n-1)) :: tl)
       { b with broadcast = newb ; events = beff @ b.events }
  | (Do msg) :: tl ->
     let (newb,beff) = Broadcast.update msg b.broadcast in
     runIter
       tl
       { b with broadcast = newb ; events = beff @ b.events }

let startIter broadcast = { broadcast = broadcast ; events = [] }
     
let tests : Test list =
  [ "should be creatable" =>
      fun donef -> let i = Broadcast.init 3 in donef ()
  ; "should send a datagram when we join a channel" =>
      fun donef ->
        let res =
          Broadcast.init 3
          |> startIter
          |> runIter
               [ Do (SetId "our-node")
               ; Do (AddNode "other-node")
               ; Wait 1
               ; Do (JoinBroadcast "foo")
               ; Wait 5
               ]
        in
        let packets =
          List.rev res.events
          |> List.map
               (fun eff ->
                 match eff with
                 | OutPacket p -> [p]
                 | _ -> []
               )
          |> List.concat
        in
        let _ = massert.ok (List.length packets > 0) in
        donef ()
  ]
