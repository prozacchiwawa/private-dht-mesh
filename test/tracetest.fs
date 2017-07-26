module TraceTest

open Util
open MochaTest
open Topology
     
type DoneF = unit -> unit
type Test = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let stepsToMessage toward res =
  let steps =
    Seq.map
      (fun (host : string) -> { Topology.host = host ; Topology.rtt = 1.0 })
      res
    |> List.ofSeq
  in
  Topology.EndTrace (toward,steps)

let isTraceMsg m =
  match m with
  | StartTrace _ -> true
  | _ -> false

let basicNetworkTwoNodes =
  Topology.init
  |> Topology.update (Topology.AddNode "a")
  |> Topology.update (Topology.AddNode "b")
  |> Topology.update Topology.Tick
  |> Topology.update Topology.Tick
  |> Topology.update Topology.Tick
  |> Topology.update
       (stepsToMessage
          "b"
          [ "a.pc" ; "a.house" ; "a.isp"
          ; "back.bone"
          ; "b.isp" ; "b.house" ; "b.pc"
          ]
       )

let cutoffB topo =
  topo
  |> Topology.update
       (stepsToMessage
          "b"
          [ "a.pc" ; "a.house" ; "a.isp"
          ; "*"
          ; "*"
          ]
       )

let tests : Test list =
  [ "should issue trace commands periodically" =>
      (fun donef ->
        let topo =
          Topology.init
          |> Topology.update (Topology.AddNode "a")
          |> Topology.update (Topology.AddNode "b")
          |> Topology.update Topology.Tick
          |> Topology.update Topology.Tick
          |> Topology.update Topology.Tick
        in
        let _ = massert.ok (List.filter isTraceMsg topo.events <> []) in
        donef ()
      )
  ; "should track whether a node is reachable" =>
      (fun donef ->
        let topo = basicNetworkTwoNodes in
        let _ = massert.ok (Topology.isReachable "b" topo) in
        let topo = cutoffB topo in
        let _ = massert.ok (not (Topology.isReachable "b" topo)) in
        donef ()
      )
  ]
