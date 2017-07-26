module Topology

open Util
open Bacon
open Return

type TraceStep =
  { host : string
  ; rtt : float
  }
  
type Msg =
  | NoOp
  | AddNode of string
  | RemoveNode of string
  | Tick
  | EndTrace of (string * TraceStep list)

type SideEffect =
  | StartTrace of string
  | NodeReachable of string
  | NodeUnreachable of string

type TraceState =
  { id : string
  ; trace : TraceStep list
  }
                
type TopologyChecker =
  { nodes : Map<string,TraceState>
  ; edges : Map<Set<string>, Set<string> >
  ; tick : int
  ; events : SideEffect list
  ; processing : string
  ; remaining : string list
  ; unreach : Set<string>
  }

let init =
  { nodes = Map.empty
  ; edges = Map.empty
  ; tick = 0
  ; events = []
  ; processing = ""
  ; remaining = []
  ; unreach = Set.empty
  }

let passOnEffect (outputBus : bacon.Bus<SideEffect,unit>) e s =
  outputBus.push e ; s

let maybeRelaunch state =
  state

let addTraceEdge node last step state =
  let newEdge = Set.ofSeq [last; step.host] in
  let newOwners =
    Map.tryFind newEdge state.edges
    |> optionDefault Set.empty
    |> Set.add node
  in
  { state with edges = Map.add newEdge newOwners state.edges }
  
let addTraceEdges node steps state =
  Seq.fold
    (fun (last,state) step ->
      match last with
      | Some l ->
         if step.host <> "*" then
           (Some step.host, addTraceEdge node l step state)
         else
           (Some step.host, { state with unreach = Set.add node state.unreach })
      | None -> (Some step.host, state)
    )
    (None,state)
    steps

let rec removeOldTrace (last : TraceStep option) (trace : TraceStep list) (node : string) state =
  match (last,trace) with
  | (None, hd :: tl) ->
     if hd.host = "*" then
       state
     else
       removeOldTrace (Some hd) tl node state
  | (Some last, hd :: tl) ->
     let key = Set.ofSeq [last.host ; hd.host] in
     let edgeOwnedBy =
       Map.tryFind key state.edges
       |> Option.map (Set.remove node)
     in
     let edges =
       match edgeOwnedBy with
       | Some e ->
          if Set.isEmpty e then
            Map.remove key state.edges
          else
            Map.add key e state.edges
       | None -> state.edges
     in
     removeOldTrace (Some hd) tl node { state with edges = edges }
  | (_, []) -> state

let isReachable node state =
  Map.tryFind node state.nodes
  |> Option.map (fun _ -> not (Set.contains node state.unreach))
  |> optionDefault false
             
let rec update msg state =
  match msg with
  | AddNode n ->
     { state with
         nodes = Map.add n { id = n ; trace = [] } state.nodes ;
         remaining = state.remaining @ [n]
     }
  | RemoveNode n ->
     { state with
         nodes = Map.remove n state.nodes ;
         remaining = List.filter (fun a -> a <> n) state.remaining ;
         processing = if state.processing = n then "" else state.processing
     }
  | Tick ->
     if state.processing = "" then
       match state.remaining with
       | hd :: tl ->
          { state with
              processing = hd ;
              remaining = tl ;
              events = (StartTrace hd) :: state.events
          }
       | [] ->
          let newproc = state.nodes |> Map.toSeq |> Seq.map fst |> List.ofSeq in
          match newproc with
          | hd :: tl ->
             { state with processing = hd ; remaining = tl ; events = (StartTrace hd) :: state.events }
          | _ -> state
     else
       state
  | EndTrace (node,steps) ->
     state.nodes
     |> Map.tryFind node
     |> Option.map
          (fun node ->
            { (removeOldTrace None node.trace node.id { state with unreach = Set.remove node.id state.unreach }) with
                nodes =
                  Map.add
                    node.id
                    { node with trace = steps }
                    state.nodes ;
                processing =
                  if node.id = state.processing then "" else state.processing
            }
            |> addTraceEdges node.id steps
          )
     |> Option.map snd
     |> optionDefault state
    
