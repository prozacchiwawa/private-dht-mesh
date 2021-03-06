module Forward

open Util
open Queue

type ConnGraph<'node> =
  { nodes : Map<string,'node>
  ; edges : Map<string, Set<string> >
  }

let init () =
  { nodes = Map.ofSeq [] ; edges = Map.ofSeq [] }

let addNode id (n : 'node) g =
  { g with nodes = Map.add id n g.nodes }

let removeNode n g =
  let edgesWithoutNode n =
    Map.toSeq g.edges
    |> Seq.map
         (fun (from,tos) ->
           (from, Seq.filter (fun t -> t <> n) tos |> Set.ofSeq)
         )
    |> Map.ofSeq
  in
  { g with
      nodes = Map.remove n g.nodes ;
      edges = edgesWithoutNode n
  }

let addEdge id1 id2 g =
  let edges0 =
      Map.tryFind id1 g.edges
    |> Option.map (fun e -> Map.add id1 (Set.add id2 e) g.edges)
    |> optionDefault (Map.add id1 (Set.ofSeq [id2]) g.edges)
  in
  let edges =
    Map.tryFind id2 edges0
  |> Option.map (fun e -> Map.add id2 (Set.add id1 e) edges0)
  |> optionDefault (Map.add id2 (Set.ofSeq [id1]) edges0)
  in
  { g with edges = edges }

let connected id1 id2 g =
  Map.tryFind id1 g.edges
  |> Option.map (Set.contains id2)
  |> Option.filter id
  |> optionDefault
       (Map.tryFind id2 g.edges
        |> Option.map (Set.contains id1)
        |> ((=) (Some true))
       )

let removeEdge id1 id2 g =
  let removeOneEdgeDirection id1 id2 g =
    let edges =
      Map.tryFind id1 g.edges
      |> Option.map (fun e -> Map.add id1 (Set.remove id2 e) g.edges)
      |> optionDefault g.edges
    in
    { g with edges = edges }
  in
  removeOneEdgeDirection id1 id2 g
  |> removeOneEdgeDirection id2 id1
    
type QueueElement =
  { reversePath : string list
  ; currentNode : string
  }

let bfs e1 e2 g =
  let rec bfsImpl q d =
    try
      let (e,q) = Queue.dequeue q in
      if e.currentNode = e2 then
        List.rev e.reversePath
      else
        let edges =
          g.edges
        |> Map.tryFind e.currentNode
        |> Option.map Set.toSeq
        |> optionDefault (Seq.ofList [])
        in
        let nq =
          Seq.fold
            (fun q edge ->
              Queue.enqueue
                q
                { reversePath = edge::e.reversePath
                ; currentNode = edge
                }
            )
            q
            edges
        in
        bfsImpl nq d
    with _ -> []
  in
  bfsImpl (Queue.ofList [{reversePath = [e1]; currentNode = e1}]) e2
