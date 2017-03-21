module Forward

type Node =
  { local : bool
  ; target : string
  ; id : string
  }

type ConnGraph =
  { nodes : Map<string,Node>
  ; edges : Set<(string * string)>
  }

let init () =
  { nodes = Map.ofSeq [] ; edges = Set.ofSeq [] }

let addNode n g =
  { g with nodes = Map.add n.id n g.nodes }

let addEdge id1 id2 g =
  { g with edges = Set.add (id1,id2) (Set.add (id2,id1) g.edges) }

let connected id1 id2 g =
  Set.contains (id1,id2) g.edges

let removeEdge id1 id2 g =
  { g with edges = Set.remove (id1,id2) (Set.remove (id2,id1) g.edges) }
