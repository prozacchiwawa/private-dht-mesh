module Forward

type Node =
  { local : bool
  ; target : string
  }

type ConnGraph =
  { nodes : Map<string,Node>
  ; edges : Map<string,Set<string> >
  }

let init () =
  { nodes = Map.ofSeq [] ; edges = Map.ofSeq [] }

