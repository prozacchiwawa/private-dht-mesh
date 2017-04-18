module DHTData

open Buffer

type Node =
  { id : Buffer
  ; port : int
  ; host : string
  ; referer : Buffer
  ; distance : Buffer
  ; queried : bool
  ; vectorClock : int
  }
and NodeIdent =
  { id : Buffer
  ; port : int
  ; host : string
  }
and HostIdent =
  { port : int
  ; host : string
  }

let identOfNode (node : Node) =
  { NodeIdent.id = node.id
  ; NodeIdent.host = node.host
  ; NodeIdent.port = node.port
  }
