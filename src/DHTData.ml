open Buffer

type node =
  { id : string
  ; port : int
  ; host : string
  ; referer : string
  ; distance : string
  ; queried : bool
  ; vectorClock : int
  }
and nodeIdent =
  { id : string
  ; port : int
  ; host : string
  }
and hostIdent =
  { port : int
  ; host : string
  }

let identOfNode (node : node) =
  { id = node.id
  ; host = node.host
  ; port = node.port
  }
