module DHTData

open Buffer

type Node =
  { id : Buffer
  ; port : int
  ; host : string
  ; roundtripToken : string
  ; referer : Buffer
  ; distance : Buffer
  ; queried : bool
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
and ResponseEmbryo =
  { port : int
  ; host : string
  ; request : bool
  ; tid : int
  }
