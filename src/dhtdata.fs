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
and AbbrevNode =
  { id : Buffer
  ; port : int
  ; host : string
  }
and NodeIdent =
  { id : Buffer
  ; port : int
  ; host : string
  }
