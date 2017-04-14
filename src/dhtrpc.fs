module DHTRPC

open DHT

type Query =
  { id : string
  ; tid : Buffer
  ; passOn : Serialize.Json
  ; directRequest : bool
  ; accumulated : Buffer array
  ; closest : Buffer array
  ; pendingContacts : Buffer list
  ; pendingReplies : Buffer array
  }

type DWQAction =
  | QueryCreated of string
  | QueryReply of (string * Buffer * Serialize.Json)
  | QueryError of (string * Buffer * string)
  | ClosestResult of (string * (Buffer * Serialize.Json) array)
  | Datagram of (Serialize.Json * NodeIdent)

type DHTWithQueryProcessing =
  { dht : DHT
  ; events : DWQAction list
  ; numActiveQueries : int
  ; activeQueries : Map<string,Query>
  ; pendingQueries : Query list
  ; maxParallel : int
  }

(* Make a direct request.
 * Deliver the indicated query directly to the desired node, doing multiple
 * _find_nodes hops if needed.
 * Accumulate a stream of query reply blocks and return the
 * resulting buffer.
 *)
let directQuery tid query dwq =
  let id = ShortId.generate () in
  let qWithId =
    Serialize.addField "qid" (Serialize.jsonString id) query
  in
  let queryObject =
    { id = id
    ; tid = tid
    ; passOn = qWithId
    ; directRequest = true
    ; accumulated = [| |]
    ; closest = currentClosest
    ; pendingContacts = []
    ; pendingReplies = [| |]
    }
  in
  if dwq.numActiveQueries + 1 >= dwq.maxParallel then
    { dwq with
        pendingQueries = queryObject :: dwq.pendingQueries ;
        events = (QueryCreated id) :: dwq.events
    }
  else
    { dwq with
        activeQueries =
          Map.add
            id
            queryObject
            dwq.activeQueries ;
        numActiveQueries = dwq.numActiveQueries + 1 ;
        dht = _findnode tid dwq.dht
    }

let tick dwq =
  let passThroughDhtEvents events =
    match events with
    | [] -> []
    | (DHT.Datagram (json,tgt)) :: tl ->
       Datagram (json,tgt) :: (passThroughDhtEvents tl)
    | (DHT.Payload (json,source)) :: tl ->
       passThroughDhtEvents tl
    | hd :: tl ->
       passThroughDhtEvents tl
  in
  let tdht = dhtUpdateFun dwq.dht in
  let events = tdht.events in
  let myEvents =
      newDwq.events
      |> Seq.map
           (fun a ->
             match a with
             | DHT.Payload (json,source) -> [(json,source)]
             | _ -> []
           )
      |> Seq.concat
  in
  let dht = { tdht with events = [] } in
  let newDwq = { dwq with dht = dht; events = (passThroughDhtEvents events) @ dwq.events } in
  List.fold
    (fun dwq (json,source) ->
      match Serialize.field "action" json |> optionMap Serialize.asString with
      | Some "" -> dwq
      | None -> dwq
    )
    newDwq
    myEvents
           
                
    
