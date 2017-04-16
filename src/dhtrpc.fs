module DHTRPC

open DHT

type Query =
  { id : string
  ; tid : Buffer
  ; passOn : Serialize.Json
  ; pieces : int
  ; accumulated : Map<int,Buffer>
  ; closest : NodeIdent array
  }

type DWQAction =
  | Bootstrapped
  | QueryCreated of string
  | QueryReply of (string * Buffer * Serialize.Json)
  | QueryError of (string * Buffer * string)
  | Datagram of (Serialize.Json * NodeIdent)
  | ReceivedQuery

type DHTWithQueryProcessing<'dht> =
  { dht : 'dht
  ; events : DWQAction list
  ; activeQueries : Map<string,Query>
  ; drilling : Map<String,Query>
  ; pendingQueries : Query list
  ; maxParallel : int
  ; bootstrapped : bool
  }

and DHTOps<'dht> =
  { findnode : Buffer -> Buffer option -> 'dht -> 'dht
  ; query : int -> Serialize.Json -> 'dht -> 'dht
  ; closest : int -> 'dht -> NodeIdent array
  }

let startQuery query dwq =
  let toask =
    if Array.length query.closest then
      Some query.closest.[0]
    else
      None
  in
  if toask = query.tid then
    { dwq with
        activeQueries =
          Map.add
            query.id
            query
            dwq.activeQueries ;
        drilling = Map.remove query.id dwq.drilling ;
        dht =
          dhtOps.query
            (dwq.drilling.Count + dwq.activeQueries.Count)
            query.passOn
            dwq.dht
    }
  else
    { dwq with
        activeQueries =
          Map.add
            id
            queryObject
            dwq.activeQueries ;
        dht = dhtOps.findnode tid toask dwq.dht
    }

(* Make a direct request.
 * Deliver the indicated query directly to the desired node, doing multiple
 * _find_nodes hops if needed.
 * Accumulate a stream of query reply blocks and return the
 * resulting buffer.
 *)
let directQuery dhtOps tid query dwq =
  let id = ShortId.generate () in
  let qWithId =
    Serialize.addField "txid" (Serialize.jsonString id) query
  in
  let currentClosest = dhtOps.closest 8 dwq.dht in
  let queryObject =
    { id = id
    ; tid = tid
    ; passOn = qWithId
    ; accumulated = Map.empty
    ; closest = currentClosest
    }
  in
  if dwq.activeQueries.Count + 1 >= dwq.maxParallel || not dwq.bootstrapped then
    { dwq with
        pendingQueries = queryObject :: dwq.pendingQueries ;
        events = (QueryCreated id) :: dwq.events
    } 
  else
    startQuery queryObject dwq

let _onresponse (id : Buffer) (resp : Serialize.Json) query dwq =
  { dwq with
      activeQueries = Map.remove query.id dwq.activeQueries ;
      events = QueryReply (query.id, id, resp) :: dwq.events
  }

let takeDatagram dhtOps resp dwq =
  Serialize.field "txid" resp
  |> optionThen (fun id -> Map.tryFind id dwq.activeQueries)
  |> optionThen
       (fun query ->
         let mt =
           (Serialize.field "_p" resp |> optionThen Serialize.floor
           ,Serialize.field "_n" resp |> optionThen Serialize.floor
           ,Serialize.field "_b" resp |> optionThen Serialize.floor
           ,Serialize.field "id" resp
            |> optionMap Serialize.asString
            |> optionMap (fun s -> Buffer.fromString s "binary")
           )
         in
         match mt with
         | (None, None, None, Some id) ->
            (* Direct result *)
            _onresponse id resp query dwq
         | (Some _p, Some _n, Some _b, Some id) ->
            let newq =
              { query with
                  pieces = _p ;
                  accumulated = Map.add _n _b query.accumulated
              }
            in
            let bufs =
              newq.accumulated |> Map.toSeq |> Seq.map (fun (k,v) -> v)
              |> List.ofSeq
            in
            let rec copyAllInto wpos tbuf l =
              match l with
              | [] -> tbuf
              | hd :: tl ->
                 begin
                   let len = Buffer.length hd in
                   Buffer.copy wpos len hd 0 tbuf ;
                   copyAllInto (wpos + len) tbuf tl
                 end
            in
            if newq.accumulated.Count = _p then
              let targetBuf =
                bufs |> Seq.map Buffer.length |> Seq.sum |> Buffer.zero
              in
              copyAllInto 0 targetBuf bufs
              |> optionThen
                   (fun b ->
                     Serialize.parse (Buffer.toString targetBuf "utf-8")
                   )
              |> optionMap (fun resp -> _onresponse id resp query dwq)
              |> optionDefault
                   { dwq with
                       activeQueries = Map.remove query.id dwq.activeQueries ;
                       events = (QueryError (query.id, id, "Decode error"))
                   }
            else
              { dwq with activeQueries = Map.add query.id newq dwq.activeQueries }
         | _ -> dwq
       )
  |> optionDefault dwq

let takeFind dhtOps peers q dwq =
  let qClosest = Set.ofSeq q.closest in
  let incoming = Set.ofseq peers in
  let total = Set.union qclosest incoming in
  if total.Count = qClosest.Count then
    (* We didn't advance, we're as close as we come *)
    if Array.length q.closest > 0 then
      startQuery q dwq
    else
      { dwq with
          drilling = Map.remove q.id dwq.drilling ;
          activeQueries = Map.remove q.id dwq.activeQueries ;
          events = (QueryError (q.id, q.tid, "node not found")) :: dwq.events
      }
  else
    (* Still advancing *)
    dwq
      
let tick dhtOps dwq =
  let passThroughDhtEvents events =
    match events with
    | [] -> []
    | (DHT.Datagram (json,tgt)) :: tl ->
       (Datagram (json,tgt)) :: (passThroughDhtEvents tl)
    | DHT.Ready :: tl ->
       Bootstrapped :: (passThroughDhtEvents tl)
    | _ :: tl ->
       passThroughDhtEvents tl
  in
  let tdht = dhtUpdateFun dwq.dht in
  let events = tdht.events in
  let myEvents =
      newDwq.events
      |> Seq.map
           (fun a ->
             match a with
             | DHT.Payload (json,source) -> [Datagram (json,source)]
             | DHT.FindNode (target,peers) -> [FindNode (target,peers)]
             | _ -> []
           )
      |> Seq.concat
  in
  let dht = { tdht with events = [] } in
  let newDwq = { dwq with dht = dht; events = (passThroughDhtEvents events) @ dwq.events } in
  List.fold
    (fun dwq evt ->
      match evt with
      | Find (target,peers) ->
         (match Map.tryFind (Buffer.toString target "binary") dwq.drilling with
          | Some q -> takeFind dhtOps peers q dwq
          | None -> dwq
         )
      | Datagram (json,source) ->
         takeDatagram dhtOps json dwq
      | Ready ->
         match dwq.pendingQueries with
         | hd :: tl ->
            startQuery hd { dwq with pendingQueries = tl }
         | _ -> dwq
      | _ -> dwq
    )
    newDwq
    myEvents
