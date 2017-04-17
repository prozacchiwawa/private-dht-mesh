module DHTRPC

open Util
open Buffer
open KBucket
open DHTData
open DHT

type Query =
  { id : string
  ; tid : Buffer
  ; passOn : Serialize.Json
  ; pieces : int
  ; accumulated : Map<int,Buffer>
  ; closest : NodeIdent array
  }

type InternalAction =
  | Bootstrapped
  | Datagram of (Serialize.Json * NodeIdent)
  | Payload of (Serialize.Json * NodeIdent)
  | Find of (Buffer * (NodeIdent array))

type DWQAction =
  | QueryCreated of string
  | QueryReply of (string * Buffer * Serialize.Json)
  | QueryError of (string * Buffer * string)
  | SendDatagram of (Serialize.Json * NodeIdent)

type DHTWithQueryProcessing<'dht> =
  { dht : 'dht
  ; events : DWQAction list
  ; activeQueries : Map<string,Query>
  ; drilling : Map<string,Query>
  ; pendingQueries : Query list
  ; maxParallel : int
  ; bootstrapped : bool
  }

and DHTOps<'dht> =
  { dhtId : 'dht -> Buffer
  ; findnode : Buffer -> Buffer option -> 'dht -> 'dht
  ; query : int -> Buffer -> Serialize.Json -> 'dht -> 'dht
  ; closest : int -> Buffer -> 'dht -> NodeIdent array
  ; harvest : 'dht -> (InternalAction list * 'dht)
  ; tick : 'dht -> 'dht
  }

let init dht =
  { dht = dht
  ; events = []
  ; activeQueries = Map.empty
  ; drilling = Map.empty
  ; pendingQueries = []
  ; maxParallel = 16
  ; bootstrapped = false
  }
    
let startQuery dhtOps (query : Query) dwq =
  let toask =
    if Array.length query.closest > 0 then
      Some query.closest.[0]
    else
      None
  in
  let _ =
    printfn
      "startQuery: ask %A"
      (toask |> optionMap (fun n -> Buffer.toString "binary" n.id))
  in
  let removeId = Buffer.toString "binary" query.tid in
  if toask |> optionMap (fun t -> Buffer.equal t.id query.tid) |> optionDefault false then
    { dwq with
        activeQueries =
          Map.add
            query.id
            query
            dwq.activeQueries ;
        drilling = Map.remove removeId dwq.drilling ;
        dht =
          dhtOps.query
            (dwq.drilling.Count + dwq.activeQueries.Count)
            query.tid
            query.passOn
            dwq.dht
    }
  else
    let _ = printfn "drill down toward our guy %A" removeId in
    { dwq with
        drilling =
          Map.add
            removeId
            query
            dwq.drilling ;
        dht =
          dhtOps.findnode
            query.tid
            (toask |> optionMap (fun toask -> toask.id))
            dwq.dht
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
  let currentClosest = dhtOps.closest 8 tid dwq.dht in
  let queryObject =
    { id = id
    ; tid = tid
    ; passOn = qWithId
    ; accumulated = Map.empty
    ; closest = currentClosest
    ; pieces = 0
    }
  in
  if dwq.activeQueries.Count + 1 >= dwq.maxParallel || not dwq.bootstrapped then
    { dwq with
        pendingQueries = queryObject :: dwq.pendingQueries ;
        events = (QueryCreated id) :: dwq.events
    } 
  else
    startQuery dhtOps queryObject dwq

let _onresponse (id : Buffer) (resp : Serialize.Json) query dwq =
  { dwq with
      activeQueries = Map.remove query.id dwq.activeQueries ;
      events = QueryReply (query.id, id, resp) :: dwq.events
  }

let takeDatagram dhtOps resp dwq =
  Serialize.field "txid" resp |> optionMap Serialize.asString
  |> optionThen (fun id -> Map.tryFind id dwq.activeQueries)
  |> optionMap
       (fun query ->
         let mt =
           (Serialize.field "_p" resp |> optionThen Serialize.floor
           ,Serialize.field "_n" resp |> optionThen Serialize.floor
           ,Serialize.field "_b" resp
            |> optionMap Serialize.asString
            |> optionMap (fun b -> Buffer.fromString b "binary")
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
              |> Buffer.toString "utf-8"
              |> Serialize.parse
              |> optionMap (fun resp -> _onresponse id resp query dwq)
              |> optionDefault
                   { dwq with
                       activeQueries = Map.remove query.id dwq.activeQueries ;
                       events =
                         (QueryError (query.id, id, "Decode error")) ::
                           dwq.events
                   }
            else
              { dwq with activeQueries = Map.add query.id newq dwq.activeQueries }
         | _ -> dwq
       )
  |> optionDefault dwq

let (kBucketOps : KBucketAbstract<Buffer,Node>) =
  { distance = KBucket.defaultDistance
  ; nodeId = fun n -> n.id
  ; arbiter = fun a b -> a
  ; keyLength = Buffer.length
  ; keyNth = Buffer.at
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let sortClosest tid (incoming : NodeIdent array) = 
  incoming
  |> Array.map (fun n -> (n, KBucket.defaultDistance kBucketOps n.id tid))
  |> Array.sortWith
       (fun (a,ad) (b,bd) ->
         if ad < bd then
           -1
         else if ad > bd then
           1
         else
           0
       )
  |> Array.map (fun (a,_) -> a)
               
let takeFind dhtOps id peers (q : Query) dwq =
  let toComparable (n : NodeIdent) =
    (Buffer.toString "binary" n.id, n.host, n.port)
  in
  let qClosest = Set.ofSeq (Array.map toComparable q.closest) in
  let incoming = Set.ofSeq (Array.map toComparable peers) in
  let total = Set.union qClosest incoming in
  let _ =
    printfn
      "Prev closest %d got %d union %d"
      qClosest.Count
      incoming.Count
      total.Count
  in
  if total.Count = qClosest.Count then
    (* We didn't advance, we're as close as we come *)
    if Array.length q.closest > 0 && Buffer.equal q.closest.[0].id q.tid then
      startQuery dhtOps q dwq
    else
      { dwq with
          drilling = Map.remove (Buffer.toString "binary" q.tid) dwq.drilling ;
          activeQueries = Map.remove q.id dwq.activeQueries ;
          events = (QueryError (q.id, q.tid, "node not found")) :: dwq.events
      }
  else
    (* Still advancing *)
    let closestWithDistances =
      incoming
      |> Set.toSeq
      |> Array.ofSeq
      |> Array.map
           (fun (id,host,port) ->
             let bid = Buffer.fromString id "binary" in
             { NodeIdent.id = bid
             ; NodeIdent.host = host
             ; NodeIdent.port = port
             }
           )
      |> sortClosest q.tid
    in
    let _ = printfn "Closest by distance %A -> %A" (Buffer.toString "binary" q.tid) (Array.map (fun (n : NodeIdent) -> (n.host,KBucket.defaultDistance kBucketOps q.tid n.id)) closestWithDistances) in
    startQuery
      dhtOps
      { q with closest = Array.sub closestWithDistances 0 8 }
      { dwq with 
          drilling = Map.remove (Buffer.toString "binary" q.tid) dwq.drilling
      }
      
let rec dhtPassThrough (dht : DHT.DHT) : (InternalAction list * DHT.DHT) =
  (dht.events
   |> Seq.map
        (fun evt ->
          match evt with
          | DHT.Datagram (json,tgt) -> [Datagram (json,tgt)]
          | DHT.Ready -> [Bootstrapped]
          | DHT.Payload (json,tgt) -> [Payload (json,tgt)]
          | DHT.FindNode (target,peers) -> [Find (target,peers)]
          | _ -> []
        )
   |> Seq.concat
   |> List.ofSeq
  ,{ dht with events = [] }
  )
    
let tick
      (dhtOps : DHTOps<'dht>)
      (dwq : DHTWithQueryProcessing<'dht>) : DHTWithQueryProcessing<'dht> =
  let (dht : 'dht) = dhtOps.tick dwq.dht in
  let (events : InternalAction list, dht) = dhtOps.harvest dht in
  let (dwq : DHTWithQueryProcessing<'dht>) = { dwq with dht = dht } in
  List.fold
    (fun (dwq : DHTWithQueryProcessing<'dht>) (evt : InternalAction) ->
      match evt with
      | Find (target,peers) ->
         let drillingFor =
           Map.tryFind (Buffer.toString "binary" target) dwq.drilling
         in
         let _ = printfn "Find found drilling %A" drillingFor in
         (match drillingFor with
          | Some q -> takeFind dhtOps target peers q dwq
          | None -> dwq
         )
      | Payload (json,source) ->
         takeDatagram dhtOps json dwq
      | Datagram (json,source) ->
         { dwq with events = (SendDatagram (json,source)) :: dwq.events }
      | Bootstrapped ->
         let _ = printfn "Bootstrapped: releasing %A" dwq.pendingQueries in
         match dwq.pendingQueries with
         | hd :: tl ->
            startQuery dhtOps hd { dwq with pendingQueries = tl }
         | _ -> dwq
    )
    dwq
    events
