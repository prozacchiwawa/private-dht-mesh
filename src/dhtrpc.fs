module DHTRPC

open Util
open Buffer
open KBucket
open DHTData
open DHT

(*
 * Transaction layer protocol:
 *
 * Query send (limited number of retries until any reply is received with
 * the rxid = query.txid)
 * 
 * {
 *  _p, _n, _b -> Packet is one of a set of reply packets.
 *  On each tick where an outstanding and not expired reply set exists, send
 *  a have bitmap back indicating missing parts.
 *  - or -
 *  arbitrary -> Packet is a short reply.
 * }
 * On complete send a limited number of acknowlegements until a
 *  command = "ackreply" is received or all acks expended.
 * Upon receiving ack, send ackreply once.
 *) 
       
type Query =
  { id : string
  ; tid : Buffer
  ; retry : int list
  ; deadline : int
  ; from : NodeIdent option
  ; passOn : Serialize.Json
  ; pieces : int
  ; accumulated : Map<int,Buffer>
  ; closestNodes : NodeIdent array
  }

type InternalAction =
  | Bootstrapped
  | Datagram of (Serialize.Json * NodeIdent)
  | Payload of (Serialize.Json * NodeIdent)
  | Find of (Buffer * (NodeIdent array))

type DWQAction =
  | QueryRequest of (string * NodeIdent * Serialize.Json)
  | QueryReply of (string * NodeIdent * Serialize.Json)
  | QueryError of (string * Buffer * string)
  | SendDatagram of (Serialize.Json * NodeIdent)

type DHTWithQueryProcessing<'dht> =
  { dht : 'dht
  ; events : DWQAction list
  ; future : Map<int,Set<string> >
  ; partial : Set<string>
  ; deadlines : Map<int,Set<string> >
  ; activeQueries : Map<string,Query>
  ; drilling : Map<string,Query>
  ; pendingQueries : Query list
  ; maxParallel : int
  ; bootstrapped : bool
  ; tick : int
  }

and DHTOps<'dht> =
  { (* Get the ID of this DHT as a Buffer *)
    dhtId : 'dht -> Buffer
  ; (* Start a findnode operation with
     * - inflight requests
     * - string Txid
     * - Buffer node to find
     * - NodeIdent to ask if known
     * - DHT
     *)
    findnode : int -> string -> Buffer -> NodeIdent option -> 'dht -> 'dht
  ; (* Deliver a query to the node whose id is in Buffer
     * - inflight requests
     * - Buffer node to contact
     * - Json query to deliver
     * - DHT
     *)
    query : int -> NodeIdent -> Serialize.Json -> 'dht -> 'dht
  ; (* See KBucket.closest *)
    getClosest : int -> Buffer -> 'dht -> NodeIdent array
  ; (* Harvest events from this object into actionable form.
     * Return the events and the drained object.
     *)
    harvest : 'dht -> (InternalAction list * 'dht)
  ; (* Run the timer tick given the current number of inflight requests *)
    tick : int -> 'dht -> 'dht
  ; (* Give a datagram to the DHT to interpret *)
    recv : int -> Serialize.Json -> NodeIdent -> 'dht -> 'dht
  }

let queryRetries = [2;4;7;15]
let ackRetries = [1;2;4]
let queryMaxLife = 25
let replyMaxLife = 15

let asl v s = v <<< s 
(* let asr v s = v >>> s *)

let init dht =
  { dht = dht
  ; events = []
  ; future = Map.empty
  ; partial = Set.empty
  ; deadlines = Map.empty
  ; activeQueries = Map.empty
  ; drilling = Map.empty
  ; pendingQueries = []
  ; maxParallel = 16
  ; bootstrapped = false
  ; tick = 0
  }

(* If the query has pending retries, add the next one to the future set.
 * We'll remove the head timeout when we process the future tick for that
 * query, so we'll defer modifying it.
 *)
let updateRetry (query : Query) (dwq : DHTWithQueryProcessing<'dht>) =
  match query.retry with
  | hd :: tl ->
     let (futureAtTick : int) = dwq.tick + hd in
     let futureSet =
       Map.tryFind futureAtTick dwq.future
       |> optionDefault Set.empty
     in
     { dwq with
         future = Map.add futureAtTick (Set.add query.id futureSet) dwq.future
     }
  | _ -> dwq

let updateDeadline query dwq =
  let deadlineAtTick = query.deadline in
  let deadlineSet =
    Map.tryFind deadlineAtTick dwq.deadlines
    |> optionDefault Set.empty
  in
  { dwq with
      deadlines =
        Map.add deadlineAtTick (Set.add query.id deadlineSet) dwq.deadlines
  }
           
(* Given a part of a wire transaction, pass it down for sending on the wire.
 * If needed, start a drilling task to find the peer's network address from
 * the DHT.
 *)
let startQuery dhtOps (query : Query) dwq =
  let toask =
    match query.from with
    | Some ask -> Some ask
    | None ->
       if Array.length query.closestNodes > 0 then
         Some query.closestNodes.[0]
       else
         None
  in
  let removeId = Buffer.toString "binary" query.tid in
  let askMatch =
    toask
    |> Option.map (fun t -> (Buffer.equal t.id query.tid, toask))
    |> optionDefault (false, None)
  in
  match askMatch with
  | (true, Some toask) ->
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
             toask
             query.passOn
             dwq.dht
     }
     |> updateRetry query
     |> updateDeadline query
  | _ ->
     { dwq with
         drilling =
           Map.add
             removeId
             query
             dwq.drilling ;
         dht =
           dhtOps.findnode
             (dwq.drilling.Count + dwq.activeQueries.Count)
             query.id
             query.tid
             toask
             dwq.dht
       }
         
(* Make a direct request.
 * Deliver the indicated query directly to the desired node, doing multiple
 * _find_nodes hops if needed.
 * Accumulate a stream of query reply blocks and return the
 * resulting buffer.
 *)
let directQuery dhtOps txid tid query dwq =
  let qWithId =
    query
    |> Serialize.addField "txid" (Serialize.jsonString txid)
    |> Serialize.addField
         "target"
         (Serialize.jsonString (Buffer.toString "base64" tid))
  in
  let currentClosest = dhtOps.getClosest 8 tid dwq.dht in
  let queryObject =
    { id = txid
    ; tid = tid
    ; retry = queryRetries
    ; deadline = dwq.tick + queryMaxLife
    ; from = None
    ; passOn = qWithId
    ; accumulated = Map.empty
    ; closestNodes = currentClosest
    ; pieces = 0
    }
  in
  if dwq.activeQueries.Count + 1 >= dwq.maxParallel || not dwq.bootstrapped then
    { dwq with
        pendingQueries = queryObject :: dwq.pendingQueries
    } 
  else
    startQuery dhtOps queryObject dwq

let shortReply
      dhtOps
      (txid : string)
      (from : NodeIdent)
      (reply : Serialize.Json)
      (dwq : DHTWithQueryProcessing<'dht>) : DHTWithQueryProcessing<'dht> =
  let qWithId =
    reply
    |> Serialize.addField "rxid" (Serialize.jsonString txid)
    |> Serialize.addField "target" (Serialize.jsonString (Buffer.toString "base64" from.id))
  in
  let currentClosest = dhtOps.getClosest 8 from.id dwq.dht in
  let queryObject =
    { id = txid
    ; tid = from.id
    ; retry = []
    ; deadline = dwq.tick + replyMaxLife
    ; from = Some from
    ; passOn = qWithId
    ; accumulated = Map.empty
    ; closestNodes = currentClosest
    ; pieces = 0
    }
  in
  if dwq.activeQueries.Count + 1 >= dwq.maxParallel || not dwq.bootstrapped then
    { dwq with
        pendingQueries = queryObject :: dwq.pendingQueries
    } 
  else
    startQuery dhtOps queryObject dwq
               
let _onresponse dhtOps (peer : NodeIdent) (resp : Serialize.Json) query dwq =
  let dwq =
    let passOn =
      Serialize.jsonObject
        [| ("ack", Serialize.jsonString query.id)
         ; ("target",
            Serialize.jsonString (Buffer.toString "base64" query.tid))
        |]
    in
    { dwq with dht = dhtOps.query 0 peer passOn dwq.dht }
  in
  { dwq with
      activeQueries = Map.remove query.id dwq.activeQueries ;
      drilling = Map.remove (Buffer.toString "binary" query.tid) dwq.drilling ;
      partial = Set.remove query.id dwq.partial ;
      events = QueryReply (query.id, peer, resp) :: dwq.events
  }

let takeDatagram dhtOps (peer : NodeIdent) (resp : Serialize.Json) dwq =
  let (txid,rxid,ack) =
    (Serialize.field "txid" resp |> Option.map Serialize.asString
    ,Serialize.field "rxid" resp |> Option.map Serialize.asString
    ,Serialize.field "ack" resp |> Option.map Serialize.asString
    )
  in
  match (txid,rxid,ack) with
  | (_,_,Some ack) ->
     (* Ack *)
     Map.tryFind ack dwq.activeQueries
     |> Option.map
          (fun query ->
            { dwq with
                activeQueries = Map.remove query.id dwq.activeQueries
            }
          )
     |> optionDefault dwq
  | (_,Some rxid,_) ->
     (* Response to existing request *)
     Map.tryFind rxid dwq.activeQueries
     |> Option.map
          (fun query ->
            let mt =
              (Serialize.field "_p" resp |> Option.bind Serialize.floor
              ,Serialize.field "_n" resp |> Option.bind Serialize.floor
              ,Serialize.field "_b" resp
               |> Option.map Serialize.asString
               |> Option.map (fun b -> Buffer.fromString b "binary")
              )
            in
            match mt with
            | (None, None, None) ->
               (* Direct result *)
               _onresponse dhtOps peer resp query dwq
            | (Some _p, Some _n, Some _b) ->
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
                 |> Option.map
                      (fun resp -> _onresponse dhtOps peer resp query dwq)
                 |> optionDefault
                      { dwq with
                          activeQueries =
                            Map.remove query.id dwq.activeQueries ;
                          events =
                            (QueryError (query.id, peer.id, "Decode error")) ::
                              dwq.events
                      }
               else
                 { dwq with
                     activeQueries = Map.add query.id newq dwq.activeQueries ;
                     partial = Set.add query.id dwq.partial
                 }
            | _ -> dwq
          )
     |> optionDefault dwq
  | (Some txid,_,_) ->
     (* Request *)
     { dwq with
         events =
           QueryRequest (txid,peer,resp) :: dwq.events
     }
  | _ -> dwq
    

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
  let qClosest = Set.ofSeq (Array.map toComparable q.closestNodes) in
  let incoming = Set.ofSeq (Array.map toComparable peers) in
  let total = Set.union qClosest incoming in
  if total.Count = qClosest.Count then
    (* We didn't advance, we're as close as we come *)
    if Array.length q.closestNodes > 0 &&
         Buffer.equal q.closestNodes.[0].id q.tid then
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
    startQuery
      dhtOps
      { q with closestNodes = Array.sub closestWithDistances 0 8 }
      { dwq with
          drilling = Map.remove (Buffer.toString "binary" q.tid) dwq.drilling
      }

let rec harvestDHT (dht : DHT.DHT) : (InternalAction list * DHT.DHT) =
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

let map dhtOps f dwq =
  let dht = f dwq.dht in
  let (events : InternalAction list, dht) = dhtOps.harvest dht in
  let (dwq : DHTWithQueryProcessing<'dht>) = { dwq with dht = dht } in
  List.fold
    (fun (dwq : DHTWithQueryProcessing<'dht>) (evt : InternalAction) ->
      match evt with
      | Find (target,peers) ->
         let drillingFor =
           Map.tryFind (Buffer.toString "binary" target) dwq.drilling
         in
         (match drillingFor with
          | Some q -> takeFind dhtOps target peers q dwq
          | None -> dwq
         )
      | Payload (json,source) ->
         takeDatagram dhtOps source json dwq
      | Datagram (json,source) ->
         { dwq with events = (SendDatagram (json,source)) :: dwq.events }
      | Bootstrapped ->
         let dwq = { dwq with bootstrapped = true } in
         match dwq.pendingQueries with
         | hd :: tl ->
            startQuery dhtOps hd { dwq with pendingQueries = tl }
         | _ -> dwq
    )
    dwq
    events
    
let recv dhtOps (json : Serialize.Json) (source : NodeIdent) dwq =
  map
    dhtOps
    (fun dht ->
      dhtOps.recv
        (dwq.drilling.Count + dwq.activeQueries.Count)
        json
        source
        dht
    )
    dwq

let expireQuery txid dwq =
  let events =
    match Map.tryFind txid dwq.activeQueries with
    | Some q ->
       if q.from = None then
         (QueryError (q.id, q.tid, "Expired in flight")) ::
           dwq.events
       else
         dwq.events
    | None -> dwq.events
  in
  { dwq with
      partial = Set.remove txid dwq.partial ;
      activeQueries = Map.remove txid dwq.activeQueries ;
      drilling = Map.remove txid dwq.drilling ;
      events = events
  }
    
let doDeadlines (dwq : DHTWithQueryProcessing<'dht>) =
  let deadlineSet =
    Map.tryFind dwq.tick dwq.deadlines
    |> optionDefault Set.empty
  in
  Seq.fold
    (fun dwq txid -> expireQuery txid dwq)
    { dwq with deadlines = Map.remove dwq.tick dwq.deadlines }
    (Set.toSeq deadlineSet)

let makeHaveBitmap query =
  Array.init
    ((query.pieces + 7) / 8)
    (fun n ->
      let s = n * 8 in
      let e = min ((n + 1) * 8) query.pieces in
      Seq.fold
        (fun byte bit ->
          match Map.tryFind (s + bit) query.accumulated with
          | Some _ -> byte ||| (asl 1 bit)
          | None -> byte
        )
        0
        (Seq.init (e - s) id)
    )
    
let checkPartials dhtOps dwq =
  Seq.fold
    (fun dwq txid ->
      match Map.tryFind txid dwq.activeQueries with
      | Some query ->
         let passOn =
           Serialize.jsonObject
             [| ("ack", Serialize.jsonString query.id)
              ; ("target",
                 Serialize.jsonString (Buffer.toString "base64" query.tid))
              ; ("have",
                 Serialize.jsonString
                   (Buffer.toString
                      "binary" (Buffer.fromArray (makeHaveBitmap query))
                   )
                )
             |]
         in
         match query.from with
         | Some from ->
            { dwq with dht = dhtOps.query 0 from passOn dwq.dht }
         | None -> dwq
      | None -> dwq
    )
    dwq
    (Set.toSeq dwq.partial)

let doTimeouts dhtOps (dwq : DHTWithQueryProcessing<'dht>) =
  let timeouts =
    Map.tryFind dwq.tick dwq.future
    |> optionDefault Set.empty
  in
  Seq.fold
    (fun dwq txid ->
      match Map.tryFind txid dwq.activeQueries with
      | Some qq ->
         let (q : Query) =
           match qq.retry with
           | hd :: tl -> { qq with retry = tl }
           | [] -> qq
         in
         startQuery dhtOps q dwq
      | None -> dwq
    )
    { dwq with future = Map.remove dwq.tick dwq.future }
    (Set.toSeq timeouts)

let tick
      (dhtOps : DHTOps<'dht>)
      (dwq : DHTWithQueryProcessing<'dht>) : DHTWithQueryProcessing<'dht> =
  (* Pass on tick to the contained DHT *)
  let dwq =
    map
      dhtOps
      (fun dht ->
        dhtOps.tick (dwq.drilling.Count + dwq.activeQueries.Count) dht
      )
      dwq
  in
  let dwq = { dwq with tick = dwq.tick + 1 } in
  let dwq = doDeadlines dwq in
  let dwq = checkPartials dhtOps dwq in
  doTimeouts dhtOps dwq

let harvest dwq =
  (dwq.events, { dwq with events = [] })
