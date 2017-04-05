module QueryStream

(* https://raw.githubusercontent.com/mafintosh/dht-rpc/master/query-stream.js *)

open Buffer
open DHTData

let infinity = Util.infinityInt

let optionMap f o =
  match o with
  | Some s -> Some (f s)
  | None -> None

let optionDefault v o =
  match o with
  | Some s -> s
  | None -> v

let max a b = if a < b then b else a

let BLANK = 
  Buffer.fromArray
    [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
       0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0
    |]

type QueryIdAndToken =
  { id : Buffer
  ; roundtripToken : string
  }

and ChunkPair =
  { key : DHTData.NodeIdent
  ; value : Buffer
  }
    
and Action =
  | NoOp
  | Error of string
  | Close
  | Chunk of ChunkPair
  | EOF
  | Warning of string

and QueryStream =
  { id : Buffer
  ; query : DHTQuery
  ; token : bool
  ; holepunching : bool
  ; commits : int
  ; responses : int
  ; errors : int
  ; destroyed : bool
  ; _committing : bool
  ; _closest : Node array
  ; _concurrency : int
  ; _updating : bool
  ; _pending : Node array
  ; _k : int option
  ; _inflight : int
  ; _moveCloser : bool
  ; _bootstrapped : bool
  ; _finalized : bool
  }

and DHTQuery =
  { command : Buffer
  ; id : Buffer
  ; target : Buffer
  ; nodes : Buffer
  ; value : Buffer
  ; roundtripToken : string
  }

and DHTOps<'dht> =
  { dhtId : 'dht -> Buffer
  ; nodeId : Node -> Buffer
  ; nodeDistance : Node -> Buffer
  ; nodeSetDistance : Buffer -> Node -> Node
  ; nodeSetReferer : Buffer -> Node -> Node
  ; dhtRequest : DHTQuery -> Node -> bool -> 'dht -> 'dht
  ; emit : Action -> Buffer -> 'dht -> 'dht
  ; getInFlightQueries : 'dht -> int
  ; adjustInFlightQueries : int -> 'dht -> 'dht
  ; getClosest : Buffer -> int -> 'dht -> Node array
  ; getBootstrap : 'dht -> Node array
  ; getNode : Buffer -> 'dht -> Node option
  ; removeNode : Buffer -> 'dht -> 'dht
  ; holepunch : Node -> Buffer -> QueryStream -> 'dht -> 'dht
  ; takeQueryStream : QueryStream -> 'dht -> 'dht
  ; withQueryStream : (QueryStream -> 'dht) -> Buffer -> 'dht -> 'dht
  }

and Opts =
  { token : bool
  ; holepunching : bool
  ; concurrency : int option
  ; closest : Node array
  ; nodes : Node array
  ; k : int option
  }

let defaultOpts =
  { token = false
  ; holepunching = false
  ; concurrency = None
  ; closest = [| |]
  ; nodes = [| |]
  ; k = None
  }

let init id opts query =
  { id = id
  ; query = query
  ; token = opts.token
  ; holepunching = opts.holepunching
  ; commits = 0
  ; responses = 0
  ; errors = 0
  ; destroyed = false
  ; _committing = false
  ; _closest = opts.closest
  ; _concurrency = opts.concurrency |> optionDefault 5
  ; _updating = false
  ; _pending = opts.nodes
  ; _k =
      if Array.length opts.nodes > 0 then None else Some (opts.k |> optionDefault 20)
  ; _inflight = 0
  ; _moveCloser = Array.length opts.nodes = 0
  ; _bootstrapped = Array.length opts.nodes > 0
  ; _finalized = false
  }

let rec destroy dhtOps dht err self =
  if not self.destroyed then
    { self with destroyed = true }
    |> _finalize dhtOps dht
    |> (fun dht ->
         match err with
         | Some e -> dhtOps.emit (Error e) self.id dht
         | None -> dht
       )
    |> dhtOps.emit Close self.id
  else
    dht

and _finalize (dhtOps : DHTOps<'dht>) (dht : 'dht) (self : QueryStream) : 'dht =
  if self._finalized then
    dht
  else
    let updatedSelf = { self with _finalized = true } in
    dht
    |> dhtOps.takeQueryStream updatedSelf
    |> dhtOps.adjustInFlightQueries -1
    |> (fun dht ->
         if updatedSelf.responses = 0 && not updatedSelf.destroyed then
           destroy dhtOps dht (Some "No nodes responded") updatedSelf
         else if updatedSelf.commits = 0 && not updatedSelf._committing && not updatedSelf.destroyed then
           destroy dhtOps dht (Some "No close nodes responded") updatedSelf
         else
           dht
       )
    |> (dhtOps.emit EOF updatedSelf.id)

let xor b1 b2 =
  let b1l = Buffer.length b1 in
  let b2l = Buffer.length b2 in
  let _min = min b1l b2l in
  let _max = max b1l b2l in
  let resArray = Array.zeroCreate _max in
  for i = 0 to _min - 1 do
    begin
      resArray.[i] <- (Buffer.at i b1) ^^^ (Buffer.at i b2)
    end ;
  let source = if b1l < b2l then b2 else b1 in
  for i = _min to _max - 1 do
    begin
      resArray.[i] <- Buffer.at i source
    end ;
  Buffer.fromArray resArray

let bufferCompare (b1 : Buffer) (b2 : Buffer) : int =
  let b1l = Buffer.length b1 in
  let b2l = Buffer.length b2 in
  let rec bufferCompareInner (n : int) (b1 : Buffer) (b2 : Buffer) =
    if n >= b1l && n >= b2l then
      0
    else if n >= b1l then
      1
    else if n >= b2l then
      -1
    else
      let bat1 = Buffer.at n b1 in
      let bat2 = Buffer.at n b2 in
      if bat1 = bat2 then
        bufferCompareInner (n+1) b1 b2
      else
        bat2 - bat1
  in
  bufferCompareInner 0 b1 b2

let getNode dhtOps id collection =
  let l =
    collection |> Seq.filter (fun n -> dhtOps.nodeId n = id) 
    |> Seq.truncate 1 |> Seq.toList
  in
  match l with
  | [] -> None
  | hd :: tl -> Some hd

let insertSorted dhtOps node max set get self =
  if not 
       (Array.length (get self) >= max && 
        bufferCompare
          (dhtOps.nodeDistance node)
          (dhtOps.nodeDistance (get self).[max-1]) >= 0
       ) then
    let existing = getNode dhtOps (dhtOps.nodeId node) (get self) in
    let self1 =
      if existing = None then
        if Array.length (get self) < max then
          set (Array.concat [ get self ; [| node |] ]) self
        else
          let acopy = Array.concat [get self] in
          let _ = acopy.[max-1] <- node in
          set acopy self
      else
        self
    in
    set (Array.sortBy (fun n -> Buffer.toArray (dhtOps.nodeId n)) self1._pending)
        self1
  else
    self

let insertPendingSorted dhtOps node max self =
  insertSorted
    dhtOps
    node
    max
    (fun c self -> { self with _pending = c }) 
    (fun self -> self._pending)
    self

let insertClosestSorted dhtOps node max self =
  insertSorted
    dhtOps
    node
    max
    (fun c self -> { self with _closest = c }) 
    (fun self -> self._closest)
    self

let _addClosest dhtOps dht (res : QueryIdAndToken) (peer : Node) self =
  if bufferCompare res.id (dhtOps.dhtId dht) <> 0 then
    let prev = getNode dhtOps res.id self._pending in
    let prev2 =
      match prev with
      | Some prev -> { prev with roundtripToken = res.roundtripToken }
      | None ->
         let (node : Node) =
           { id = res.id
           ; port = peer.port
           ; host = peer.host
           ; distance = xor res.id self.query.target
           ; roundtripToken = res.roundtripToken
           ; referer = peer.referer
           ; queried = false
           }
         in
         node
    in
    insertClosestSorted dhtOps prev2 (self._k |> optionDefault infinity) self
  else
    self

let _addPending (dhtOps : DHTOps<'dht>) (dht : 'dht) (node : Node) (r : Buffer option) (self : QueryStream) : QueryStream =
  if bufferCompare node.id (dhtOps.dhtId dht) <> 0 then
    let newNode =
      node
      |> dhtOps.nodeSetDistance (xor self.query.target node.id)
      |> (fun dht ->
            r |> optionMap (fun r -> dhtOps.nodeSetReferer r dht) |> optionDefault dht
         )
    in
    insertPendingSorted dhtOps newNode (self._k |> optionDefault infinity) self
  else
    self

let _send (dhtOps : DHTOps<'dht>) (dht : 'dht) (node : Node) (force : bool) (useToken : bool) (self : QueryStream) : 'dht =
  let node2 =
    if not force then
      if node.queried then
        node
      else
        { node with queried = true }
    else
      node
  in
  let query = 
    if not force then
      if node.queried then
        self.query
      else
        if useToken && node.roundtripToken <> "" then
          { self.query with roundtripToken = node.roundtripToken }
        else
          self.query
    else
      self.query
  in
  let self2 = { self with _inflight = self._inflight + 1 } in
  dhtOps.dhtRequest
    query
    node
    false
    (dhtOps.takeQueryStream self2 dht)

let updateSelfForSend (dhtOps : DHTOps<'dht>) (dht : 'dht) (force : bool) (useToken : bool) (node : Node) (self : QueryStream) : 'dht =
  dhtOps.withQueryStream
    (fun self -> _send dhtOps dht node force useToken self)
    self.id
    dht

let _sendAll (dhtOps : DHTOps<'dht>) (dht : 'dht) (nodes : Node array) (force : bool) (useToken : bool) (self : QueryStream) : (int * 'dht) =
  let sent = 0 in
  let free = max 0 (self._concurrency - (dhtOps.getInFlightQueries dht)) in
  
  let free1 = if free = 0 && self._inflight = 0 then 1 else free in
  let sendToNode ((sent,dht) : (int * 'dht)) (node : Node) =
    let (theDht : 'dht) = dht in
    if sent < free1 then
      (sent+1,(dhtOps.withQueryStream (updateSelfForSend dhtOps theDht force useToken node) self.id theDht))
    else
      (sent,dht)
  in
  let (sentAndDht : (int * 'dht)) = (sent,dht) in
  Array.fold sendToNode (sent,dht) nodes

let _sendTokens (dhtOps : DHTOps<'dht>) (dht : 'dht) (self : QueryStream) : (int * 'dht) =
  if self.destroyed then
    (0,dht)
  else
    let (sent,dht2) = _sendAll dhtOps dht self._closest false true self in
    (sent,
     dhtOps.withQueryStream
       (fun self ->
         if (sent = 0 && self._inflight = 0) then
           dhtOps.takeQueryStream self dht2
         else
           _finalize dhtOps dht2 self
       )
       self.id
       dht2
    )

let _bootstrap dhtOps dht self =
  let selfBSTrue = { self with _bootstrapped = true } in
  let bootstrap =
    dhtOps.getClosest self.query.target (self._k |> optionDefault infinity) dht
  in
  let self2 =
    Array.fold
      (fun self node ->
        _addPending dhtOps dht node None self
      )
      selfBSTrue
      bootstrap
  in
  let (dhtBootstrap : Node array) = dhtOps.getBootstrap dht in
  let dht2 = dhtOps.takeQueryStream self2 dht in
  let applySend (dht : 'dht) (node : Node) : 'dht =
    dhtOps.withQueryStream (updateSelfForSend dhtOps dht true false node) self.id dht
  in
  if Array.length bootstrap < Array.length dhtBootstrap then
    Array.fold applySend dht2 dhtBootstrap
  else
    dhtOps.takeQueryStream self dht2

let _sendPending dhtOps (dht : 'dht) (self : QueryStream) : 'dht =
  if self.destroyed then
    dht
  else
    let dht2 =
      if not self._bootstrapped then
        _bootstrap dhtOps dht self
      else
        dht
    in
    let (sent,dht3) = _sendAll dhtOps dht2 self._pending false false self in
    if sent <> 0 || self._inflight <> 0 then
      dht3
    else
      dhtOps.withQueryStream
        (fun self ->
          if self.token then
            let closestQueriedFalse =
              Array.map
                (fun n -> { n with queried = false })
                self._closest
            in
            let selfWithQFalse =
              { self with
                  _closest = closestQueriedFalse ;
                  _committing = true
              }
            in
            dhtOps.takeQueryStream selfWithQFalse dht3
          else
            _finalize dhtOps dht3 self
        )
        self.id
        dht3

let _holepunch dhtOps dht peer query self =
  dhtOps.holepunch peer peer.referer self dht

let validateId id = Buffer.length id = 32

let decodeNodes (buf : Buffer option) : Node array =
  match buf with
  | None -> Array.empty
  | Some b -> Array.empty

let _read (dhtOps : DHTOps<'dht>) (dht : 'dht) (self : QueryStream) : 'dht =
  if self._committing then
    let (sent,dht2) = _sendTokens dhtOps dht self in
    dht2
  else
    _sendPending dhtOps dht self

let _readMaybe (dhtOps : DHTOps<'dht>) (dht : 'dht) (self : QueryStream) : QueryStream =
  _read dhtOps dht self

let raiseError (dhtOps : DHTOps<'dht>) (dht : 'dht) (err : string) (peer : Node option) (self : QueryStream) =
  let self2 = { self with _inflight = self._inflight - 1 } in
  if self2.destroyed then
    dht
  else
    let performErrorRaise (dht : 'dht) : 'dht =
      dht
      |> dhtOps.takeQueryStream self
      |> (fun dht ->
           match peer with
           | None -> (dht,None)
           | Some node -> (dht,dhtOps.getNode node.id dht)
         )
      |> (fun (dht,node) ->
           match node with
           | None -> dht
           | Some node -> dhtOps.removeNode node.id dht
         )
      |> dhtOps.takeQueryStream { self2 with errors = self2.errors + 1 }
      |> dhtOps.emit (Warning err) self2.id
      |> (fun dht ->
           dhtOps.withQueryStream
             (fun self ->
               dhtOps.takeQueryStream
                 (_readMaybe dhtOps dht self)
                 dht
             )
             self2.id
             dht
         )
    in
    performErrorRaise dht

let takeResponse (dhtOps : DHTOps<'dht>) (dht : 'dht) (res : DHTQuery) (peer : Node) (self : QueryStream) : QueryStream =
  let self2 =
    { self with
        responses = self.responses + 1 ;
        commits = if self._committing then self.commits + 1 else self.commits
    }
  in
  let dht2 =
    dhtOps.takeQueryStream
      (_addClosest
         dhtOps
         dht 
         { id = res.id ; roundtripToken = res.roundtripToken }
         peer
         self
      )
      dht
  in
  let dht3 =
    if self._moveCloser then
      let candidates = decodeNodes (Some res.nodes) in
      Array.fold
        (fun dht node ->
          dhtOps.withQueryStream
            (fun self ->
              dhtOps.takeQueryStream
                (_addPending dhtOps dht node (Some res.id) self)
                dht
            )
            self.id
            dht
        )
        dht2
        candidates
    else
      if not (validateId res.id) || (self.token && not self._committing) then
        dhtOps.withQueryStream
          (fun self ->
            dhtOps.takeQueryStream
              (_readMaybe dhtOps dht2 self)
              dht2
          )
          self.id
          dht2
      else
        dht2
  in
  let data = 
    { key = { id = res.id; port = peer.port; host = peer.host } ;
      value = res.value
    }
  in
  dhtOps.emit (Chunk data) self.id dht3
