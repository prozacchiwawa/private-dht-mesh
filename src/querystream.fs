module QueryStream

(* https://raw.githubusercontent.com/mafintosh/dht-rpc/master/query-stream.js *)

open Buffer

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

type Action =
  | NoOp
  | Error of string
  | Close
  | Chunk of Buffer
  | EOF

and QueryStream =
  { concurrency : int
  ; query : DHTQuery
  ; id : Buffer
  ; target : Buffer
  ; token : string option
  ; holepunching : bool
  ; commits : int
  ; responses : int
  ; errors : int
  ; verbose : bool
  ; destroyed : bool
  ; _committing : bool
  ; _closest : Node array
  ; _concurrency : int
  ; _updating : bool
  ; _pending : Node array
  ; _k : int option
  ; _inflight : int
  ; _moveCloser : bool
  ; _bootstrap : Node array
  ; _bootstrapped : bool
  ; _finalized : bool
  }
and Node =
  { id : Buffer
  ; port : int
  ; host : string
  ; roundtripToken : string
  ; referer : Buffer
  ; distance : Buffer
  ; queried : bool
  }
and DHTQuery =
  { command : Buffer
  ; id : Buffer
  ; target : Buffer
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
  ; takeQueryStream : QueryStream -> 'dht -> 'dht
  ; emit : Action -> Buffer -> 'dht -> 'dht
  ; getInFlightQueries : 'dht -> int
  ; adjustInFlightQueries : int -> 'dht -> 'dht
  ; withQueryStream : (QueryStream -> 'dht) -> Buffer -> 'dht -> 'dht
  ; getClosest : Buffer -> int -> 'dht -> Node array
  ; getBootstrap : 'dht -> Node array
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

let _addClosest dhtOps dht res peer self =
  if bufferCompare res.id (dhtOps.dhtId dht) <> 0 then
    let prev = getNode dhtOps res.id self._pending in
    let prev2 =
      match prev with
      | Some prev -> { prev with roundtripToken = res.roundtripToken }
      | None ->
         { id = res.id
         ; port = peer.port
         ; host = peer.host
         ; distance = xor res.id self.target
         ; roundtripToken = res.roundtripToken
         ; referer = peer.referer
         ; queried = false
         }
    in
    insertClosestSorted dhtOps prev2 (self._k |> optionDefault infinity) self
  else
    self

let _addPending (dhtOps : DHTOps<'dht>) (dht : 'dht) (node : Node) (r : Buffer option) (self : QueryStream) : QueryStream =
  if bufferCompare node.id (dhtOps.dhtId dht) <> 0 then
    let newNode =
      node
      |> dhtOps.nodeSetDistance (xor self.target node.id)
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

let updateSelfForSend (dhtOps : DHTOps<'dht>) (dht : 'dht) (force : bool) (useToken : bool) (node : Node) (self : QueryStream) : QueryStream =
  _send dhtOps dht node force useToken self

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
    let (sent,self2) = _sendAll dhtOps dht self._closest false true self in
    if  (sent = 0 && self2._inflight = 0) then
      (sent,dhtOps.takeQueryStream self2 dht)
    else
      (sent,_finalize dhtOps dht self2)

let _bootstrap dhtOps dht self =
  let selfBSTrue = { self with _bootstrapped = true } in
  let bootstrap = dhtOps.getClosest self.target (self._k |> optionDefault infinity) dht in
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

(*
QueryStream.prototype._readMaybe = function () {
  if (this._readableState.flowing === true) this._read()
                                              }

QueryStream.prototype._sendPending = function () {
  if (this.destroyed) return
  if (!this._bootstrapped) this._bootstrap()

  var sent = this._sendAll(this._pending, false, false)
  if (sent || this._inflight) return

  if (this.token) {
    for (var i = 0; i < this._closest.length; i++) this._closest[i].queried = false
    this._committing = true
    this._sendTokens()
       } else {
    this._finalize()
    }
                                                }

QueryStream.prototype._read = function () {
  if (this._committing) this._sendTokens()
  else this._sendPending()
                                         }

QueryStream.prototype._holepunch = function (peer, query) {
  var self = this

  this._dht._holepunch(peer, peer.referrer, function (err) {
    if (err) return self._callback(err, null, peer)
    self._dht._request(query, peer, false, self._onresponse)
                                                       })
                                              }

QueryStream.prototype._callback = function (err, res, peer) {
  this._inflight--
  if (this.destroyed) return

  if (err) {
    if (res && res.id) {
      var node = this._dht.nodes.get(res.id)
      if (node) this._dht._removeNode(node)
         }
    this.errors++
    this.emit('warning', err)
    this._readMaybe()
    return
       }

  this.responses++
  if (this._committing) this.commits++
  this._addClosest(res, peer)

  if (this._moveCloser) {
    var candidates = decodeNodes(res.nodes)
    for (var i = 0; i < candidates.length; i++) this._addPending(candidates[i], peer)
       }

  if (!validateId(res.id) || (this.token && !this.verbose && !this._committing)) {
    this._readMaybe()
    return
       }

  var data = this._map({
    node: {
      id: res.id,
      port: peer.port,
      host: peer.host
      },
    value: res.value
                        })

  if (!data) {
    this._readMaybe()
    return
       }

  this.push(data)
                                             }

function decodeNodes (buf) {
  if (!buf) return []
  try {
    return nodes.decode(buf)
    } catch (err) {
    return []
      }
                     }

function validateId (id) {
  return id && id.length === 32
                    }

function echo (a) {
  return a
              }
*)
