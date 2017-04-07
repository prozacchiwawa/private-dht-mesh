module DHT

(* https://github.com/mafintosh/dht-rpc/blob/master/index.js *)

open Util
open Buffer
open Crypto
open ShortId
open KBucket
open DHTData
open QueryStream

type Action =
  | Ready
  | Request of Buffer * HostIdent
  | StreamOp of QueryStream.Action
  | Destroyed
  | Holepunch of Node * HostIdent
  | ForwardRequest of Serialize.Json * Node * HostIdent
  | MethodCall of MethodCallData
  | Response of Buffer * HostIdent
  | AddNode of Node
  | RemoveNode of Node

and NodeListElement =
  { peer : Node
  ; tick : int
  }

and MethodCallData =
  { tick : int
  ; method_ : string
  ; command : string
  ; query : Serialize.Json
  ; id : int
  ; nodes : Node array
  ; roundtripToken : Buffer
  }

and WaitingReply =
  { tick : int
  ; id : int
  ; method_ : string
  ; command : string
  ; query : Serialize.Json
  ; nodes : Node array
  ; roundtripToken : Buffer
  }

(*
function DHT (opts) {
  if (!(this instanceof DHT)) return new DHT(opts)
  if (!opts) opts = {}

  events.EventEmitter.call(this)

  var self = this

  this.concurrency = opts.concurrency || 16
  this.id = opts.id || crypto.randomBytes(32)
  this.ephemeral = !!opts.ephemeral
  this.nodes = new KBucket({localNodeId: this.id, arbiter: arbiter})
  this.nodes.on('ping', onnodeping)
  this.inflightQueries = 0

  this.socket = udp({
    socket: opts.socket,
    requestEncoding: messages.Request,
    responseEncoding: messages.Response
                     })

  this.socket.on('request', onrequest)
  this.socket.on('response', onresponse)
  this.socket.on('close', onclose)

  this._bootstrap = [].concat(opts.bootstrap || []).map(parseAddr)
  this._queryId = this.ephemeral ? null : this.id
  this._bootstrapped = false
  this._pendingRequests = []
  this._tick = 0
  this._secrets = [crypto.randomBytes(32), crypto.randomBytes(32)]
  this._secretsInterval = setInterval(rotateSecrets, 5 * 60 * 1000)
  this._tickInterval = setInterval(tick, 5 * 1000)
  this._top = null
  this._bottom = null

  if (opts.nodes) {
    for (var i = 0; i < opts.nodes.length; i++) {
      this._addNode(opts.nodes[i].id, opts.nodes[i])
          }
       }
 *)

type DHT =
  { id : Buffer
  ; ephermeral : bool
  ; nodes : KBucket<Buffer,Node>
  ; concurrency : int
  ; inFlightQueries : int
  ; _bootstrap : string list
  ; _queryId : int option
  ; _bootstrapped : bool
  ; _pendingRequests : (Buffer * NodeIdent) array
  ; _tick : int
  ; _secrets : (Buffer * Buffer)
  ; _secretsInterval : int
  ; _tickInterval : int
  ; _bottom : NodeListElement list
  ; _queryData : Map<int array,QueryInfo>
  ; _results : Map<int array,QueryStream.Action>
  ; destroyed : bool
  ; events : Action list
  ; replacementNodes : Node list
  ; waitingReply : Map<int,WaitingReply>
  }

and QueryInfo =
  { qs : QueryStream
  ; actions : QueryStream.Action list
  }

and Opts =
  { q : QueryStream.Opts
  }

let _token peer i self =
  let sha256Hasher = Crypto.createHash "sha256" in
  let theSecret =
    match (self._secrets,i) with
    | ((s,_),false) -> s
    | ((_,s),_) -> s
  in
  let addr = IPAddr.parse peer.host in
  let hostBuffer = Buffer.fromArray (addr.toByteArray ()) in
  let _ = Crypto.updateBuffer theSecret sha256Hasher in
  let _ = Crypto.updateBuffer hostBuffer sha256Hasher in
  Crypto.digestBuffer sha256Hasher

let hashId id =
  let sha256Hasher = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString id "binary") sha256Hasher in
  Crypto.digestBuffer sha256Hasher

let defaultOpts =
  { q = QueryStream.defaultOpts
  }

let query query opts self =
  let newId = ShortId.generate () in
  let hashedId = hashId newId in
  { self with
      inFlightQueries = self.inFlightQueries + 1 ;
      _queryData =
        Map.add
          (Buffer.toArray hashedId)
          { qs = QueryStream.init hashedId opts.q query
          ; actions = []
          }
          self._queryData
  }

let update q opts self =
  let o1 = { opts with q = { opts.q with token = true } } in
  query q o1 self

let _request socketInFlight request (peer : NodeIdent) important self =
  let newRequest = (request,peer) in
  let self2 =
    { self with
        _pendingRequests =
          Array.concat
            [self._pendingRequests;[|newRequest|]]
    }
  in
  if socketInFlight >= self.concurrency ||
       (Array.length self._pendingRequests) > 0
  then
    self2
  else
    { self2 with
        events =
          (Request (request,{ host = peer.host ; port = peer.port })) ::
            self2.events
    }

let destroy self =
  if self.destroyed then
    self
  else
    { self with destroyed = true ; events = Destroyed :: self.events }

let makePingBody qid self =
  Serialize.jsonObject
    [| ("command", Serialize.jsonString "_ping") ;
       ("id", 
        qid
        |> optionMap Serialize.jsonInt
        |> optionDefault (Serialize.jsonNull ())
       )
    |]  

let _check socketInFlight node self =
  let requestString =
    makePingBody self._queryId self |> Serialize.stringify
  in
  let requestBuffer = Buffer.fromString requestString "utf-8" in
  _request
    socketInFlight
    requestBuffer
    node
    false
    self
    (* if (err) self._removeNode(node) <-- ping reply *)

let _pingSome socketInFlight self =
  let cnt = if self.inFlightQueries > 2 then 1 else 3 in
  let oldest = ref self._bottom in
  List.fold
    (fun self _ ->
      match !oldest with
      | hd :: tl ->
         if self._tick - hd.tick >= 3 then
           let last = hd in
           let _ = oldest := tl in
           _check socketInFlight { id = last.peer.id ; host = last.peer.host ; port = last.peer.port } self
         else
           self
      | [] -> self
    )
    self

let _rotateSecrets self =
  let secret = Crypto.randomBytes 32 in
  let (s0,s1) = self._secrets in
  { self with _secrets = (secret, s0) }

let _ping socketInFlight peer self =
  let requestString =
    makePingBody self._queryId self |> Serialize.stringify
  in
  let requestBuffer = Buffer.fromString requestString "utf-8" in
  _request
    socketInFlight
    requestBuffer
    peer
    false
    self

(* Peers:
 * Number of ip4 peers as 16-bit uint.
 * ip4 peers as 6 bytes each.
 * ip6 peers as 18 bytes each.
 * Returns string
 *)
let encodePeers (peers : HostIdent array) : string =
  let (ip4peersS,ip6peersS) =
    seqPartition
      (fun (p : HostIdent) ->
        let parsed = IPAddr.parse p.host in
        parsed.kind () = "ipv4"
      )
      peers
  in
  let (ip4peers,ip6peers) = (Array.ofSeq ip4peersS, Array.ofSeq ip6peersS) in
  let numip4peers = Array.length ip4peers in
  let numip6peers = Array.length ip6peers in
  let fulllength = 2 + (6 * numip4peers) + (18 * numip6peers) in
  let buffer = Buffer.zero fulllength in
  let _ = Buffer.writeUInt16BE 0 numip4peers buffer in
  let _ =
    for i = 0 to numip4peers - 1 do
      begin
        let parsed = IPAddr.parse ip4peers.[i].host in
        let buf = parsed.toByteArray () |> Buffer.fromArray in
        let _ = Buffer.copy (2 + (6 * i)) (Buffer.length buf) buf 0 buffer in
        Buffer.writeUInt16BE (4 + (6 * i)) ip4peers.[i].port buffer
      end ;
  let _ =
    for i = 0 to numip6peers - 1 do
      begin
        let parsed = IPAddr.parse ip6peers.[i].host in
        let buf = parsed.toByteArray () |> Buffer.fromArray in
        let _ = Buffer.copy (2 + (6 * numip4peers) + (18 * i)) (Buffer.length buf) buf 0 buffer in
        Buffer.writeUInt16BE (4 + (6 * numip4peers) + (18 * i)) ip6peers.[i].port buffer
      end
  in
  Buffer.toString "binary" buffer

(*
 * Decode encoded peers.
 *)
let decodePeers (str : string) : HostIdent array =
  try
    let buf = Buffer.fromString str "binary" in
    let l = Buffer.length buf in
    let numip4peers = Buffer.readUInt16BE 0 buf in
    let numip6peers = ((l - 2) - (numip4peers * 6)) / 18 in
    let peer i =
      if i < numip4peers then
        let start = 2 + (i * 6) in
        let ipbuf = Buffer.slice start (start + 4) buf in
        let port = Buffer.readUInt16BE (start + 4) buf in
        let ipaddr = IPAddr.fromByteArray (Buffer.toArray ipbuf) in
        { HostIdent.host = ipaddr.toString () ; HostIdent.port = port }
      else
        let start = 2 + (numip4peers * 6) + (i * 18) in
        let ipbuf = Buffer.slice start (start + 16) buf in
        let port = Buffer.readUInt16BE (start + 16) buf in
        let ipaddr = IPAddr.fromByteArray (Buffer.toArray ipbuf) in
        { HostIdent.host = ipaddr.toString () ; HostIdent.port = port }
    in
    Array.init (numip4peers + numip6peers) peer
  with _ ->
    [| |]

let _holepunch socketInFlight peer referer self =
  let requestString =
    Serialize.jsonObject
      [| ("command", Serialize.jsonString "_ping") ;
         ("id", 
          self._queryId
          |> optionMap Serialize.jsonInt
          |> optionDefault (Serialize.jsonNull ())
         ) ;
         ("forwardRequest",
          encodePeers [|peer|] |> Serialize.jsonString
         )
      |]
    |> Serialize.stringify
  in
  let requestBuffer = Buffer.fromString requestString "utf-8" in
  _request
    socketInFlight
    requestBuffer
    referer
    false
    self

let _forwardResponse request (peer : Node) self =
  let forResponse = request |> Serialize.field "forwardResponse" in
  match forResponse with
  | None -> self
  | Some resp ->
     let from = decodePeers (Serialize.asString resp) in
     if Array.length from = 0 then
       self
     else
       let reqString =
         request
         |> Serialize.addField "host" (Serialize.jsonString from.[0].host)
         |> Serialize.addField "port" (Serialize.jsonInt from.[0].port)
         |> Serialize.addField "request" (Serialize.jsonBool true)
         |> Serialize.stringify
       in
       let reqBuffer = Buffer.fromString reqString "utf-8" in
       { self with events = (Response (reqBuffer, { host = peer.host ; port = peer.port })) :: self.events }

let _forwardRequest request (peer : Node) self =
  let forRequest = request |> Serialize.field "forwardRequest" in
  match forRequest with
  | None -> self
  | Some req ->
     let _to = decodePeers (Serialize.asString req) in
     if Array.length _to = 0 then
       self
     else
       let peerEnc = encodePeers [| { host = peer.host ; port = peer.port } |] in
       request
       |> Serialize.addField "forwardRequest" (Serialize.jsonNull ())
       |> Serialize.addField "forwardResponse" (Serialize.jsonString peerEnc)
       |> (fun fr ->
            { self with
                events =
                  (ForwardRequest (fr,peer,{ host = _to.[0].host ; port = _to.[0].port })) ::
                    (Holepunch (peer,{ host = _to.[0].host ; port = _to.[0].port })) :: self.events
            }
          )

let _reping
      socketInFlight
      (oldContacts : Node array)
      (newContact : Node)
      (self : DHT) : DHT =
  Array.fold
    (fun (self : DHT) (contact : Node) ->
      let requestString =
        makePingBody self._queryId self |> Serialize.stringify
      in
      let requestBuffer = Buffer.fromString requestString "utf-8" in
      _request
        socketInFlight
        requestBuffer
        { id = contact.id ; host = contact.host ; port = contact.port }
        true
        self
    )
    { self with replacementNodes = newContact :: self.replacementNodes }
    oldContacts


(* 
-> error from socket
    self._removeNode(next)
    self.nodes.add(newContact)
 *)

let validateId id = Buffer.length id = 32

let (kBucketOps : KBucket.KBucketAbstract<Buffer,Node>) =
  { distance = KBucket.defaultDistance
  ; nodeId = fun n -> n.id
  ; arbiter = fun i e -> if i.vectorClock > e.vectorClock then i else e
  ; keyNth = Buffer.at
  ; keyLength = Buffer.length
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let _onquery request peer (self : DHT) : DHT =
  Serialize.field "target" request
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionMap
       (fun (target : Buffer) ->
         if not (validateId target) then
           self
         else
           let mt =
             (Serialize.field "id" request,
              Serialize.field "command" request,
              Serialize.field "value" request,
              Serialize.field "roundtripToken" request
             )
           in
           match mt with
           | (Some id, Some command, Some value, rtt) ->
              let query =
                Serialize.jsonObject
                  [| ("node",
                      Serialize.jsonObject
                        [| ("id", id) ;
                           ("port", Serialize.jsonInt peer.port) ;
                           ("host", Serialize.jsonString peer.host) ;
                        |]
                     ) ;
                     ("command", command) ;
                     ("target", Serialize.jsonString (Buffer.toString "binary" target)) ;
                     ("value", value) ;
                     ("roundtripToken", rtt |> optionDefault (Serialize.jsonNull ()))
                  |]
              in
              let _method = if rtt = None then "query" else "update" in
              let token = _token { host = peer.host ; port = peer.port } false self in
              { self with
                  _queryId = self._queryId |> optionMap (fun q -> q + 1) ;
                  events =
                    (MethodCall
                       { tick = self._tick
                       ; id = self._queryId |> optionDefault 0
                       ; method_ = _method
                       ; command = Serialize.asString command
                       ; query = request
                       ; nodes =
                           KBucket.closest
                             kBucketOps
                             self.nodes
                             target
                             20
                             None
                       ; roundtripToken = token
                       }
                    ) :: self.events
              }
           | _ -> self
       )
  |> optionDefault self

let _onping request peer self =
  let token = Buffer.toString "binary" (_token peer false self) in
  let resString = 
    Serialize.jsonObject
      [| ("id", self._queryId |> optionMap Serialize.jsonInt |> optionDefault (Serialize.jsonNull ())) ;
         ("value", Serialize.jsonString (encodePeers [|peer|])) ;
         ("roundtripToken", Serialize.jsonString token)
      |] |> Serialize.stringify
  in
  let resBuffer = Buffer.fromString resString "utf-8" in
  { self with events = (Response (resBuffer, peer)) :: self.events }

let _onfindnode request peer self =
  let token = Buffer.toString "binary" (_token peer false self) in
  Serialize.field "target" request
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionMap
       (fun (target : Buffer) ->
         if not (validateId target) then
           self
         else
           let resString =
             Serialize.jsonObject
               [| ("id", self._queryId |> optionMap Serialize.jsonInt |> optionDefault (Serialize.jsonNull ())) ;
                  ("nodes",
                   Serialize.jsonString
                     (encodePeers
                        (KBucket.closest
                           kBucketOps
                           self.nodes
                           target
                           20
                           None
                         |> Array.map (fun n -> { host = n.host ; port = n.port })
                        )
                     )
                  ) ;
                  ("roundtripToken", Serialize.jsonString token)
               |] |> Serialize.stringify
           in
           let resBuffer = Buffer.fromString resString "utf-8" in
           { self with events = (Response (resBuffer, peer)) :: self.events }
       )
  |> optionDefault self

let _onrequest request peer self =
  Serialize.field "target" request
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionMap
       (fun (target : Buffer) ->
         if not (validateId target) then
           self
         else
           let rtt =
             Serialize.field "roundtripToken" request |> optionMap Serialize.asString
           in
           let rttBuffer = ref None in
           let _ =
             match rtt with
             | Some rtt ->
                begin
                  rttBuffer :=
                    Some (Buffer.fromString rtt "binary") ;
                  let (s0,s1) = self._secrets in
                  match !rttBuffer with
                  | Some rb ->
                     if not (Buffer.equal s0 rb) && not (Buffer.equal s1 rb) then
                       rttBuffer := None ;
                  | None -> ()
                end
             | None -> ()
           in
           if Serialize.field "forwardRequest" request <> None then
             _forwardRequest request peer self
           else if Serialize.field "forwardResponse" request <> None then
             _forwardResponse request peer self
           else
             match Serialize.field "command" request |> optionMap Serialize.asString with
             | Some "ping" -> _onping request { host = peer.host ; port = peer.port } self
             | Some "_find_node" -> _onfindnode request { host = peer.host ; port = peer.port } self
             | _ -> _onquery request { host = peer.host ; port = peer.port } self
       )

let add (node : Node) (self : DHT) : DHT =
  { self with
      _bottom =
        { peer = node ; tick = self._tick } :: 
          (List.filter
             (fun n -> not (Buffer.equal n.peer.id node.id))
             self._bottom
          )
  }

let remove (node : Node) (self : DHT) : DHT =
  { self with
      _bottom =
        List.filter
          (fun n -> not (Buffer.equal n.peer.id node.id))
          self._bottom
  }

let rec applyKBucketEvents
          socketInFlight
          (evts : KBucket.Action<Buffer,Node> list)
          (self : DHT) : DHT =
  match evts with
  | [] ->
     self
  | Ping (nodes,contact) :: tl ->
     _reping
       socketInFlight
       nodes
       contact
       (applyKBucketEvents socketInFlight tl self)
  
let _removeNode socketInFlight (node : Node) (self : DHT) : DHT =
  let (newNodes, newEvents) =
    KBucket.remove kBucketOps self.nodes node.id None
  in
  applyKBucketEvents
    socketInFlight
    newEvents
    { remove node self with
        nodes = newNodes ;
        events = (RemoveNode node) :: self.events
    }

let _addNode socketInFlight (id : Buffer) (peer : HostIdent) (token : Buffer option) (self : DHT) : DHT =
  if Buffer.equal id self.id then
    let node = KBucket.get kBucketOps self.nodes id None in
    let (fresh,defNode) =
      match node with
      | Some node -> (false,node)
      | None ->
         (true,
          { id = id
          ; queried = false
          ; distance = Buffer.fromArray (KBucket.defaultDistance kBucketOps id self.id)
          ; port = peer.port
          ; host = peer.host
          ; roundtripToken = token |> optionMap (Buffer.toString "binary") |> optionDefault ""
          ; vectorClock = self._tick
          ; referer = id
          }
         )
    in
    let self2 = if not fresh then remove defNode self else self in
    let self3 = add defNode self in
    let (newNodes,newEvents) = KBucket.add kBucketOps self.nodes defNode None in
    applyKBucketEvents
      socketInFlight
      newEvents
      { self3 with
          nodes = newNodes ;
          events =
            if fresh then (AddNode defNode) :: self3.events else self3.events
      }
  else
    self

let _onresponse socketInFlight response peer self =
  let rec doPendingInner n self =
    if n < 1 || Array.length self._pendingRequests < 1 then
      self
    else
      let (req, node) = self._pendingRequests.[0] in
      doPendingInner
        (n-1)
        { self with events = (Request (req, { host = node.host ; port = node.port })) :: self.events }
  in
  Serialize.field "target" response
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionMap
       (fun (target : Buffer) ->
         if not (validateId target) then
           self
         else
           let mt =
             (Serialize.field "id" response,
              Serialize.field "roundtripToken" response
             )
           in
           match mt with
           | (Some id, roundtripToken) ->
              let rtt =
                roundtripToken
                |> optionMap
                     (fun rtt ->
                       Buffer.fromString (Serialize.asString rtt) "binary"
                     )
              in
              doPendingInner
                (min socketInFlight self.concurrency)
                (_addNode
                   socketInFlight
                   (Buffer.fromString (Serialize.asString id) "binary")
                   peer
                   rtt
                   self
                )
           | (None, _) -> self
       )

(*
DHT.prototype.holepunch = function (peer, referrer, cb) {
  this._holepunch(parseAddr(peer), parseAddr(referrer), cb)
}

DHT.prototype.ping = function (peer, cb) {
  this._ping(parseAddr(peer), function (err, res, peer) {
    if (err) return cb(err)
    var rinfo = decodePeer(res.value)
    if (!rinfo) return cb(new Error('Invalid pong'))
    cb(null, rinfo, {port: peer.port, host: peer.host, id: res.id})
  })
}

DHT.prototype.toArray = function () {
  return this.nodes.toArray()
}

DHT.prototype.address = function () {
  return this.socket.address()
}

DHT.prototype.bootstrap = function (cb) {
  var self = this

  if (!this._bootstrap.length) return process.nextTick(done)

  var backgroundCon = Math.min(self.concurrency, Math.max(2, Math.floor(self.concurrency / 8)))
  var qs = this.query({
    command: '_find_node',
    target: this.id
  })

  qs.on('data', update)
  qs.on('error', onerror)
  qs.on('end', done)

  update()

  function onerror (err) {
    if (cb) cb(err)
  }

  function done () {
    if (!self._bootstrapped) {
      self._bootstrapped = true
      self.emit('ready')
    }
    if (cb) cb()
  }

  function update () {
    qs._concurrency = self.inflightQueries === 1 ? self.concurrency : backgroundCon
  }
}

DHT.prototype.listen = function (port, cb) {
  this.socket.listen(port, cb)
}

function parseAddr (addr) {
  if (typeof addr === 'object' && addr) return addr
  if (typeof addr === 'number') return parseAddr(':' + addr)
  if (addr[0] === ':') return parseAddr('127.0.0.1' + addr)
  return {port: Number(addr.split(':')[1] || 3282), host: addr.split(':')[0]}
}

function arbiter (incumbant, candidate) {
  return candidate
}

DHT.prototype.ready = function (cb) {
  if (!this._bootstrapped) this.once('ready', cb)
  else cb()
}
*)
