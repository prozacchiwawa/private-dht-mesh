module DHT

open Buffer
open Crypto
open ShortId
open KBucket
open DHTData
open QueryStream

type Action =
  | Ready
  | Request of Buffer * UdpMessages.RInfo
  | StreamOp of QueryStream.Action
  | Destroyed

type KBucketNode =
  { id : Buffer
  ; vectorClock : int
  }

let min a b = Seq.min [a;b]
let max a b = Seq.max [a;b]
let floor x = int x

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

type DHT<'a,'b> =
  { id : Buffer
  ; ephermeral : bool
  ; nodes : KBucket<Buffer,KBucketNode>
  ; concurrency : int
  ; inFlightQueries : int
  ; _bootstrap : string list
  ; _queryId : Buffer option
  ; _bootstrapped : bool
  ; _pendingRequests : 'a array
  ; _tick : int
  ; _secrets : (Buffer * Buffer)
  ; _secretsInterval : int
  ; _tickInterval : int
  ; _top : 'b option
  ; _bottom : 'b option
  ; _queryData : Map<int array,QueryInfo>
  ; _results : Map<int array,QueryStream.Action>
  ; destroyed : bool
  ; events : Action list
  }

and QueryInfo =
  { qs : QueryStream
  ; actions : QueryStream.Action list
  }

and Opts =
  { q : QueryStream.Opts
  }

and Request<'r> =
  { request : 'r
  ; peer : Node
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

let _request socketInFlight request peer important self =
  let self2 =
    { self with
        _pendingRequests =
          Array.concat [self._pendingRequests;[|{request = request; peer = peer }|]]
    }
  in
  if socketInFlight >= self.concurrency ||
       (Array.length self._pendingRequests) > 0
  then
    self2
  else
    { self2 with
        events =
          (Request (request,{ address = peer.host ; port = peer.port })) ::
            self2.events
    }

let destroy self =
  if self.destroyed then
    self
  else
    { self with destroyed = true ; events = Destroyed :: self.events }

(*

DHT.prototype._pingSome = function () {
  var cnt = this.inflightQueries > 2 ? 1 : 3
  var oldest = this._bottom

  while (cnt--) {
    if (!oldest || this._tick - oldest.tick < 3) continue
    this._check(oldest)
    oldest = oldest.next
  }
}

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

DHT.prototype._rotateSecrets = function () {
  var secret = crypto.randomBytes(32)
  this._secrets[1] = this._secrets[0]
  this._secrets[0] = secret
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

DHT.prototype._ping = function (peer, cb) {
  this._request({command: '_ping', id: this._queryId}, peer, false, cb)
}

DHT.prototype._holepunch = function (peer, referrer, cb) {
  this._request({command: '_ping', id: this._queryId, forwardRequest: encodePeer(peer)}, referrer, false, cb)
}

DHT.prototype._onrequest = function (request, peer) {
  if (validateId(request.id)) this._addNode(request.id, peer, request.roundtripToken)

  if (request.roundtripToken) {
    if (!bufferEquals(request.roundtripToken, this._token(peer, 0))) {
      if (!bufferEquals(request.roundtripToken, this._token(peer, 1))) {
        request.roundtripToken = null
      }
    }
  }

  if (request.forwardRequest) {
    this._forwardRequest(request, peer)
    return
  }

  if (request.forwardResponse) peer = this._forwardResponse(request, peer)

  switch (request.command) {
    case '_ping': return this._onping(request, peer)
    case '_find_node': return this._onfindnode(request, peer)
  }

  this._onquery(request, peer)
}

DHT.prototype._forwardResponse = function (request, peer) {
  if (request.command !== '_ping') return // only allow ping for now

  try {
    var from = peers.decode(request.forwardResponse)[0]
    if (!from) return
  } catch (err) {
    return
  }

  from.request = true
  from.tid = peer.tid

  return from
}

DHT.prototype._forwardRequest = function (request, peer) {
  if (request.command !== '_ping') return // only allow ping forwards right now

  try {
    var to = peers.decode(request.forwardRequest)[0]
    if (!to) return
  } catch (err) {
    return
  }

  this.emit('holepunch', peer, to)
  request.forwardRequest = null
  request.forwardResponse = encodePeer(peer)
  this.socket.forwardRequest(request, peer, to)
}

DHT.prototype._onquery = function (request, peer) {
  if (!validateId(request.target)) return

  var self = this
  var query = {
    node: {
      id: request.id,
      port: peer.port,
      host: peer.host
    },
    command: request.command,
    target: request.target,
    value: request.value,
    roundtripToken: request.roundtripToken
  }

  var method = request.roundtripToken ? 'update' : 'query'

  if (!this.emit(method + ':' + request.command, query, callback) && !this.emit(method, query, callback)) callback()

  function callback (err, value) {
    if (err) return

    var res = {
      id: self._queryId,
      value: value || null,
      nodes: nodes.encode(self.nodes.closest(request.target, 20)),
      roundtripToken: self._token(peer, 0)
    }

    self.socket.response(res, peer)
  }
}

DHT.prototype._onresponse = function (response, peer) {
  if (validateId(response.id)) this._addNode(response.id, peer, response.roundtripToken)

  while (this.socket.inflight < this.concurrency && this._pendingRequests.length) {
    var next = this._pendingRequests.shift()
    this.socket.request(next.request, next.peer, next.callback)
  }
}

DHT.prototype._onping = function (request, peer) {
  var res = {
    id: this._queryId,
    value: encodePeer(peer),
    roundtripToken: this._token(peer, 0)
  }

  this.socket.response(res, peer)
}

DHT.prototype._onfindnode = function (request, peer) {
  if (!validateId(request.target)) return

  var res = {
    id: this._queryId,
    nodes: nodes.encode(this.nodes.closest(request.target, 20)),
    roundtripToken: this._token(peer, 0)
  }

  this.socket.response(res, peer)
}

DHT.prototype._onnodeping = function (oldContacts, newContact) {
  if (!this._bootstrapped) return // bootstrapping, we've recently pinged all nodes

  var reping = []

  for (var i = 0; i < oldContacts.length; i++) {
    var old = oldContacts[i]

    if (this._tick - old.tick < 3) { // less than 10s since we talked to this peer ...
      this.nodes.add(oldContacts[i])
      continue
    }

    reping.push(old)
  }

  if (reping.length) this._reping(reping, newContact)
}

DHT.prototype._check = function (node) {
  var self = this
  this._request({command: '_ping', id: this._queryId}, node, false, function (err) {
    if (err) self._removeNode(node)
  })
}

DHT.prototype._reping = function (oldContacts, newContact) {
  var self = this
  var next = null

  ping()

  function ping () {
    next = oldContacts.shift()
    if (next) self._request({command: '_ping', id: self._queryId}, next, true, afterPing)
  }

  function afterPing (err) {
    if (!err) return ping()

    self._removeNode(next)
    self.nodes.add(newContact)
  }
}

DHT.prototype._addNode = function (id, peer, token) {
  if (bufferEquals(id, this.id)) return

  var node = this.nodes.get(id)
  var fresh = !node

  if (!node) node = {}

  node.id = id
  node.port = peer.port
  node.host = peer.host
  node.roundtripToken = token
  node.tick = this._tick

  if (!fresh) remove(this, node)
  add(this, node)

  this.nodes.add(node)
  if (fresh) this.emit('add-node', node)
}

DHT.prototype._removeNode = function (node) {
  remove(this, node)
  this.nodes.remove(node.id)
  this.emit('remove-node', node)
}

DHT.prototype.listen = function (port, cb) {
  this.socket.listen(port, cb)
}

function encodePeer (peer) {
  return peer && peers.encode([peer])
}

function decodePeer (buf) {
  try {
    return buf && peers.decode(buf)[0]
  } catch (err) {
    return null
  }
}

function parseAddr (addr) {
  if (typeof addr === 'object' && addr) return addr
  if (typeof addr === 'number') return parseAddr(':' + addr)
  if (addr[0] === ':') return parseAddr('127.0.0.1' + addr)
  return {port: Number(addr.split(':')[1] || 3282), host: addr.split(':')[0]}
}

function validateId (id) {
  return id && id.length === 32
}

function arbiter (incumbant, candidate) {
  return candidate
}

function remove (self, node) {
  if (self._bottom !== node && self._top !== node) {
    node.prev.next = node.next
    node.next.prev = node.prev
    node.next = node.prev = null
  } else {
    if (self._bottom === node) {
      self._bottom = node.next
      if (self._bottom) self._bottom.prev = null
    }
    if (self._top === node) {
      self._top = node.prev
      if (self._top) self._top.next = null
    }
  }
}

function add (self, node) {
  if (!self._top && !self._bottom) {
    self._top = self._bottom = node
    node.prev = node.next = null
  } else {
    self._top.next = node
    node.prev = self._top
    node.next = null
    self._top = node
  }
}

DHT.prototype.ready = function (cb) {
  if (!this._bootstrapped) this.once('ready', cb)
  else cb()
}
*)
