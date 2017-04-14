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
  | Datagram of Serialize.Json * NodeIdent
  | Payload of Serialize.Json * NodeIdent
  | AddNode of Node
  | RemoveNode of Node

and NodeListElement =
  { peer : Node
  ; tick : int
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

type DHT =
  { id : Buffer
  ; nodes : KBucket<Buffer,Node>
  ; concurrency : int
  ; inFlightQueries : int
  ; _bootstrap : NodeIdent array
  ; _bootstrapped : bool
  ; _pendingRequests : (Serialize.Json * NodeIdent) array
  ; _tick : int
  ; _secrets : (Buffer * Buffer)
  ; _secretsInterval : int
  ; _tickInterval : int
  ; _bottom : NodeListElement list
  ; _results : Map<int array,QueryStream.Action>
  ; destroyed : bool
  ; events : Action list
  ; replacementNodes : Node list
  }

and QueryInfo =
  { qs : QueryStream
  ; actions : QueryStream.Action list
  }

and Opts =
  { q : QueryStream.Opts
  ; id : Buffer option
  ; bootstrap : NodeIdent array
  }

let hashId id =
  let sha256Hasher = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString id "binary") sha256Hasher in
  Crypto.digestBuffer sha256Hasher

let defaultOpts =
  { q = QueryStream.defaultOpts
  ; id = None
  ; bootstrap = [| |]
  }

let init opts =
  let id = opts.id |> optionDefault (hashId (ShortId.generate ()))
  { id = id
  ; nodes = KBucket.init id
  ; concurrency = 16
  ; inFlightQueries = 0
  ; _bootstrap = opts.bootstrap
  ; _bootstrapped = false
  ; _pendingRequests = [| |]
  ; _tick = 0
  ; _secrets = (hashId (ShortId.generate ()), hashId (ShortId.generate ()))
  ; _secretsInterval = 5 * 60
  ; _tickInterval = 5
  ; _bottom = []
  ; _results = Map.empty
  ; destroyed = false
  ; events = []
  ; replacementNodes = []
  }

let _bootstrap _addNode (self : DHT) =
  Array.fold
    (fun self (node : NodeIdent) ->
      _addNode
        0
        { NodeIdent.id = node.id
        ; NodeIdent.host = node.host
        ; NodeIdent.port = node.port
        }
        None
        self
    )
    self
    self._bootstrap

let _token (peer : NodeIdent) i self =
  let sha256Hasher = Crypto.createHash "sha256" in
  let theSecret =
    match (self._secrets,i) with
    | ((s,_),false) -> s
    | ((_,s),_) -> s
  in
  let addr = IPAddr.parse peer.host in
  let hostBuffer = Buffer.fromArray (IPAddr.toByteArray addr) in
  let _ = Crypto.updateBuffer theSecret sha256Hasher in
  let _ = Crypto.updateBuffer hostBuffer sha256Hasher in
  Crypto.digestBuffer sha256Hasher

let (kBucketOps : KBucket.KBucketAbstract<Buffer,Node>) =
  { distance = KBucket.defaultDistance
  ; nodeId = fun n -> n.id
  ; arbiter = fun i e -> if i.vectorClock > e.vectorClock then i else e
  ; keyNth = Buffer.at
  ; keyLength = Buffer.length
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let _request
      socketInFlight
      (request : Serialize.Json)
      (peer : NodeIdent)
      (important : bool)
      (self : DHT) : DHT =
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
          (Datagram
             (request,
              { id = peer.id
              ; host = peer.host
              ; port = peer.port
              }
             )
          ) ::
            self2.events
    }

let query
      socketInFlight
      (query : Serialize.Json)
      opts
      (self : DHT) : DHT =
  let newId = ShortId.generate () in
  let hashedId = hashId newId in
  let target =
    Serialize.field "target" query
    |> optionDefault (Serialize.jsonNull ())
    |> Serialize.asString
    |> (fun s -> Buffer.fromString s "binary")
  in
  let closest = KBucket.closest kBucketOps self.nodes target 1 None in
  if Array.length closest > 0 then
    _request
      socketInFlight
      query
      { id = target
      ; host = closest.[0].host
      ; port = closest.[0].port
      }
      true
      self
  else
    self

let update socketInFlight q opts self =
  let o1 = { opts with q = { opts.q with token = true } } in
  query socketInFlight q o1 self

let makePingBody (self : DHT) =
  Serialize.jsonObject
    [| ("command", Serialize.jsonString "_ping") ;
       ("id", Buffer.toString "binary" self.id |> Serialize.jsonString)
    |]

let _check socketInFlight node self =
  _request
    socketInFlight
    (makePingBody self)
    node
    false
    self
    (* if (err) self._removeNode(node) <-- ping reply *)

let _pingSome socketInFlight (self : DHT) : DHT =
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
    !oldest

let _rotateSecrets self =
  let secret = Crypto.randomBytes 32 in
  let (s0,s1) = self._secrets in
  { self with _secrets = (secret, s0) }

let _ping
      socketInFlight
      (peer : NodeIdent)
      (self : DHT) : DHT =
  _request
    socketInFlight
    (makePingBody self)
    peer
    false
    self

(* Peers:
 * Number of ip4 peers as 16-bit uint.
 * ip4 peers as 6 bytes each plus sha256.
 * ip6 peers as 18 bytes each plus sha256.
 * Returns string
 *)
let encodePeers (peers : NodeIdent array) : string =
  let (ip4peersS,ip6peersS) =
    seqPartition
      (fun (p : NodeIdent) ->
        let parsed = IPAddr.parse p.host in
        IPAddr.kind parsed = "ipv4"
      )
      peers
  in
  let (ip4peers,ip6peers) = (Array.ofSeq ip4peersS, Array.ofSeq ip6peersS) in
  let numip4peers = Array.length ip4peers in
  let numip6peers = Array.length ip6peers in
  let fulllength = 2 + (38 * numip4peers) + (50 * numip6peers) in
  let buffer = Buffer.zero fulllength in
  let _ = Buffer.writeUInt16BE 0 numip4peers buffer in
  let _ =
    for i = 0 to numip4peers - 1 do
      begin
        let start = 2 + 38 * i in
        let parsed = IPAddr.parse ip4peers.[i].host in
        let buf = IPAddr.toByteArray parsed |> Buffer.fromArray in
        let _ = Buffer.writeUInt16BE start ip4peers.[i].port buffer in
        let _ = Buffer.copy (start + 2) (Buffer.length buf) buf 0 buffer in
        Buffer.copy
          (start + 6)
          (Buffer.length ip4peers.[i].id)
          ip4peers.[i].id 0 buffer
      end ;
  let _ =
    for i = 0 to numip6peers - 1 do
      begin
        let start = 2 + (38 * numip4peers) + (50 * i) in
        let parsed = IPAddr.parse ip6peers.[i].host in
        let buf = IPAddr.toByteArray parsed |> Buffer.fromArray in
        let _ = Buffer.writeUInt16BE start ip6peers.[i].port buffer in
        let _ =
          Buffer.copy
            (start + 2)
            (Buffer.length buf) buf 0 buffer
        in
        Buffer.copy
          (start + 18)
          (Buffer.length ip6peers.[i].id)
          ip6peers.[i].id 0 buffer
      end
  in
  Buffer.toString "binary" buffer

(*
 * Decode encoded peers.
 *)
let decodePeers (str : string) : NodeIdent array =
  try
    let buf = Buffer.fromString str "binary" in
    let l = Buffer.length buf in
    let numip4peers = Buffer.readUInt16BE 0 buf in
    let numip6peers = ((l - 2) - (numip4peers * 38)) / 18 in
    let peer i =
      if i < numip4peers then
        let start = 2 + (i * 38) in
        let port = Buffer.readUInt16BE start buf in
        let ipbuf = Buffer.slice (start + 2) (start + 6) buf in
        let idbuf = Buffer.slice (start + 6) (start + 38) buf in
        let ipaddr_ = IPAddr.fromByteArray (Buffer.toArray ipbuf) in
        { NodeIdent.id = idbuf
        ; NodeIdent.host = IPAddr.toString ipaddr_
        ; NodeIdent.port = port
        }
      else
        let start = 2 + (numip4peers * 38) + (i * 50) in
        let port = Buffer.readUInt16BE start buf in
        let ipbuf = Buffer.slice (start + 2) (start + 18) buf in
        let idbuf = Buffer.slice (start + 18) (start + 50) buf in
        let ipaddr_ = IPAddr.fromByteArray (Buffer.toArray ipbuf) in
        { NodeIdent.id = idbuf
        ; NodeIdent.host = IPAddr.toString ipaddr_
        ; NodeIdent.port = port
        }
    in
    Array.init (numip4peers + numip6peers) peer
  with _ ->
    [| |]

let _reping
      socketInFlight
      (oldContacts : Node array)
      (newContact : Node)
      (self : DHT) : DHT =
  Array.fold
    (fun (self : DHT) (contact : Node) ->
      _request
        socketInFlight
        (makePingBody self)
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

let _onquery request (peer : NodeIdent) (self : DHT) : DHT =
  Serialize.field "target" request
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionThen (fun target -> if validateId target then Some target else None)
  |> optionMap
       (fun (target : Buffer) ->
         { self with
             events =
               (Payload (request, peer)) :: self.events
         }
       )
  |> optionDefault self

let _onping request (peer : NodeIdent) self =
  let token = Buffer.toString "binary" (_token peer false self) in
  let res =
    Serialize.jsonObject
      [| ("id", Buffer.toString "binary" self.id |> Serialize.jsonString) ;
         ("value", Serialize.jsonString (encodePeers [|peer|])) ;
         ("roundtripToken", Serialize.jsonString token)
      |]
  { self with events = (Datagram (res, peer)) :: self.events }

let _onfindnode
      (request : Serialize.Json)
      (peer : NodeIdent)
      (self : DHT) : DHT =
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
           let res =
             Serialize.jsonObject
               (Seq.concat
                  [ [ ("id",
                       Buffer.toString "binary" self.id
                       |> Serialize.jsonString
                      )
                    ; ("nodes",
                       Serialize.jsonString
                         (encodePeers
                            (KBucket.closest
                               kBucketOps
                               self.nodes
                               target
                               20
                               None
                             |> Array.map
                                  (fun n ->
                                    { id = n.id
                                    ; host = n.host
                                    ; port = n.port
                                    }
                                  )
                            )
                         )
                      )
                    ]
                  ; let qid =
                      Serialize.field "qid" request
                      |> optionMap Serialize.asString
                    in
                    match qid with
                    | Some qid -> [("qid", Serialize.jsonString qid)]
                    | None -> []
                  ]
                |> Array.ofSeq
               )
           in
           { self with events = (Datagram (res, peer)) :: self.events }
       )
  |> optionDefault self

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

let _addNode
      socketInFlight
      (peer : NodeIdent)
      (token : Buffer option)
      (self : DHT) : DHT =
  if not (Buffer.equal peer.id self.id) then
    let node = KBucket.get kBucketOps self.nodes peer.id None in
    let (fresh,defNode) =
      match node with
      | Some node -> (false,node)
      | None ->
         (true,
          { id = peer.id
          ; queried = false
          ; distance = Buffer.fromArray (KBucket.defaultDistance kBucketOps peer.id self.id)
          ; port = peer.port
          ; host = peer.host
          ; roundtripToken = token |> optionMap (Buffer.toString "binary") |> optionDefault ""
          ; vectorClock = self._tick
          ; referer = peer.id
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

let _onfindreply
      socketInFlight
      (request : Serialize.Json)
      (peer : NodeIdent)
      (self0 : DHT) : DHT =
  let nodeString =
    Serialize.field "nodes" request
    |> optionMap Serialize.asString
    |> optionDefault "\0\0"
  in
  Array.fold
    (fun self peer ->
      _addNode
        socketInFlight
        peer
        None
        self
    )
    (_addNode
       socketInFlight
       peer
       None
       self0)
    (decodePeers nodeString)

let _onrequest socketInFlight request (peer : NodeIdent) self0 =
  Serialize.field "target" request
  |> optionMap
       (fun (target : Serialize.Json) ->
         Buffer.fromString (Serialize.asString target) "binary"
       )
  |> optionMap
       (fun (target : Buffer) ->
         if not (validateId target) then
           self0
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
                  let (s0,s1) = self0._secrets in
                  match !rttBuffer with
                  | Some rb ->
                     if not (Buffer.equal s0 rb) && not (Buffer.equal s1 rb) then
                       rttBuffer := None ;
                  | None -> ()
                end
             | None -> ()
           in
           let self =
             (_addNode
                socketInFlight
                peer
                !rttBuffer
                self0
             )
           in
           let m =
             Serialize.field "command" request
             |> optionMap Serialize.asString
           in
           match m with
           | Some "ping" ->
              _onping
                request
                { NodeIdent.id = peer.id
                ; NodeIdent.host = peer.host
                ; NodeIdent.port = peer.port
                }
                self
           | Some "_find_node" ->
              _onfindnode
                request
                { NodeIdent.id = peer.id
                ; NodeIdent.host = peer.host
                ; NodeIdent.port = peer.port
                }
                self
           | Some "_find_repl" ->
              _onfindreply
                socketInFlight
                request
                { NodeIdent.id = peer.id
                ; NodeIdent.host = peer.host
                ; NodeIdent.port = peer.port
                }
                self
           | _ ->
              _onquery
                request
                { NodeIdent.id = peer.id
                ; NodeIdent.host = peer.host
                ; NodeIdent.port = peer.port
                }
                self
       )
  |> optionDefault self0

let _onresponse
      socketInFlight
      (response : Serialize.Json)
      (peer : NodeIdent)
      (self0 : DHT) : DHT =
  let nodeString =
    Serialize.field "nodes" response
    |> optionMap Serialize.asString
    |> optionDefault "\0\0"
  in
  let self =
    Array.fold
      (fun self peer ->
        _addNode
          socketInFlight
          peer
          None
          self
      )
      (_addNode
         socketInFlight
         peer
         None
         self0)
      (decodePeers nodeString)
  in
  let rec doPendingInner n self =
    if n < 1 || Array.length self._pendingRequests < 1 then
      self
    else
      let (req, node) = self._pendingRequests.[0] in
      doPendingInner
        (n-1)
        { self with
            events =
              (Datagram
                 (req,
                  { id = node.id
                  ; host = node.host
                  ; port = node.port
                  }
                 )
              ) :: self.events
        }
  in
  let (self : DHT) = { self with _bootstrapped = true } in
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
           let rtt =
             match Serialize.field "roundtripToken" response with
             | Some rtt ->
                Some (Buffer.fromString (Serialize.asString rtt) "binary")
             | None -> None
           in
           doPendingInner
             (min socketInFlight self.concurrency)
             (_addNode
                socketInFlight
                peer
                rtt
                self
             )
       )
  |> optionDefault self

let _findnode socketInFlight (qid : string option) (id : Buffer) (self : DHT) : DHT =
  let q =
    Serialize.jsonObject
      (Seq.concat
         [ [ ("command", Serialize.jsonString "_find_node") ;
             ("target", Serialize.jsonString (Buffer.toString "binary" id))
           ]
         ; [("id", Serialize.jsonString (Buffer.toString "binary" self.id))]
         ; match qid with
           | Some qid -> [("qid", Serialize.jsonString qid)]
           | None -> []
         ]
       |> Array.ofSeq
      )
  in
  query socketInFlight q defaultOpts self

let tick socketInFlight self =
  let nextTick = self._tick + 1 in
  let uself =
    if self._bootstrapped then
      { self with _tick = nextTick }
    else
      _findnode socketInFlight None self.id { self with _tick = nextTick }
  in
  if nextTick &&& 7 = 0 then
    _pingSome socketInFlight uself
  else
    uself

let bootstrap =
  _bootstrap _addNode

(*

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
