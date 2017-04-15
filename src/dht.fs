module DHT

(* https://github.com/mafintosh/dht-rpc/blob/master/index.js *)

open Util
open Buffer
open Crypto
open ShortId
open KBucket
open DHTData
open QueryStream

let queryTimeout = 30
       
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

and Request =
  { peer : NodeIdent
  ; body : Serialize.Json
  ; launched : int
  }

type DHT =
  { id : Buffer
  ; nodes : KBucket<Buffer,Node>
  ; concurrency : int
  ; _bootstrap : NodeIdent array
  ; _bootstrapped : bool
  ; _pendingRequests : Map<string, Request>
  ; _inFlightRequests : Map<string, Request>
  ; _tick : int
  ; _tickInterval : int
  ; _bottom : NodeListElement list
  ; _results : Map<int array,QueryStream.Action>
  ; destroyed : bool
  ; events : Action list
  ; replacementNodes : Node list
  }

and Opts =
  { id : Buffer option
  ; bootstrap : NodeIdent array
  }
    
let defaultOpts =
  { id = None
  ; bootstrap = [| |]
  }

let hashId id =
  let sha256Hasher = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString id "binary") sha256Hasher in
  Crypto.digestBuffer sha256Hasher
    
let init opts =
  let id = opts.id |> optionDefault (hashId (ShortId.generate ()))
  { id = id
  ; nodes = KBucket.init id
  ; concurrency = 16
  ; _bootstrap = opts.bootstrap
  ; _bootstrapped = false
  ; _pendingRequests = Map.empty
  ; _inFlightRequests = Map.empty
  ; _tick = 0
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
        self
    )
    self
    self._bootstrap

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
      (qid : string)
      (request : Serialize.Json)
      (peer : NodeIdent)
      (important : bool)
      (self : DHT) : DHT =
  let newRequest =
    { Request.peer = peer
    ; Request.body = request
    ; Request.launched = self._tick
    }
  in
  if socketInFlight >= (self.concurrency - self._inFlightRequests.Count) then
    { self with _pendingRequests = Map.add qid newRequest self._pendingRequests }
  else
    { self with
        events =
          (Datagram
             (request,
              { id = peer.id
              ; host = peer.host
              ; port = peer.port
              }
             )
          ) ::
            self.events ;
        _pendingRequests = Map.remove qid self._pendingRequests ;
        _inFlightRequests = Map.add qid newRequest self._inFlightRequests
    }

let query
      socketInFlight
      (query : Serialize.Json)
      (self : DHT) : DHT =
  let qid = ShortId.generate () in
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
      qid
      query
      { id = target
      ; host = closest.[0].host
      ; port = closest.[0].port
      }
      true
      self
  else
    self

let makePingBody qid (self : DHT) =
  Serialize.jsonObject
    [| ("command", Serialize.jsonString "_ping") ;
       ("qid", Serialize.jsonString qid) ;
       ("id", Buffer.toString "binary" self.id |> Serialize.jsonString)
    |]

let _check socketInFlight node self =
  let qid = ShortId.generate () in
  _request
    socketInFlight
    qid
    (makePingBody qid self)
    node
    false
    self

let _pingSome socketInFlight (self : DHT) : DHT =
  let cnt = if self._inFlightRequests.Count > 2 then 1 else 3 in
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

let _ping
      socketInFlight
      (peer : NodeIdent)
      (self : DHT) : DHT =
  let qid = ShortId.generate () in
  _request
    socketInFlight
    qid
    (makePingBody qid self)
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
      let qid = ShortId.generate () in
      _request
        socketInFlight
        qid
        (makePingBody qid self)
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

let _onping rid request (peer : NodeIdent) (self : DHT) : DHT =
  let res =
    [| ("id", Buffer.toString "binary" self.id |> Serialize.jsonString) ;
       ("rid", Serialize.jsonString rid) ;
       ("value", Serialize.jsonString (encodePeers [|peer|]))
    |]
    |> Serialize.jsonObject
  in
  { self with events = (Datagram (res, peer)) :: self.events }

let _onfindnode
      (rid : string)
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
                    ; ("command", Serialize.jsonString "_find_repl")
                    ; ("rid", Serialize.jsonString rid)
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
                  ; (let qid =
                       Serialize.field "qid" request
                       |> optionMap Serialize.asString
                     in
                     match qid with
                     | Some qid -> [("qid", Serialize.jsonString qid)]
                     | None -> []
                    )
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
          ; vectorClock = self._tick
          ; referer = peer.id
          }
         )
    in
    let self = if not fresh then remove defNode self else self in
    let self =
      if Array.length self._bootstrap = 0 then
        { self with _bootstrap = [| peer |] }
      else
        self
    in
    let self = add defNode self in
    let (newNodes,newEvents) = KBucket.add kBucketOps self.nodes defNode None in
    applyKBucketEvents
      socketInFlight
      newEvents
      { self with
          nodes = newNodes ;
          events =
            if fresh then (AddNode defNode) :: self.events else self.events
      }
  else
    self

let _onpong
      socketInFlight
      (request : Serialize.Json)
      (peer : NodeIdent)
      (self : DHT) : DHT =
  _addNode
    socketInFlight
    peer
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
        self
    )
    (_addNode
       socketInFlight
       peer
       self0)
    (decodePeers nodeString)

let _onresponse
      socketInFlight
      (rid : string)
      (response : Serialize.Json)
      (peer : NodeIdent)
      (self : DHT) : DHT =
  let nodeString =
    Serialize.field "nodes" response
    |> optionMap Serialize.asString
    |> optionDefault "\0\0"
  in
  let pendingSeq =
    Map.toSeq self._pendingRequests
    |> Seq.filter (fun (id,r) -> r.launched + queryTimeout >= self._tick)
  in
  let filtered =
    { self with
        _inFlightRequests = Map.remove rid self._inFlightRequests ;
        _pendingRequests = Map.ofSeq pendingSeq
    }
  in
  let (updated : DHT) =
    match Serialize.field "command" response |> optionMap Serialize.asString with
    | Some "_pong" ->
       _onpong
         socketInFlight
         response
         { NodeIdent.id = peer.id
         ; NodeIdent.host = peer.host
         ; NodeIdent.port = peer.port
         }
         filtered
    | Some "_find_repl" ->
       _onfindreply
         socketInFlight
         response
         { NodeIdent.id = peer.id
         ; NodeIdent.host = peer.host
         ; NodeIdent.port = peer.port
         }
         { filtered with _bootstrapped = true }
    | _ ->
       filtered
  in
  let newRequests =
    min 0 (updated.concurrency - updated._inFlightRequests.Count)
  in
  let requestsToTake = pendingSeq |> Seq.truncate newRequests in
  Seq.fold
    (fun self (rid,req) ->
      _request
        socketInFlight
        rid
        req.body
        req.peer
        true
        self
    )
    updated
    requestsToTake

let _onrequest socketInFlight request (peer : NodeIdent) self0 =
  let self =
    (_addNode
       socketInFlight
       peer
       self0
    )
  in
  let mt =
    (Serialize.field "rid" request |> optionMap Serialize.asString,
     Serialize.field "qid" request |> optionMap Serialize.asString,
     Serialize.field "command" request |> optionMap Serialize.asString
    )
  in
  match mt with
  | (Some rid, _, _) ->
     _onresponse socketInFlight rid request peer self
  | (_, Some qid, Some "_ping") ->
     _onping
       qid
       request
       { NodeIdent.id = peer.id
       ; NodeIdent.host = peer.host
       ; NodeIdent.port = peer.port
       }
       self
  | (_, Some qid, Some "_find_node") ->
     _onfindnode
       qid
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

let _findnode socketInFlight (qid : string) (id : Buffer) (self : DHT) : DHT =
  let q =
    [| ("command", Serialize.jsonString "_find_node")
     ; ("target", Serialize.jsonString (Buffer.toString "binary" id))
     ; ("id", Serialize.jsonString (Buffer.toString "binary" self.id))
     ; ("qid", Serialize.jsonString qid)
    |]
    |> Serialize.jsonObject
  in
  query socketInFlight (dump "q" q) self

let tick socketInFlight self =
  let nextTick = self._tick + 1 in
  let tself = { self with _tick = nextTick } in
  let uself =
    if self._bootstrapped then
      tself
    else if Array.length self._bootstrap > 0 &&
              self._inFlightRequests.Count < self.concurrency
    then
      _findnode
        socketInFlight
        (ShortId.generate ())
        self.id
        tself
    else
      tself
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
