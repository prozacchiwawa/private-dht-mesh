module UdpMessages

(* Ported from https://raw.githubusercontent.com/mafintosh/udp-request/master/index.js *)

open Buffer
open DHTData

let RETRIES = [|4;8;12|]

type Error =
  | ETIMEDOUT
  | Unknown of string

and Opts =
  { timeout : int option
  ; retry : int option
  }

and RInfo =
  { address : string
  ; port : int
  }

and Action =
  | Error of string
  | Receive of (Buffer * RInfo)
  | Send of (Buffer * RInfo)
  | SetTimeout of int
  | Cancel of (Error * Request)
  | Close

and Request =
  { tid : int
  ; request : Buffer
  ; peer : Node
  ; buffer : Buffer
  ; timeout : int
  ; tries : int
  }

and UDP =
  { timeout : int
  ; retry : int option
  ; inflight : int
  ; destroyed : bool
  ; _tick : int
  ; _out_req : Map<int,Request>
  ; events : Action list
  }

let optionDefault d o =
  match o with
  | Some v -> v
  | None -> d

let optionOrThen v o =
  match o with
  | Some v -> Some v
  | None -> v

let requestBufferFromMsgAndTid request tid _val =
  let targetBuf = Buffer.zero ((Buffer.length _val) + 2) in
  let _ = Buffer.copy 2 (Buffer.length _val) _val 0 targetBuf in
  let hdr = (if request then 32768 else 0) ||| tid in
  let _ = writeUInt16BE 0 hdr targetBuf in
  targetBuf

let _request _val peer opts self =
  if self.destroyed then
    self
  else
    let tid = (self._tick + 1) &&& 0x32767 in
    let message = requestBufferFromMsgAndTid true tid _val in
    { self with
        events =
          (Send (message, { address = peer.host ; port = peer.port })) ::
            self.events ;
        _tick = (self._tick + 1) &&& 0xffff
    }

let _forward request _val (from : Request) _to self =
  if self.destroyed then
    self
  else
    let message = requestBufferFromMsgAndTid request from.tid _val in
    { self with
        events =
          (Send (message, { address = _to.host ; port = _to.port })) ::
            self.events
    }

let forwardRequest _val from _to self = _forward true _val from _to self
let forwardResponse _val from _to self = _forward false _val from _to self

let _push tid req buf peer opts self =
  let retry = opts.retry |> optionOrThen self.retry in
  { self with
      inflight = self.inflight + 1 ;
      _out_req =
        Map.add
          tid
          { tid = self._tick
          ; request = req
          ; peer = peer
          ; buffer = buf
          ; timeout = 5
          ; tries = retry |> optionDefault (Array.length RETRIES)
          }
          self._out_req
  }

let _cancel tid err self =
  match Map.tryFind tid self._out_req with
  | Some req ->
     { self with
         _out_req = Map.remove tid self._out_req ;
         inflight = self.inflight - 1 ;
         events = (Cancel (err,req)) :: self.events
     }
  | None ->
     self

let response _val (from : Request) self =
  if self.destroyed then
    self
  else
    let message = requestBufferFromMsgAndTid false from.tid _val in
    { self with
        events =
          (Send (message, { address = from.peer.host ; port = from.peer.port })) ::
            self.events
    }

let isCancel e =
  match e with
  | Cancel c -> [c]
  | _ -> []

let _checkTimeouts self =
  let events = ref [] in
  let processRequest ((k,req) : (int * Request)) =
    if req.timeout > 0 then
      (k,{ req with timeout = req.timeout - 1 })
    else
      if req.tries < Array.length RETRIES then
        let newReq =
          { req with timeout = RETRIES.[req.tries] ; tries = req.tries + 1 }
        in
        let _ =
          events :=
            (Send (req.buffer, { address = req.peer.host ; port = req.peer.port })) ::
              !events
        in
        (k,newReq)
      else
        let _ =
          events := (Cancel (Unknown "canceled", req)) :: !events
        in
        (k,req)
  in
  let activeRequests =
    Map.toList self._out_req
    |> List.map processRequest
    |> Map.ofSeq
  in
  let isSend s =
    match s with
    | Send s -> [s]
    | _ -> []
  in
  let cancels = Seq.map isCancel !events |> Seq.concat in
  let sends = Seq.map isSend !events |> Seq.concat in
  Seq.fold
    (fun self (err,req) -> _cancel req.tid err self)
    { self with
        _out_req = activeRequests ; events = List.concat [!events;self.events]
    }
    cancels

let destroy err self =
  let cancels =
    self._out_req |> Map.toSeq
    |> Seq.map (fun (k,req) -> (Unknown "Destroyed", req))
  in
  Seq.fold
    (fun self (err,req) -> _cancel req.tid err self)
    { self with
        _out_req = Map.empty ;
        events = List.concat [cancels |> Seq.map Cancel |> List.ofSeq;self.events] ;
        destroyed = true
    }
    cancels

(*

UDP.prototype.address = function () {
  return this.socket.address()
                                   }

UDP.prototype.listen = function (port, cb) {
  if (typeof port === 'function') return this.listen(0, port)
  if (!port) port = 0
  this.socket.bind(port, cb)
                                  }

UDP.prototype.request = function (val, peer, opts, cb) {
  if (typeof opts === 'function') return this._request(val, peer, {}, opts)
  return this._request(val, peer, opts || {}, cb || noop)
                                   }

UDP.prototype.cancel = function (tid, err) {
  var i = this._out_req.indexOf(tid)
  if (i > -1) this._cancel(i, err)
                                  }

UDP.prototype._onmessage = function (message, rinfo) {
  if (this.destroyed) return

  var request = !!(message[0] & 128)
  var tid = message.readUInt16BE(0) & 32767
  var enc = request ? this.requestEncoding : this.responseEncoding

  try {
    var value = enc.decode(message, 2)
    } catch (err) {
    this.emit('warning', err)
    return
      }

  var peer = {port: rinfo.port, host: rinfo.address, tid: tid, request: request}

  if (request) {
    this.emit('request', value, peer)
    return
       }

  var state = this._pull(tid)

  this.emit('response', value, peer, state && state.request)
  if (state) state.callback(null, value, peer, state.request)
                                      }

UDP.prototype._pull = function (tid) {
  var free = this._out_req.indexOf(tid)
  if (free === -1) return null

  var req = this._reqs[free]
  this._reqs[free] = null
  this._out_req[free] = -1

  this.inflight--

  return req
                                 }

function noop () {}

*)
