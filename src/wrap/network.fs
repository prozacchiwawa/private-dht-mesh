module Network

open Util
open Fable.Core
open Fable.Core.JsInterop

type InterfaceDesc =
  { address : string ;
    family : string ;
    netmask : string ;
    mac : string ;
    nonpublic : bool
  }

type FlatInterfaceDesc = { name : string; desc : InterfaceDesc array }
    
type NetworkModule = Unused0
    
[<Emit("(function () { var n = require('os'); return n; })()")>]
let os_ : unit -> NetworkModule = fun _ -> failwith "JS only"

let os_module = os_ ()

[<Emit("(function () { var ifmap = $0.networkInterfaces(); var iflist = []; for (var i in ifmap) { iflist.push({ name: i, desc: ifmap[i] }); } setTimeout(function() { $1(null,iflist); }, 0) })()")>]
let get_interfaces_list__ : NetworkModule -> (string -> FlatInterfaceDesc array -> unit) -> unit = fun nm cb -> failwith "JS only"

let get_interfaces_list_ (cb : (string option -> FlatInterfaceDesc array option -> unit)) : unit =
  get_interfaces_list__ os_module (uncurriedFunction cb)

let get_interfaces_list _ : Q.Promise<FlatInterfaceDesc array, string> =
  let f = Q.defer () in
  let _ =
    get_interfaces_list_
      (fun err ilist ->
        match (err,ilist) with
        | (Some e, _) -> Q.reject (toString e) f
        | (_, None) -> Q.reject "No error specified" f
        | (_, Some v) -> Q.resolve v f
      )
  in
  f.promise

let convertRtt s =
  try
    let split = Util.stringSplit " " s in
    match split with
    | [|n;"ns"|] -> (float n) / 1000000000.0
    | [|n;"us"|] -> (float n) / 1000000.0
    | [|n;"ms"|] -> (float n) / 1000.0
    | [|n;"s"|] ->  (float n) / 1.0
    | [|n|] -> float n
    | _ -> 1.0
  with _ ->
    1.0
  
type NJSTraceroute = Unused1
type TraceHop =
  { hop : int
  ; ip : string
  ; rtt1 : float
  }
                   
[<Emit("(function() { var njt = require('nodejs-traceroute'); return njt; })()")>]
let traceroute_module_ : unit -> NJSTraceroute = fun _ -> failwith "JS"

let traceroute_module = traceroute_module_ ()

[<Emit("(function(njt,cvt,host) { var tracer = new njt(); var q = require('q'); var d = q.defer(); var res = []; tracer.on('close', function() { d.resolve(res); }); tracer.on('hop', function(h) { res.push({hop: h.hop, ip: h.ip, rtt1: cvt(h.rtt1)}); }); tracer.trace(host); return d.promise; })($0,$1,$2)")>]
let trace_ : NJSTraceroute -> (string -> float) -> string -> Q.Promise<TraceHop list,unit> = fun t c h -> failwith "JS"

let trace = trace_ traceroute_module convertRtt
