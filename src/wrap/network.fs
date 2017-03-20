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
