module Dns

open Fable.Core

open Q
open Util

type DNSModule = Unused0

[<Emit("(function() { var dns = require('dns'); return dns; })()")>]
let dns_ : unit -> DNSModule = fun _ -> failwith "JS"

let dns_module_ = dns_ ()

[<Emit("(function() { try { $0.lookup($1, $2, function(err,addr,fam) { if (addr) { $3(addr); } else { $4('' + err); } }); } catch (e) { $4('' + e); } })()")>]
let lookup_ : DNSModule -> string -> int -> (string -> unit) -> (string -> unit) -> unit = fun m a f cb er -> failwith "JS"

let lookup host family =
  let future : Q.Future<string,string> = Q.defer () in
  let _ =
    lookup_
      dns_module_
      host
      family
      (fun addr -> Q.resolve addr future)
      (fun err -> Q.reject err future)
  in
  future.promise
