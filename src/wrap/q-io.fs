module QIO

open Fable.Core

open Buffer
open Q

type FSModule = Unused0

[<Emit("(function () { var fs = require('fs'); return fs; })()")>]
let fs_ : unit -> FSModule = fun _ -> failwith "JS"

let fs_module = fs_ ()
  
[<Emit("(function () { try { return $0.readFile($1,{flags: 'r'}, function (err,data) { if (err) { $2('' + err); } else { if (typeof(data) === 'string') { $3(data); } else { $3(data.toString()); } } }); } catch (err) { setTimeout($2('' + err), 0); } })()")>]
let readText_ : FSModule -> string -> (string -> unit) -> (string -> unit) -> unit = fun fs n errf textf -> failwith "JS"

[<Emit("(function () { try { return $0.readFile($1,{flags: 'b'}, function (err,data) { if (err) { $2('' + err); } else { if (typeof(data) === 'string') { $3(data); } else { $3(data); } } }); } catch (err) { setTimeout($2('' + err), 0); } })()")>]
let readBuffer_ : FSModule -> string -> (string -> unit) -> (Buffer -> unit) -> unit = fun fs n errf textf -> failwith "JS"

[<Emit("(function () { try { return $0.writeFile($1,$2,function (err) { if (err) { $3('' + err); } else { $4([]); } }); } catch (err) { setTimeout(function () { $3('' + err) }, 0); } })()")>]
let writeText_ : FSModule -> string -> string -> (string -> unit) -> (unit -> unit) -> unit = fun fs n t errf donef -> failwith "JS"

let readText name =
  let f : Q.Future<string,string> = Q.defer () in
  let _ =
    readText_
      fs_module
      name
      (fun err -> Q.reject err f)
      (fun data -> Q.resolve data f)
  in
  f.promise

let readBuffer name =
  let f : Q.Future<Buffer,string> = Q.defer () in
  let _ =
    readBuffer_
      fs_module
      name
      (fun err -> Q.reject err f)
      (fun data -> Q.resolve data f)
  in
  f.promise

let writeText name data =
  let f : Q.Future<unit,string> = Q.defer () in
  let _ =
    writeText_
      fs_module
      name
      data
      (fun err -> Q.reject err f)
      (fun data -> Q.resolve () f)
  in
  f.promise

[<Emit("try { $0.unlink($1,$3); } catch (e) { $2('' + e); }")>]
let unlink_ : FSModule -> string -> (string -> unit) -> (unit -> unit) -> unit = fun fs n err cb -> failwith "JS"

let unlink name =
  let f : Q.Future<unit,string> = Q.defer () in
  let _ =
    unlink_
      fs_module
      name
      (fun e -> Q.reject (Util.toString e) f)
      (fun _ -> Q.resolve () f)
  in
  f.promise
