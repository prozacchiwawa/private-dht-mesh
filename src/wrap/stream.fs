module Stream

open Fable.Core

open Buffer

type Readable = Unused0
type Writable = Unused1

[<Emit("$1.on('close',$0)")>]
let onWClose : (unit -> unit) -> Writable -> unit = fun w f -> failwith "JS"

[<Emit("$1.on('drain',$0)")>]
let onWDrain : (unit -> unit) -> Writable -> unit = fun w f -> failwith "JS"

[<Emit("$1.on('error',function(err) { $0('' + err); })")>]
let onWError : (string -> unit) -> Writable -> unit = fun w f -> failwith "JS"

[<Emit("$1.end($0)")>]
let endString : string -> Writable -> unit = fun s w -> failwith "JS"

[<Emit("$1.write($0)")>]
let writeString : string -> Writable -> unit = fun s w -> failwith "JS"

[<Emit("$1.write($0)")>]
let writeBuffer : Buffer -> Writable -> unit = fun s w -> failwith "JS"

[<Emit("$0.cork()")>]
let cork : Writable -> unit = fun w -> failwith "JS"

[<Emit("$0.uncork()")>]
let uncork : Writable -> unit = fun w -> failwith "JS"

[<Emit("$1.on('data',function(b) { $0(b.toString()); })")>]
let onRDataString : (string -> unit) -> Readable -> unit = fun f r -> failwith "JS"

[<Emit("$1.on('data',function(b) { $0(b); })")>]
let onRDataBuffer : (Buffer -> unit) -> Readable -> unit = fun f r -> failwith "JS"

[<Emit("$1.on('close',$0)")>]
let onRClose : (unit -> unit) -> Readable -> unit = fun f r -> failwith "JS"

[<Emit("$1.on('error',function (err) { $0('' + err); })")>]
let onRError : (string -> unit) -> Readable -> unit = fun f r -> failwith "JS"

[<Emit("$1.on('end',$0)")>]
let onREnd : (unit -> unit) -> Readable -> unit = fun f r -> failwith "JS"

[<Emit("$0.pause()")>]
let pause : Readable -> unit = fun r -> failwith "JS"

[<Emit("$0.resume()")>]
let resume : Readable -> unit = fun r -> failwith "JS"

[<Emit("$0.isPaused()")>]
let isPaused : Readable -> bool = fun r -> failwith "JS"

[<Emit("$1.pipe($0)")>]
let pipe : Readable -> Writable -> unit = fun r w -> failwith "JS"
