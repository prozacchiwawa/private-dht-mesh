module Util

open Fable.Core

[<Emit("(function () { console.log($0,$1); return $1; })()")>]
let log : string -> 'a -> 'a = fun s o -> failwith "JS only"

[<Emit("(function () { console.log($0,JSON.stringify($1)); return $1; })()")>]
let dump : string -> 'a -> 'a = fun s o -> failwith "JS only"

[<Emit("(function (evar) { return process.env[evar]; })($0)")>]
let getenv : string -> string option = fun s -> failwith "JS only"

[<Emit("'' + $0")>]
let toString : 'a -> string = fun a -> failwith "JS only"

[<Emit("('' + $0).toLowerCase()")>]
let toLowerCase : string -> string = fun a -> failwith "JS only"

[<Emit("$0.toString(16)")>]
let toHex : int -> string = fun a -> failwith "JS only"

[<Emit("(function() { try { return parseInt($0); } catch (e) { return null; } })()")>]
let parseInt : string -> int option = fun a -> failwith "JS only"

[<Emit("(function() { try { return parseInt($0, 16); } catch (e) { return null; } })()")>]
let parseHex : string -> int option = fun a -> failwith "JS only"

let rec zeropad n txidStr =
  if String.length txidStr >= n then
    txidStr
  else
    zeropad n (String.concat "" ["0" ; txidStr])

[<Emit("($1.charCodeAt($0) || 0)")>]
let charAt : int -> string -> int = fun n s -> failwith "JS only"

[<Emit("(function() { return $0.charCodeAt(0); })()")>]
let charCode : char -> int = fun c -> failwith "JS only"

[<Emit("String.fromCharCode($0)")>]
let stringFromCharCode : int -> string = fun cc -> failwith "JS only"

[<Emit("('' + $1).indexOf($0)")>]
let stringFind : string -> string -> int = fun delim haystack -> failwith "JS only"

[<Emit("('' + $1).split($0)")>]
let stringSplit : string -> string -> string array = fun delim haystack -> failwith "JS only"

[<Emit("('' + $0).trim()")>]
let stringTrim : string -> string = fun st -> failwith "JS"

[<Emit("('' + $2).substr($0,$1)")>]
let substr : int -> int -> string -> string = fun st ed str -> failwith "JS only"

[<Emit("('' + $2).replace(new RegExp($0, 'g'), $1)")>]
let replaceall : string -> string -> string -> string = fun re ns str -> failwith "JS only"

[<Emit("(function(a,b) { return $0(a)(b); })")>]
let uncurriedFunction : ('a -> 'b -> 'c) -> 'x = fun f -> failwith "JS only"

[<Emit("$0")>]
let toFloat : int -> float = fun i -> failwith "JS only"

[<Emit("Math.floor($0)")>]
let floor : float -> int = fun f -> failwith "JS only"

[<Emit("$0 >>> $1")>]
let rshift : int -> int -> int = fun a b -> failwith "JS only"

[<Emit("$0 << $1")>]
let lshift : int -> int -> int = fun a b -> failwith "JS only"

[<Emit("Math.random()")>]
let random : unit -> float = fun _ -> failwith "JS"

let randomInt mx =
  floor ((toFloat mx) * (random ()))

[<Emit("(function() { return Number.POSITIVE_INFINITY; })()")>]
let infinity_ : unit -> float = fun _ -> failwith "JS"
[<Emit("(function() { return Number.POSITIVE_INFINITY; })()")>]
let infinityInt_ : unit -> int = fun _ -> failwith "JS"
let infinity = infinity_ ()
let infinityInt = infinityInt_ ()

let optionElse f o =
  match o with
  | Some a -> Some a
  | None -> f ()

let optionEither a b o =
  match o with
  | Some o -> a
  | None -> b

let optionDefault d o =
  match o with
  | Some a -> a
  | None -> d

let optionToList v =
  match v with
  | Some v -> [v]
  | None -> []

[<Emit("(function() { var a = $0; var na = Array.prototype.slice.call(a); for (var i = 0; i < a.length * 2; i++) { var p1 = Math.floor(Math.random() * a.length); var p2 = Math.floor(Math.random() * a.length); var t = na[p1]; na[p1] = na[p2]; na[p2] = t; } return na; })()")>]
let arrayShuffle : 'a array -> 'a array = fun a -> failwith "JS"

let singletonList a = [a]

let flip f a b = f b a

let replicate list num =
  let rec replicateInner ch num =
    match num with
    | 0 -> []
    | _ -> ch::replicateInner ch (num-1)
  in
  let rec replicateOuter list num =
    match list with
    | [] -> []
    | h::t -> (replicateInner h num) @ replicateOuter t num
  in
  replicateOuter list num

[<Emit("(function() { if ($0 < $1) { return -1; } else if ($0 > $1) { return 1; } else { return 0; } })()")>]
let strcmp : string -> string -> int = fun a b -> failwith "JS"

type RegExp = Unused
[<Emit("new RegExp($0)")>]
let re : string -> RegExp = fun re -> failwith "JS"

(* Revise later *)
[<Emit("!!($1.match($0))")>]
let reMatch : RegExp -> string -> bool = fun re str -> failwith "JS"

(* Elm basics *)

(*| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:
    not << isEven << sqrt
You can think of this operator as equivalent to the following:
    (g << f)  ==  (\x -> g (f x))
So our example expands out to something like this:
    \n -> not (isEven (sqrt n))
*)
(* (<<) : (b -> c) -> (a -> b) -> (a -> c) *)
let (<<) g f x =
  g (f x)

(*| Function composition, passing results along in the suggested direction. For
example, the following code checks if the square root of a number is odd:
    sqrt >> isEven >> not
This direction of function composition seems less pleasant than `(<<)` which
reads nicely in expressions like: `filter (not << isRegistered) students`
*)
(* (>>) : (a -> b) -> (b -> c) -> (a -> c) *)
let (>>) f g x =
  g (f x)

let curry f (a,b) = f a b

let min a b = if a < b then a else b
let max a b = if a > b then a else b

let windowed (n : int) s =
  let out = ref None in
  let i = ref 0 in
  (Seq.concat [Seq.map Some s;List.toSeq [None]])
  |> Seq.map
       (fun ch ->
         let res =
           match ch with
           | Some ch ->
              let a =
                match !out with
                | Some a -> a
                | None ->
                   let a = Array.init n (fun _ -> ch) in
                   let _ = out := Some a in
                   a
              in
              let _ = a.[!i % n] <- ch in
              let res =
                if (!i % n) = (n - 1) then
                  Array.map id a
                else
                  Array.empty
              in
              let _ = i := !i + 1 in
              res
           | None ->
              match !out with
              | Some a -> Array.init !i (fun n -> a.[n])
              | None -> Array.empty
         in
         res
       )
  |> Seq.filter (fun a -> Array.length a <> 0)

let tupleSndWith l r = (l,r)
let tupleFstWith r l = (l,r)

type Error = Unused0
[<Emit("(function(e) { return new Error(e); })($0)")>]
let newError : string -> Error = fun e -> failwith "JS"                     
                     
[<Emit("(function(e) { throw e; })($0)")>]
let raise : 'e -> 'a = fun e -> failwith "JS"

let seqAllOrNone s =
  if Seq.contains None s then
    None
  else
    Some (Seq.concat (Seq.map optionToList s))

[<Emit("(function(e) { return e.stack; })($0)")>]
let getStack : 'a -> string = fun e -> failwith "JS"

let listCons a b = a :: b

let seqPartition condition values =     
    let pairs = seq {
        for i in values do
            if condition i then
                yield Some(i), None
            else
                yield None, Some(i) }

    pairs |> Seq.choose fst, pairs |> Seq.choose snd
