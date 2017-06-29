open Str
open Char
open Unix
open Printf
open String
open Random
   
let dump s o =
  Printf.printf "%s" s ;
  output_value Pervasives.stdout o ;
  o
  
let log s o =
  dump s o

let getenv v =
  try
    Some (Unix.getenv v)
  with _ ->
    None

let toString a = "TODO-toString"

let toLowerCase s = String.lowercase s

let toHex n = Printf.sprintf "%x" n

let parseInt a =
  try
    Some (int_of_string a)
  with _ ->
    None

let parseHex a =
  try
    Some (int_of_string (String.concat "x" ["0";a]))
  with _ ->
    None

let rec zeropad n txidStr =
  if String.length txidStr >= n then
    txidStr
  else
    zeropad n (String.concat "" ["0" ; txidStr])

let charAt = String.get

let charCode = Char.code

let stringFromCharCode c = String.make 1 (Char.chr c)

let stringFind delim haystack =
  try
    Str.search_forward (Str.regexp (Str.quote delim)) haystack 0
  with _ ->
    -1

let stringSplit delim str =
  Str.split (Str.regexp (Str.quote delim)) str

let stringTrim s =
  let re = Str.regexp "^[ \\t]*\\(.*\\)[ \\t]*$" in
  Str.replace_first re "\\1" s

let substr st ed str =
  String.sub str st (ed - st)

let replaceall re ns str =
  Str.global_replace re ns str

let uncurriedFunction a = a


let toFloat a = float_of_int a

let floor a = int_of_float (Pervasives.floor a)

let rshift a b = a lsr b

let lshift a b = a lsl b

let random = Random.float

let randomInt = Random.int

let infinityInt = int_of_float infinity

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

(*
[<Emit("(function() { var a = $0; var na = Array.prototype.slice.call(a); for (var i = 0; i < a.length * 2; i++) { var p1 = Math.floor(Math.random() * a.length); var p2 = Math.floor(Math.random() * a.length); var t = na[p1]; na[p1] = na[p2]; na[p2] = t; } return na; })()")>]
let arrayShuffle : 'a array -> 'a array = fun a -> failwith "JS"

[<Emit("(function() { if ($0 < $1) { return -1; } else if ($0 > $1) { return 1; } else { return 0; } })()")>]
let strcmp : string -> string -> int = fun a b -> failwith "JS"

type RegExp = Unused
[<Emit("new RegExp($0)")>]
let re : string -> RegExp = fun re -> failwith "JS"

(* Revise later *)
[<Emit("!!($1.match($0))")>]
let reMatch : RegExp -> string -> bool = fun re str -> failwith "JS"
 *)
  
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

(*                                    
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
 *)
            
let tupleSndWith l r = (l,r)
let tupleFstWith r l = (l,r)

(*
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
 *)
                     
let listCons a b = a :: b

                          (*
let seqPartition condition values =     
    let pairs = seq {
        for i in values do
            if condition i then
                yield Some(i), None
            else
                yield None, Some(i) }

    pairs |> Seq.choose fst, pairs |> Seq.choose snd
                           *)
let rec skipWhile f l =
  match l with
  | hd :: tl -> if f hd then skipWhile f tl else (hd :: tl)
  | [] -> []

let rec truncate n l =
  match l with
  | hd :: tl -> if n < 1 then (hd :: tl) else truncate (n - 1) tl
  | [] -> []

let arraySortWith f a =
  let aa = Array.copy a in
  let _ = Array.sort f aa in
  aa
