(* From FreshEyeBall/elm-return *)
open Util

(*|
@docs Respond, append, sum, zero, comap
*)


(*|
A function from a model to a Cmd.
Basically there are times where you want to
have a side effect on the world if the model
has a certain shape. `Respond` facilitates
this use case.
*)
type ('msg,'a) respond =
  'a -> 'msg list
  

(*| *)
(* append : Respond<'msg,'a> -> Respond<'msg,'a> -> Respond<'msg,'a> *)
let append f g a =
  List.concat [ f a ; g a ]


(*| *)
(* sum : List (Respond<'msg,'a>) -> Respond<'msg,'a> *)
let sum rs a =
  List.map (fun r -> r a) rs |> List.concat


(*| *)
(* zero : Respond<'msg,'a> *)
let zero _ =
  []


(*|
Add a function to the front
`b -> a >> a -> Cmd msg`
*)
(* comap : (b -> a) -> Respond msg a -> Respond msg b *)
let comap = (>>)
