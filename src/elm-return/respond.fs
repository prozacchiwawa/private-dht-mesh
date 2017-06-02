(* From FreshEyeBall/elm-return *)
module Respond

open Util
open Cmd

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
type ('msg,'a) Respond =
    'a -> Cmd<'msg>


(*| *)
(* append : Respond<'msg,'a> -> Respond<'msg,'a> -> Respond<'msg,'a> *)
let append f g a =
    Cmd.batch [ f a ; g a ]


(*| *)
(* sum : List (Respond<'msg,'a>) -> Respond<'msg,'a> *)
let sum rs a =
    List.map (fun r -> r a) rs
  |> Cmd.batch


(*| *)
(* zero : Respond<'msg,'a> *)
let zero _ =
    Cmd.none


(*|
Add a function to the front
`b -> a >> a -> Cmd msg`
*)
(* comap : (b -> a) -> Respond msg a -> Respond msg b *)
let comap = (>>)
