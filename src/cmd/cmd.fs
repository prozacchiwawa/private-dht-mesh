module Cmd

type 'a Cmd =
  | And of Cmd<'a> list
  | Do of Q.Promise<'a,unit>

let batch l =
  And l

let batchUp f l =
  l |> List.map f |> And

let none = And []

let rec map f c =
  match c with
  | Do p -> Do (Q.map f p)
  | And l -> And (l |> List.map (map f))

let message m =
  Do (Q.value m)

let promise p =
  Do p

let rec flatten se =
  match se with
  | And l -> List.concat (List.map flatten l)
  | Do eff -> [eff]

let withEffects e =
  e
  |> flatten
  |> List.toArray
  |> Q.all
