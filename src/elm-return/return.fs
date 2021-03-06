module Return

(*|
## Type
Modeling the `update` tuple as a Monad similar to `Writer`
@docs Return, ReturnF

## Mapping
@docs map, map2, map3, map4, map5, andMap, mapWith, mapCmd, mapBoth, dropCmd

## Piping
@docs piper, pipel, zero

## Basics
@docs singleton, andThen, (>>>), (<<<)

## Write `Cmd`s
@docs return, command, effect_

## Fancy non-sense
@docs sequence, flatten
*)

open Respond
open Util

(*| *)
type Return<'msg,'model> =
    ( 'model * 'msg list )


(*| *)
type ReturnF<'msg,'model> =
    Return<'msg,'model> -> Return<'msg,'model>


(*| *)
(* piper : List (ReturnF msg model) -> ReturnF msg model *)
let piper l =
    List.fold (<<) zero l


(*| *)
(* pipel : List (ReturnF msg model) -> ReturnF msg model *)
let pipel l v =
    List.foldBack (>>) l v


(*| *)
(* zero : ReturnF msg model *)
let zero a = a


(*|
Transform the `Model` the `Cmd` will be left untouched
*)
(* map : (a -> b) -> Return msg a -> Return msg b *)
let map f ( model, cmd ) =
    ( (f model), cmd )


(*|
Map an `Return` into a `Return` containing a `Model` function
*)
(* andMap : Return msg a -> Return msg (a -> b) -> Return msg b *)
let andMap ( model, cmd_ ) ( f, cmd ) =
    (f model , Seq.concat [ cmd ; cmd_ ] |> List.ofSeq )


(*|
Transform the `Model` of and add a new `Cmd` to the queue
*)
(* mapWith : (a -> b) -> Cmd msg -> Return msg a -> Return msg b *)
let mapWith f cmd (model, cmd_) =
    andMap (model, cmd_) (f, cmd)


(*|
Map over both the model and the msg type of the `Return`.
This is useful for easily embedding a `Return` in a Union Type.
For example

```elm
import Foo

type Msg = Foo Foo.Msg
type Model = FooModel Foo.Model

...

update : Msg -> Model -> Return Msg Model
update msg model =
   case msg of
     Foo foo -> Foo.update foo model.foo
           |> mapBoth Foo FooModel
```
*)
(* mapBoth : (a -> b) -> (c -> d) -> Return a c -> Return b d *)
let mapBoth f f_ ( model, cmd ) =
    ( f_ model, List.map f cmd )


(*|
Combine 2 `Return`s with a function

```elm
map2
  (\modelA modelB -> { modelA | foo = modelB.foo })
  retA
  retB
```
*)
(* map2 :
    (a -> b -> c)
    -> Return msg a
    -> Return msg b
    -> Return msg c *)
let map2 f ( x, cmd ) ( y, cmd_ ) =
    (f x y , [ cmd, cmd_ ])


(*| *)
(* map3 :
    (a -> b -> c -> d)
    -> Return msg a
    -> Return msg b
    -> Return msg c
    -> Return msg d *)
let map3 f ( x, cmd ) ( y, cmd_ ) ( z, cmd__ ) =
    (f x y z , [ cmd, cmd_, cmd__ ])


(*| *)
(* map4 :
    (a -> b -> c -> d -> e)
    -> Return msg a
    -> Return msg b
    -> Return msg c
    -> Return msg d
    -> Return msg e *)
let map4 f ( w, cmda ) ( x, cmdb ) ( y, cmdc ) ( z, cmdd ) =
    (f w x y z , [ cmda, cmdb, cmdc, cmdd ])


(*| *)
(* map5 :
    (a -> b -> c -> d -> e -> f)
    -> Return msg a
    -> Return msg b
    -> Return msg c
    -> Return msg d
    -> Return msg e
    -> Return msg f *)
let map5 f ( v, cmda ) ( w, cmdb ) ( x, cmdc ) ( y, cmdd ) ( z, cmde ) =
    (f v w x y z , [ cmda, cmdb, cmdc, cmdd, cmde ])


(*|
Create a `Return` from a given `Model`
*)
(* singleton : model -> Return msg model *)
let singleton a =
    (a,[])

(*|
```elm
-- arbitrary function to demonstrate
foo : Model -> Return Msg Model
foo ({bar} as model) =
  -- forking logic
  if bar < 10
  -- that side effects may be added
  then (model, getAjaxThing)
  -- that the model may be updated
  else ({model | bar = model.bar - 2 }, Cmd.none)
```

They are now chainable with `andThen`...

```elm
resulting : Return msg { model | bar : Int }
resulting =
  myReturn |> andThen foo
                          |> andThen foo
                          |> andThen foo
```

Here we changed up `foo` three times, but we can use any function of
type `(a -> Return msg b)`.

Commands will be accumulated automatically as is the case with all
functions in this library.
*)
(* andThen : (a -> Return msg b) -> Return msg a -> Return msg b *)
let andThen f ( model, cmd ) =
    let
        ( model_, cmd_ ) =
            f model
    in
        (model_ , (Seq.concat [ cmd ; cmd_ ] |> List.ofSeq))


(*|
Construct a new `Return` from parts
*)
(* return : model -> Cmd msg -> Return msg model *)
let ret a b = (a,b)


(*|

Go point free with `andThen` chaining. Looking at the example from `andThen`

```elm
resulting : Return msg { model | bar : Int }
resulting =
  myReturn |> andThen foo
                 |> andThen foo
                 |> andThen foo
```

this code roughly becomes:

```elm
doFoo3Times : { model | bar : Int } -> Return msg { model | bar : Int }
doFoo3Times =
  foo >>> foo >>> foo
```
*)
(* (<<<) : (b -> Return msg c) -> (a -> Return msg b) -> a -> Return msg c *)
let (<<<) f f_ model =
    andThen f (f_ model)


(*| *)
(* (>>>) : (a -> Return msg b) -> (b -> Return msg c) -> a -> Return msg c *)
let (>>>) a b =
    (<<<) b a

(* >> -- tuareg *)

(*|
Add a `Cmd` to a `Return`, the `Model` is uneffected
*)
(* command : Cmd msg -> ReturnF msg model *)
let command cmd ( model, cmd_ ) =
    (model , Seq.concat [ cmd ; cmd_ ] |> List.ofSeq )


(*|
Add a `Cmd` to a `Return` based on its `Model`, the `Model` will not be effected
*)
(* effect_ : Respond msg model -> ReturnF msg model *)
let effect_ f ( model, cmd ) =
    ( model, Seq.concat [ cmd ; f model ] |> List.ofSeq )


(*|
Map on the `Cmd`.
*)
(* mapCmd : (a -> b) -> Return a model -> Return b model *)
let mapCmd f ( model, cmd ) =
    ( model, List.map f cmd )


(*|
Drop the current `Cmd` and replace with an empty thunk
*)
(* dropCmd : ReturnF msg model *)
let dropCmd ret =
    ret |> fst |> singleton


(*| *)
(* sequence : List (Return msg model) -> Return msg (List model) *)
let sequence l =
    let
        f ( model, cmd ) ( models, cmds ) =
            ((model :: models) , Seq.concat [ cmd ; cmds ] |> List.ofSeq )
    in
        List.foldBack f l ( [], [] )


(*| *)
(* chain : (m -> model -> (model, Cmd msg)) -> List m -> Return msg model -> Return msg model *)
let chain u l r =
    List.fold (fun r e -> r |> andThen (u e)) r l

(*| *)
(* flatten : Return msg (Return msg model) -> Return msg model *)
let flatten r =
    andThen id r
