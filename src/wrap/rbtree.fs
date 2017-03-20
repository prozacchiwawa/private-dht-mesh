module RBTree

open Fable.Core

open Util

[<Import("*","functional-red-black-tree")>]
module rbtree =
  type Tree<'key,'value> =
    abstract keys : 'key array
    abstract values : 'value array
    abstract length : int
    abstract insert : 'key -> 'value -> Tree<'key,'value>
    abstract remove : 'key -> Tree<'key,'value>
    abstract find : 'key -> TreeIter<'key,'value>
    abstract ge : 'key -> TreeIter<'key,'value>
    abstract gt : 'key -> TreeIter<'key,'value>
    abstract lt : 'key -> TreeIter<'key,'value>
    abstract le : 'key -> TreeIter<'key,'value>
    abstract key : 'key
    abstract value : 'value
    abstract root : Node<'key,'value>
  and TreeIter<'key,'value> =
    abstract node : Node<'key,'value> option
    abstract clone : unit -> TreeIter<'key,'value>
    abstract remove : unit -> Tree<'key,'value>
    abstract update : 'value -> Tree<'key,'value>
    abstract next : unit -> unit
    abstract prev : unit -> unit
    abstract hasNext : bool
    abstract hasPrev : bool
    abstract valid : bool
  and Node<'key,'value> =
    abstract key : 'key
    abstract value : 'value

[<Emit("(function() { var rbt = require('functional-red-black-tree'); return rbt(function(a,b) { return $0(a)(b); }); })()")>]
let createTree : ('key -> 'key -> int) -> rbtree.Tree<'key,'value> = fun c -> failwith "JS"

let rec iterSeq (i : rbtree.TreeIter<'key,'value>) : seq<('key * 'value)> =
  let j = i.clone () in
  let node = ref i.node in
  seq {
      while !node <> None do
        match !node with
        | Some n ->
           let _ =
             if j.hasNext then
               let _ = j.next () in
               node := j.node
             else
               node := None
           in
           yield (n.key, n.value)
        | None -> node := None
      done
    }

[<Emit("$0.begin")>]
let head : rbtree.Tree<'key,'value> -> rbtree.TreeIter<'key,'value> = fun t -> failwith "JS"
[<Emit("$0.end")>]
let tail : rbtree.Tree<'key,'value> -> rbtree.TreeIter<'key,'value> = fun t -> failwith "JS"

let contains (k : 'key) (t : rbtree.Tree<'key,'value>) : bool =
  (t.find k).valid
