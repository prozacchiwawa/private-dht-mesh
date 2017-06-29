val _DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET : int
val _DEFAULT_NUMBER_OF_NODES_TO_PING : int
type ('id, 'a) storage =
    Self of 'a array
  | Split of (('id, 'a) kbucket * ('id, 'a) kbucket)
and ('id, 'a) kbucket = {
  localNodeId : 'id;
  dontSplit : bool;
  storage : ('id, 'a) storage;
  ver : int;
}
type ('id, 'a) kbucketAbstract = {
  distance : ('id, 'a) kbucketAbstract -> 'id -> 'id -> int array;
  nodeId : 'a -> 'id;
  arbiter : 'a -> 'a -> 'a;
  keyLength : 'id -> int;
  keyNth : int -> 'id -> int;
  idEqual : 'id -> 'id -> bool;
  idLess : 'id -> 'id -> bool;
}
type ('id, 'a) action = Ping of ('a array * 'a)
val optionDefault : 'a -> 'a option -> 'a
val arrayRemove : int -> int -> 'a array -> 'a array
val listHeadOption : 'a list -> 'a option
val ( &> ) : 'v -> ('id, 'a) action list -> 'v * ('id, 'a) action list
val ( &+ ) :
  'v * ('id, 'a) action list ->
  ('v -> 'v * ('id, 'a) action list) -> 'v * ('id, 'a) action list
val init : 'a -> ('a, 'b) kbucket
val indexOf :
  (string, 'a) kbucketAbstract -> string -> ('b, 'a) kbucket -> int
val defaultDistance : ('a, 'b) kbucketAbstract -> 'a -> 'a -> bytes
val determineBucket : ('a, 'b) kbucketAbstract -> 'a -> int option -> int
type ('id, 'a) internalOps = {
  update :
    ('id, 'a) kbucketAbstract ->
    ('id, 'a) kbucket ->
    'a -> int -> ('id, 'a) kbucket * ('id, 'a) action list;
  splitAndAddInternal :
    ('id, 'a) kbucketAbstract ->
    ('id, 'a) internalOps ->
    ('id, 'a) kbucket ->
    'a -> int option -> ('id, 'a) kbucket * ('id, 'a) action list;
  addInternal :
    ('id, 'a) kbucketAbstract ->
    ('id, 'a) internalOps ->
    ('id, 'a) kbucket ->
    'a -> int option -> ('id, 'a) kbucket * ('id, 'a) action list;
}
val addInternal :
  (string, 'a) kbucketAbstract ->
  (string, 'a) internalOps ->
  (string, 'a) kbucket ->
  'a -> int option -> (string, 'a) kbucket * (string, 'a) action list
val closest :
  ('id, 'a) kbucketAbstract ->
  ('id, 'a) kbucket -> 'id -> int -> int option -> 'a array
val count : ('id, 'a) kbucket -> int
val get :
  (string, 'a) kbucketAbstract ->
  (string, 'a) kbucket -> string -> int option -> 'a option
val remove :
  (string, 'a) kbucketAbstract ->
  (string, 'a) kbucket ->
  string -> int option -> (string, 'a) kbucket * ('b, 'c) action list
val splitAndAddInternal :
  (string, 'a) kbucketAbstract ->
  (string, 'a) internalOps ->
  (string, 'a) kbucket ->
  'a -> int option -> (string, 'a) kbucket * (string, 'a) action list
val toArray : ('id, 'a) kbucket -> 'a array
val update :
  ('id, 'a) kbucketAbstract ->
  ('id, 'a) kbucket -> 'a -> int -> ('id, 'a) kbucket * ('b, 'c) action list
val internalOps_ : (string, 'a) internalOps
val add :
  (string, 'a) kbucketAbstract ->
  (string, 'a) kbucket ->
  'a -> int option -> (string, 'a) kbucket * (string, 'a) action list
val splitAndAdd :
  (string, 'a) kbucketAbstract ->
  (string, 'a) kbucket ->
  'a -> int option -> (string, 'a) kbucket * (string, 'a) action list
