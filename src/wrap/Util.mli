val dump : string -> 'a -> 'a
val log : string -> 'a -> 'a
val getenv : string -> string option
val toString : 'a -> string
val toLowerCase : string -> string
val toHex : int -> string
val parseInt : string -> int option
val parseHex : string -> int option
val zeropad : int -> string -> string
val charAt : string -> int -> char
val charCode : char -> int
val stringFromCharCode : int -> string
val stringFind : string -> string -> int
val stringSplit : string -> string -> string list
val stringTrim : string -> string
val substr : int -> int -> string -> string
val replaceall : Str.regexp -> string -> string -> string
val uncurriedFunction : 'a -> 'a
val toFloat : int -> float
val floor : float -> int
val rshift : int -> int -> int
val lshift : int -> int -> int
val random : float -> float
val randomInt : int -> int
val infinityInt : int
val optionElse : (unit -> 'a option) -> 'a option -> 'a option
val optionEither : 'a -> 'a -> 'b option -> 'a
val optionDefault : 'a -> 'a option -> 'a
val optionToList : 'a option -> 'a list
val singletonList : 'a -> 'a list
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val replicate : 'a list -> int -> 'a list
val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val curry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
val tupleSndWith : 'a -> 'b -> 'a * 'b
val tupleFstWith : 'a -> 'b -> 'b * 'a
val listCons : 'a -> 'a list -> 'a list
val skipWhile : ('a -> bool) -> 'a list -> 'a list
val truncate : int -> 'a list -> 'a list
val arraySortWith : ('a -> 'a -> int) -> 'a array -> 'a array
