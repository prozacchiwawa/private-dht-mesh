type ('msg, 'a) respond = 'a -> 'msg list
val append : ('a -> 'b list) -> ('a -> 'b list) -> 'a -> 'b list
val sum : ('a -> 'b list) list -> 'a -> 'b list
val zero : 'a -> 'b list
val comap : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
