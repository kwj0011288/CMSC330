val abs : int -> int
val rev_tup : 'a * 'b * 'c -> 'c * 'b * 'a
val is_even : int -> bool
val area : int * int -> int * int -> int

val fibonacci : int -> int
val pow : int -> int -> int
val log : int -> int -> int
val gcf : int -> int -> int

val reverse : 'a list -> 'a list
val zip : ('a * 'b) list -> ('c * 'd) list -> ('a * 'b * 'c * 'd) list
val merge : 'a list -> 'a list -> 'a list
val is_present: 'a list -> 'a -> bool 
val every_nth : int -> 'a list -> 'a list
val jumping_tuples : ('a * 'b) list -> ('c * 'a) list -> 'a list
val max_func_chain : 'a -> ('a -> 'a) list -> 'a

val is_there: 'a list -> 'a -> bool
val count_occ : 'a list -> 'a -> int
val uniq : 'a list -> 'a list
val every_xth : int -> 'a list -> 'a list
