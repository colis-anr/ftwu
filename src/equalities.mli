
type t

val create : int -> t

exception Unsat

val eq : t -> int -> int -> t

val neq : t -> int -> int -> t
