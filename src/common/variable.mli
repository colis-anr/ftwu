
type t

val fresh : ?hint:string -> unit -> t

module Set : (Set.S with type elt = t)

val hint : t -> string option

val id : t -> int
