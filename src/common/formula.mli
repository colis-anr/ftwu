
type t =
  | True
  | False
  | Atom of Atom.t
  | Or of t * t
  | And of t * t
  | Not of t
  | Implies of t * t
  | Equiv of t * t
  | Exists of Variable.t * t
  | Forall of Variable.t * t

val or_l : t list -> t
val and_l : t list -> t
val equiv_l : t list -> t

val exists_l : Variable.t list * t -> t
val forall_l : Variable.t list * t -> t
