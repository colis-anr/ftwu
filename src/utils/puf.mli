(** {1 Persistent Union-Find}

   @see<https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf>  "A
   Persistent Union-Find Data Structure" by Sylvain Conchon and
   Jean-Christophe Filliatre *)

type t

val create : int -> t

val find : t -> int -> int

val union : t -> int -> int -> t
(** Computes the union of the two given integers and returns the
   corresponding Puf. *)

(** {2 Other endpoints} *)

val union_verbose : t -> int -> int -> (t * int * int)

val extend : t -> int -> t
