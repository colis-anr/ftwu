(** {1 Persistent Union-Find}

   @see<https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf>  "A
   Persistent Union-Find Data Structure" by Sylvain Conchon and
   Jean-Christophe Filliatre *)

type t

val create : int -> t

val find : t -> int -> int

val union : t -> int -> int -> t
