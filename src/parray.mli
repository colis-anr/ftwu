(** {1 Persistent Arrays}

   @see<https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf>  "A
   Persistent Union-Find Data Structure" from Sylvain Conchon and
   Jean-Christophe Filliatre *)

type 'a t

val create : int -> 'a -> 'a t

val init : int -> (int -> 'a) -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> 'a t
