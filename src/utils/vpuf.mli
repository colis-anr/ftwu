(** {1 Valuated Persistent Union-Find}

   A wrapper around Puf that allows for the abstract manipulation of
   arrays over classes of integers. *)


type 'a t

type cls
   
val make : int -> 'a -> 'a t

val cls_of_int : int -> cls
(** [cls_of_int n] returns the class associated to the integer [n]. *)

val eq_cls : 'a t -> cls -> cls -> bool

val comp_cls : 'a t -> cls -> cls -> int
  
val get : 'a t -> cls -> 'a
(** [get h x] returns the value associated with the class [x]. *)

val set : 'a t -> cls -> 'a -> 'a t
(** [set h x v] returns a new structure where the class [x] is
   associated with the value [v]. *)

val union : 'a t -> cls -> cls -> ('a t * 'a)
(** [union h x y] returns a new structure where the classes [x] and
   [y] have been merged. Since both these classes were associated with
   values and since the structure does not know how to merge them, the
   new unified class is associated to one of them and the other value
   is returned. It is up to the client module to do the merging and
   update the structure accordingly. *)

val extend : 'a t -> int -> 'a -> 'a t
(** [extend h n] returns a copy of [h] allowed to talk about variables
   in [\[0..n-1\]]. *)

                                    
