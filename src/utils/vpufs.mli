(** {1 Valuated Persistent Union-Separate}

   A wrapper around Puf that allows for the abstract manipulation of
   arrays over classes of integers. *)


module Class : sig
  (** A module representing classes of integers. *)

  type t

  val of_int : int -> t
  (** [of_int n] returns the class associated to the integer [n]. *)
end

type 'a t

val make : int -> 'a -> 'a t

val get : 'a t -> Class.t -> 'a
(** [get h x] returns the value associated with the class [x]. *)

val set : 'a t -> Class.t -> 'a -> 'a t
(** [set h x v] returns a new structure where the class [x] is
   associated with the value [v]. *)

exception Unsat
(** Exception raised when it is not possible to have all the declared
   unions and separations. *)

val union : 'a t -> Class.t -> Class.t -> ('a t * 'a)
(** [union h x y] returns a new structure where the classes [x] and
   [y] have been merged. Since both these classes were associated with
   values and since the structure does not know how to merge them, the
   new unified class is associated to one of them and the other value
   is returned. It is up to the client module to do the merging and
   update the structure accordingly.

   @raise Unsat when the two classes are declared separate *)

val separate : 'a t -> Class.t -> Class.t -> 'a t
(** [separate h x y] returns a new structure where the classes [x] and
   [y] are declared separate. These classes cannot be unified
   afterwards.

   @raise Unsat when the two classes are the same *)
