(** {1 Eq}

   This module is a wrapper around a persistent union-find that allows
   to manipulate set of equalities. It is mostly here to ensure
   readability. The use of the {!Class} module also provides some
   safety when manipulating classes of equal integers. *)

type t
(** The abstract type for a satisfiable set of equalities and
   disequalities. *)

val create : int -> t
(** [create n] returns an (empty) set of equalities talking about the
   integers in [\[0..n\[]. *)

exception Unsat
(** An exception raised when the set of equalities becomes
   unsatisfiable. *)

val add_eq : t -> int -> int -> t
(** [add_eq s x y] returns a set of equalities consisting of that of
   [s] plus ["x = y"].

   @raise Unsat when adding this equality makes the set
   unsatisfiable. *)

val add_neq : t -> int -> int -> t
(** [add_neq s x y] returns a set of equalities consisting of that of
   [s] plus ["x != y"].

   @raise Unsat when adding this disequality makes the set
   unsatisfiable. *)

module Class : sig
  type t
  (** The type of an equality class of integers.

     {b Warning}: the polymorphic comparison function DOES NOT WORK on
     these values. One reason is the fact that these equality class
     only make sense in a set of equalities. *)

  val of_int : int -> t
  (** Returns the equality class of the given integer. *)
end
