(** This module is here to represent clauses in normal-form.

   It provides an abstract type for clauses. Any value of this type
   that has been created using this interface is guaranteed to be in
   normal-form. Such clauses in normal-form are always satisfiable.

   @see <https://hal.archives-ouvertes.fr/hal-01807474>, "Deciding the
   First-Order Theory of an Algebra of Feature Trees with Updates"
   (Nicolas Jeannerod, Ralf Treinen).

   Most of the functions in this interface raise the {!Unsat}
   exception when the modification that they are supposed to make
   would lead to an unsatisfiable clause. *)

type t
(** The abstract type of clauses in normal-form. *)

val empty : t
(** The empty clause (that is, "true"). *)

exception Unsat
(** The exception raised when a modification would lead to
   unsatisfiability of the clause. *)

open Ftwu_common

val exists : Variable.t list -> t -> t
(** [exists xs c] returns the clause corresponding to [exists xs. c]
   *)

val exists_comp : Variable.t list -> t -> t
(** [exists xs c] returns the clause corresponding to [exists Cxs. c]
   where [Cxs] represents the complement of [xs]. *)

val literal : Literal.t -> t -> t list
(** [literal l c] returns the clause corresponding to [l and c].

   @raise Unsat when [l and c] is unsatisfiable *)

(** {3 Lower-level constructors} *)

val atom : Atom.t -> t -> t list
(** [atom a c] returns the clause corresponding to [a and c].

   @raise Unsat when [a and c] is unsatisfiable *)

val natom : Atom.t -> t -> t list
(** [natom a c] returns the clause corresponding to [not a and c].

   @raise Unsat when [not a and c] is unsatisfiable *)

val eq : Variable.t -> Variable.t -> t -> t
(** [eq x y c] returns the clause corresponding to [x = y and c].

   @raise Unsat *)

val neq : Variable.t -> Variable.t -> t -> t
(** [neq x y c] returns the clause corresponding to [x != y and c].

   @raise Unsat *)

val feat : Variable.t -> Feature.t -> Variable.t -> t -> t
(** [feat x f y c] returns the clause corresponding to [x\[f\]y and c].

   @raise Unsat *)

val nfeat : Variable.t -> Feature.t -> Variable.t -> t -> t
(** [nfeat x f y c] returns the clause corresponding to [not x\[f\]y and c].

   @raise Unsat *)

val abs : Variable.t -> Feature.t -> t -> t
(** [abs x f c] returns the clause corresponding to [x\[f\]^ and c].

   @raise Unsat *)

val nabs : Variable.t -> Feature.t -> t -> t
(** [nabs x f c] returns the clause corresponding to [not x\[f\]^ and c].

   @raise Unsat *)

val fen : Variable.t -> Feature.Set.t -> t -> t
(** [fen x fs c] returns the clause corresponding to [x\[fs\] and c].

   @raise Unsat *)

val nfen : Variable.t -> Feature.Set.t -> t -> t
(** [nfen x fs c] returns the clause corresponding to [not x\[fs\] and
   c].

   @raise Unsat *)

val sim : Variable.t -> Feature.Set.t -> Variable.t -> t -> t
(** [sim x fs y] returns the clause corresponding to [x ~fs y and c].

   @raise Unsat *)

val nsim : Variable.t -> Feature.Set.t -> Variable.t -> t -> t
(** [sim x fs y] returns the clause corresponding to [not (x ~fs y) and c].

   @raise Unsat *)
