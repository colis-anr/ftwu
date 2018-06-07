(** This module is here to represent clauses in normal-form.

   It provides an abstract type for clauses. Any value of this type
   that has been created using this interface is guaranteed to be in
   normal-form.

   The article "Deciding the First-Order Theory of an Algebra of
   Feature Trees with Updates" (Nicolas Jeannerod, Ralf Treinen) then
   tells us that such a clause is satisfiable.

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

val neq : Variable.t -> Variable.t -> t -> t
(** [neq x y] returns the clause corresponding to [x = y and c]. *) 
