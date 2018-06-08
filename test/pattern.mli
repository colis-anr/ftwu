
open Ftwu_common

type patom =
  | PEq of Variable.t * Variable.t
  | PFeat of Variable.t * Variable.t * Variable.t
  | PAbs of Variable.t * Variable.t
  | PFen of Variable.t * Variable.t
  | PSim of Variable.t * Variable.t * Variable.t

type pliteral =
  | PPos of patom
  | PNeg of patom

type pclause = pliteral list

type vmap = Variable.t Variable.Map.t
type fmap = Feature.t Variable.Map.t
type fsmap = Feature.Set.t Variable.Map.t

val patom_to_atom : vmap -> fmap -> fsmap -> patom -> Atom.t
val pliteral_to_literal : vmap -> fmap -> fsmap -> pliteral -> Literal.t
val pclause_to_clause : vmap -> fmap -> fsmap -> pclause -> Clause.t

type t =
  { pattern : pclause ;
    guard : vmap -> fmap -> fsmap -> bool }

val match_all : t -> Clause.t -> (vmap * fmap * fsmap * Clause.t) list

val match_one : t -> Clause.t -> (vmap * fmap * fsmap * Clause.t)
(** @raise (Failure "Pattern.match_one") when no match is found *)

val match_exist : t -> Clause.t -> bool
