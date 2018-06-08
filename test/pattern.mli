
open Ftwu_common

type patom =
  | PEq of Variable.t * Variable.t
  | PFeat of Variable.t * Feature.t * Variable.t
  | PAbs of Variable.t * Feature.t
  | PFen of Variable.t * Feature.t
  | PSim of Variable.t * Feature.t * Variable.t

type pliteral =
  | PPos of patom
  | PNeg of patom

type pclause = pliteral list

type vmap = Variable.t Variable.Map.t
type fmap = Feature.t Feature.Map.t
type fsmap = Feature.Set.t Feature.Map.t

type t =
  { pattern : pclause ;
    guard : vmap -> fmap -> fsmap -> bool }

val match_all : t -> Clause.t -> (vmap * fmap * fsmap * Clause.t) list

val match_one : t -> Clause.t -> (vmap * fmap * fsmap * Clause.t)
(** @raise (Failure "Pattern.match_one") when no match is found *)
