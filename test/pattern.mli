
open Ftwu_common

type map = Variable.t Variable.Map.t
   
type t =
  { pattern : Clause.t ;
    guard : map -> bool }

val match_ : t -> Clause.t -> map * Clause.t
