
open Ftwu_common
open Literal
open Atom
open Pattern

let triv =
  let x = Variable.fresh () in
  let y = Variable.fresh () in
  { pattern = [ Pos (Eq (x, y)) ] ;
    guard = (fun map ->
      Variable.Map.find x map
        <> Variable.Map.find y map) }

let () =
  ()
