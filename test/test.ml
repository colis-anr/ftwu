
open Ftwu_common
open Literal
open Atom
open Pattern

let triv =
  let x = Variable.fresh () in
  let y = Variable.fresh () in
  { pattern = [ PPos (PEq (x, y)) ] ;
    guard = (fun vmap _fmap _fsmap ->
      Variable.Map.find x vmap
        <> Variable.Map.find y vmap) }

let () =
  ()
