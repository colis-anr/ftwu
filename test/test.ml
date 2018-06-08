
open Ftwu_common
open Literal
open Atom
open Pattern

let pat =
  let x = Variable.fresh ~hint:"x" () in
  let y = Variable.fresh ~hint:"y" () in
  { pattern = [ PPos (PEq (x, y)) ] ;
    guard = (fun vmap _fmap _fsmap ->
      Variable.Map.find x vmap
        <> Variable.Map.find y vmap) }

let cl =
  let a = Variable.fresh ~hint:"a" () in
  let b = Variable.fresh ~hint:"b" () in
  let c = Variable.fresh ~hint:"c" () in
  [ Pos (Eq (a, a)) ; Pos (Eq (b, a)) ; Pos (Eq (b, c)) ]

let () =
  match_all pat cl
  |> List.iteri
       (fun i (vmap, fmap, fsmap, c) ->
         Format.printf "Match %d:@." (i+1);
         Misc.pp_vmap Format.std_formatter vmap;
         Misc.pp_fmap Format.std_formatter fmap;
         Misc.pp_fsmap Format.std_formatter fsmap;
         Format.printf "Rest: ";
         Misc.pp_clause Format.std_formatter c;
         Format.printf ".@.")
