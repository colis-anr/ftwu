
open Ftwu_common
open Literal
open Atom
open Pattern

let x = Variable.fresh ~hint:"x" ()
let y = Variable.fresh ~hint:"y" ()

let pat =
  { pattern = [ PPos (PEq (x, y)) ] ;
    guard = (fun vmap _fmap _fsmap ->
      Variable.Map.find x vmap
        <> Variable.Map.find y vmap) }

let a = Variable.fresh ~hint:"a" ()
let b = Variable.fresh ~hint:"b" ()
let c = Variable.fresh ~hint:"c" ()

let cl =
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

let r = Variable.fresh ~hint:"r" ()
let r' = Variable.fresh ~hint:"r'" ()

let () =
  let c =
    [ Pos (Feat (r, Feature.of_string "usr", x)) ;
      Pos (Sim (r, Feature.Set.singleton (Feature.of_string "share"), r')) ]
  in
  let c' =
    Rules.p_feat c
  in
  Misc.pp_clause Format.std_formatter c'
