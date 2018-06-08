
open Ftwu_common
open Pattern

let pp_variable fmt v =
  Format.fprintf fmt "%s%d"
    (match Variable.hint v with
     | None -> "?"
     | Some h -> h)
    (Variable.id v)

let pp_feature_set fmt fs =
  let first = ref true in
  Format.fprintf fmt "{";
  Feature.Set.iter
    (fun f ->
      if !first then
        first := false
      else
        Format.fprintf fmt ", ";
      Feature.pp_print fmt f)
    fs;
  Format.fprintf fmt "}"

let pp_vmap fmt vmap =
  Format.fprintf fmt "vmap:@.";
  Variable.Map.iter
    (fun px x ->
      Format.fprintf fmt "| %a -> %a@." pp_variable px pp_variable x)
    vmap

let pp_fmap fmt fmap =
  Format.fprintf fmt "fmap:@.";
  Feature.Map.iter
    (fun pf f ->
      Format.fprintf fmt "| %a -> %a@." Feature.pp_print pf Feature.pp_print f)
    fmap

let pp_fsmap fmt fsmap =
  Format.fprintf fmt "fsmap:@.";
  Feature.Map.iter
    (fun pf fs ->
      Format.fprintf fmt "| %a -> %a@." Feature.pp_print pf pp_feature_set fs)
    fsmap

let pp_atom fmt a =
  let open Atom in
  match a with
  | Eq (x, y) -> Format.fprintf fmt "%a=%a" pp_variable x pp_variable y
  | _ -> assert false
  
let pp_literal fmt l =
  let open Literal in
  match l with
  | Pos a -> pp_atom fmt a
  | Neg a -> Format.fprintf fmt "not (%a)" pp_atom a
  
let pp_clause fmt c =
  let first = ref true in
  Format.fprintf fmt "[";
  List.iter
    (fun l ->
      if !first then
        first := false
      else
        Format.fprintf fmt ", ";
      pp_literal fmt l)
    c;
  Format.fprintf fmt "]"
