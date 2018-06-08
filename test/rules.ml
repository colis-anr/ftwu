
open Ftwu_common
open Pattern
module VMap = Variable.Map

let noguard _vmap _fmap _fsmap =  true

exception Unsat

let x = Variable.fresh ~hint:"x" ()
let y = Variable.fresh ~hint:"y" ()
let z = Variable.fresh ~hint:"z" ()

let f = Variable.fresh ~hint:"f" ()
let g = Variable.fresh ~hint:"g" ()

let fs = Variable.fresh ~hint:"fs" ()
let gs = Variable.fresh ~hint:"gs" ()

let clash_from_pattern p c =
  if match_exist p c then
    raise Unsat
  else
    ()

let c_cycle c =
  () (* FIXME *)

let c_feat_abs =
  (* x[f]y and x[f]^ *)
  clash_from_pattern
    { pattern = [ PPos (PFeat (x, f, y)) ; PPos (PAbs (x, f)) ] ;
      guard = noguard }

let c_feat_fen =
  clash_from_pattern
    { pattern = [ PPos (PFeat (x, f, y)) ; PPos (PFen (x, fs)) ] ;
      guard = (fun vmap fmap fsmap ->
        not (Feature.Set.mem (VMap.find f fmap) (VMap.find fs fsmap))) }

let c_neq_refl =
  clash_from_pattern
    { pattern = [ PNeg (PEq (x, x)) ] ;
      guard = noguard }

let c_nsim_refl =
  clash_from_pattern
    { pattern = [ PNeg (PSim (x, fs, x)) ] ;
      guard = noguard }
