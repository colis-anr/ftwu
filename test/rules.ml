
open Ftwu_common
open Atom
open Literal
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

let trans_from_pattern_and_rhs p rhs c =
  let (vmap, fmap, fsmap, rest) = match_one p c in
  (pclause_to_clause vmap fmap fsmap rhs) @ rest

(* Clash rules *)

let c_cycle c =
  () (* FIXME *)

let c_feat_abs =
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

(* Transformation rules for the positive case *)

let s_eq = ()

let s_feats = ()

let s_feats_glob = ()

let s_sims = ()

let p_feat =
  trans_from_pattern_and_rhs
    { pattern = [ PPos (PSim (x, fs, y)) ; PPos (PFeat (x, f, z)) ] ;
      guard = (fun vmap fmap fsmap ->
        not (Feature.Set.mem (VMap.find f fmap) (VMap.find fs fsmap))) }
    [ PPos (PSim (x, fs, y)) ; PPos (PFeat (x, f, z)) ; PPos (PFeat (y, f, z)) ]

let p_abs = ()

let p_fen = ()

let p_sim = ()

(* Transformation rules for the negative case *)

let r_neq = ()
let r_nfeat = ()
let r_nabs = ()
let r_nfen_fen = ()
let r_nsim_sim = ()
let r_nsim_fen = ()
let e_nfen = ()
let e_nsim = ()

(* Rules that have lower precedence *)

let p_nfen = ()
let p_nsim = ()
