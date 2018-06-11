
open Ftwu_common
open Ftwu_utils

type cls = Vpuf.cls

(* In all this file, we manipulate three objects that may be think of
   as "variables":

   - the variables coming from the outside. They are the variables
   defined in {!Variable}. They are only here for communication with
   the outside.

   - the actual variables. They are not the objects that we want to
   manipulate.

   - the classes of equal variables. This is what we want to
   manipulate. This is just a convenient tool provided by the module
   {!Vpufs}. *)

type info =
  (* All the "local" information that we keep on a class of equal
     variables. *)
  {
    (* Features. This is a map from features to variable options;
       [None] meaning an absence, and [Some y] meaning a feature
       constraint towards this class of equal variables [y]. *)
    feats : cls option Feature.Map.t ;

    (* Positive fences. We know that there can be at most one (because
       of S-Fens). *)
    fen : Feature.Set.t option ;

    (* Negative fences. We have to keep most of them. We still have
       that for all set [F] in [nfences], there is no [G] in [nfences]
       such that [F] is included in [G]. We also have that if this
       list is non-empty, then [fen] is [None]. *)
    nfens : Feature.Set.t list ;

    (* Positive similarities. We know that there can be only one for a
       pair of variables. We cannot use a map or something that would
       guarantee this unicity, because there is no comparison function
       for classes of equal variables.

       This similarity will be represented twice: in the information
       of the two classes of equal variables. *)
    sims : (Feature.Set.t * cls) list ;

    (* Negative similarities. *)
    nsims : (Feature.Set.t list * cls) list
  }

let empty_info =
  { feats = Feature.Map.empty ;
    fen = None ;
    nfens = [] ;
    sims = [] ;
    nsims = [] }

type t =
  { (* [total_size] is the number of available variables. It is always
       a power of 2. When no variable is available and a new variable
       is required, we copy the whole structure to a structure twice
       bigger. *)
    total_size : int ;

    (* [size] is the number of used variables. It is always smaller
       than [total_size]. Since variables start at [0], this is also
       the value of the first available variable. *)
    size : int ;

    (* [globals] is a map from external variables to internal classes
       of variables. The variables in the map are in fact the free
       variables of the constraint. Because the others internal
       classes of variables are not accessible from the map, there is
       no way from outside to manipulate them. *)
    globals : cls Variable.Map.t ;

    (* [infos] is a special structure provided by the [Vpufs] module
       that associates efficiently classes of variables to their
       information. All the infos are in the [info] type except for
       the equalities and disequalities that are encoded directly in
       the [Vpufs] structure. *)
    infos : info Vpuf.t }

let empty =
  let total_size = 8 in
  { total_size ;
    size = 0 ;
    globals = Variable.Map.empty ;
    infos = Vpuf.make total_size empty_info }

type disj = t list

let top = [empty]

let bottom = []

let sat d = d <> bottom

let double_size (c : t) : t =
  (* return a copy of this structure of twice the total_size *)
  let total_size = c.total_size lsl 1 in
  { total_size ;
    size = c.size ;
    globals = c.globals ;
    infos = Vpuf.extend c.infos total_size empty_info }

let fresh_class (c : t) : cls * t =
  (* finds a fresh class in the clause [c] *)
  let fresh = Vpuf.cls_of_int c.size in
  let size = c.size + 1 in
  let c =
    if size <= c.total_size then
      c
    else
      (* if no fresh class is available, we double the size and *pouf*
         here it is *)
      double_size c
  in
  (fresh, { c with size })

let class_from_variable (c : t) (v : Variable.t) : cls * t =
  (* [v] is a variable from the outside. We associate an internal
     class to it by first looking in [c.globals] if this association
     exists already or by finding a fresh class for it *)
  match Variable.Map.find v c.globals with
  | cls -> (cls, c)
  | exception Not_found ->
     let (cls, c) = fresh_class c in
     (cls, { c with globals = Variable.Map.add v cls c.globals })

let on_disj (f : t -> t) (d : disj) : disj =
  d |> List.map f

let on_disj_l (f : t -> disj) (d : disj) : disj =
  d |> List.map f |> List.flatten

exception Cycle

let check_cycles_from c x =
  let rec visit trace x =
    if List.mem x trace then
      raise Cycle
    else
      Feature.Map.iter
        (fun f -> function
          | None -> ()
          | Some y -> visit (x :: trace) y)
        (Vpuf.get c.infos x).feats
  in
  visit [] x

let has_cycles_from c x =
  try
    check_cycles_from c x;
    false
  with
    Cycle -> true

let exists vs c =
  { c with
    globals = Variable.Map.filter (fun v _ -> not (List.mem v vs)) c.globals }

let exists_d vs d =
  on_disj (exists vs) d

let exists_comp vs c =
  { c with
    globals = Variable.Map.filter (fun v _ -> List.mem v vs) c.globals }

let exists_comp_d vs d =
  on_disj (exists_comp vs) d

(* Internal atoms, no propagation. They work on classes of variables. *)

let feat_i_np (x:cls) f (y:cls) (c:t) : t =
  let info_x = Vpuf.get c.infos x in
  let info_x = { info_x with feats = Feature.Map.add f (Some y) info_x.feats } in
  { c with infos = Vpuf.set c.infos x info_x }

let abs_i_np (x:cls) f (c:t) : t =
  let info_x = Vpuf.get c.infos x in
  let info_x = { info_x with feats = Feature.Map.add f None info_x.feats } in
  { c with infos = Vpuf.set c.infos x info_x }

(* Internal atoms. These work on classes of variables. *)

let rec eq_i (x:cls) (y:cls) (c:t) : disj =
  (* We first declare the union to the [Vpuf]. This will return a new
     set of infos where we kept the informations of one of the classes
     and lost the other informations. These other informations are
     also returned, and this is up to us to decide what we will do
     with them. *)
  let (infos, info_y) = Vpuf.union c.infos x y in
  let c = { c with infos } in
  let d = [c] in
  (* And what we do is that we take all the lost infos and put them in
     the structure again. Since we use the _i functions, the
     disjunction is in normal form at every stage. We start with
     features and absences. *)
  let d =
    Feature.Map.fold
      (fun f z d ->
        match z with
        | None -> on_disj_l (abs_i x f) d
        | Some z -> on_disj_l (feat_i x f z) d)
      info_y.feats
      d
  in
  (* Now the (possibly not there) positive fence. *)
  let d =
    match info_y.fen with
    | None -> d
    | Some fs -> on_disj_l (fen_i x fs) d
  in
  (* The negative fences. *)
  let d =
    List.fold_left
      (fun d fs ->
        on_disj_l (nfen_i x fs) d)
      d
      info_y.nfens
  in
  (* The positive similarities. *)
  let d =
    List.fold_left
      (fun d (fs, z) ->
        on_disj_l (sim_i x fs z) d)
      d
      info_y.sims
  in
  (* The negative similarities. *)
  let d =
    List.fold_left
      (fun d (fsl, z) ->
        List.fold_left (fun d fs ->
            on_disj_l (nsim_i x fs z) d)
          d fsl)
      d
      info_y.nsims
  in
  (* And we return. All these steps may have triggered a lot of
     rules. *)
  d

and feat_i (x:cls) f (y:cls) (c:t) : disj =
  let info_x = Vpuf.get c.infos x in
  match Feature.Map.find f info_x.feats with
  | None ->
     (* There is already an absence x[f]^. This triggers the clash
        C-Feat-Abs. *)
     bottom
  | Some z ->
     (* There is already a feature x[f]z. This triggers the rule
        S-Feats. *)
     eq_i y z c
  | exception Not_found ->
     (* There is nothing at the feature [f]. We just need to add the
        feature and propagate it. We still have to be careful and
        check that it does not clash with a fence or that it does not
        create a cycle. *)
     (
       match info_x.fen with
       | Some fs when not (Feature.Set.mem f fs) ->
          bottom
       | _ ->
          (* Add and propagate. We know that [x]'s brothers do not
             have the feature [f], nor an absence, nor a fence that
             prevents it, because we are in normal form, and such a
             case would have triggered a clash check on our side. *)
          let c = feat_i_np x f y c in
          let c =
            List.fold_left
              (fun c (fs, z) ->
                if Feature.Set.mem f fs
                then c
                else feat_i_np z f y c)
              c
              info_x.sims
          in
          (* Check for cycles and return. We only need to check them
             from [x] and [x]'s brothers because this is where we
             added the only features. *)
          try
            check_cycles_from c x;
            List.iter
              (fun (_, z) ->
                check_cycles_from c z)
              info_x.sims;
            [c]
          with
            Cycle -> bottom
     )

and abs_i (x:cls) f (c:t) : disj =
  let info_x = Vpuf.get c.infos x in
  match Feature.Map.find f info_x.feats with
  | None ->
     (* There is already an absence x[f]^. Then we added no
        information, we can just return directly. *)
     [c]
  | Some z ->
     (* There is already a feature x[f]z. This triggers the clash
        C-Feat-Abs. *)
     bottom
  | exception Not_found ->
     (* There is nothing at the feature f. We just need to add the
        absence and propagate it. We know that [x]'s brothers do not
        have the feature f because we are in normal form and such a
        case would have triggered a clash check on our side. *)
     let c = abs_i_np x f c in
     let c =
       List.fold_left
         (fun c (fs, z) ->
           if Feature.Set.mem f fs
           then c
           else abs_i_np z f c)
         c
         info_x.sims
     in
     [c]

and fen_i (x:cls) fs (c:t) : disj =
  assert false

and nfen_i (x:cls) fs (c:t) : disj =
  assert false

and sim_i (x:cls) fs (y:cls) (c:t) : disj =
  assert false

and nsim_i (x:cls) fs (y:cls) (c:t) : disj =
  (* C-NSim-Refl *)
  if Vpuf.eq_cls c.infos x y then
    bottom
  else
    assert false

let neq_i (x:cls) (y:cls) (c:t) : disj =
  if Vpuf.eq_cls c.infos x y then
    bottom
  else
    assert false

let nfeat_i (x:cls) f (y:cls) (c:t) : disj =
  assert false

let nabs_i (x:cls) f (c:t) : disj =
  (* R-NAbs *)
  let (z, c) = fresh_class c in
  feat_i x f z c

(* External atoms. These are just wrappers around internal atoms and
   [class_from_variable]. *)

let eq x y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  eq_i x y c

let neq x y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  neq_i x y c

let feat x f y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  feat_i x f y c

let nfeat x f y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  nfeat_i x f y c

let abs x f c =
  let (x, c) = class_from_variable c x in
  abs_i x f c

let nabs x f c =
  let (x, c) = class_from_variable c x in
  nabs_i x f c

let fen x fs c =
  let (x, c) = class_from_variable c x in
  fen_i x fs c

let nfen x fs c =
  let (x, c) = class_from_variable c x in
  nfen_i x fs c

let sim x fs y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  sim_i x fs y c

let nsim x fs y c =
  let (x, c) = class_from_variable c x in
  let (y, c) = class_from_variable c y in
  nsim_i x fs y c

(* Higher-level endpoints. *)

let atom (a : Atom.t) (c : t) : disj =
  let open Atom in
  match a with
  | Eq (x, y) -> eq x y c
  | Feat (x, f, y) -> feat x f y c
  | Abs (x, f) -> abs x f c
  | Fen (x, fs) -> fen x fs c
  | Sim (x, fs, y) -> sim x fs y c

let natom (a : Atom.t) (c : t) : disj =
  let open Atom in
  match a with
  | Eq (x, y) -> neq x y c
  | Feat (x, f, y) -> nfeat x f y c
  | Abs (x, f) -> nabs x f c
  | Fen (x, fs) -> nfen x fs c
  | Sim (x, fs, y) -> nsim x fs y c

let literal (l : Literal.t) (c : t) : disj =
  let open Literal in
  match l with
  | Pos a -> atom a c
  | Neg a -> natom a c

let atom_d a d =
  on_disj_l (atom a) d

let natom_d a d =
  on_disj_l (natom a) d

let literal_d l d =
  on_disj_l (literal l) d
