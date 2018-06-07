
open Ftwu_common
open Ftwu_utils

type cls = Vpufs.Class.t

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
       such that [F] is included in [G]. *)
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
    infos : info Vpufs.t }

let empty =
  let total_size = 8 in
  { total_size ;
    size = 0 ;
    globals = Variable.Map.empty ;
    infos = Vpufs.make total_size empty_info }

let double_size (c : t) : t =
  (* return a copy of this structure of twice the total_size *)
  let total_size = c.total_size lsl 1 in
  { total_size ;
    size = c.size ;
    globals = c.globals ;
    infos = Vpufs.extend c.infos total_size empty_info }

exception Unsat

let fresh_class (c : t) : cls * t =
  (* finds a fresh class in the clause [c] *)
  let fresh = Vpufs.Class.of_int c.size in
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

let exists vs c =
  { c with
    globals = Variable.Map.filter (fun v _ -> not (List.mem v vs)) c.globals }

let exists_comp vs c =
  { c with
    globals = Variable.Map.filter (fun v _ -> List.mem v vs) c.globals }
