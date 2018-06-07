
open Ftwu_common
open Ftwu_utils

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
    feats : Vpufs.Class.t option Feature.Map.t ;

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
    sims : (Feature.Set.t * Vpufs.Class.t) list ;

    (* Negative similarities. *)
    nsims : (Feature.Set.t list * Vpufs.Class.t) list
  }

let empty_info =
  { feats = Feature.Map.empty ;
    fen = None ;
    nfens = [] ;
    sims = [] ;
    nsims = [] }
