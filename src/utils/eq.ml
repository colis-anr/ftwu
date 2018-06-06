
module ISet = Set.Make(struct type t = int let compare = compare end)

type t = ISet.t Vpuf.t
(* a vpuf where the puf carries the equalities and the values the disequalities *)

let create size =
  Vpuf.make size ISet.empty

exception Unsat

let add_eq vpuf x y =
  let (vpuf, neq_y') = Vpuf.union vpuf x y in
  let x' = Vpuf.find vpuf x in
  let neq_x' = Vpuf.get vpuf x in
  let neq = ISet.union neq_y' neq_x' in
  ISet.iter
    (fun z ->
      if x' = Vpuf.find vpuf z then
        raise Unsat)
    neq;
  Vpuf.set vpuf x neq

let add_neq vpuf x y =
  if Vpuf.find vpuf x = Vpuf.find vpuf y then
    raise Unsat;
  let neq_x = Vpuf.get vpuf x in
  Vpuf.set vpuf x (ISet.add y neq_x)

module Class =
  struct
    type t = int (* a "set" of variables that are equal *)

    let of_int x = x
  end
