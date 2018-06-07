(* Valuated Persistent Union-Find-Separate *)

module ISet = Set.Make(struct type t = int let compare = compare end)

module Class =
  struct
    type t = int

    let of_int x = x
  end

type 'a t =
  (* FIXME: persistent Hashtable would probably be more efficient in
     both time and space. *)
  { puf : Puf.t ;
    sep : ISet.t Parray.t ;
    vals : 'a Parray.t }

let make size v =
  { puf = Puf.create size ;
    sep = Parray.make size ISet.empty ;
    vals = Parray.make size v }

let init size f =
  { puf = Puf.create size ;
    sep = Parray.make size ISet.empty ;
    vals = Parray.init size f }

let get heap x =
  let a = Puf.find heap.puf x in
  Parray.get heap.vals a

let set heap x v =
  let a = Puf.find heap.puf x in
  { heap with
    vals = Parray.set heap.vals a v }

exception Unsat

let union heap x y =
  let (puf, x, y) = Puf.union_verbose heap.puf x y in
  (* [x] is the winner, the new representant for both classes. [y] is
     the looser. *)

  (* We first compute the union of the separations of [x] and
     [y]. We optimize a bit by going straight for the ancestors. We
     check while doing that that [x] is not declared separate from
     itself. *)
  let sep_x = Parray.get heap.sep x in
  let sep_y = Parray.get heap.sep y in
  let sep_x =
    ISet.map
      (fun z ->
        let a = Puf.find puf z in
        if a = x then
          raise Unsat;
        a)
      (ISet.union sep_x sep_y)
  in
  (* FIXME: would be nice to free the cell instead of filling it with
     something useless *)
  let sep = Parray.set heap.sep y ISet.empty in
  let sep = Parray.set sep x sep_x in

  (* We then erase the value for the looser but return it so that the
     client module can handle the merge itself. *)
  let val_y = Parray.get heap.vals y in
  (* FIXME: would be nice to free the cell instead of filling it with
     something useless *)
  let vals = Parray.set heap.vals y (Obj.magic 0) in

  (* Return the new Vpufs and the value of the loser. *)
  { puf ; sep ; vals }, val_y

let separate h x y =
  (* Find the ancestors, check that they are not the same. *)
  let a_x = Puf.find h.puf x in
  let a_y = Puf.find h.puf y in
  if a_x = a_y then
    raise Unsat;

  (* Tell one of the ancestors that it is now separate from the
     other. FIXME: optim, tell only the one that has less? *)
  let sep_x = Parray.get h.sep a_x in
  let sep_x = ISet.add a_y sep_x in
  let sep = Parray.set h.sep a_x sep_x in

  { h with sep }

let extend h n v =
  let parray_extend a n v =
    let s = Parray.length a in
    Parray.append a (Parray.make (n - s) v)
  in
  { puf = Puf.extend h.puf n ;
    sep = parray_extend h.sep n ISet.empty ;
    vals = parray_extend h.vals n v }
