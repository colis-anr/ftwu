(* Valuated Persistent Union-Find *)

module ISet = Set.Make(struct type t = int let compare = compare end)

type cls = int

let cls_of_int x = x

type 'a t =
  (* FIXME: persistent Hashtable would probably be more efficient in
     both time and space. *)
  { puf : Puf.t ;
    vals : 'a Parray.t }

let eq_cls h x y =
  let a_x = Puf.find h.puf x in
  let a_y = Puf.find h.puf y in
  a_x = a_y

let comp_cls h x y =
  let a_x = Puf.find h.puf x in
  let a_y = Puf.find h.puf y in
  compare a_x a_y

let make size v =
  { puf = Puf.create size ;
    vals = Parray.make size v }

let init size f =
  { puf = Puf.create size ;
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

  (* We erase the value for the looser but return it so that the
     client module can handle the merge itself. *)
  let val_y = Parray.get heap.vals y in
  (* FIXME: would be nice to free the cell instead of filling it with
     something useless *)
  let vals = Parray.set heap.vals y (Obj.magic 0) in

  (* Return the new Vpufs and the value of the loser. *)
  { puf ; vals }, val_y

let extend h n v =
  let parray_extend a n v =
    let s = Parray.length a in
    Parray.append a (Parray.make (n - s) v)
  in
  { puf = Puf.extend h.puf n ;
    vals = parray_extend h.vals n v }
