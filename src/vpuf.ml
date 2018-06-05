(* Persistent Union-Find with Values

   addapted from that of Jean-Christophe Filliatre and Sylvain Conchon
 *)

type 'a t =
  { puf : Puf.t ;
    values : 'a Parray.t }

let make size v =
  { puf = Puf.create size ;
    values = Parray.make size v }

let init size f =
  { puf = Puf.create size ;
    values = Parray.init size f }

let find heap x =
  Puf.find heap.puf x

let union heap x y =
  let (puf, x, y) = Puf.union_verbose heap.puf x y in
  let value_y = Parray.get heap.values y in
  let values = Parray.set heap.values y (Obj.magic 0) in
  ({ puf ; values }, value_y)

let get heap x =
  let a_x = Puf.find heap.puf x in
  Parray.get heap.values a_x

let set heap x v =
  let a_x = Puf.find heap.puf x in
  { heap with values = Parray.set heap.values a_x v }
