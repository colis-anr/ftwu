(* Persistent Union-Find

   from Jean-Christophe Filliatre and Sylvain Conchon
 *)

type t = {
    mutable fathers : int Parray.t ;
    ranks : int Parray.t ;
  }

let create size =
  { ranks = Parray.make size 0 ;
    fathers = Parray.init size (fun i -> i) }

let rec find_aux fathers x =
  let father_x = Parray.get fathers x in
  if father_x == x then
    fathers, x
  else
    let fathers, ancestor_x = find_aux fathers father_x in
    let fathers = Parray.set fathers x ancestor_x in
    fathers, ancestor_x

let find heap x =
  let fathers, ancestor_x = find_aux heap.fathers x in
  heap.fathers <- fathers;
  ancestor_x

let union_verbose heap x y =
  let ancestor_x = find heap x in
  let ancestor_y = find heap y in
  if ancestor_x = ancestor_y then
    (
      heap ,
      ancestor_x ,
      ancestor_y
    )
  else
    (
      let rank_x = Parray.get heap.ranks ancestor_x in
      let rank_y = Parray.get heap.ranks ancestor_y in
      if rank_x > rank_y then
        (
          { heap with
            fathers = Parray.set heap.fathers ancestor_y ancestor_x } ,
          ancestor_x ,
          ancestor_y
        )
      else if rank_x < rank_y then
        (
          { heap with
            fathers = Parray.set heap.fathers ancestor_x ancestor_y } ,
          ancestor_y ,
          ancestor_x
        )
      else
        (
          { ranks = Parray.set heap.ranks ancestor_x (rank_x + 1);
	    fathers = Parray.set heap.fathers ancestor_y ancestor_x } ,
          ancestor_x ,
          ancestor_y
        )
    )

let union heap x y =
  let (puf, _, _) = union_verbose heap x y in
  puf

let parray_extend_f a n f =
  let s = Parray.length a in
  Parray.init (n - s) (fun i -> f (s + i))
  |> Parray.append a

let parray_extend a n v =
  let s = Parray.length a in
  Parray.make (n - s) v
  |> Parray.append a

let extend h n =
  { ranks = parray_extend h.ranks n 0 ;
    fathers = parray_extend_f h.fathers n (fun i -> i) }
