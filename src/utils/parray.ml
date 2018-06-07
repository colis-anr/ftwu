(* Persistent Arrays

   addapted from that of Jean-Christophe Filliatre *)

type 'a t = 'a data ref
and 'a data =
  | Array of 'a array
  | Diff of int * 'a * 'a t

let make n v =
  ref (Array (Array.make n v))

let init n f =
  ref (Array (Array.init n f))

let rec rerootk t k =
  match !t with
  | Array _ -> k ()
  | Diff (i, v, t') ->
     rerootk t' (fun () ->
         (
           match !t' with
	   | Array a as n ->
	      let v' = a.(i) in
	      a.(i) <- v;
	      t := n;
	      t' := Diff (i, v', t)
	   | Diff _ -> assert false
         );
         k()
       )

let reroot t =
  rerootk t (fun () -> ())

let rec get t i =
  match !t with
  | Array a ->
     a.(i)
  | Diff _ ->
     reroot t;
     (
       match !t with
       | Array a -> a.(i)
       | Diff _ -> assert false
     )

let set t i v =
  reroot t;
  match !t with
  | Array a as n ->
     let old = a.(i) in
     if old == v then
       t
     else
       (
	 a.(i) <- v;
	 let res = ref n in
	 t := Diff (i, old, res);
	 res
       )
  | Diff _ ->
     assert false

let reroot_and_apply f t =
  reroot t;
  match !t with
  | Array a -> f a
  | Diff _ -> assert false

let unsafe_from_array a =
  ref (Array a)

let from_array a =
  unsafe_from_array (Array.copy a)
            
let length t = reroot_and_apply Array.length t

let append t1 t2 =
  reroot t1;
  reroot t2;
  match !t1, !t2 with
  | Array a1, Array a2 ->
     unsafe_from_array (Array.append a1 a2)
  | _ ->
     assert false
