(* Persistent Union-Find

   from Jean-Christophe Filliatre *)

type 'a t = 'a data ref
and 'a data =
  | Array of 'a array
  | Diff of int * 'a * 'a t

let create n v =
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

let rec get t i = match !t with
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
