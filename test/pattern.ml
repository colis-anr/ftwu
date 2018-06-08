
open Ftwu_common
module VMap = Variable.Map

type patom =
  | PEq of Variable.t * Variable.t
  | PFeat of Variable.t * Variable.t * Variable.t
  | PAbs of Variable.t * Variable.t
  | PFen of Variable.t * Variable.t
  | PSim of Variable.t * Variable.t * Variable.t

type pliteral =
  | PPos of patom
  | PNeg of patom

type pclause = pliteral list

type vmap = Variable.t VMap.t
type fmap = Feature.t VMap.t
type fsmap = Feature.Set.t VMap.t

let add_fail k v m =
  (* FIXME: much better with VMap.update but >= OCaml 4.06 *)
  match VMap.find k m with
  | w when v = w ->
     m
  | _ ->
     failwith "add_fail"
  | exception Not_found ->
     VMap.add k v m

type t =
  { pattern : pclause ;
    guard : vmap -> fmap -> fsmap -> bool }

let match_atoms vmap fmap fsmap pa a =
  let open Atom in
  match pa, a with
  | PEq (px, py) , Eq (x, y) ->
     [ ( vmap |> add_fail px x |> add_fail py y ,
         fmap , fsmap ) ;

       ( vmap |> add_fail px y |> add_fail py x ,
         fmap , fsmap ) ]

  | PFeat (px, pf, py) , Feat (x, f, y) ->
     [ ( vmap |> add_fail px x |> add_fail py y ,
         fmap |> add_fail pf f ,
         fsmap ) ]

  | PAbs (px, pf) , Abs (x, f) ->
     [ ( vmap |> add_fail px x ,
         fmap |> add_fail pf f ,
         fsmap ) ]

  | PFen (px, pf) , Fen (x, fs) ->
     [ ( vmap |> add_fail px x ,
         fmap ,
         fsmap |> add_fail pf fs ) ]

  | PSim (px, pf, py) , Sim (x, fs, y) ->
     [ ( vmap |> add_fail px x |> add_fail py y ,
         fmap ,
         fsmap |> add_fail pf fs ) ;

       ( vmap |> add_fail px y |> add_fail py x ,
         fmap ,
         fsmap |> add_fail pf fs ) ]

  | _ -> []

let match_literals vmap fmap fsmap pl l =
  let open Literal in
  match pl, l with
  | PPos pa , Pos a -> match_atoms vmap fmap fsmap pa a
  | PNeg pa , Neg a -> match_atoms vmap fmap fsmap pa a
  | _ -> []

let rec match_literal_with_clause vmap fmap fsmap pl = function
  | [] -> []
  | l :: c ->
     (
       match_literals vmap fmap fsmap pl l
       |> List.map
            (fun (vmap, fmap, fsmap) ->
              (vmap, fmap, fsmap, c))
     )
     @ (
      match_literal_with_clause vmap fmap fsmap pl c
      |> List.map
           (fun (vmap, fmap, fsmap, c) ->
             (vmap, fmap, fsmap, l :: c))
    )

let rec match_aux vmap fmap fsmap p c =
  match p with
  | [] -> [ (vmap, fmap, fsmap, c) ]
  | pl :: p ->
     match_literal_with_clause vmap fmap fsmap pl c
     |> List.map
          (fun (vmap, fmap, fsmap, c) ->
            match_aux vmap fmap fsmap p c)
     |> List.flatten

let match_all p c =
  match_aux VMap.empty VMap.empty VMap.empty p.pattern c
  |> List.filter
       (fun (vmap, fmap, fsmap, _c) ->
         p.guard vmap fmap fsmap)

let match_one p c =
  match match_all p c with
  | [] -> failwith "Pattern.match_one"
  | m :: _ -> m

let match_exist p c =
  match match_all p c with
  | [] -> false
  | _ -> true
