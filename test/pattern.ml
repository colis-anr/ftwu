
open Ftwu_common
module VMap = Variable.Map

type patom =
  | PEq of Variable.t * Variable.t
  | PFeat of Variable.t * Feature.t * Variable.t
  | PAbs of Variable.t * Feature.t
  | PFen of Variable.t * Feature.t
  | PSim of Variable.t * Feature.t * Variable.t

type pliteral =
  | PPos of patom
  | PNeg of patom

type pclause = pliteral list

type vmap = Variable.t Variable.Map.t
type fmap = Feature.t Feature.Map.t
type fsmap = Feature.Set.t Feature.Map.t

let vmap_add_fail k v m =
  match Variable.Map.find k m with
  | w when v = w ->
     m
  | _ ->
     failwith "vmap_add_or_fail"
  | exception Not_found ->
     Variable.Map.add k v m

let fmap_add_fail k v m =
  match Feature.Map.find k m with
  | w when v = w ->
     m
  | _ ->
     failwith "vmap_add_or_fail"
  | exception Not_found ->
     Feature.Map.add k v m

let fsmap_add_fail k v m =
  fmap_add_fail k v m

type t =
  { pattern : pclause ;
    guard : vmap -> fmap -> fsmap -> bool }

let match_atoms vmap fmap fsmap pa a =
  let open Atom in
  match pa, a with
  | PEq (px, py) , Eq (x, y) ->
     [ ( vmap |> vmap_add_fail px x |> vmap_add_fail py y ,
         fmap , fsmap ) ;

       ( vmap |> vmap_add_fail px y |> vmap_add_fail py x ,
         fmap , fsmap ) ]

  | PFeat (px, pf, py) , Feat (x, f, y) ->
     [ ( vmap |> vmap_add_fail px x |> vmap_add_fail py y ,
         fmap |> fmap_add_fail pf f ,
         fsmap ) ]

  | PAbs (px, pf) , Abs (x, f) ->
     [ ( vmap |> vmap_add_fail px x ,
         fmap |> fmap_add_fail pf f ,
         fsmap ) ]

  | PFen (px, pf) , Fen (x, fs) ->
     [ ( vmap |> vmap_add_fail px x ,
         fmap ,
         fsmap |> fsmap_add_fail pf fs ) ]

  | PSim (px, pf, py) , Sim (x, fs, y) ->
     [ ( vmap |> vmap_add_fail px x |> vmap_add_fail py y ,
         fmap ,
         fsmap |> fsmap_add_fail pf fs ) ;

       ( vmap |> vmap_add_fail px y |> vmap_add_fail py x ,
         fmap ,
         fsmap |> fsmap_add_fail pf fs ) ]

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
  match_aux Variable.Map.empty Feature.Map.empty Feature.Map.empty p.pattern c
  |> List.filter
       (fun (vmap, fmap, fsmap, _c) ->
         p.guard vmap fmap fsmap)

let match_one p c =
  match match_all p c with
  | [] -> failwith "Pattern.match_one"
  | m :: _ -> m
