
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

type t =
  { pattern : pclause ;
    guard : vmap -> fmap -> fsmap -> bool }

let match_atoms pa a =
  let open Atom in
  match pa, a with
  | PEq (px, py) , Eq (x, y) ->
     [ ( Variable.Map.empty
         |> Variable.Map.add px x
         |> Variable.Map.add py y ,
         Feature.Map.empty ,
         Feature.Map.empty ) ;

       ( Variable.Map.empty
         |> Variable.Map.add px y
         |> Variable.Map.add py x ,
         Feature.Map.empty ,
         Feature.Map.empty ) ]

  | PFeat (px, pf, py) , Feat (x, f, y) ->
     [ ( Variable.Map.empty
         |> Variable.Map.add px x
         |> Variable.Map.add py y ,
         Feature.Map.singleton pf f ,
         Feature.Map.empty ) ]

  | PAbs (px, pf) , Abs (x, f) ->
     [ ( Variable.Map.singleton px x ,
         Feature.Map.singleton pf f ,
         Feature.Map.empty ) ]

  | PFen (px, pf) , Fen (x, fs) ->
     [ ( Variable.Map.singleton px x ,
         Feature.Map.empty ,
         Feature.Map.singleton pf fs ) ]

  | PSim (px, pf, py) , Sim (x, fs, y) ->
     [ ( Variable.Map.empty
         |> Variable.Map.add px x
         |> Variable.Map.add py y ,
         Feature.Map.empty ,
         Feature.Map.singleton pf fs ) ;

       ( Variable.Map.empty
         |> Variable.Map.add px y
         |> Variable.Map.add py x ,
         Feature.Map.empty ,
         Feature.Map.singleton pf fs ) ]

  | _ -> []

let match_literals pl l =
  let open Literal in
  match pl, l with
  | PPos pa , Pos a -> match_atoms pa a
  | PNeg pa , Neg a -> match_atoms pa a
  | _ -> []

let rec match_literal_with_clause pl = function
  | [] -> []
  | l :: c ->
     (
       match_literals pl l
       |> List.map
            (fun (vmap, fmap, fsmap) ->
              (vmap, fmap, fsmap, c))
     )
     @ (
      match_literal_with_clause pl c
      |> List.map
           (fun (vmap, fmap, fsmap, c) ->
             (vmap, fmap, fsmap, l :: c))
    )

let rec match_aux vmap fmap fsmap p c =
  match p with
  | [] -> [ (vmap, fmap, fsmap, c) ]
  | pl :: p ->
     assert false

let match_all p c =
  match_aux Variable.Map.empty Feature.Map.empty Feature.Map.empty p c

let match_one p c =
  match match_all p c with
  | [] -> failwith "Pattern.match_one"
  | m :: _ -> m
