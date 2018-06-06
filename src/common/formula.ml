
type t =
  | True
  | False
  | Atom of Atom.t
  | Or of t * t
  | And of t * t
  | Not of t
  | Implies of t * t
  | Equiv of t * t
  | Exists of Variable.t * t
  | Forall of Variable.t * t

let rec pairs_of_list pair = function
  | [] -> raise (Invalid_argument "pairs_of_list")
  | [e] -> e
  | h :: t -> pair h (pairs_of_list pair t)

let or_l = pairs_of_list (fun e f -> Or (e, f))
let and_l = pairs_of_list (fun e f -> And (e, f))
let equiv_l = pairs_of_list (fun e f -> Equiv (e, f))

let exists_l (xs, e) = List.fold_left (fun e x -> Exists (x, e)) e (List.rev xs)
let forall_l (xs, e) = List.fold_left (fun e x -> Forall (x, e)) e (List.rev xs)
