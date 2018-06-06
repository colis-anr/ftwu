
type t = string

let of_string f = f

let to_string f = f
let pp_print ppf f =
  Format.pp_print_string ppf (to_string f)

module Set =
  struct
    include Set.Make(struct type s = t type t = s let compare = compare end)
  end

module Map =
  struct
    include Map.Make(struct type s = t type t = s let compare = compare end)
  end
