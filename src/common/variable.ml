
type t =
  { hint : string ;
    id : int }

let fresh =
  let c = ref 0 in
  fun ?(hint="") () ->
  incr c;
  { hint = hint ; id = !c }

module Set =
  struct
    include Set.Make(struct type s = t type t = s let compare = compare end)
  end

module Map =
  struct
    include Map.Make(struct type s = t type t = s let compare = compare end)
  end
  
let hint v =
  if v.hint = "" then
    None
  else
    Some v.hint

let id v =
  v.id
