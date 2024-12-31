type t = { v : int; scale : int } [@@deriving make]

let v { v; _ } = v
let scale { scale; _ } = scale

let pow a b =
  let rec aux res i = if i = 0 then res else aux (res * a) (i - 1) in
  aux 1 b

let loose_int_of_string_opt s =
  (* allow e.g., 01 *)
  let rec aux sum i =
    if i = String.length s then Some sum
    else
      let v = Char.code s.[i] - Char.code '0' in
      if 0 <= v && v <= 9 then aux ((sum * 10) + v) (i + 1) else None
  in
  aux 0 0

let of_string s =
  match
    s |> String.split_on_char ',' |> String.concat ""
    |> String.split_on_char '.'
  with
  | [ x ] -> (
      match loose_int_of_string_opt x with
      | None -> Error (`General "Decimal.of_string: invalid input")
      | Some v -> Ok { v; scale = 0 })
  | [ s1; s2 ] -> (
      match (loose_int_of_string_opt s1, loose_int_of_string_opt s2) with
      | None, _ | _, None -> Error (`General "Decimal.of_string: invalid input")
      | Some v1, Some v2 ->
          let scale = max 1 (String.length s2) in
          Ok { v = (v1 * pow 10 scale) + v2; scale })
  | _ -> Error (`General "Decimal.of_string: invalid input")

let to_string { v; scale } =
  let s = string_of_int v in
  if scale = 0 then s
  else
    let l = String.length s - scale in
    if l <= 0 then "0." ^ String.init (-l) (Fun.const '0') ^ s
    else String.concat "." [ String.sub s 0 l; String.sub s l scale ]
