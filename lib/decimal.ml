type t = { neg : bool; pos_v : int; scale : int } [@@deriving make]

let neg { neg; _ } = neg
let pos_v { pos_v; _ } = pos_v
let scale { scale; _ } = scale

let pow a b =
  let rec aux res i = if i = 0 then res else aux (res * a) (i - 1) in
  aux 1 b

let pos_int_of_string_opt s =
  (* allow e.g., 01 *)
  let rec aux sum i =
    if i = String.length s then Some sum
    else if i = 0 && s.[0] = '-' then aux sum (i + 1)
    else
      let v = Char.code s.[i] - Char.code '0' in
      if 0 <= v && v <= 9 then aux ((sum * 10) + v) (i + 1) else None
  in
  aux 0 0

let of_string s =
  let neg = String.length s > 0 && s.[0] = '-' in
  match
    (if neg then String.sub s 1 (String.length s - 1) else s)
    |> String.split_on_char ',' |> String.concat "" |> String.split_on_char '.'
  with
  | [ x ] -> (
      match pos_int_of_string_opt x with
      | None -> Error (`General "Decimal.of_string: invalid input")
      | Some pos_v -> Ok { neg; pos_v; scale = 0 })
  | [ s1; s2 ] -> (
      match (pos_int_of_string_opt s1, pos_int_of_string_opt s2) with
      | None, _ | _, None -> Error (`General "Decimal.of_string: invalid input")
      | Some v1, Some v2 ->
          let scale = max 1 (String.length s2) in
          let pos_v = (v1 * pow 10 scale) + v2 in
          Ok { neg; pos_v; scale })
  | _ -> Error (`General "Decimal.of_string: invalid input")

let to_string { neg; pos_v; scale } =
  let s = string_of_int pos_v in
  let s =
    if scale = 0 then s
    else
      let l = String.length s - scale in
      if l <= 0 then "0." ^ String.init (-l) (Fun.const '0') ^ s
      else String.concat "." [ String.sub s 0 l; String.sub s l scale ]
  in
  if neg then "-" ^ s else s
