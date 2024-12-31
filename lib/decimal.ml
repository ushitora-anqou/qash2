type t = { v : int; scale : int }

let pow a b =
  let rec aux res i = if i = 0 then res else aux (res * a) (i - 1) in
  aux 1 b

let of_string s =
  match
    s |> String.split_on_char ',' |> String.concat ""
    |> String.split_on_char '.'
  with
  | [ x ] -> Ok { v = int_of_string x; scale = 0 }
  | [ s1; s2 ] ->
      let v1 = int_of_string s1 in
      let v2 = int_of_string s2 in
      let scale =
        if v2 = 0 then 1 else (v2 |> float_of_int |> log10 |> int_of_float) + 1
      in
      Ok { v = (v1 * pow 10 scale) + v2; scale }
  | _ -> Error (`General "Decimal.of_string: invalid input")
