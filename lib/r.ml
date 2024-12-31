module Error = struct
  type t' = [ `General of string ]
  type t = [ t' | `Wrapped of string * t ]

  let is (expected : t') =
    let rec aux got =
      match (expected, got) with
      | _, `Wrapped (_, e) -> aux e
      | e1, (#t' as e2) -> e1 = e2
    in
    aux

  let wrap msg e = `Wrapped (msg, e)

  let rec to_string = function
    | `Wrapped (msg, e) -> Printf.sprintf "%s: %s" msg (to_string e)
    | `General msg -> msg

  let of_exn exn = `General (Printexc.to_string exn)
end

type 'a t = ('a, Error.t) result

let iter_list f =
  let rec loop = function
    | [] -> Ok ()
    | x :: xs -> ( match f x with Ok () -> loop xs | Error e -> Error e)
  in
  loop

let iteri_list f =
  let rec loop i = function
    | [] -> Ok ()
    | x :: xs -> (
        match f i x with Ok () -> loop (i + 1) xs | Error e -> Error e)
  in
  loop 0

let run_if_error f k =
  match k () with
  | Ok r -> Ok r
  | Error e ->
      f ();
      Error e

let wrap_error msg f =
  match f () with
  | Ok result -> Ok result
  | Error e -> Error (`Wrapped (msg, e))
