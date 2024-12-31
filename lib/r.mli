module Error : sig
  type t' = [ `General of string ]
  type t = [ t' | `Wrapped of string * t ]

  val is : t' -> t -> bool
  val wrap : string -> t -> t
  val to_string : t -> string
  val of_exn : exn -> t
end

type 'a t = ('a, Error.t) result

val iter_list : ('a -> unit t) -> 'a list -> unit t
val iteri_list : (int -> 'a -> unit t) -> 'a list -> unit t
val run_if_error : (unit -> unit) -> (unit -> 'a t) -> 'a t
val wrap_error : string -> (unit -> 'a t) -> 'a t
