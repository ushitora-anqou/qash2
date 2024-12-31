type t

val make : v:int -> scale:int -> t
val v : t -> int
val scale : t -> int
val of_string : string -> t R.t
val to_string : t -> string
