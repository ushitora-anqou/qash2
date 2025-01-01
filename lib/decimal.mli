type t

val make : neg:bool -> pos_v:int -> scale:int -> t
val neg : t -> bool
val pos_v : t -> int
val scale : t -> int
val of_string : string -> t R.t
val to_string : t -> string
