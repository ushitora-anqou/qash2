module Lexer : sig
  val clear : unit -> unit
  val main : int -> Lexing.lexbuf -> P.token
end

val string_of_token : P.token -> string
val parse_string : string -> Syntax.program R.t
val parse_in_channel : string -> In_channel.t -> Syntax.program R.t
val parse_expr : string -> Syntax.expr R.t
