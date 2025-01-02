open Util

let string_of_token = function
  | P.APP -> "APP"
  | BR -> "BR"
  | COMMA -> "COMMA"
  | DATE _ -> "DATE"
  | DECIMAL _ -> "DECIMAL"
  | DEDENT -> "DEDENT"
  | EOF -> "EOF"
  | FUN -> "FUN"
  | ID s -> "ID(" ^ s ^ ")"
  | IMPORT -> "IMPORT"
  | INDENT -> "INDENT"
  | LPAREN -> "LPAREN"
  | MINUS -> "MINUS"
  | MOD -> "MOD"
  | PLUS -> "PLUS"
  | PROC -> "PROC"
  | RARROW -> "RARROW"
  | RPAREN -> "RPAREN"
  | SLASH -> "SLASH"
  | SPACE _ -> "SPACE"
  | STAR -> "STAR"
  | STRING _ -> "STRING"
  | TAG _ -> "TAG"

(* Thanks to: https://zehnpaard.hatenablog.com/entry/2019/06/11/090829 *)
module Lexer = struct
  let current_indent = ref 0
  let queued_src_token = ref []

  let peek_src_token lex =
    match !queued_src_token with
    | x :: _ -> x
    | [] ->
        let token = L.main lex in
        queued_src_token := token :: !queued_src_token;
        token

  let next_src_token lex =
    let ret = peek_src_token lex in
    queued_src_token := List.tl !queued_src_token;
    ret

  let main' width lex () =
    let repeat n v = List.init n (fun _ -> v) |> List.concat in
    let token =
      (* Skip SPACE sequence *)
      let rec aux token =
        match (token, peek_src_token lex) with
        | P.SPACE _, SPACE _ -> aux (next_src_token lex)
        | _ -> token
      in
      aux (next_src_token lex)
    in
    (* Printf.eprintf "Lexer.main': eat %s\n" (string_of_token token); *)
    match token with
    | SPACE n ->
        (* Convert space(s) to indent(s) *)
        let cur = !current_indent in
        let next = n / width in
        current_indent := next;
        if next > cur then repeat (next - cur) [ P.INDENT ]
        else if next < cur then repeat (cur - next) [ P.DEDENT ]
        else [ P.BR ]
    | EOF -> repeat !current_indent [ P.DEDENT ] @ [ P.EOF ]
    | e -> [ e ]

  let cached_tokens = ref []

  let cache f =
    match !cached_tokens with
    | x :: xs ->
        cached_tokens := xs;
        x
    | [] -> (
        match f () with
        | x :: xs ->
            cached_tokens := xs;
            x
        | [] -> assert false)

  let clear () =
    current_indent := 0;
    queued_src_token := [];
    cached_tokens := []

  let main width lex = cache (main' width lex)
end

let parse_lex real_parse lex =
  let err msg =
    let pos = lex.Lexing.lex_start_p in
    let msg =
      Printf.sprintf "%s:%d:%d: %s" pos.pos_fname pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol)
        msg
    in
    Error (`General msg)
  in
  Lexer.clear ();
  try Ok (real_parse (Lexer.main 2) lex) with
  | L.Error s -> err s
  | P.Error -> err "syntax error"
  | _ -> raise Unreachable

let parse_string s = parse_lex P.program (Lexing.from_string s)

let parse_in_channel filename ic =
  let lex = Lexing.from_channel ic in
  Lexing.set_filename lex filename;
  parse_lex P.program lex

let parse_expr s = parse_lex P.expr (Lexing.from_string s)
