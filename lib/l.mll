{
open Util
exception Error of string
}

let indent = '\n' ' '*
let whitespace = [ ' ' '\t' ]
let digit = [ '0'-'9' ]
let name =
  [ ^ '!' ' ' '\t' '\n' '+' '-' '*' '/' '|' ',' '#' '"' '(' ')' '0'-'9' ]
  [ ^ '!' ' ' '\t' '\n' '+' '-' '*' '/' '|' ',' '#' '"' '(' ')' ]+

rule main = parse
| indent as s {
  Lexing.new_line lexbuf;
  P.SPACE (String.length s - 1)
}
| whitespace+ {
  main lexbuf
}
| "(*" {
  comment lexbuf;
  main lexbuf
}
| "//" {
  line_comment lexbuf;
  main lexbuf
}
| ('2' '0' digit digit as year) '-'
  (('0' digit | '1' ['0'-'2']) as month) '-'
  ((['0'-'2'] digit | '3' ['0' '1']) as day) {
  P.DATE (int_of_string year, int_of_string month, int_of_string day)
}
| digit [ '0'-'9' ',' '.' ]* as s {
  P.DECIMAL (
    match Decimal.of_string s with
    | Ok d -> d
    | Error _ -> raise Unreachable
  )
}
| '"' ([ ^ '"' ]* as s) '"' {
  P.STRING s
}
| '+' {
  P.PLUS
}
| '-' {
  P.MINUS
}
| '*' {
  P.STAR
}
| '/' {
  P.SLASH
}
| '|' {
  P.PIPE
}
| ',' {
  P.COMMA
}
| '(' {
  P.LPAREN
}
| ')' {
  P.RPAREN
}
| '#' (name as s) {
  P.TAG s
}
| name {
  match Lexing.lexeme lexbuf with
  | "import" -> P.K_IMPORT
  | "mod" -> P.K_MOD
  | "proc" -> P.K_PROC
  | id -> P.ID id
}
| eof {
  P.EOF
}
| _ as c {
  raise (Error ("unexpected char: " ^ (String.make 1 c)))
}

and comment = parse
| "(*" {
  comment lexbuf;
  comment lexbuf
}
| "*)" {
  ()
}
| _ {
  comment lexbuf
}

and line_comment = parse
| [ ^ '\n' ]* {
  ()
}
