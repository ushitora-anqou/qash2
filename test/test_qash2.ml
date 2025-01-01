open Qash2

let test_decimal _ =
  [
    ("123", Some (123, 0, "123"));
    ("123.", Some (1230, 1, "123.0"));
    ("123.0", Some (1230, 1, "123.0"));
    ("123.1", Some (1231, 1, "123.1"));
    ("123.00", Some (12300, 2, "123.00"));
    ("123.01", Some (12301, 2, "123.01"));
    ("123.10", Some (12310, 2, "123.10"));
    ("123.11", Some (12311, 2, "123.11"));
    ("0123", Some (123, 0, "123"));
    ("0123.", Some (1230, 1, "123.0"));
    ("0123.0", Some (1230, 1, "123.0"));
    ("0123.1", Some (1231, 1, "123.1"));
    ("0123.00", Some (12300, 2, "123.00"));
    ("0123.01", Some (12301, 2, "123.01"));
    ("0123.10", Some (12310, 2, "123.10"));
    ("0123.11", Some (12311, 2, "123.11"));
    ("1,23", Some (123, 0, "123"));
    ("1,23.", Some (1230, 1, "123.0"));
    ("1,23.0", Some (1230, 1, "123.0"));
    ("1,23.1", Some (1231, 1, "123.1"));
    ("1,23.0,0", Some (12300, 2, "123.00"));
    ("1,23.0,1", Some (12301, 2, "123.01"));
    ("1,23.1,0", Some (12310, 2, "123.10"));
    ("1,23.1,1", Some (12311, 2, "123.11"));
    ("0,123", Some (123, 0, "123"));
    ("0,123.", Some (1230, 1, "123.0"));
    ("0,123.0", Some (1230, 1, "123.0"));
    ("0,123.1", Some (1231, 1, "123.1"));
    ("0,123.0,0", Some (12300, 2, "123.00"));
    ("0,123.0,1", Some (12301, 2, "123.01"));
    ("0,123.1,0", Some (12310, 2, "123.10"));
    ("0,123.1,1", Some (12311, 2, "123.11"));
    ("abc", None);
    ("12a", None);
    ("1a2", None);
    ("12.a", None);
    ("12.3a", None);
    ("12.a3", None);
    ("12.3a4", None);
    ("a.0", None);
    (".a", None);
    ("1.a", None);
    ("a.", None);
    ("12..34", None);
    ("0.1", Some (1, 1, "0.1"));
    ("0.10", Some (10, 2, "0.10"));
    ("0.01", Some (1, 2, "0.01"));
    ("0.11", Some (11, 2, "0.11"));
    ("00.1", Some (1, 1, "0.1"));
    ("00.10", Some (10, 2, "0.10"));
    ("00.01", Some (1, 2, "0.01"));
    ("00.11", Some (11, 2, "0.11"));
    (".1", Some (1, 1, "0.1"));
    (".01", Some (1, 2, "0.01"));
    (".10", Some (10, 2, "0.10"));
    (".11", Some (11, 2, "0.11"));
  ]
  |> List.iter (fun (input, expected) ->
         let open Decimal in
         let d = of_string input in
         match expected with
         | None -> assert (Result.is_error d)
         | Some (expected_v, expected_scale, expected_string) ->
             assert (Result.is_ok d);
             let d = Result.get_ok d in
             assert (v d = expected_v);
             assert (scale d = expected_scale);
             assert (to_string d = expected_string);
             ())

let string_of_token = function
  | P.BR -> "BR"
  | EOF -> "EOF"
  | COMMA -> "COMMA"
  | INDENT -> "INDENT"
  | DEDENT -> "DEDENT"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | PIPE -> "PIPE"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | K_MOD -> "K_MOD"
  | K_IMPORT -> "K_IMPORT"
  | K_PROC -> "K_PROC"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | SPACE _ -> "SPACE"
  | DECIMAL _ -> "DECIMAL"
  | DATE _ -> "DATE"
  | ID s -> "ID(" ^ s ^ ")"
  | STRING _ -> "STRING"
  | TAG _ -> "TAG"
[@@warning "-32"]

let test_lexer _ =
  let test input expected =
    let lex = Lexing.from_string input in
    expected
    |> List.iter (fun e ->
           let token = L.main lex in
           assert (token = e))
  in
  test "2025-01-01" [ P.DATE (2025, 1, 1) ];
  test "2020-10-30" [ P.DATE (2020, 10, 30) ];
  test "2024-12-31" [ P.DATE (2024, 12, 31) ];
  test "2000-01-01" [ P.DATE (2000, 1, 1) ];
  test "1999-12-31"
    P.
      [
        DECIMAL (Decimal.make ~v:1999 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~v:12 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~v:31 ~scale:0);
      ];
  test "2100-01-01"
    P.
      [
        DECIMAL (Decimal.make ~v:2100 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~v:1 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~v:1 ~scale:0);
      ];
  test "2000 -01-01"
    [
      DECIMAL (Decimal.make ~v:2000 ~scale:0);
      MINUS;
      DECIMAL (Decimal.make ~v:1 ~scale:0);
      MINUS;
      DECIMAL (Decimal.make ~v:1 ~scale:0);
    ];
  test "あいう()" [ ID "あいう"; LPAREN; RPAREN ];
  test "あいう(え,お)" [ ID "あいう"; LPAREN; ID "え"; COMMA; ID "お"; RPAREN ];
  test "#あい" [ TAG "あい" ];
  ()

let () =
  let open OUnit2 in
  run_test_tt_main
    ("qash2" >::: [ "decimal" >:: test_decimal; "lexer" >:: test_lexer ])
