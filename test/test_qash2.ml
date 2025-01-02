open Qash2

let test_decimal _ =
  [
    ("123", Some (false, 123, 0, "123"));
    ("123.", Some (false, 1230, 1, "123.0"));
    ("123.0", Some (false, 1230, 1, "123.0"));
    ("123.1", Some (false, 1231, 1, "123.1"));
    ("123.00", Some (false, 12300, 2, "123.00"));
    ("123.01", Some (false, 12301, 2, "123.01"));
    ("123.10", Some (false, 12310, 2, "123.10"));
    ("123.11", Some (false, 12311, 2, "123.11"));
    ("0123", Some (false, 123, 0, "123"));
    ("0123.", Some (false, 1230, 1, "123.0"));
    ("0123.0", Some (false, 1230, 1, "123.0"));
    ("0123.1", Some (false, 1231, 1, "123.1"));
    ("0123.00", Some (false, 12300, 2, "123.00"));
    ("0123.01", Some (false, 12301, 2, "123.01"));
    ("0123.10", Some (false, 12310, 2, "123.10"));
    ("0123.11", Some (false, 12311, 2, "123.11"));
    ("1,23", Some (false, 123, 0, "123"));
    ("1,23.", Some (false, 1230, 1, "123.0"));
    ("1,23.0", Some (false, 1230, 1, "123.0"));
    ("1,23.1", Some (false, 1231, 1, "123.1"));
    ("1,23.0,0", Some (false, 12300, 2, "123.00"));
    ("1,23.0,1", Some (false, 12301, 2, "123.01"));
    ("1,23.1,0", Some (false, 12310, 2, "123.10"));
    ("1,23.1,1", Some (false, 12311, 2, "123.11"));
    ("0,123", Some (false, 123, 0, "123"));
    ("0,123.", Some (false, 1230, 1, "123.0"));
    ("0,123.0", Some (false, 1230, 1, "123.0"));
    ("0,123.1", Some (false, 1231, 1, "123.1"));
    ("0,123.0,0", Some (false, 12300, 2, "123.00"));
    ("0,123.0,1", Some (false, 12301, 2, "123.01"));
    ("0,123.1,0", Some (false, 12310, 2, "123.10"));
    ("0,123.1,1", Some (false, 12311, 2, "123.11"));
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
    ("0.1", Some (false, 1, 1, "0.1"));
    ("0.10", Some (false, 10, 2, "0.10"));
    ("0.01", Some (false, 1, 2, "0.01"));
    ("0.11", Some (false, 11, 2, "0.11"));
    ("00.1", Some (false, 1, 1, "0.1"));
    ("00.10", Some (false, 10, 2, "0.10"));
    ("00.01", Some (false, 1, 2, "0.01"));
    ("00.11", Some (false, 11, 2, "0.11"));
    (".1", Some (false, 1, 1, "0.1"));
    (".01", Some (false, 1, 2, "0.01"));
    (".10", Some (false, 10, 2, "0.10"));
    (".11", Some (false, 11, 2, "0.11"));
    ("-123", Some (true, 123, 0, "-123"));
    ("-123.", Some (true, 1230, 1, "-123.0"));
    ("-123.0", Some (true, 1230, 1, "-123.0"));
    ("-123.1", Some (true, 1231, 1, "-123.1"));
    ("-123.00", Some (true, 12300, 2, "-123.00"));
    ("-123.01", Some (true, 12301, 2, "-123.01"));
    ("-123.10", Some (true, 12310, 2, "-123.10"));
    ("-123.11", Some (true, 12311, 2, "-123.11"));
    ("-0123", Some (true, 123, 0, "-123"));
    ("-0123.", Some (true, 1230, 1, "-123.0"));
    ("-0123.0", Some (true, 1230, 1, "-123.0"));
    ("-0123.1", Some (true, 1231, 1, "-123.1"));
    ("-0123.00", Some (true, 12300, 2, "-123.00"));
    ("-0123.01", Some (true, 12301, 2, "-123.01"));
    ("-0123.10", Some (true, 12310, 2, "-123.10"));
    ("-0123.11", Some (true, 12311, 2, "-123.11"));
    ("-0.1", Some (true, 1, 1, "-0.1"));
    ("-0.10", Some (true, 10, 2, "-0.10"));
    ("-0.01", Some (true, 1, 2, "-0.01"));
    ("-0.11", Some (true, 11, 2, "-0.11"));
    ("-00.1", Some (true, 1, 1, "-0.1"));
    ("-00.10", Some (true, 10, 2, "-0.10"));
    ("-00.01", Some (true, 1, 2, "-0.01"));
    ("-00.11", Some (true, 11, 2, "-0.11"));
  ]
  |> List.iter (fun (input, expected) ->
         let open Decimal in
         let d = of_string input in
         match expected with
         | None -> assert (Result.is_error d)
         | Some (expected_neg, expected_v, expected_scale, expected_string) ->
             assert (Result.is_ok d);
             let d = Result.get_ok d in
             assert (neg d = expected_neg);
             assert (pos_v d = expected_v);
             assert (scale d = expected_scale);
             assert (to_string d = expected_string);
             ())

let test_lexer _ =
  let test input expected =
    let lex = Lexing.from_string input in
    Parser.Lexer.clear ();
    expected
    |> List.iter (fun e ->
           let token = Parser.Lexer.main 2 lex in
           (*Printf.eprintf ">>> %s %s\n" (Parser.string_of_token e)
             (Parser.string_of_token token);*)
           assert (token = e));
    Parser.Lexer.clear ()
  in
  test "2025-01-01" [ P.DATE (Syntax.make_date ~year:2025 ~month:1 ~day:1) ];
  test "2020-10-30" [ P.DATE (Syntax.make_date ~year:2020 ~month:10 ~day:30) ];
  test "2024-12-31" [ P.DATE (Syntax.make_date ~year:2024 ~month:12 ~day:31) ];
  test "2000-01-01" [ P.DATE (Syntax.make_date ~year:2000 ~month:1 ~day:1) ];
  test "1999-12-31"
    P.
      [
        DECIMAL (Decimal.make ~neg:false ~pos_v:1999 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~neg:false ~pos_v:12 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~neg:false ~pos_v:31 ~scale:0);
      ];
  test "2100-01-01"
    P.
      [
        DECIMAL (Decimal.make ~neg:false ~pos_v:2100 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
        MINUS;
        DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
      ];
  test "2000 -01-01"
    [
      DECIMAL (Decimal.make ~neg:false ~pos_v:2000 ~scale:0);
      MINUS;
      DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
      MINUS;
      DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
    ];
  test "あいう()" [ ID "あいう"; LPAREN; RPAREN ];
  test "あいう(え,お)" [ ID "あいう"; LPAREN; ID "え"; COMMA; ID "お"; RPAREN ];
  test "#あい" [ TAG "あい" ];
  test "fun a -> a" [ FUN; ID "a"; RARROW; ID "a" ];
  test "fun a b -> a + b" [ FUN; ID "a"; ID "b"; RARROW; ID "a"; PLUS; ID "b" ];
  test "fun a b -> a(1, 2) + b"
    [
      FUN;
      ID "a";
      ID "b";
      RARROW;
      ID "a";
      LPAREN;
      DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
      COMMA;
      DECIMAL (Decimal.make ~neg:false ~pos_v:2 ~scale:0);
      RPAREN;
      PLUS;
      ID "b";
    ];
  test
    {|
import "foo"
  * 2025-01-01 "はろー"
    わーるど 1+2
|}
    [
      BR;
      IMPORT;
      STRING "foo";
      INDENT;
      STAR;
      DATE { year = 2025; month = 1; day = 1 };
      STRING "はろー";
      INDENT;
      ID "わーるど";
      DECIMAL (Decimal.make ~neg:false ~pos_v:1 ~scale:0);
      PLUS;
      DECIMAL (Decimal.make ~neg:false ~pos_v:2 ~scale:0);
      DEDENT;
      DEDENT;
    ];
  ()

let test_parser _ =
  let one = Syntax.Decimal (Decimal.of_string "1" |> Result.get_ok) in
  let two = Syntax.Decimal (Decimal.of_string "2" |> Result.get_ok) in
  let three = Syntax.Decimal (Decimal.of_string "3" |> Result.get_ok) in

  let test_expr input expected =
    let got = Parser.parse_expr input |> Result.get_ok in
    assert (got = expected)
  in
  test_expr "1+2" Syntax.(Add (one, two));
  test_expr "1+2*3" Syntax.(Add (one, Multiply (two, three)));
  test_expr "(1+2)*3" Syntax.(Multiply (Add (one, two), three));
  test_expr "a 1 2 3" Syntax.(Apply (Apply (Apply (Var "a", one), two), three));
  test_expr "a 1,2 3"
    Syntax.(
      Apply
        ( Apply (Var "a", Decimal (Decimal.of_string "12" |> Result.get_ok)),
          three ));
  test_expr "fun a b -> a + b"
    Syntax.(Function ([ "a"; "b" ], Add (Var "a", Var "b")));
  test_expr "(fun a b -> a) + b"
    Syntax.(Add (Function ([ "a"; "b" ], Var "a"), Var "b"));
  test_expr "fun a b -> a 1 2 + b"
    Syntax.(
      Function ([ "a"; "b" ], Add (Apply (Apply (Var "a", one), two), Var "b")));
  test_expr "a b c" Syntax.(Apply (Apply (Var "a", Var "b"), Var "c"));
  test_expr "a b (c d)"
    Syntax.(Apply (Apply (Var "a", Var "b"), Apply (Var "c", Var "d")));

  let test_program input expected =
    let got =
      match Parser.parse_string input with
      | Ok x -> x
      | Error e -> failwith (R.Error.to_string e)
    in
    assert (got.decls = expected)
  in
  let test_tx =
    Syntax.
      {
        date = { year = 2025; month = 1; day = 1 };
        desc = "はろー";
        tags = [];
        postings = Some [ { account = "わーるど"; amount = Some (Add (one, two)) } ];
      }
  in
  test_program
    {|
* 2025-01-01 "はろー"
  わーるど 1+2

import "foo"

import "mf" "foo"

import "foo"
  * 2025-01-01 "はろー"
    わーるど 1+2

import "mf" "foo"
  * 2025-01-01 "はろー"
    わーるど 1+2
|}
    Syntax.
      [
        Transaction test_tx;
        Import { format = None; path = "foo"; overlays = [] };
        Import { format = Some "mf"; path = "foo"; overlays = [] };
        Import { format = None; path = "foo"; overlays = [ test_tx ] };
        Import { format = Some "mf"; path = "foo"; overlays = [ test_tx ] };
      ];
  ()

let () =
  let open OUnit2 in
  run_test_tt_main
    ("qash2"
    >::: [
           "decimal" >:: test_decimal;
           "lexer" >:: test_lexer;
           "parser" >:: test_parser;
         ])
