let test _ = ()
let () =
  let open OUnit2 in
  run_test_tt_main
    ("qash2"
    >::: [
           "test" >:: test;
         ])
