open OUnit2

let suite =
  "Frontend tests"
  >::: [
         Test_typing.suite; Test_parsing.suite; Test_desugaring.suite; E2e.suite;
       ]

let () = run_test_tt_main suite
