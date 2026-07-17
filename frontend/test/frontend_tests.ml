open OUnit2

let suite =
  "Frontend tests"
  >::: [ Test_parsing.suite; Test_typing.suite; Test_desugaring.suite ]

let () = run_test_tt_main suite
