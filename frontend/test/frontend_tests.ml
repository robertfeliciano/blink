open OUnit2

let suite =
  "Frontend tests"
  >::: [
         Test_parsing.suite;
         Test_type_aliases.suite;
         Test_tuples.suite;
         Test_typing.suite;
         Test_desugaring.suite;
         Test_interfaces.suite;
         Test_generic_classes.suite;
         Test_generic_interfaces.suite;
       ]

let () = run_test_tt_main suite
