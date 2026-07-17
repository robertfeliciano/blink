open OUnit2

type fixture = { name : string; expected_exit : int }

let fixtures =
  [
    { name = "arithmetic"; expected_exit = 42 };
    { name = "function-call"; expected_exit = 42 };
    { name = "array-index"; expected_exit = 15 };
    { name = "object-field"; expected_exit = 44 };
  ]

let test_fixture fixture test_context =
  let compiler =
    Native_test_support.executable_path "backend_fixture_compiler.exe"
  in
  Native_test_support.in_temp_dir
    ~prefix:("blink-backend-" ^ fixture.name ^ "-") test_context (fun () ->
      let command =
        Printf.sprintf "%s %s" (Filename.quote compiler)
          (Filename.quote fixture.name)
      in
      Native_test_support.assert_success_silently "backend AST compiler" command;
      Native_test_support.compile_and_run ~expected_exit:fixture.expected_exit)

let suite =
  "Backend bridge and codegen"
  >::: List.map (fun fixture -> fixture.name >:: test_fixture fixture) fixtures

let () = run_test_tt_main suite
