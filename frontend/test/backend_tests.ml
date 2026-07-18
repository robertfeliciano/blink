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
    ~prefix:("blink-backend-" ^ fixture.name ^ "-")
    test_context
    (fun () ->
      let command =
        Printf.sprintf "%s %s" (Filename.quote compiler)
          (Filename.quote fixture.name)
      in
      Native_test_support.assert_success_silently "backend AST compiler" command;
      Native_test_support.compile_and_run ~expected_exit:fixture.expected_exit)

let test_array_literal_uses_heap test_context =
  let compiler =
    Native_test_support.executable_path "backend_fixture_compiler.exe"
  in
  Native_test_support.in_temp_dir ~prefix:"blink-backend-array-heap-"
    test_context (fun () ->
      let command =
        Printf.sprintf "%s %s" (Filename.quote compiler)
          (Filename.quote "array-index")
      in
      Native_test_support.assert_success_silently "backend AST compiler" command;
      let ir = Core.In_channel.read_all "new_output.ll" in
      assert_bool "array literal should call malloc"
        (Core.String.is_substring ir ~substring:"array_lit = call ptr @malloc");
      assert_bool "array literal should not use alloca"
        (not (Core.String.is_substring ir ~substring:"array_lit = alloca"));
      Native_test_support.compile_and_run ~expected_exit:15)

let test_invalid_ir_is_rejected test_context =
  let compiler =
    Native_test_support.executable_path "backend_fixture_compiler.exe"
  in
  Native_test_support.in_temp_dir ~prefix:"blink-backend-invalid-ir-"
    test_context (fun () ->
      let command =
        Printf.sprintf "%s %s > command.stdout 2> command.stderr"
          (Filename.quote compiler) (Filename.quote "invalid-ir")
      in
      match Core_unix.system command with
      | Error _ -> ()
      | Ok () -> assert_failure "invalid LLVM IR unexpectedly passed verification")

let suite =
  "Backend bridge and codegen"
  >::: (List.map (fun fixture -> fixture.name >:: test_fixture fixture) fixtures
       @ [
           "array literal uses heap" >:: test_array_literal_uses_heap;
           "invalid IR is rejected" >:: test_invalid_ir_is_rejected;
         ])

let () = run_test_tt_main suite
