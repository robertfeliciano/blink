open OUnit2

type fixture = { name : string; expected_exit : int }

let fixtures =
  [
    { name = "global-string-aliases"; expected_exit = 42 };
    { name = "global-storage"; expected_exit = 42 };
    { name = "global-literals"; expected_exit = 42 };
    { name = "arithmetic"; expected_exit = 42 };
    { name = "function-call"; expected_exit = 42 };
    { name = "array-index"; expected_exit = 15 };
    { name = "object-field"; expected_exit = 44 };
    { name = "conditional"; expected_exit = 42 };
    { name = "literal-values"; expected_exit = 42 };
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
      Native_test_support.assert_success "LLVM verification"
        "llc --filetype=null new_output.ll -o /dev/null";
      (if fixture.name = "global-storage" then
         let ir = Core.In_channel.read_all "new_output.ll" in
         assert_bool "mutable binding uses internal global storage"
           (Core.String.is_substring ir
              ~substring:"@count = internal global i32 20"));
      (if fixture.name = "global-literals" then
         let ir = Core.In_channel.read_all "new_output.ll" in
         assert_bool "const binding uses LLVM constant storage"
           (Core.String.is_substring ir
              ~substring:"@signed = internal constant i128"));
      Native_test_support.compile_and_run ~expected_exit:fixture.expected_exit)

let suite =
  "Backend bridge and codegen"
  >::: List.map (fun fixture -> fixture.name >:: test_fixture fixture) fixtures

let () = run_test_tt_main suite
