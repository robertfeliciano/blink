open OUnit2

type fixture = { name : string; source : string; expected_exit : int }

let fixtures =
  [
    {
      name = "arithmetic";
      source = "fun main() => i32 { return 5 + 3 * 4; }";
      expected_exit = 17;
    };
    {
      name = "function-call";
      source =
        "fun twice(value: i32) => i32 { return value * 2; }\n\
         fun main() => i32 { return twice(9) + 1; }";
      expected_exit = 19;
    };
    {
      name = "control-flow";
      source =
        "fun main() => i32 {\n\
        \  let value = 0;\n\
        \  while value < 5 { value += 1; }\n\
        \  if value == 5 { return 23; } else { return 24; }\n\
         }";
      expected_exit = 23;
    };
    {
      name = "for-loop";
      source =
        "fun main() => i32 {\n\
        \  let total = 0;\n\
        \  for i in 0..5 { total += i; }\n\
        \  return total;\n\
         }";
      expected_exit = 10;
    };
    {
      name = "array-foreach";
      source =
        "fun main() => i32 {\n\
        \  let values = [1, 2, 3];\n\
        \  let total = 0;\n\
        \  for value in values { total += value; }\n\
        \  return total;\n\
         }";
      expected_exit = 6;
    };
    {
      name = "object-fields";
      source =
        "class Box {\n\
        \  let left: i32 = 0;\n\
        \  let right: i32 = 0;\n\
         }\n\
         fun main() => i32 {\n\
        \  let box = new Box { left = 20, right = 24 };\n\
        \  return box.left + box.right;\n\
         }";
      expected_exit = 44;
    };
    {
      name = "array-index";
      source =
        "fun main() => i32 {\n\
        \  let values = [4, 8, 15];\n\
        \  return values[2];\n\
         }";
      expected_exit = 15;
    };
    {
      name = "capturing-lambda";
      source =
        "fun main() => i32 {\n\
        \  let scale = 12;\n\
        \  let apply: [i32, i32] -> i32 = fn[scale](left, right) {\n\
        \    return left + right * scale;\n\
        \  };\n\
        \  let result = apply(3, 4);\n\
        \  free apply;\n\
        \  return result;\n\
         }";
      expected_exit = 51;
    };
  ]

let stage_failure fixture stage error =
  assert_failure
    (Printf.sprintf "%s failed during %s: %s" fixture.name stage
       (Core.Error.to_string_hum error))

let check_frontend_stages fixture =
  let ast =
    match Parsing.Parse.parse_prog (Lexing.from_string fixture.source) with
    | Ok ast -> ast
    | Error error -> stage_failure fixture "parsing" error
  in
  let typed =
    match Typing.Type.type_prog ast with
    | Ok typed -> typed
    | Error error -> stage_failure fixture "type checking" error
  in
  match Desugaring.Desugar.desugar_prog typed with
  | Ok _ -> ()
  | Error error -> stage_failure fixture "desugaring" error

let test_fixture fixture test_context =
  check_frontend_stages fixture;
  let compiler = Native_test_support.executable_path "../src/blink.exe" in
  Native_test_support.in_temp_dir ~prefix:("blink-" ^ fixture.name ^ "-")
    test_context (fun () ->
      Core.Out_channel.write_all "program.bl" ~data:fixture.source;
      let compile_command =
        Printf.sprintf "%s %s" (Filename.quote compiler)
          (Filename.quote "program.bl")
      in
      Native_test_support.assert_success_silently "Blink compiler"
        compile_command;
      Native_test_support.compile_and_run
        ~expected_exit:fixture.expected_exit)

let test_parse_failure_stops_at_parser _ =
  let source = "fun main( => i32 { return 1; }" in
  match Parsing.Parse.parse_prog (Lexing.from_string source) with
  | Error _ -> ()
  | Ok _ -> assert_failure "invalid syntax unexpectedly parsed"

let test_type_failure_stops_at_type_checker _ =
  let source = "fun main() => i32 { return true; }" in
  match Parsing.Parse.parse_prog (Lexing.from_string source) with
  | Error error ->
      assert_failure
        (Printf.sprintf "type-error fixture failed to parse: %s"
           (Core.Error.to_string_hum error))
  | Ok ast -> (
      match Typing.Type.type_prog ast with
      | Error _ -> ()
      | Ok _ -> assert_failure "invalid program unexpectedly type-checked")

let suite =
  let executable_tests =
    List.map (fun fixture -> fixture.name >:: test_fixture fixture) fixtures
  in
  "Compiler end-to-end"
  >::: [
         "stage failures"
         >::: [
                "parsing" >:: test_parse_failure_stops_at_parser;
                "type checking" >:: test_type_failure_stops_at_type_checker;
              ];
         "native execution" >::: executable_tests;
       ]

let () = run_test_tt_main suite
