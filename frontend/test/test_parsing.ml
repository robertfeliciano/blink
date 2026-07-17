open OUnit2
open Ast

let parse source = Parsing.Parse.parse_prog (Lexing.from_string source)

let parse_exn source =
  match parse source with
  | Ok program -> program
  | Error error ->
      assert_failure
        (Printf.sprintf "expected parsing to succeed:\n%s"
           (Core.Error.to_string_hum error))

let assert_parse_error source =
  match parse source with
  | Error _ -> ()
  | Ok program ->
      assert_failure
        (Printf.sprintf "expected parsing to fail, got:\n%s" (show_prog program))

let valid_programs =
  [
    ("minimal function", "fun main() => i32 { return 1; }");
    ("comments", "// before\nfun main() => i32 { /* inside */ return 0; }");
    ( "prototype",
      "@C fun puts(s: string) => i32;\nfun main() => i32 { return 0; }" );
    ( "class and method",
      "class Point {\n  let x: i32 = 0;\n  fun get() => i32 { return x; }\n}" );
    ( "control flow",
      "fun main() => i32 {\n\
      \  let total = 0;\n\
      \  while total < 3 { total += 1; }\n\
      \  for i in 0..3 { total += i; }\n\
      \  return total;\n\
       }" );
    ( "arrays and indexing",
      "fun main() => i32 { let xs = [1, 2, 3]; return xs[1]; }" );
    ( "capturing lambda",
      "fun main() => i32 {\n\
      \  let scale = 4;\n\
      \  let apply: [i32] -> i32 = fn[scale](value) {\n\
      \    return value * scale;\n\
      \  };\n\
      \  return apply(3);\n\
       }" );
  ]

let invalid_programs =
  [
    ("broken argument list", "fun main( => i32 { return 1; }");
    ("missing semicolon", "fun main() => i32 { return 1 }");
    ("unclosed block", "fun main() => i32 { return 1;");
    ("invalid declaration", "fun main() => i32 { let = 1; return 0; }");
  ]

let test_valid_program source _ = ignore (parse_exn source)
let test_invalid_program source _ = assert_parse_error source

let test_top_level_declarations _ =
  let source =
    "fun helper() => void;\n\
     class Box { let value: i32 = 0; }\n\
     fun main() => i32 { return 0; }"
  in
  match parse_exn source with
  | Prog (functions, classes, prototypes) ->
      assert_equal ~printer:string_of_int 1 (List.length functions);
      assert_equal ~printer:string_of_int 1 (List.length classes);
      assert_equal ~printer:string_of_int 1 (List.length prototypes)

let test_operator_precedence _ =
  match parse_exn "fun main() => i32 { return 2 + 3 * 4; }" with
  | Prog ([ function_ ], [], []) -> (
      match function_.elt.body with
      | [ { elt = Ret (Some expression); _ } ] -> (
          match expression.elt with
          | Bop
              ( Add,
                { elt = Int two; _ },
                {
                  elt = Bop (Mul, { elt = Int three; _ }, { elt = Int four; _ });
                  _;
                } ) ->
              assert_equal (Z.of_int 2) two;
              assert_equal (Z.of_int 3) three;
              assert_equal (Z.of_int 4) four
          | _ ->
              assert_failure "multiplication should bind tighter than addition")
      | _ -> assert_failure "expected a single return statement")
  | program ->
      assert_failure
        (Printf.sprintf "unexpected AST for precedence test:\n%s"
           (show_prog program))

let test_error_has_source_position _ =
  match parse "fun main() => i32 {\n  let = 1;\n}" with
  | Ok _ -> assert_failure "expected a parse error"
  | Error error ->
      let message = Core.Error.to_string_hum error in
      assert_bool
        (Printf.sprintf "expected source position in error, got: %s" message)
        (Core.String.is_substring message ~substring:"line 2")

let suite =
  let valid_tests =
    List.map
      (fun (name, source) -> name >:: test_valid_program source)
      valid_programs
  in
  let invalid_tests =
    List.map
      (fun (name, source) -> name >:: test_invalid_program source)
      invalid_programs
  in
  "Parsing"
  >::: [
         "valid programs" >::: valid_tests;
         "invalid programs" >::: invalid_tests;
         "top-level declarations" >:: test_top_level_declarations;
         "operator precedence" >:: test_operator_precedence;
         "errors include positions" >:: test_error_has_source_position;
       ]
