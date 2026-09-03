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
    ( "inline function",
      "inline fun increment(value: i32) => i32 { return value + 1; }" );
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
      \  let apply: (i32) -> i32 = fn[scale](value) {\n\
      \    return value * scale;\n\
      \  };\n\
      \  return apply(3);\n\
       }" );
    ( "parenthesized function type",
      "fun takesLambda(f: (i32, i32) -> i32, x: u8) => i32 {\n\
      \  return f(10, 10) + (x as i32);\n\
       }" );
    ( "zero-argument function type",
      "fun invoke(f: () -> i32) => i32 { return f(); }" );
  ]

let invalid_programs =
  [
    ("broken argument list", "fun main( => i32 { return 1; }");
    ("missing semicolon", "fun main() => i32 { return 1 }");
    ("unclosed block", "fun main() => i32 { return 1;");
    ("invalid declaration", "fun main() => i32 { let = 1; return 0; }");
    ( "bracketed function type",
      "fun apply(f: [i32] -> i32) => i32 { return f(1); }" );
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

let test_inline_modifier _ =
  let source =
    "inline fun increment(value: i32) => i32 { return value + 1; }\n\
     fun main() => i32 { return increment(1); }"
  in
  match parse_exn source with
  | Prog ([ inline_function; main ], [], []) ->
      assert_bool "inline function should retain its modifier"
        inline_function.elt.inline;
      assert_bool "ordinary function should not be inline" (not main.elt.inline)
  | program ->
      assert_failure
        (Printf.sprintf "unexpected AST for inline modifier:\n%s"
           (show_prog program))

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

let returned_expression source =
  match parse_exn source with
  | Prog ([ function_ ], [], []) -> (
      match function_.elt.body with
      | [ { elt = Ret (Some expression); _ } ] -> expression.elt
      | _ -> assert_failure "expected a single return statement")
  | program ->
      assert_failure
        (Printf.sprintf "unexpected numeric literal program:\n%s"
           (show_prog program))

let test_numeric_separators _ =
  let integer =
    returned_expression "fun main() => i32 { return 1_000_000; }"
  in
  let two_digit_leading_group =
    returned_expression "fun main() => i32 { return 12_345; }"
  in
  let decimal =
    returned_expression "fun main() => f64 { return 1_000.25; }"
  in
  let scientific =
    returned_expression "fun main() => f64 { return 1_000e-3; }"
  in
  (match integer with
  | Int value -> assert_equal (Z.of_int 1_000_000) value
  | _ -> assert_failure "expected a separated integer literal");
  (match two_digit_leading_group with
  | Int value -> assert_equal (Z.of_int 12_345) value
  | _ -> assert_failure "expected a two-digit leading group");
  (match decimal with
  | Float value -> assert_equal 1000.25 value
  | _ -> assert_failure "expected a separated decimal literal");
  match scientific with
  | Float value -> assert_equal 1. value
  | _ -> assert_failure "expected a separated scientific literal"

let test_invalid_numeric_separators _ =
  [
    "_100";
    "100_";
    "1__000";
    "12_3_45";
    "1_00";
    "1234_567";
    "1_0000";
    "1_000.25_0";
    "1e1_000";
  ]
  |> List.iter (fun literal ->
         assert_parse_error
           (Printf.sprintf "fun main() => i32 { return %s; }" literal))

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
         "inline modifier" >:: test_inline_modifier;
         "operator precedence" >:: test_operator_precedence;
         "numeric separators" >:: test_numeric_separators;
         "invalid numeric separators" >:: test_invalid_numeric_separators;
         "errors include positions" >:: test_error_has_source_position;
       ]
