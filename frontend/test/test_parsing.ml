open OUnit2
open Ast
open Util
open Parsing.Parse

let lexbuf_of_string s = Lexing.from_string s

let assert_ok_parse lexbuf =
  match parse_prog lexbuf with
  | Ok _ -> ()
  | Error e ->
      assert_failure
        (Printf.sprintf "Parse failed: %s" (Core.Error.to_string_hum e))

let test_parse_string_simple _ =
  let s = "fun main(argv: [string; 1]) => u64 { return 1; }" in
  assert_ok_parse (lexbuf_of_string s)

let test_parse_examples_super_simple _ =
  let ch = test_file_channel "super_simple.bl" in
  assert_ok_parse (lexbuf_of_string ch)

let test_parse_examples_simple _ =
  let ch = test_file_channel "simple.bl" in
  assert_ok_parse (lexbuf_of_string ch)

let test_parse_examples_sum _ =
  let ch = test_file_channel "sum.bl" in
  assert_ok_parse (lexbuf_of_string ch)

let test_show_manual_ast _ =
  (* manually construct a tiny program and ensure show_prog runs *)
  let fn =
    {
      elt =
        {
          frtyp = RetVal (TInt (TSigned Ti32));
          fname = "f";
          args = [ (TInt (TSigned Ti32), "x") ];
          body = [];
        };
      loc = Ast.Range.norange;
    }
  in
  let prog = Prog ([ fn ], []) in
  let _ = show_prog prog in
  ()

let test_parse_string_with_comment _ =
  let s = "fun main() => u64 { // comment\n return 0; }" in
  assert_ok_parse (lexbuf_of_string s)

let test_parse_examples_pemdas _ =
  let ch = test_file_channel "pemdas.bl" in
  assert_ok_parse (lexbuf_of_string ch)

let test_parse_examples_double_loop _ =
  let ch = test_file_channel "double_loop.bl" in
  assert_ok_parse (lexbuf_of_string ch)

let test_parse_invalid_syntax _ =
  let s = "fun main( => u64 { return 1; }" in
  match parse_prog (lexbuf_of_string s) with
  | Ok _ -> assert_failure "Expected parse error"
  | Error _ -> ()

let test_parse_object _ =
  let s =
    "class Point { \n\
    \    let x: i8 = 0;\n\
    \    let y: bool = false;\n\
     }\n\n\
     fun main(argv: [string; 2]) => i32 {\n\
    \    let p = new Point { x = 2, y = true};\n\
    \    let x = p.x + (9 as i8);\n\
    \    return (x + 14 as i8) as i32;\n\
    \    //let y = p.y;\n\
     }"
  in
  assert_ok_parse (lexbuf_of_string s)

let test_show_prog_complex _ =
  let fn1 =
    {
      elt = { frtyp = RetVoid; fname = "a"; args = []; body = [] };
      loc = Ast.Range.norange;
    }
  in
  let fn2 =
    {
      elt =
        {
          frtyp = RetVal (TInt (TSigned Ti32));
          fname = "b";
          args = [];
          body = [];
        };
      loc = Ast.Range.norange;
    }
  in
  let _ = show_prog (Prog ([ fn1; fn2 ], [])) in
  ()

let suite =
  "Parsing tests"
  >::: [
         "parse simple string" >:: test_parse_string_simple;
         "show manual ast" >:: test_show_manual_ast;
         "parse with comment" >:: test_parse_string_with_comment;
         "invalid syntax" >:: test_parse_invalid_syntax;
         "show prog complex" >:: test_show_prog_complex;
         "parse super simple" >:: test_parse_examples_super_simple;
         "parse simple" >:: test_parse_examples_simple;
         "parse pemdas" >:: test_parse_examples_pemdas;
         "parse double loop" >:: test_parse_examples_double_loop;
         "parse sum" >:: test_parse_examples_sum;
       ]

let () = run_test_tt_main suite
