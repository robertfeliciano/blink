open OUnit2
open Ast
open Z

let run_pipeline_from_lex lex =
  match Parsing.Parse.parse_prog lex with
  | Error e -> Error e
  | Ok ast -> (
      match Typing.Type.type_prog ast with
      | Error e -> Error e
      | Ok tprog -> Desugaring.Desugar.desugar_prog tprog)

let run_pipeline_from_string s = run_pipeline_from_lex (Lexing.from_string s)

(* let test_e2e_example_super_simple _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/super_simple.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_e2e_example_simple _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/simple.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_e2e_example_sum _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/sum.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_e2e_example_pemdas _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/pemdas.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_e2e_example_double_loop _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/double_loop.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_e2e_example_matrix _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/matrix.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e) *)

let test_e2e_invalid_syntax_string _ =
  let s = "fun main( => u64 { return 1; }" in
  match run_pipeline_from_string s with
  | Ok _ -> assert_failure "expected parse error"
  | Error _ -> ()

let test_e2e_manual_ast_success _ =
  (* fun main() => u64 { return 1; } *)
  let fn =
    {
      elt =
        {
          annotations = [];
          frtyp = RetVal (TInt (TSigned Ti32));
          fname = "main";
          args = [];
          body =
            [
              {
                elt = Ret (Some (no_loc (Int (of_int 1))));
                loc = Range.norange;
              };
            ];
        };
      loc = Range.norange;
    }
  in
  let prog = Prog ([ fn ], []) in
  match Typing.Type.type_prog prog with
  | Error e -> assert_failure (Core.Error.to_string_hum e)
  | Ok tprog -> (
      match Desugaring.Desugar.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))

let test_e2e_manual_ast_type_error _ =
  (* declare x: i32 = "str" should error *)
  let vdecl =
    ("x", Some (TInt (TSigned Ti32)), Some (no_loc (Str "s")), false)
  in
  let stmt = Decl vdecl in
  let fn =
    {
      elt =
        {
          annotations = [];
          frtyp = RetVoid;
          fname = "f";
          args = [];
          body = [ no_loc stmt ];
        };
      loc = Range.norange;
    }
  in
  let prog = Prog ([ fn ], []) in
  match Typing.Type.type_prog prog with
  | Ok _ -> assert_failure "expected type error"
  | Error _ -> ()

let test_e2e_string_roundtrip_small _ =
  let s = "fun foo() => i32 { return 2 + 3 * 4; }" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e)

(* let test_e2e_infer_example _ =
  let s = Core.In_channel.read_all "/home/robert/programming/compilers/blink/examples/infer.bl" in
  match run_pipeline_from_string s with
  | Ok _ -> ()
  | Error e -> assert_failure (Core.Error.to_string_hum e) *)

let suite =
  "E2E tests"
  >::: [
         (* "super_simple" >:: test_e2e_example_super_simple;
    "simple" >:: test_e2e_example_simple;
    "sum" >:: test_e2e_example_sum;
    "pemdas" >:: test_e2e_example_pemdas;
    "double_loop" >:: test_e2e_example_double_loop;
    "matrix" >:: test_e2e_example_matrix; *)
         "invalid syntax" >:: test_e2e_invalid_syntax_string;
         "manual ast success" >:: test_e2e_manual_ast_success;
         "manual ast type error" >:: test_e2e_manual_ast_type_error;
         "small roundtrip" >:: test_e2e_string_roundtrip_small;
         (* "infer example" >:: test_e2e_infer_example; *)
       ]

let () = run_test_tt_main suite
