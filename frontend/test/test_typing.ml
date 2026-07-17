open OUnit2
open Ast
module Ts = Typing.Type_stmt
module Tc = Typing.Tctxt
module Tu = Typing.Type_util
module Typed = Typing.Typed_ast

let mk_node = no_loc
let show_ty = Typing.Pprint_typed_ast.show_ty
let assert_ty expected actual = assert_equal ~printer:show_ty expected actual

let assert_type_error thunk =
  match
    try Ok (thunk ()) with
    | Tu.TypeError message -> Error (`Type_error message)
    | exn -> Error (`Unexpected exn)
  with
  | Error (`Type_error _) -> ()
  | Ok _ -> assert_failure "expected a TypeError"
  | Error (`Unexpected exn) ->
      assert_failure
        (Printf.sprintf "expected TypeError, got %s" (Printexc.to_string exn))

let type_exp ?expected ?(tc = Tc.empty) expression =
  Ts.type_exp ?expected tc (mk_node expression) None

let parse_exn source =
  match Parsing.Parse.parse_prog (Lexing.from_string source) with
  | Ok program -> program
  | Error error ->
      assert_failure
        (Printf.sprintf "test fixture did not parse: %s"
           (Core.Error.to_string_hum error))

let assert_program_type_error source =
  match Typing.Type.type_prog (parse_exn source) with
  | Error _ -> ()
  | Ok program ->
      assert_failure
        (Printf.sprintf "expected type checking to fail, got:\n%s"
           (Typing.Pprint_typed_ast.show_typed_program program))

let assert_program_type_checks source =
  match Typing.Type.type_prog (parse_exn source) with
  | Ok _ -> ()
  | Error error ->
      assert_failure
        (Printf.sprintf "expected type checking to succeed: %s"
           (Core.Error.to_string_hum error))

let test_literal_types _ =
  let _, int_ty = type_exp (Int (Z.of_int 1)) in
  let _, bool_ty = type_exp (Bool true) in
  let _, float_ty = type_exp (Float 3.25) in
  let _, string_ty = type_exp (Str "blink") in
  assert_ty Typed.(TInt (TSigned Ti32)) int_ty;
  assert_ty Typed.TBool bool_ty;
  assert_ty Typed.(TFloat Tf64) float_ty;
  assert_ty Typed.(TRef RString) string_ty

let test_expected_integer_type _ =
  let expected = Typed.(TInt (TUnsigned Tu8)) in
  let _, actual = type_exp ~expected (Int (Z.of_int 255)) in
  assert_ty expected actual

let test_integer_overflow _ =
  let expected = Typed.(TInt (TUnsigned Tu8)) in
  assert_type_error (fun () -> type_exp ~expected (Int (Z.of_int 256)))

let test_numeric_binary_expression _ =
  let expression =
    Bop (Add, mk_node (Int (Z.of_int 2)), mk_node (Int (Z.of_int 4)))
  in
  let _, ty = type_exp expression in
  assert_ty Typed.(TInt (TSigned Ti32)) ty

let test_binary_type_mismatch _ =
  let expression =
    Bop (Add, mk_node (Int (Z.of_int 1)), mk_node (Str "not a number"))
  in
  assert_type_error (fun () -> type_exp expression)

let test_comparison_returns_bool _ =
  let int_ty = Typed.(TInt (TSigned Ti32)) in
  let tc =
    Tc.empty |> fun tc ->
    Tc.add_local tc "left" (int_ty, false) |> fun tc ->
    Tc.add_local tc "right" (int_ty, false)
  in
  let expression = Bop (Lt, mk_node (Id "left"), mk_node (Id "right")) in
  let _, ty = type_exp ~tc expression in
  assert_ty Typed.TBool ty

let test_boolean_operator_rejects_numbers _ =
  let expression =
    Bop (And, mk_node (Int (Z.of_int 1)), mk_node (Int (Z.of_int 2)))
  in
  assert_type_error (fun () -> type_exp expression)

let test_cast _ =
  let expression = Cast (mk_node (Int (Z.of_int 3)), TInt (TSigned Ti64)) in
  let _, ty = type_exp expression in
  assert_ty Typed.(TInt (TSigned Ti64)) ty;
  assert_type_error (fun () ->
      type_exp (Cast (mk_node (Bool true), TInt (TSigned Ti32))))

let test_array_and_index _ =
  let array =
    Array [ mk_node (Int (Z.of_int 1)); mk_node (Int (Z.of_int 2)) ]
  in
  let array_node = mk_node array in
  let _, array_ty = type_exp array in
  assert_ty Typed.(TRef (RArray (TInt (TSigned Ti32), 2))) array_ty;
  let _, element_ty =
    type_exp (Index (array_node, mk_node (Int (Z.of_int 1))))
  in
  assert_ty Typed.(TInt (TSigned Ti32)) element_ty

let test_array_rejects_mixed_elements _ =
  let array = Array [ mk_node (Int (Z.of_int 1)); mk_node (Bool true) ] in
  assert_type_error (fun () -> type_exp array)

let test_index_requires_integer _ =
  let array = Array [ mk_node (Int (Z.of_int 1)) ] |> mk_node in
  assert_type_error (fun () -> type_exp (Index (array, mk_node (Bool false))))

let test_function_call _ =
  let open Typed in
  let fn_ty = TRef (RFun ([ TInt (TSigned Ti32) ], RetVal TBool)) in
  let tc = Tc.add_global Tc.empty "positive" (fn_ty, false) in
  let call =
    Ast.Call (mk_node (Ast.Id "positive"), [ mk_node (Ast.Int (Z.of_int 42)) ])
  in
  let _, ty = type_exp ~tc call in
  assert_ty TBool ty

let test_function_call_wrong_arity _ =
  let open Typed in
  let fn_ty = TRef (RFun ([ TInt (TSigned Ti32) ], RetVal TBool)) in
  let tc = Tc.add_global Tc.empty "positive" (fn_ty, false) in
  assert_type_error (fun () ->
      type_exp ~tc (Ast.Call (mk_node (Ast.Id "positive"), [])))

let test_method_call _ =
  let open Typed in
  let header =
    ("get", RetVal (TInt (TSigned Ti32)), [ (TInt (TSigned Ti32), "i") ])
  in
  let tc =
    Tc.add_class Tc.empty "Box" [] [ header ] |> fun tc ->
    Tc.add_global tc "box" (TRef (RClass "Box"), false)
  in
  let call =
    Ast.Call
      ( mk_node (Ast.Proj (mk_node (Ast.Id "box"), "get")),
        [ mk_node (Ast.Int (Z.of_int 0)) ] )
  in
  let _, ty = type_exp ~tc call in
  assert_ty (TInt (TSigned Ti32)) ty

let test_const_assignment_rejected _ =
  let int_ty = Typed.(TInt (TSigned Ti32)) in
  let tc = Tc.add_local Tc.empty "answer" (int_ty, true) in
  let statement =
    Assn (mk_node (Id "answer"), Eq, mk_node (Int (Z.of_int 43)))
  in
  assert_type_error (fun () ->
      Ts.type_stmt None tc Typed.RetVoid (mk_node statement) false)

let test_loop_control_scope _ =
  assert_type_error (fun () ->
      Ts.type_stmt None Tc.empty Typed.RetVoid (mk_node Break) false);
  ignore (Ts.type_stmt None Tc.empty Typed.RetVoid (mk_node Break) true)

let test_program_errors _ =
  assert_program_type_error
    "fun main() => i32 { let value: i32 = \"wrong\"; return value; }";
  assert_program_type_error "fun main() => i32 { let value = 1; }";
  assert_program_type_error
    "fun main() => i32 { if (1) { return 1; } else { return 2; } }"

let test_object_initialization _ =
  assert_program_type_checks
    "class Box { let value: i32 = 0; }\n\
     fun main() => i32 {\n\
    \  let box = new Box { value = 9 };\n\
    \  return box.value;\n\
     }"

let test_object_rejects_unknown_field _ =
  assert_program_type_error
    "class Box { let value: i32 = 0; }\n\
     fun main() => i32 {\n\
    \  let box = new Box { missing = 9 };\n\
    \  return box.value;\n\
     }"

let test_capturing_lambda _ =
  assert_program_type_checks
    "fun main() => i32 {\n\
    \  let scale = 4;\n\
    \  let apply: [i32] -> i32 = fn[scale](value) {\n\
    \    return value * scale;\n\
    \  };\n\
    \  let result = apply(3);\n\
    \  free apply;\n\
    \  return result;\n\
     }"

let test_lambda_rejects_wrong_return_type _ =
  assert_program_type_error
    "fun main() => i32 {\n\
    \  let apply: [i32] -> bool = fn[](value) { return value; };\n\
    \  if apply(3) { return 1; } else { return 0; }\n\
     }"

let suite =
  "Typing"
  >::: [
         "literal types" >:: test_literal_types;
         "expected integer type" >:: test_expected_integer_type;
         "integer overflow" >:: test_integer_overflow;
         "numeric binary expression" >:: test_numeric_binary_expression;
         "binary type mismatch" >:: test_binary_type_mismatch;
         "comparison returns bool" >:: test_comparison_returns_bool;
         "boolean operator rejects numbers"
         >:: test_boolean_operator_rejects_numbers;
         "casts" >:: test_cast;
         "array and index" >:: test_array_and_index;
         "mixed array elements" >:: test_array_rejects_mixed_elements;
         "index requires integer" >:: test_index_requires_integer;
         "function call" >:: test_function_call;
         "function arity" >:: test_function_call_wrong_arity;
         "method call" >:: test_method_call;
         "const assignment" >:: test_const_assignment_rejected;
         "loop control scope" >:: test_loop_control_scope;
         "program errors" >:: test_program_errors;
         "object initialization" >:: test_object_initialization;
         "unknown object field" >:: test_object_rejects_unknown_field;
         "capturing lambda" >:: test_capturing_lambda;
         "lambda return type" >:: test_lambda_rejects_wrong_return_type;
       ]
