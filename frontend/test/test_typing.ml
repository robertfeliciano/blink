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

let string_starts_with ~prefix value =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length
  && String.sub value 0 prefix_length = prefix

let assert_program_type_error source =
  match Typing.Type.type_prog (parse_exn source) with
  | Error error ->
      let message = Core.Error.to_string_hum error in
      assert_bool
        ("expected a Type Error, got: " ^ message)
        (string_starts_with ~prefix:"Type Error:" message)
  | Ok program ->
      assert_failure
        (Printf.sprintf "expected type checking to fail, got:\n%s"
           (Typing.Pprint_typed_ast.show_typed_program program))

let type_program_exn source =
  match Typing.Type.type_prog (parse_exn source) with
  | Ok program -> program
  | Error error ->
      assert_failure
        (Printf.sprintf "expected type checking to succeed: %s"
           (Core.Error.to_string_hum error))

let assert_program_type_checks source = ignore (type_program_exn source)

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

let test_unsigned_128_literal_inference _ =
  let value = Z.shift_left Z.one 127 in
  let _, actual = type_exp (Int value) in
  assert_ty Typed.(TInt (TUnsigned Tu128)) actual

let test_numeric_binary_expression _ =
  let expression =
    Bop (Add, mk_node (Int (Z.of_int 2)), mk_node (Int (Z.of_int 4)))
  in
  let _, ty = type_exp expression in
  assert_ty Typed.(TInt (TSigned Ti32)) ty

let assert_promoted_binary_type left_ty right_ty expected_ty =
  let tc =
    Tc.empty |> fun tc -> Tc.add_local tc "left" (left_ty, false)
    |> fun tc -> Tc.add_local tc "right" (right_ty, false)
  in
  let expression = Bop (Add, mk_node (Id "left"), mk_node (Id "right")) in
  match type_exp ~tc expression with
  | Typed.Bop (_, left, right, result_ty), actual_ty ->
      assert_ty expected_ty result_ty;
      assert_ty expected_ty actual_ty;
      let assert_operand_promoted original_ty = function
        | Typed.Cast (_, cast_ty) ->
            assert_bool "only operands whose type changes should be cast"
              (not (Tu.equal_ty original_ty expected_ty));
            assert_ty expected_ty cast_ty
        | _ -> assert_ty expected_ty original_ty
      in
      assert_operand_promoted left_ty left;
      assert_operand_promoted right_ty right
  | _ -> assert_failure "expected a promoted typed binary expression"

let test_signed_unsigned_integer_promotion _ =
  let open Typed in
  assert_promoted_binary_type
    (TInt (TSigned Ti16))
    (TInt (TUnsigned Tu16))
    (TInt (TSigned Ti32));
  assert_promoted_binary_type
    (TInt (TSigned Ti32))
    (TInt (TUnsigned Tu32))
    (TInt (TSigned Ti64));
  assert_promoted_binary_type
    (TInt (TSigned Ti64))
    (TInt (TUnsigned Tu64))
    (TInt (TSigned Ti128))

let test_unrepresentable_integer_promotion _ =
  let open Typed in
  let tc =
    Tc.empty
    |> fun tc -> Tc.add_local tc "signed" (TInt (TSigned Ti128), false)
    |> fun tc -> Tc.add_local tc "unsigned" (TInt (TUnsigned Tu128), false)
  in
  let expression =
    Ast.Bop
      (Ast.Add, mk_node (Ast.Id "signed"), mk_node (Ast.Id "unsigned"))
  in
  assert_type_error (fun () -> type_exp ~tc expression)

let test_integer_float_promotion _ =
  let open Typed in
  assert_promoted_binary_type
    (TInt (TSigned Ti16))
    (TFloat Tf32) (TFloat Tf32);
  assert_promoted_binary_type
    (TInt (TSigned Ti32))
    (TFloat Tf32) (TFloat Tf64);
  assert_promoted_binary_type
    (TFloat Tf32) (TFloat Tf64) (TFloat Tf64)

let test_numeric_comparison_promotion _ =
  let open Typed in
  let tc =
    Tc.empty
    |> fun tc -> Tc.add_local tc "left" (TInt (TSigned Ti16), false)
    |> fun tc -> Tc.add_local tc "right" (TInt (TUnsigned Tu16), false)
  in
  let expression =
    Ast.Bop (Ast.Lt, mk_node (Ast.Id "left"), mk_node (Ast.Id "right"))
  in
  match type_exp ~tc expression with
  | Typed.Bop (_, Typed.Cast (_, left_ty), Typed.Cast (_, right_ty), result_ty),
    actual_ty ->
      assert_ty (TInt (TSigned Ti32)) left_ty;
      assert_ty (TInt (TSigned Ti32)) right_ty;
      assert_ty TBool result_ty;
      assert_ty TBool actual_ty
  | _ -> assert_failure "expected comparison operands to be promoted"

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
      type_exp ~tc (Ast.Call (mk_node (Ast.Id "positive"), [])));
  assert_type_error (fun () ->
      type_exp ~tc
        (Ast.Call
           ( mk_node (Ast.Id "positive"),
             [ mk_node (Ast.Int Z.zero); mk_node (Ast.Int Z.one) ] )))

let test_void_function_call_wrong_arity _ =
  List.iter assert_program_type_error
    [
      "fun consume(value: i32) => void { }
       fun main() => i32 { consume(); return 0; }";
      "fun consume(value: i32) => void { }
       fun main() => i32 { consume(1, 2); return 0; }";
    ]

let test_constant_arithmetic_failures_are_type_errors _ =
  let int value = mk_node (Int value) in
  let binary operator left right = Bop (operator, int left, int right) in
  List.iter
    (fun expression -> assert_type_error (fun () -> type_exp expression))
    [ binary Div Z.one Z.zero; binary Mod Z.one Z.zero ];
  let huge = Z.shift_left Z.one 100 in
  assert_type_error (fun () -> type_exp (binary Shl Z.one huge));
  assert_type_error (fun () ->
      Tu.eval_const_exp (mk_node (binary Pow (Z.of_int 2) Z.minus_one)));
  assert_type_error (fun () ->
      Tu.eval_const_exp (mk_node (binary Pow (Z.of_int 2) huge)))

let test_function_call_promotes_numeric_argument _ =
  let open Typed in
  let parameter_ty = TInt (TSigned Ti64) in
  let argument_ty = TInt (TSigned Ti16) in
  let fn_ty = TRef (RFun ([ parameter_ty ], RetVal TBool)) in
  let tc =
    Tc.empty |> fun tc -> Tc.add_global tc "positive" (fn_ty, false)
    |> fun tc -> Tc.add_local tc "value" (argument_ty, false)
  in
  let call =
    Ast.Call (mk_node (Ast.Id "positive"), [ mk_node (Ast.Id "value") ])
  in
  match type_exp ~tc call with
  | Typed.Call (_, [ Typed.Cast (_, cast_ty) ], [ recorded_ty ], TBool), TBool ->
      assert_ty parameter_ty cast_ty;
      assert_ty parameter_ty recorded_ty
  | _ -> assert_failure "expected function argument to be promoted"

let test_return_promotes_numeric_value _ =
  assert_program_type_checks
    "fun widen(value: i16) => i64 { return value; }\n\
     fun main() => i32 { return widen(1) as i32; }"

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

let test_assignment_type_mismatch_rejected _ =
  let int_ty = Typed.(TInt (TSigned Ti32)) in
  let tc = Tc.add_local Tc.empty "value" (int_ty, false) in
  let comparison =
    Bop (Lt, mk_node (Int (Z.of_int 1)), mk_node (Int (Z.of_int 2)))
  in
  let statement = Assn (mk_node (Id "value"), Eq, mk_node comparison) in
  assert_type_error (fun () ->
      Ts.type_stmt None tc Typed.RetVoid (mk_node statement) false)

let test_assignment_promotes_numeric_value _ =
  let open Typed in
  let target_ty = TInt (TSigned Ti64) in
  let source_ty = TInt (TSigned Ti16) in
  let tc =
    Tc.empty |> fun tc -> Tc.add_local tc "target" (target_ty, false)
    |> fun tc -> Tc.add_local tc "source" (source_ty, false)
  in
  let statement =
    Ast.Assn (mk_node (Ast.Id "target"), Ast.Eq, mk_node (Ast.Id "source"))
  in
  match Ts.type_stmt None tc RetVoid (mk_node statement) false with
  | _, Assn (_, Eq, Cast (_, cast_ty), assn_ty), false ->
      assert_ty target_ty cast_ty;
      assert_ty target_ty assn_ty
  | _ -> assert_failure "expected assignment RHS to be promoted"

let test_compound_assignment_validates_operator _ =
  let tc = Tc.add_local Tc.empty "flag" (Typed.TBool, false) in
  let statement =
    Assn (mk_node (Id "flag"), PluEq, mk_node (Bool false))
  in
  assert_type_error (fun () ->
      Ts.type_stmt None tc Typed.RetVoid (mk_node statement) false)

let test_loop_control_scope _ =
  assert_type_error (fun () ->
      Ts.type_stmt None Tc.empty Typed.RetVoid (mk_node Break) false);
  ignore (Ts.type_stmt None Tc.empty Typed.RetVoid (mk_node Break) true)

let test_float_loop_default_step_preserves_type _ =
  let statement =
    For
      ( mk_node "value",
        (mk_node (Float 0.0), mk_node (Float 2.0), false),
        None,
        [] )
  in
  match Ts.type_stmt None Tc.empty Typed.RetVoid (mk_node statement) false with
  | ( _,
      Typed.For (_, _, _, _, Typed.Float (_, Typed.Tf64), step_ty, []),
      false ) ->
      assert_ty Typed.(TFloat Tf64) step_ty
  | _ -> assert_failure "expected an f64 default loop step"

let test_integer_loop_default_step_preserves_width _ =
  let bound_ty = Typed.(TInt (TSigned Ti16)) in
  let tc =
    Tc.empty |> fun tc -> Tc.add_local tc "start" (bound_ty, false)
    |> fun tc -> Tc.add_local tc "finish" (bound_ty, false)
  in
  let statement =
    For
      ( mk_node "value",
        (mk_node (Id "start"), mk_node (Id "finish"), false),
        None,
        [] )
  in
  match Ts.type_stmt None tc Typed.RetVoid (mk_node statement) false with
  | ( _,
      Typed.For (_, _, _, _, Typed.Int (_, Typed.TSigned Typed.Ti16), step_ty, []),
      false ) ->
      assert_ty bound_ty step_ty
  | _ -> assert_failure "expected an i16 default loop step"

let test_loop_body_return_does_not_guarantee_function_return _ =
  List.iter assert_program_type_error
    [
      "fun main() => i32 { while false { return 1; } }";
      "fun main() => i32 { for i in 0..1 { return i; } }";
      "fun main() => i32 { for value in [1] { return value; } }";
    ]

let test_return_after_loop_satisfies_function_return _ =
  assert_program_type_checks
    "fun main() => i32 { while false { return 1; } return 2; }"

let test_program_errors _ =
  assert_program_type_error
    "fun main() => i32 { let value: i32 = \"wrong\"; return value; }";
  assert_program_type_error "fun main() => i32 { let value = 1; }";
  assert_program_type_error
    "fun main() => i32 { if (1) { return 1; } else { return 2; } }"

let test_function_body_return_completeness _ =
  assert_program_type_error "fun main() => i32 { }";
  assert_program_type_checks "fun main() => void { }"

let test_matching_prototype_and_definition _ =
  let source =
    "fun identity(value: i32) => i32;
     fun identity(value: i32) => i32 { return value; }
     fun main() => i32 { return identity(7); }"
  in
  match type_program_exn source with
  | Typed.Prog (_, _, _, []) -> ()
  | _ -> assert_failure "resolved prototype should not be emitted"

let test_matching_void_prototype_and_definition _ =
  assert_program_type_checks
    "fun consume(value: i32) => void;
     fun consume(value: i32) => void { }
     fun main() => i32 { consume(7); return 0; }"

let test_mismatched_prototype_and_definition _ =
  assert_program_type_error
    "fun identity(value: i32) => i32;
     fun identity(value: i64) => i32 { return value as i32; }
     fun main() => i32 { return 0; }";
  assert_program_type_error
    "fun identity(value: i32) => i32;
     fun identity(value: i32) => i64 { return value; }
     fun main() => i32 { return 0; }"

let test_undefined_prototype_rejected _ =
  assert_program_type_error
    "fun identity(value: i32) => i32;
     fun main() => i32 { return 0; }"

let test_external_prototype_is_defined _ =
  let source =
    "@C fun external(value: i32) => void;
     fun main() => i32 { return 0; }"
  in
  match type_program_exn source with
  | Typed.Prog (_, _, _, [ _ ]) -> ()
  | _ -> assert_failure "external prototype should be emitted"

let test_declared_source_types_are_validated _ =
  List.iter assert_program_type_error
    [
      "fun main() => i32 { let value: Missing = null; return 0; }";
      "fun main() => i32 { let value = 1 as Missing; return 0; }";
      "class Box { let value: Missing = null; }
       fun main() => i32 { return 0; }";
      "class Box { fun value() => Missing { return null; } }
       fun main() => i32 { return 0; }";
      "fun invalid(value: Missing) => i32 { return 0; }
       fun main() => i32 { return 0; }";
      "fun main() => i32 {
         let apply = fn[](value: Missing) -> i32 { return 0; };
         return 0;
       }";
    ]

let test_class_type_validation_supports_forward_references _ =
  assert_program_type_checks
    "class Holder { let value: Value = null; }
     class Value { }
     class Node { let next: Node = null; }
     fun main() => i32 { return 0; }"

let test_array_length_must_fit_target_int _ =
  let huge_length = Z.shift_left Z.one 100 in
  let array_ty = TRef (RArray (TInt (TSigned Ti32), huge_length)) in
  assert_type_error (fun () ->
      Tu.validate_and_convert_ty (mk_node ()) Tc.empty array_ty)

let test_unsupported_generic_type_is_type_error _ =
  let generic_ty = TRef (RGeneric ("Box", [ TInt (TSigned Ti32) ])) in
  assert_type_error (fun () ->
      Tu.validate_and_convert_ty (mk_node ()) Tc.empty generic_ty)

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

let test_typed_lambda_signature_must_match_expected_type _ =
  List.iter assert_program_type_error
    [
      "fun main() => i32 {
       \  let apply: [i32] -> i32 =
       \    fn[](value: bool) -> i32 { return 0; };
       \  return 0;
       }";
      "fun main() => i32 {
       \  let apply: [i32] -> i32 =
       \    fn[](left: i32, right: i32) -> i32 { return left; };
       \  return 0;
       }";
    ]

let test_nonvoid_lambda_requires_return _ =
  assert_program_type_error
    "fun main() => i32 {
    \  let apply: [i32] -> i32 = fn[](value) { let copy = value; };
    \  return 0;
     }";
  assert_program_type_error
    "fun main() => i32 {
    \  let apply = fn[](value: i32) -> i32 { let copy = value; };
    \  return 0;
     }"

let test_nonvoid_lambda_accepts_complete_returns _ =
  assert_program_type_checks
    "fun main() => i32 {
    \  let apply: [i32] -> i32 = fn[](value) {
    \    if value > 0 { return value; } else { return 0; }
    \  };
    \  return apply(1);
     }"

let test_void_lambda_does_not_require_return _ =
  assert_program_type_checks
    "fun main() => i32 {
    \  let consume: [i32] -> void = fn[](value) { let copy = value; };
    \  consume(1);
    \  free consume;
    \  return 0;
     }"

let suite =
  "Typing"
  >::: [
         "literal types" >:: test_literal_types;
         "expected integer type" >:: test_expected_integer_type;
         "integer overflow" >:: test_integer_overflow;
         "u128 literal inference" >:: test_unsigned_128_literal_inference;
         "numeric binary expression" >:: test_numeric_binary_expression;
         "signed/unsigned integer promotion"
         >:: test_signed_unsigned_integer_promotion;
         "unrepresentable integer promotion"
         >:: test_unrepresentable_integer_promotion;
         "integer/float promotion" >:: test_integer_float_promotion;
         "numeric comparison promotion" >:: test_numeric_comparison_promotion;
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
         "void function arity" >:: test_void_function_call_wrong_arity;
         "constant arithmetic diagnostics"
         >:: test_constant_arithmetic_failures_are_type_errors;
         "function argument promotion"
         >:: test_function_call_promotes_numeric_argument;
         "return promotion" >:: test_return_promotes_numeric_value;
         "method call" >:: test_method_call;
         "const assignment" >:: test_const_assignment_rejected;
         "assignment type mismatch" >:: test_assignment_type_mismatch_rejected;
         "assignment numeric promotion"
         >:: test_assignment_promotes_numeric_value;
         "compound assignment operator validation"
         >:: test_compound_assignment_validates_operator;
         "loop control scope" >:: test_loop_control_scope;
         "float loop default step type"
         >:: test_float_loop_default_step_preserves_type;
         "integer loop default step width"
         >:: test_integer_loop_default_step_preserves_width;
         "loop return is not guaranteed"
         >:: test_loop_body_return_does_not_guarantee_function_return;
         "return after loop" >:: test_return_after_loop_satisfies_function_return;
         "program errors" >:: test_program_errors;
         "function return completeness"
         >:: test_function_body_return_completeness;
         "matching prototype definition"
         >:: test_matching_prototype_and_definition;
         "matching void prototype definition"
         >:: test_matching_void_prototype_and_definition;
         "mismatched prototype definition"
         >:: test_mismatched_prototype_and_definition;
         "undefined prototype" >:: test_undefined_prototype_rejected;
         "external prototype" >:: test_external_prototype_is_defined;
         "declared source type validation"
         >:: test_declared_source_types_are_validated;
         "forward class type validation"
         >:: test_class_type_validation_supports_forward_references;
         "array length target fit" >:: test_array_length_must_fit_target_int;
         "unsupported generic diagnostic"
         >:: test_unsupported_generic_type_is_type_error;
         "object initialization" >:: test_object_initialization;
         "unknown object field" >:: test_object_rejects_unknown_field;
         "capturing lambda" >:: test_capturing_lambda;
         "lambda return type" >:: test_lambda_rejects_wrong_return_type;
         "typed lambda signature"
         >:: test_typed_lambda_signature_must_match_expected_type;
         "non-void lambda missing return" >:: test_nonvoid_lambda_requires_return;
         "non-void lambda complete returns"
         >:: test_nonvoid_lambda_accepts_complete_returns;
         "void lambda return optional"
         >:: test_void_lambda_does_not_require_return;
       ]
