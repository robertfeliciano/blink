open OUnit2
open Typing.Typed_ast
module Desugar = Desugaring.Desugar
module DA = Desugaring.Desugared_ast
module Printer = Desugaring.Pprint_desugared_ast

let int_ty = TInt (TSigned Ti32)
let int value = Int (Z.of_int value, TSigned Ti32)

let function_program body =
  Prog
    ( [ { annotations = []; frtyp = RetVoid; fname = "test"; args = []; body } ],
      [],
      [] )

let desugar_exn program =
  match Desugar.desugar_prog program with
  | Ok program -> program
  | Error error ->
      assert_failure
        (Printf.sprintf "desugaring failed: %s"
           (Core.Error.to_string_hum error))

let only_function_body = function
  | DA.Prog ([ fn ], [], []) -> fn.body
  | program ->
      assert_failure
        (Printf.sprintf "expected one function and no declarations:\n%s"
           (Printer.show_desugared_program program))

let test_compound_assignment _ =
  let lhs = Id ("x", int_ty) in
  let program = function_program [ Assn (lhs, PluEq, int 1, int_ty) ] in
  match desugar_exn program |> only_function_body with
  | [ DA.Assn (DA.Id ("x", _), DA.Bop (DA.Add, DA.Id ("x", _), _, _), _) ] -> ()
  | body ->
      assert_failure
        (Printf.sprintf "compound assignment was not lowered:\n%s"
           (Printer.show_block body))

let test_projected_call _ =
  let arg_types = [ int_ty ] in
  let call =
    SCall
      ( Proj
          ( Id ("box", TRef (RClass "Box")),
            "set",
            "Box",
            TRef (RFun (arg_types, RetVoid)) ),
        [ int 2 ],
        arg_types,
        RetVoid )
  in
  match desugar_exn (function_program [ call ]) |> only_function_body with
  | [ DA.SCall (name, [ DA.Id ("box", _); DA.Int ("2", _) ]) ] ->
      assert_bool "method name should be mangled" (name <> "set")
  | body ->
      assert_failure
        (Printf.sprintf "projected call was not lowered:\n%s"
           (Printer.show_block body))

let test_for_loop _ =
  let loop =
    For
      ( "i",
        int 0,
        int 4,
        false,
        int 1,
        int_ty,
        [ Assn (Id ("total", int_ty), PluEq, Id ("i", int_ty), int_ty) ] )
  in
  match desugar_exn (function_program [ loop ]) |> only_function_body with
  | [
   DA.Decl ("%step", _, _, true); DA.Decl ("i", _, _, false); DA.If (_, _, _);
  ] ->
      ()
  | body ->
      assert_failure
        (Printf.sprintf "for loop was not lowered:\n%s"
           (Printer.show_block body))

let test_array_foreach _ =
  let array_ty = TRef (RArray (int_ty, 2)) in
  let collection = Array ([ int 3; int 5 ], array_ty) in
  let loop = ForEach ("item", collection, array_ty, []) in
  match desugar_exn (function_program [ loop ]) |> only_function_body with
  | [ DA.Decl _; DA.Decl _; DA.While (_, [ DA.Decl _; DA.Assn _ ]) ] -> ()
  | body ->
      assert_failure
        (Printf.sprintf "array foreach was not lowered:\n%s"
           (Printer.show_block body))

let test_class_method_extraction _ =
  let field = { fieldName = "value"; ftyp = int_ty; init = int 7 } in
  let method_ =
    {
      annotations = [];
      frtyp = RetVal int_ty;
      fname = "get";
      args = [];
      body = [ Ret (Some (Id ("value", int_ty))) ];
    }
  in
  let class_ =
    {
      annotations = [];
      cname = "Box";
      impls = [];
      fields = [ field ];
      methods = [ method_ ];
    }
  in
  match desugar_exn (Prog ([], [ class_ ], [])) with
  | DA.Prog
      ( [ { fname; args = [ (DA.TRef (DA.RClass "Box"), "this") ]; _ } ],
        [ { cname = "Box"; fields = [ { fieldName = "value"; _ } ]; _ } ],
        [] ) ->
      assert_bool "method name should be mangled" (fname <> "get")
  | program ->
      assert_failure
        (Printf.sprintf "class was not split into struct and method:\n%s"
           (Printer.show_desugared_program program))

let parse_and_type_exn source =
  let ast =
    match Parsing.Parse.parse_prog (Lexing.from_string source) with
    | Ok ast -> ast
    | Error error ->
        assert_failure
          (Printf.sprintf "test fixture did not parse: %s"
             (Core.Error.to_string_hum error))
  in
  match Typing.Type.type_prog ast with
  | Ok program -> program
  | Error error ->
      assert_failure
        (Printf.sprintf "test fixture did not type-check: %s"
           (Core.Error.to_string_hum error))

let pipeline_programs =
  [
    ( "branch and loop",
      "fun main() => i32 {\n\
      \  let value = 0;\n\
      \  while value < 3 { value += 1; }\n\
      \  if value == 3 { return 7; } else { return 8; }\n\
       }" );
    ( "for loop",
      "fun main() => i32 {\n\
      \  let total = 0;\n\
      \  for i in 0..4 { total += i; }\n\
      \  return total;\n\
       }" );
    ( "array foreach",
      "fun main() => i32 {\n\
      \  let values = [1, 2, 3];\n\
      \  let total = 0;\n\
      \  for value in values { total += value; }\n\
      \  return total;\n\
       }" );
  ]

let test_full_frontend_pipeline source _ =
  ignore (parse_and_type_exn source |> desugar_exn)

let test_lambda_lifting _ =
  let source =
    "fun main() => i32 {\n\
    \  let scale = 4;\n\
    \  let apply: [i32] -> i32 = fn[scale](value) {\n\
    \    return value * scale;\n\
    \  };\n\
    \  let result = apply(3);\n\
    \  free apply;\n\
    \  return result;\n\
     }"
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (functions, classes, []) ->
      assert_bool "lambda should produce a lifted function"
        (List.length functions > 1);
      assert_bool "lambda should produce closure structs"
        (List.length classes >= 2)
  | program ->
      assert_failure
        (Printf.sprintf "unexpected lambda lowering:\n%s"
           (Printer.show_desugared_program program))

let suite =
  let pipeline_tests =
    List.map
      (fun (name, source) -> name >:: test_full_frontend_pipeline source)
      pipeline_programs
  in
  "Desugaring"
  >::: [
         "compound assignment" >:: test_compound_assignment;
         "projected call" >:: test_projected_call;
         "for loop" >:: test_for_loop;
         "array foreach" >:: test_array_foreach;
         "class method extraction" >:: test_class_method_extraction;
         "lambda lifting" >:: test_lambda_lifting;
         "parsed and typed programs" >::: pipeline_tests;
       ]
