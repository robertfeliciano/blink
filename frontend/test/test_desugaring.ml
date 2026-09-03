open OUnit2
open Typing.Typed_ast
module Desugar = Desugaring.Desugar
module DA = Desugaring.Desugared_ast
module Printer = Desugaring.Pprint_desugared_ast

let int_ty = TInt (TSigned Ti32)
let int value = Int (Z.of_int value, TSigned Ti32)
let optimization_level = Util.Optimization_level.O0

let function_program body =
  Prog
    ( optimization_level,
      [
        {
          annotations = [];
          frtyp = RetVoid;
          fname = "test";
          args = [];
          body;
          inline = false;
        };
      ],
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
  | DA.Prog (_, [ fn ], [], []) -> fn.body
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
      inline = false;
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
  match desugar_exn (Prog (optimization_level, [], [ class_ ], [])) with
  | DA.Prog
      ( _,
        [ { fname; args = [ (DA.TRef (DA.RClass "Box"), "this") ]; _ } ],
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

let test_optimization_level_propagation _ =
  let source = "fun main() => i32 { return 0; }" in
  let ast =
    match Parsing.Parse.parse_prog (Lexing.from_string source) with
    | Ok ast -> ast
    | Error error ->
        assert_failure
          (Printf.sprintf "test fixture did not parse: %s"
             (Core.Error.to_string_hum error))
  in
  let requested = Util.Optimization_level.O3 in
  let typed =
    match Typing.Type.type_prog ~optimization_level:requested ast with
    | Ok (Typing.Typed_ast.Prog (actual, _, _, _) as typed) ->
        assert_equal ~msg:"type checker optimization level" requested actual;
        typed
    | Error error ->
        assert_failure
          (Printf.sprintf "test fixture did not type-check: %s"
             (Core.Error.to_string_hum error))
  in
  match desugar_exn typed with
  | DA.Prog (actual, _, _, _) ->
      assert_equal ~msg:"desugarer optimization level" requested actual

let test_inline_propagation _ =
  let source =
    "inline fun increment(value: i32) => i32 { return value + 1; }"
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, [ fn ], [], []) ->
      assert_bool "desugaring should preserve inline functions" fn.inline
  | program ->
      assert_failure
        (Printf.sprintf "unexpected inline program:\n%s"
           (Printer.show_desugared_program program))

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
    \  let apply: (i32) -> i32 = fn[scale](value) {\n\
    \    return value * scale;\n\
    \  };\n\
    \  let result = apply(3);\n\
    \  free apply;\n\
    \  return result;\n\
     }"
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, classes, []) ->
      assert_bool "lambda should produce a lifted function"
        (List.length functions > 1);
      assert_bool "lambda should produce closure structs"
        (List.length classes >= 2)
  | program ->
      assert_failure
        (Printf.sprintf "unexpected lambda lowering:\n%s"
           (Printer.show_desugared_program program))

let rec exp_contains_lambda (exp : DA.exp) =
  match exp with
  | DA.Lambda _ -> true
  | DA.Call (_, args, _) | DA.Array (args, _) ->
      List.exists exp_contains_lambda args
  | DA.ObjInit (_, fields) ->
      List.exists (fun (_, exp) -> exp_contains_lambda exp) fields
  | DA.Bop (_, lhs, rhs, _) ->
      exp_contains_lambda lhs || exp_contains_lambda rhs
  | DA.Uop (_, exp, _) | DA.Cast (exp, _) | DA.Proj (exp, _, _) ->
      exp_contains_lambda exp
  | DA.Index (collection, index, _) ->
      exp_contains_lambda collection || exp_contains_lambda index
  | DA.PartialApply _ -> true
  | DA.Bool _ | DA.Int _ | DA.Float _ | DA.Str _ | DA.Id _ | DA.Null _ -> false

let rec stmt_contains_lambda (stmt : DA.stmt) =
  match stmt with
  | DA.Assn (lhs, rhs, _) -> exp_contains_lambda lhs || exp_contains_lambda rhs
  | DA.Decl (_, _, init, _) -> exp_contains_lambda init
  | DA.Ret (Some exp) -> exp_contains_lambda exp
  | DA.Ret None -> false
  | DA.SCall (_, args) | DA.Free args -> List.exists exp_contains_lambda args
  | DA.If (cond, then_block, else_block) ->
      exp_contains_lambda cond
      || List.exists stmt_contains_lambda then_block
      || List.exists stmt_contains_lambda else_block
  | DA.While (cond, body) ->
      exp_contains_lambda cond || List.exists stmt_contains_lambda body
  | DA.Break | DA.Continue -> false

let test_nested_lambda_lifting _ =
  let source =
    {|
fun main() => i32 {
    let x: i32 = 10;
    let outer: (i32) -> i32 = fn [x](y) {
        let inner: (i32) -> i32 = fn [x, y](z) {
            return x + y + z;
        };
        return inner(3);
    };
    return outer(2);
}
|}
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, classes, []) ->
      let lifted_functions =
        List.filter
          (fun (fn : DA.fdecl) ->
            Core.String.is_prefix fn.fname ~prefix:"Lifted")
          functions
      in
      let environment_structs =
        List.filter
          (fun (class_ : DA.cdecl) ->
            Core.String.is_prefix class_.cname ~prefix:"Env")
          classes
      in
      assert_equal ~msg:"both lambdas should be lifted" 2
        (List.length lifted_functions);
      assert_equal ~msg:"both closures should have environments" 2
        (List.length environment_structs);
      assert_bool "no lambda expressions should remain"
        (not
           (List.exists
              (fun (fn : DA.fdecl) -> List.exists stmt_contains_lambda fn.body)
              functions))
  | program ->
      assert_failure
        (Printf.sprintf "unexpected nested lambda lowering:\n%s"
           (Printer.show_desugared_program program))

let test_partial_application_lifting _ =
  let source =
    {|
fun add(left: i32, middle: i32, right: i32) => i32 {
    return left + middle + right;
}
fun main() => i32 {
    let add_left: (i32, i32) -> i32 = add(10);
    let result = add_left(20, 12);
    free add_left;
    return result;
}
|}
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, _, []) ->
      assert_bool "partial application should produce a lifted wrapper"
        (List.exists
           (fun (fn : DA.fdecl) ->
             Core.String.is_prefix fn.fname ~prefix:"Lifted")
           functions);
      assert_bool "no partial-application nodes should remain"
        (not
           (List.exists
              (fun (fn : DA.fdecl) -> List.exists stmt_contains_lambda fn.body)
              functions))
  | program ->
      assert_failure
        (Printf.sprintf "unexpected partial-application lowering:\n%s"
           (Printer.show_desugared_program program))

let rec exp_contains_call (exp : DA.exp) =
  match exp with
  | DA.Call _ -> true
  | DA.Array (args, _) -> List.exists exp_contains_call args
  | DA.ObjInit (_, fields) ->
      List.exists (fun (_, value) -> exp_contains_call value) fields
  | DA.Bop (_, lhs, rhs, _) ->
      exp_contains_call lhs || exp_contains_call rhs
  | DA.Uop (_, value, _) | DA.Cast (value, _) | DA.Proj (value, _, _) ->
      exp_contains_call value
  | DA.Index (collection, index, _) ->
      exp_contains_call collection || exp_contains_call index
  | DA.PartialApply (_, args, _, _, _) -> List.exists exp_contains_call args
  | DA.Lambda (_, _, _, body) -> List.exists stmt_contains_call body
  | DA.Bool _ | DA.Int _ | DA.Float _ | DA.Str _ | DA.Id _ | DA.Null _ -> false

and stmt_contains_call (stmt : DA.stmt) =
  match stmt with
  | DA.Assn (lhs, rhs, _) -> exp_contains_call lhs || exp_contains_call rhs
  | DA.Decl (_, _, init, _) -> exp_contains_call init
  | DA.Ret (Some value) -> exp_contains_call value
  | DA.Ret None -> false
  | DA.SCall _ -> true
  | DA.Free values -> List.exists exp_contains_call values
  | DA.If (condition, then_block, else_block) ->
      exp_contains_call condition
      || List.exists stmt_contains_call then_block
      || List.exists stmt_contains_call else_block
  | DA.While (condition, body) ->
      exp_contains_call condition || List.exists stmt_contains_call body
  | DA.Break | DA.Continue -> false

let test_chained_partial_application_cleanup _ =
  let source =
    {|
fun add(left: i32, middle: i32, right: i32) => i32 {
    return left + middle + right;
}
fun main() => i32 {
    let result = add(10)(20)(12);
    return result;
}
|}
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, _, []) ->
      let main =
        match
          List.find_opt
            (fun (fn : DA.fdecl) -> Core.String.equal fn.fname "main")
            functions
        with
        | Some fn -> fn
        | None -> assert_failure "expected a lowered main function"
      in
      let indexed_body = List.mapi (fun index stmt -> (index, stmt)) main.body in
      let cleaned_closure_names =
        List.concat_map
          (fun (free_index, stmt) ->
            match stmt with
            | DA.Free values ->
                List.filter_map
                  (function
                    | DA.Id (closure_name, DA.TRef (DA.RClass _)) ->
                        if
                          List.exists
                            (fun (decl_index, prior_stmt) ->
                              match prior_stmt with
                              | DA.Decl (name, _, _, _)
                                when Core.String.equal name closure_name ->
                                  decl_index < free_index
                                  && List.exists
                                       (fun (call_index, candidate) ->
                                         decl_index < call_index
                                         && call_index < free_index
                                         && stmt_contains_call candidate)
                                       indexed_body
                              | _ -> false)
                            indexed_body
                          && List.exists
                               (fun (return_index, candidate) ->
                                 return_index > free_index
                                 && match candidate with
                                    | DA.Ret _ -> true
                                    | _ -> false)
                               indexed_body
                        then Some closure_name
                        else None
                    | _ -> None)
                  values
            | _ -> [])
          indexed_body
        |> List.sort_uniq Core.String.compare
      in
      assert_bool
        (Printf.sprintf
           "expected the generated intermediate closure to be declared, \
            consumed, and then cleaned up before return:\n%s"
           (Printer.show_block main.body))
        (List.length cleaned_closure_names >= 1)
  | program ->
      assert_failure
        (Printf.sprintf "unexpected chained partial-application lowering:\n%s"
           (Printer.show_desugared_program program))

let test_anonymous_void_partial_application_cleanup _ =
  let source =
    {|
fun consume(left: i32, right: i32) => void {}
fun main() => i32 {
    consume(1)(2);
    return 42;
}
|}
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, _, []) ->
      let main =
        match
          List.find_opt
            (fun (fn : DA.fdecl) -> Core.String.equal fn.fname "main")
            functions
        with
        | Some fn -> fn
        | None -> assert_failure "expected a lowered main function"
      in
      let indexed_body = List.mapi (fun index stmt -> (index, stmt)) main.body in
      let has_ordered_cleanup =
        List.exists
          (fun (free_index, stmt) ->
            match stmt with
            | DA.Free
                [
                  DA.Id (cleanup_env, _);
                  DA.Id (closure_name, DA.TRef (DA.RClass _));
                ] ->
                List.exists
                  (fun (call_index, call_stmt) ->
                    match call_stmt with
                    | DA.SCall
                        (callee, [ DA.Id (call_env, _); DA.Int ("2", _) ]) ->
                        call_index < free_index
                        && List.exists
                             (fun (decl_index, candidate) ->
                               decl_index < call_index
                               && match candidate with
                                  | DA.Decl
                                      ( name,
                                        _,
                                        DA.Proj
                                          ( DA.Id (source, _),
                                            "lambdaptr",
                                            _ ),
                                        true ) ->
                                      Core.String.equal name callee
                                      && Core.String.equal source closure_name
                                  | _ -> false)
                             indexed_body
                        && List.exists
                             (fun (decl_index, candidate) ->
                               decl_index < call_index
                               && match candidate with
                                  | DA.Decl
                                      ( name,
                                        _,
                                        DA.Proj
                                          (DA.Id (source, _), "envptr", _),
                                        true ) ->
                                      Core.String.equal name call_env
                                      && Core.String.equal source closure_name
                                  | _ -> false)
                             indexed_body
                        && List.exists
                             (fun (decl_index, candidate) ->
                               call_index < decl_index
                               && decl_index < free_index
                               && match candidate with
                                  | DA.Decl
                                      ( name,
                                        _,
                                        DA.Proj
                                          (DA.Id (source, _), "envptr", _),
                                        true ) ->
                                      Core.String.equal name cleanup_env
                                      && Core.String.equal source closure_name
                                  | _ -> false)
                             indexed_body
                    | _ -> false)
                  indexed_body
            | _ -> false)
          indexed_body
      in
      assert_bool
        (Printf.sprintf
           "expected the generated void partial closure to be called before \
            its environment and closure are freed:\n%s"
           (Printer.show_block main.body))
        has_ordered_cleanup
  | program ->
      assert_failure
        (Printf.sprintf "unexpected void partial-application lowering:\n%s"
           (Printer.show_desugared_program program))

let test_function_reassignment_projects_current_closure _ =
  let source =
    {|
fun add(left: i32, right: i32) => i32 {
    return left + right;
}
fun main() => i32 {
    let f: (i32) -> i32 = add(1);
    if true { f = add(2); }
    let result = f(40);
    free f;
    return result;
}
|}
  in
  match parse_and_type_exn source |> desugar_exn with
  | DA.Prog (_, functions, _, []) ->
      let main =
        match
          List.find_opt
            (fun (fn : DA.fdecl) -> Core.String.equal fn.fname "main")
            functions
        with
        | Some fn -> fn
        | None -> assert_failure "expected a lowered main function"
      in
      let rec check_after_branch = function
        | DA.If _
          :: DA.Decl
               ( call_fptr,
                 _,
                 DA.Proj (DA.Id ("f", _), "lambdaptr", _),
                 true )
          :: DA.Decl
               ( call_env,
                 _,
                 DA.Proj (DA.Id ("f", _), "envptr", _),
                 true )
          :: DA.Decl
               ( "result",
                 _,
                 DA.Call
                   ( callee,
                     DA.Id (call_env_arg, _) :: [ DA.Int ("40", _) ],
                     _ ),
                 false )
          :: DA.Decl
               ( free_env,
                 _,
                 DA.Proj (DA.Id ("f", _), "envptr", _),
                 true )
          :: DA.Free [ DA.Id (free_env_arg, _); DA.Id ("f", _) ]
          :: _
          when Core.String.equal callee call_fptr
               && Core.String.equal call_env_arg call_env
               && Core.String.equal free_env_arg free_env ->
            ()
        | _ :: rest -> check_after_branch rest
        | [] ->
            assert_failure
              (Printf.sprintf
                 "call and free should project from the current closure after \
                  the branch:\n%s"
                 (Printer.show_block main.body))
      in
      check_after_branch main.body
  | program ->
      assert_failure
        (Printf.sprintf "unexpected function reassignment lowering:\n%s"
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
         "optimization level propagation"
         >:: test_optimization_level_propagation;
         "inline propagation" >:: test_inline_propagation;
         "lambda lifting" >:: test_lambda_lifting;
         "nested lambda lifting" >:: test_nested_lambda_lifting;
         "partial application lifting" >:: test_partial_application_lifting;
         "chained partial application cleanup"
         >:: test_chained_partial_application_cleanup;
         "anonymous void partial application cleanup"
         >:: test_anonymous_void_partial_application_cleanup;
         "function reassignment projects current closure"
         >:: test_function_reassignment_projects_current_closure;
         "parsed and typed programs" >::: pipeline_tests;
       ]
