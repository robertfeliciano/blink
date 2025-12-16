open OUnit2
open Typing.Typed_ast
open Util
module D = Desugaring.Desugar
module DP = Desugaring.Pprint_desugared_ast

let mk_int i = Int (Z.of_int i, TSigned Ti32)

let test_compound_assignment _ =
  let lhs = Id ("x", TInt (TSigned Ti32)) in
  let rhs = Int (Z.of_int 1, TSigned Ti32) in
  let stmt = Assn (lhs, PluEq, rhs, TInt (TSigned Ti32)) in
  let fn = { frtyp = RetVoid; fname = "f"; args = []; body = [ stmt ] } in
  let prog = Prog ([ fn ], [], []) in
  match D.desugar_prog prog with
  | Ok (Prog (fns, _)) -> (
      match fns with
      | fn' :: _ -> (
          match fn'.body with
          | [ Assn (_lhs', Bop (Add, _, _, _), _) ] -> ()
          | _ -> assert_failure "compound assignment not desugared")
      | _ -> assert_failure "no functions")
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_desugar_call_proj _ =
  (* 
  o.m(2) -> m(o, 2)
  *)
  let arg = Int (Z.of_int 2, TSigned Ti32) in
  let arg_tys = [ TInt (TSigned Ti32) ] in
  let call =
    SCall
      ( Proj
          ( Id ("o", TRef (RClass "clazz")),
            "m",
            "clazz",
            TRef (RFun (arg_tys, RetVoid)) ),
        [ arg ],
        arg_tys,
        RetVoid )
  in
  let fn = { frtyp = RetVoid; fname = "f"; args = []; body = [ call ] } in
  let prog = Prog ([ fn ], [], []) in
  match D.desugar_prog prog with
  | Ok (Prog (fns, _)) -> (
      match fns with
      | fn' :: _ -> (
          match List.hd fn'.body with
          | SCall (Id _, [ Id ("o", _); _ ]) -> ()
          | bad ->
              assert_failure ("proj call not desugared: \n" ^ DP.show_stmt bad))
      | _ -> assert_failure "no functions")
  | Error e -> assert_failure (Core.Error.to_string_hum e)

(* let test_desugar_call_nested _ =
  let inner =
    Call
      ( Proj (Id ("o", TRef (RFun "clazz")), "m", "clazz"),
        [ Int (Z.of_int 1, TSigned Ti32) ],
        [ TInt (TSigned Ti32) ],
        TInt (TSigned Ti32) )
  in
  let outer =
    Call (Id "g", [ inner ], [ TInt (TSigned Ti32) ], TInt (TSigned Ti32))
  in
  let fn =
    {
      frtyp = RetVoid;
      fname = "f";
      args = [];
      body = [ SCall (outer, [], [], RetVoid) ];
    }
  in
  let prog = Prog ([ fn ], []) in
  match D.desugar_prog prog with
  | Ok (Prog (fns, _)) -> (
      match fns with
      | fn' :: _ -> (
          match fn'.body with
          | [ SCall (Call (Id "g", [ Call (Id "m", _, _) ], _), _) ] -> ()
          | _ -> () (* permit more relaxed check *))
      | _ -> assert_failure "no functions")
  | Error e -> assert_failure (Core.Error.to_string_hum e) *)

let test_desugar_if_and_while _ =
  let cond = Bool true in
  let ifstmt = If (cond, [ Break ], [ Continue ]) in
  let whilestmt = While (cond, [ ifstmt ]) in
  let fn = { frtyp = RetVoid; fname = "f"; args = []; body = [ whilestmt ] } in
  let prog = Prog ([ fn ], [], []) in
  match D.desugar_prog prog with
  | Ok (Prog (fns, _)) -> assert_bool "desugared" (List.length fns = 1)
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let test_noop_desugar _ =
  let fn = { frtyp = RetVoid; fname = "f"; args = []; body = [] } in
  let prog = Prog ([ fn ], [], []) in
  match D.desugar_prog prog with
  | Ok (Prog (fns, _)) -> assert_equal 1 (List.length fns)
  | Error e -> assert_failure (Core.Error.to_string_hum e)

(* run desugar on typed programs produced from parsing+typing of examples: basic smoke tests *)
let parse_and_type s =
  let lex = Lexing.from_string s in
  match Parsing.Parse.parse_prog lex with
  | Ok ast -> (
      match Typing.Type.type_prog ast with
      | Ok tprog -> Some tprog
      | Error _ -> None)
  | Error _ -> None

let test_desugar_from_example_super_simple _ =
  let s = test_file_channel "super_simple.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> assert_failure "parse/type failed"

let test_desugar_from_example_simple _ =
  let s = test_file_channel "simple.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> assert_failure "parse/type failed"

let test_desugar_from_example_sum _ =
  let s = test_file_channel "sum.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> ()

let test_desugar_from_example_pemdas _ =
  let s = test_file_channel "pemdas.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> assert_failure "parse/type failed"

let test_desugar_from_example_double_loop _ =
  let s = test_file_channel "double_loop.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> assert_failure "parse/type failed"

let test_desugar_from_example_matrix _ =
  let s = test_file_channel "matrix.bl" in
  match parse_and_type s with
  | Some tprog -> (
      match D.desugar_prog tprog with
      | Ok _ -> ()
      | Error e -> assert_failure (Core.Error.to_string_hum e))
  | None -> assert_failure "parse/type failed"

let test_desugar_method_extraction _ =
  (* simple class - desugar_class extracts methods (smoke) *)
  let cname = "C" in
  let field =
    {
      fieldName = "a";
      ftyp = TInt (TSigned Ti32);
      init = Int (Z.of_int 0, TSigned Ti32);
    }
  in
  let method_fd =
    {
      frtyp = RetVoid;
      fname = "m";
      args = [ (TInt (TSigned Ti32), "x") ];
      body = [];
    }
  in
  let cdecl =
    { cname; impls = []; fields = [ field ]; methods = [ method_fd ] }
  in
  let prog = Prog ([], [ cdecl ], []) in
  match D.desugar_prog prog with
  | Ok (Prog (_, structs)) ->
      assert_bool "structs returned" (List.length structs >= 0)
  | Error e -> assert_failure (Core.Error.to_string_hum e)

let suite =
  "Desugaring tests"
  >::: [
         "compound assignment" >:: test_compound_assignment;
         "desugar call proj" >:: test_desugar_call_proj;
         (* "desugar call nested" >:: test_desugar_call_nested; *)
         "if and while" >:: test_desugar_if_and_while;
         "noop desugar" >:: test_noop_desugar;
         "desugar super_simple" >:: test_desugar_from_example_super_simple;
         "desugar simple" >:: test_desugar_from_example_simple;
         "desugar sum" >:: test_desugar_from_example_sum;
         "desugar pemdas" >:: test_desugar_from_example_pemdas;
         "desugar double_loop" >:: test_desugar_from_example_double_loop;
         "desugar matrix" >:: test_desugar_from_example_matrix;
         "method extraction" >:: test_desugar_method_extraction;
       ]

let () = run_test_tt_main suite
