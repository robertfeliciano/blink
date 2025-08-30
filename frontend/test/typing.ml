open OUnit2
open Ast
open Z

module T = Typing.Type
module Tc = Typing.Tctxt

let mk_node elt = no_loc elt

let assert_error (f: unit -> 'a) = 
  let raised = 
    try 
      let _ = f () in 
      false 
    with 
    | T.TypeError _ -> true
    | _ -> false
  in 
  assert_bool "TypeError raised" raised

let test_int _ =
  let e = Int (of_int 1) in
  let _, ty = T.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TInt (TSigned Ti32))

let test_bool _ =
  let e = Bool true in
  let _, ty = T.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.TBool

let test_float _ =
  let e = Float 3.245 in
  let _, ty = T.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TFloat (Tf64))
  
let test_simple_err _ = 
  let e = Bop (Add, mk_node (Int (of_int 1)), mk_node (Str "some string")) in
  let f = fun () -> T.type_exp Tc.empty (mk_node e) in 
  assert_error f

let test_stmt _ = 
  let e = Bop (Add, mk_node (Int (of_int 1)), mk_node (Int (of_int 4))) in
  let s = Decl ("x", None, mk_node e, false) in 
  let _, ts = T.type_stmt Tc.empty (RetVoid) (mk_node s) false in 
  let ty_s = match ts with 
  | Typing.Typed_ast.Decl(_, ty_s, _, _)  -> ty_s 
  | _ -> TRef RString in
  assert_equal ty_s (TInt (TSigned (Ti32)))
  
let suite =
  "Typing tests" >::: [
    "test int" >:: test_int;
    "test bool" >:: test_bool;
    "test float" >:: test_float;
    "test stmt" >:: test_stmt;
    "simple err" >:: test_simple_err;
  ]

let () = run_test_tt_main suite
