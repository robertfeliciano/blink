open OUnit2
open Ast

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
  let e = Int 1L in
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
  let e = Bop (Add, mk_node (Int 1L), mk_node (Str "some string")) in
  let f = fun () -> T.type_exp Tc.empty (mk_node e) in 
  assert_error f
  
let suite =
  "Typing tests" >::: [
    "test int" >:: test_int;
    "test bool" >:: test_bool;
    "test float" >:: test_float;
    "simple err" >:: test_simple_err;
  ]

let () = run_test_tt_main suite
