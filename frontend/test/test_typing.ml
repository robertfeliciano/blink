open OUnit2
open Ast
open Z
module T = Typing.Type
module Tu = Typing.Type_util

(* module Te = Typing.Type_exp *)
module Ts = Typing.Type_stmt
module Tc = Typing.Tctxt

let mk_node elt = no_loc elt

let assert_error (f : unit -> 'a) =
  let raised =
    try
      let _ = f () in
      false
    with
    | Tu.TypeError _ -> true
    | _ -> false
  in
  assert_bool "TypeError raised" raised

let test_int _ =
  let e = Int (of_int 1) in
  let _, ty = Ts.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TInt (TSigned Ti32))

let test_bool _ =
  let e = Bool true in
  let _, ty = Ts.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.TBool

let test_float _ =
  let e = Float 3.245 in
  let _, ty = Ts.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TFloat Tf64)

let test_simple_err _ =
  let e = Bop (Add, mk_node (Int (of_int 1)), mk_node (Str "some string")) in
  let f = fun () -> Ts.type_exp Tc.empty (mk_node e) in
  assert_error f

let test_stmt _ =
  let e = Bop (Add, mk_node (Int (of_int 1)), mk_node (Int (of_int 4))) in
  let s = Decl ("x", None, mk_node e, false) in
  let _, ts, _ = Ts.type_stmt Tc.empty RetVoid (mk_node s) false in
  let ty_s =
    match ts with
    | Typing.Typed_ast.Decl (_, ty_s, _, _) -> ty_s
    | _ -> TRef RString
  in
  assert_equal ty_s (TInt (TSigned Ti32))

(* additional tests *)

let test_cast_ok _ =
  let e = Cast (mk_node (Int (of_int 3)), TInt (TSigned Ti32)) in
  let _, ty = Ts.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TInt (TSigned Ti32))

let test_cast_err _ =
  let e = Cast (mk_node (Bool true), TInt (TSigned Ti32)) in
  let f = fun () -> Ts.type_exp Tc.empty (mk_node e) in
  assert_error f

let test_array_and_index _ =
  let arr = Array [ mk_node (Int (of_int 1)); mk_node (Int (of_int 2)) ] in
  let arr_node = mk_node arr in
  let _, aty = Ts.type_exp Tc.empty arr_node in
  (* array should be RArray of TInt and length 2 *)
  (match aty with
  | Typing.Typed_ast.TRef (Typing.Typed_ast.RArray (_, len)) ->
      assert_equal len (Int64.of_int 2)
  | _ -> assert_failure "expected array type");
  let idx = Index (arr_node, mk_node (Int (of_int 1))) in
  let _, ity = Ts.type_exp Tc.empty (mk_node idx) in
  assert_equal ity Typing.Typed_ast.(TInt (TSigned Ti32))

let test_fn_call_ok _ =
  let open Typing.Typed_ast in
  let fn_ty =
    TRef (RFun ([ TInt (TSigned Ti32) ], RetVal (TInt (TSigned Ti32))))
  in
  let tc = Tc.add_global Tc.empty "f" fn_ty in
  let call = Ast.(Call (mk_node (Id "f"), [ mk_node (Int (of_int 42)) ])) in
  let _, ty = Ts.type_exp tc (mk_node call) in
  assert_equal ty (TInt (TSigned Ti32))

let test_fn_call_err _ =
  let call = Call (mk_node (Id "not_a_fn"), [ mk_node (Int (of_int 42)) ]) in
  let f = fun () -> Ts.type_exp Tc.empty (mk_node call) in
  assert_error f

let test_method_call_ok _ =
  let open Typing.Typed_ast in
  (* create class C with method m(x:i32) -> i32 *)
  let method_hdr =
    ("m", RetVal (TInt (TSigned Ti32)), [ (TInt (TSigned Ti32), "x") ])
  in
  let tc1 = Tc.add_class Tc.empty "C" [] [ method_hdr ] in
  let tc2 = Tc.add_global tc1 "c" (TRef (RClass "C")) in
  let call =
    Ast.(
      Call (mk_node (Proj (mk_node (Id "c"), "m")), [ mk_node (Int (of_int 7)) ]))
  in
  let _, ty = Ts.type_exp tc2 (mk_node call) in
  assert_equal ty (TInt (TSigned Ti32))

let test_uop_err _ =
  let e = Uop (Neg, mk_node (Bool true)) in
  let f = fun () -> Ts.type_exp Tc.empty (mk_node e) in
  assert_error f

let suite =
  "Typing tests"
  >::: [
         "test int" >:: test_int;
         "test bool" >:: test_bool;
         "test float" >:: test_float;
         "test stmt" >:: test_stmt;
         "simple err" >:: test_simple_err;
         "cast ok" >:: test_cast_ok;
         "cast err" >:: test_cast_err;
         "array and index" >:: test_array_and_index;
         "fn call ok" >:: test_fn_call_ok;
         "fn call err" >:: test_fn_call_err;
         "method call ok" >:: test_method_call_ok;
         "uop err" >:: test_uop_err;
       ]

let () = run_test_tt_main suite
