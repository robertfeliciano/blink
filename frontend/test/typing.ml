open OUnit2

module T = Typing.Type
module Tc = Typing.Tctxt

let () =
  Printexc.register_printer (function
    | T.TypeError msg -> Some ("Type error: " ^ msg)
    | _ -> None)


let mk_node elt = Ast.no_loc elt

let test_simple_expr _ =
  let e = Ast.Float 0.34 in
  let _, ty = T.type_exp Tc.empty (mk_node e) in
  assert_equal ty Typing.Typed_ast.(TInt (TSigned Ti32))

let suite =
  "Typing tests" >::: [
    "simple expr" >:: test_simple_expr;
  ]

let () = run_test_tt_main suite
