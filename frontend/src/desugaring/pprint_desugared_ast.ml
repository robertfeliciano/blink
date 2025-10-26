open Desugared_ast
module PT = Typing.Pprint_typed_ast
module Base_PT = Typing.Typed_ast

(* Since most types in desugared_ast alias Typed_ast,
   we can reuse the Pprint_typed_ast show functions directly. *)

let show_ty = PT.show_ty
let show_ret_ty = PT.show_ret_ty
let show_unop = Base_PT.show_unop
let show_binop = Base_PT.show_binop
let show_exp = PT.show_exp
let show_vdecl = PT.show_vdecl
let show_stmt = PT.show_stmt
let show_block = PT.show_block
let show_fdecl = PT.show_fdecl
let show_field = PT.show_field

(* The only structure that differs is cdecl (simplified) and program. *)
let show_cdecl { cname; fields } =
  Printf.sprintf "cdecl{name=%s; fields=[\n%s\n]}"
    cname
    (String.concat "\n" (List.map PT.show_field fields))

let show_desugared_program (Prog (fns, cns)) =
  let cns_str = String.concat "\n" (List.map show_cdecl cns) in
  let fns_str = String.concat "\n" (List.map PT.show_fdecl fns) in
  Printf.sprintf "program{\nclasses=[\n%s\n];\nfunctions=[\n%s\n]\n}" cns_str fns_str
