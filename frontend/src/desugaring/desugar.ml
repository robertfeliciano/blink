open Desugar_util
open Desugar_stmt
open Desugar_class
open Desugared_ast
open Conversions
module Typed = Typing.Typed_ast

let mangle_lambda lname = function 
  | Typed.TRef (RFun (arg_tys, ret_ty)) -> mangle_name lname arg_tys ret_ty
  | _ -> lname

let desugar_fn (fn : Typed.fdecl) : fdecl =
  let body = desugar_block fn.body in
  let mangled_name =
    mangle_name fn.fname (List.map (fun (t, _) -> t) fn.args) fn.frtyp
  in
  {
    frtyp = convert_ret_ty fn.frtyp;
    fname = mangled_name;
    args = List.map (fun (t, i) -> (convert_ty t, mangle_lambda i t)) fn.args;
    body;
  }

let desugar_program (prog : Typed.program) : program =
  let (Prog (fns, cns)) = prog in
  let desugared_fns = List.map desugar_fn fns in
  let extracted_methods, structs = List.split (List.map desugar_class cns) in
  Prog (List.flatten extracted_methods @ desugared_fns, structs)

let desugar_prog (prog : Typed.program) : (program, Core.Error.t) result =
  try Ok (desugar_program prog)
  with DesugarError msg ->
    let err = Fmt.str "Error: %s" msg in
    Error (Core.Error.of_string err)
