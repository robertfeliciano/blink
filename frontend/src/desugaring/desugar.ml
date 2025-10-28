open Desugar_util
open Desugar_stmt
open Desugar_class
open Desugared_ast
open Conversions
module Typed = Typing.Typed_ast

let desugar_fn (fn : Typed.fdecl) : fdecl =
  let body = desugar_block fn.body in
  {
    frtyp = convert_ret_ty fn.frtyp;
    fname = fn.fname;
    args = List.map (fun (t, i) -> (convert_ty t, i)) fn.args;
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
