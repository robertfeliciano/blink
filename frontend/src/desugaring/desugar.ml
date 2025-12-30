open Desugar_util
open Desugar_stmt
open Desugar_class
open Desugared_ast
open Desugar_lambdas

(* open Pprint_desugared_ast *)

(* open Desugar_lambdas *)
open Conversions
module Typed = Typing.Typed_ast

let desugar_proto (pn : Typed.proto) : proto =
  {
    annotations = List.map desugar_annotation pn.annotations;
    frtyp = convert_ret_ty pn.frtyp;
    fname = pn.fname;
    args = List.map (fun t -> convert_ty t) pn.args;
  }

let desugar_fn (fn : Typed.fdecl) : fdecl =
  let body = desugar_block fn.body in
  {
    annotations = List.map desugar_annotation fn.annotations;
    frtyp = convert_ret_ty fn.frtyp;
    fname = fn.fname;
    args = List.map (fun (t, i) -> (convert_ty t, i)) fn.args;
    body;
  }

let desugar_program (prog : Typed.program) : program =
  let (Prog (fns, cns, pns)) = prog in
  let desugared_fns = List.map desugar_fn fns in
  let desugared_protos = List.map desugar_proto pns in
  let extracted_methods, structs = List.split (List.map desugar_class cns) in
  let initial_fn_list = List.flatten extracted_methods @ desugared_fns in
  (* let p_init = Prog (initial_fn_list, structs, desugared_protos) in  *)

  (* Printf.printf "%s\n" (show_desugared_program p_init); *)
  (* thread struct list and accumalte lifted functions while updating 
    fdecl body
    *)
  let final_cs, final_fns =
    List.fold_left
      (fun (acc_cs, acc_fs) f ->
        let next_cs, new_lifted_fs = lift_lambda_from_fdecl acc_cs f in
        (* next_cs: structs including any new env/lambda structs
           acc_fs @ new_lifted_fs: 
           collecting all generated code into one list
        *)
        (next_cs, acc_fs @ new_lifted_fs))
      (structs, []) initial_fn_list
  in

  Prog (final_fns, final_cs, desugared_protos)

let desugar_prog (prog : Typed.program) : (program, Core.Error.t) result =
  try Ok (desugar_program prog)
  with DesugarError msg ->
    let err = Fmt.str "Error: %s" msg in
    Error (Core.Error.of_string err)
