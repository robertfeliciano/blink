open Desugar_util
open Desugar_stmt
open Desugar_class
open Desugared_ast
open Desugar_lambdas

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
  let desugared_fns = List.map (fun f -> desugar_fn f) fns in
  let desugared_protos = List.map desugar_proto pns in
  let extracted_methods, structs = List.split (List.map desugar_class cns) in
  let fn_list = List.flatten extracted_methods @ desugared_fns in
  (* Prog (fn_list, structs, desugared_protos) *)

  let fns' = List.map (lift_lambda_from_fdecl structs) fn_list in

  (* 1. First Pass: Update all top-level signatures to use Lambda structs *)
  (* let fn_list_transformed, structs_with_interfaces =
    check_fdecls fn_list structs
  in

  (* 2. Second Pass: Lift lambdas from bodies and collect new functions *)
  let final_structs, final_fns =
    List.fold_left
      (fun (cs_acc, fs_acc) f ->
        (* Thread the current struct list into the lifter *)
        let next_cs, new_lifted_fs, transformed_f =
          lift_lambda_from_fdecl cs_acc f
        in
        (* Accumulate: 
         - Updated structs (next_cs)
         - All previous functions + new lifted ones + the transformed original
      *)
        (next_cs, fs_acc @ new_lifted_fs @ [ transformed_f ]))
      (structs_with_interfaces, [])
      fn_list_transformed
  in *)

  (* Prog (final_fns, final_structs, desugared_protos) *)
  Prog (fns', structs, desugared_protos)

let desugar_prog (prog : Typed.program) : (program, Core.Error.t) result =
  try Ok (desugar_program prog)
  with DesugarError msg ->
    let err = Fmt.str "Error: %s" msg in
    Error (Core.Error.of_string err)
