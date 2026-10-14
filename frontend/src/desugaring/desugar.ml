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
    inline = fn.inline;
  }

let lower_interface (i : Typed.idecl) : idecl =
  { iname = i.iname; protos = List.map desugar_proto i.protos }

let impl_symbol cname (p : proto) =
  mangle_name ~enclosing_class:cname p.fname
    (TRef (RClass cname) :: p.args)
    p.frtyp

let impls_for_class interfaces (c : Typed.cdecl) =
  let impl_for_interface name =
    let interface = List.find (fun (i : idecl) -> i.iname = name) interfaces in
    { iname = name; methods = List.map (impl_symbol c.cname) interface.protos }
  in
  List.map impl_for_interface c.impls

let lower_interface_signature structs (p : proto) =
  {
    p with
    args = List.map (fun ty -> fst (transform_ty ty structs)) p.args;
    frtyp = fst (transform_ret_ty p.frtyp structs);
  }

let desugar_global (gn : Typed.gdecl) : gdecl =
  let prelude, ginit = desugar_exp gn.ginit in
  if prelude <> [] then
    desugar_error "Global initializer requires runtime code.";
  { gname = gn.gname; gtyp = convert_ty gn.gtyp; ginit; gconst = gn.gconst }

let desugar_program (prog : Typed.program) : program =
  let (Prog (optimization_level, fns, cns, pns, gds, interfaces)) = prog in
  let desugared_fns = List.map desugar_fn fns in
  let desugared_protos = List.map desugar_proto pns in
  let interfaces = List.map lower_interface interfaces in
  let lower_class (c : Typed.cdecl) =
    desugar_class ~impls:(impls_for_class interfaces c) c
  in
  let extracted_methods, structs = List.split (List.map lower_class cns) in
  let initial_fn_list = List.flatten extracted_methods @ desugared_fns in
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

  let interfaces =
    List.map
      (fun (i : idecl) ->
        {
          i with
          protos = List.map (lower_interface_signature final_cs) i.protos;
        })
      interfaces
  in
  Prog
    ( optimization_level,
      final_fns,
      final_cs,
      desugared_protos,
      List.map desugar_global gds,
      interfaces )

let desugar_prog (prog : Typed.program) : (program, Core.Error.t) result =
  try Ok (desugar_program prog)
  with DesugarError msg ->
    let err = Fmt.str "Error: %s" msg in
    Error (Core.Error.of_string err)
