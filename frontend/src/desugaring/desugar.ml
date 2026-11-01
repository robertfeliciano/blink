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
  {
    iname = i.iname;
    protos = List.map desugar_proto i.protos;
    fields =
      List.map
        (fun (f : Typed.ifield) ->
          {
            fieldName = f.fieldName;
            ftyp = convert_ty f.ftyp;
            fconst = f.fconst;
          })
        i.fields;
  }

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
  let args, arg_structs = transform_tys p.args structs in
  let structs = add_cdecls arg_structs structs in
  let frtyp, ret_structs = transform_ret_ty p.frtyp structs in
  (add_cdecls ret_structs structs, { p with args; frtyp })

let desugar_global (gn : Typed.gdecl) : gdecl =
  let prelude, ginit = desugar_exp gn.ginit in
  if prelude <> [] then
    desugar_error "Global initializer requires runtime code.";
  { gname = gn.gname; gtyp = convert_ty gn.gtyp; ginit; gconst = gn.gconst }

(* Field defaults run when an object is constructed. Use the same declaration
   lowering as locals so tuple elements containing closures get their runtime
   representation and any lifted functions, without evaluating defaults early. *)
let lower_class_fields structs classes =
  List.fold_left
    (fun (structs, functions) (c : cdecl) ->
      let (structs, functions), fields =
        List.fold_left_map
          (fun (structs, functions) (field : field) ->
            let name = gensym "field_default" in
            let structs, lifted, prelude =
              lift_lambda_from_block structs []
                (field.prelude @ [ Decl (name, field.ftyp, field.init, true) ])
            in
            let ty = fst (transform_ty field.ftyp structs) in
            ( (structs, functions @ lifted),
              { field with ftyp = ty; prelude; init = Id (name, ty) } ))
          (structs, functions) c.fields
      in
      let c = { c with fields } in
      ( List.map (fun old -> if old.cname = c.cname then c else old) structs,
        functions ))
    (structs, []) classes

let desugar_program (prog : Typed.program) : program =
  let (Prog (optimization_level, fns, cns, pns, gds, interfaces)) = prog in
  let desugared_fns = List.map desugar_fn fns in
  let desugared_protos = List.map desugar_proto pns in
  let interfaces = List.map lower_interface interfaces in
  let lower_class (c : Typed.cdecl) =
    desugar_class ~impls:(impls_for_class interfaces c) c
  in
  let extracted_methods, structs = List.split (List.map lower_class cns) in
  let structs, field_functions = lower_class_fields structs structs in
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
      (structs, field_functions) initial_fn_list
  in

  let final_cs, interfaces =
    List.fold_left_map
      (fun structs (i : idecl) ->
        let structs, protos =
          List.fold_left_map lower_interface_signature structs i.protos
        in
        let structs, fields =
          List.fold_left_map
            (fun structs (f : ifield) ->
              let ftyp, declarations = transform_ty f.ftyp structs in
              (add_cdecls declarations structs, { f with ftyp }))
            structs i.fields
        in
        (structs, { i with protos; fields }))
      final_cs interfaces
  in
  let final_cs, desugared_protos =
    List.fold_left_map
      (fun structs (p : proto) ->
        if List.mem "C" p.annotations then (structs, p)
        else lower_interface_signature structs p)
      final_cs desugared_protos
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
