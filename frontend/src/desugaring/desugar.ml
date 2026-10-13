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

let desugar_global (gn : Typed.gdecl) : gdecl =
  let prelude, ginit = desugar_exp gn.ginit in
  if prelude <> [] then
    desugar_error "Global initializer requires runtime code.";
  { gname = gn.gname; gtyp = convert_ty gn.gtyp; ginit; gconst = gn.gconst }

let desugar_program (prog : Typed.program) : program =
  let (Prog (optimization_level, fns, cns, pns, gds, interfaces)) = prog in
  let desugared_fns = List.map desugar_fn fns in
  let desugared_protos = List.map desugar_proto pns in
  let extracted_methods, structs = List.split (List.map desugar_class cns) in
  let structs, field_fns =
    List.fold_left
      (fun (cs, fs) (cn : cdecl) ->
        let (cs, fs), fields =
          List.fold_left_map
            (fun (cs, fs) (field : field) ->
              let ty, declaration = transform_ty field.ftyp cs in
              let cs =
                match declaration with Some cd -> add_cdecl cd cs | None -> cs
              in
              let cs, new_fs, prelude =
                lift_lambda_from_block cs [] field.prelude
              in
              let cs, init_fs, setup, _, _, init, _ =
                lift_lambdas_from_exps cs [] None field.init
              in
              ( (cs, fs @ new_fs @ init_fs),
                { field with ftyp = ty; init; prelude = prelude @ setup } ))
            (cs, fs) cn.fields
        in
        ( List.map
            (fun (c : cdecl) ->
              if c.cname = cn.cname then { c with fields } else c)
            cs,
          fs ))
      (structs, []) structs
  in
  let initial_fn_list =
    field_fns @ List.flatten extracted_methods @ desugared_fns
  in
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
      (fun (name, methods) -> (name, List.map desugar_proto methods))
      interfaces
  in
  let implementations =
    List.concat_map
      (fun (c : Typed.cdecl) ->
        List.map
          (fun name ->
            let methods = List.assoc name interfaces in
            let symbols =
              List.map
                (fun (p : proto) ->
                  mangle_name ~enclosing_class:c.cname p.fname
                    (TRef (RClass c.cname) :: p.args)
                    p.frtyp)
                methods
            in
            (c.cname, name, symbols))
          c.impls)
      cns
  in
  let interfaces =
    List.map
      (fun (name, methods) ->
        ( name,
          List.map
            (fun (p : proto) ->
              {
                p with
                args =
                  List.map (fun ty -> fst (transform_ty ty final_cs)) p.args;
                frtyp = fst (transform_ret_ty p.frtyp final_cs);
              })
            methods ))
      interfaces
  in
  Prog
    ( optimization_level,
      final_fns,
      final_cs,
      desugared_protos,
      List.map desugar_global gds,
      interfaces,
      implementations )

let desugar_prog (prog : Typed.program) : (program, Core.Error.t) result =
  try Ok (desugar_program prog)
  with DesugarError msg ->
    let err = Fmt.str "Error: %s" msg in
    Error (Core.Error.of_string err)
