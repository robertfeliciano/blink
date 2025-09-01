open Ast
open Tctxt
open Type_stmt
open Type_util
open Conversions
module Printer = Pprint_typed_ast

let type_fn (tc : Tctxt.t) (fn : fdecl node) : Typed_ast.fdecl =
  let { elt = f; loc = _ } = fn in
  let { frtyp; fname; args; body } = f in
  let tc' =
    List.fold_left
      (fun acc (t, a) ->
        typecheck_ty fn acc t;
        add_local acc a (convert_ty t))
      tc args
  in
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, id) -> (convert_ty ty, id)) args in
  let _tc_final, typed_body = type_block tc' frtyp' body false in
  { frtyp = frtyp'; fname; args = args'; body = typed_body }

let create_fn_ctxt (tc : Tctxt.t) (fns : fdecl node list) : Tctxt.t =
  let rec aux (tc : Tctxt.t) = function
    | fn :: t -> (
        let func_type = get_fdecl_type fn tc in
        match lookup_global_option fn.elt.fname tc with
        | Some _ ->
            type_error fn
              (Printf.sprintf "function with name %s already exists"
                 fn.elt.fname)
        | None ->
            let new_tc =
              Tctxt.add_global tc fn.elt.fname (convert_ty func_type)
            in
            aux new_tc t)
    | [] -> tc
  in
  aux tc fns

let type_program (prog : Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  (* create class ctxt *)
  let (Prog (fns, _cns)) = prog in
  let fc = create_fn_ctxt Tctxt.empty fns in
  let fns_t = List.map (fun fn -> type_fn fc fn) fns in
  Prog fns_t

let type_prog (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program prog)
  with TypeError msg ->
    let err = Fmt.str "Type Error: %s" msg in
    Error (Core.Error.of_string err)
