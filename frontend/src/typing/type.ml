open Ast
open Tctxt
open Type_stmt
open Type_exp
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
  let _tc_final, typed_body, does_ret = type_block tc' frtyp' body false in
  if frtyp' <> RetVoid && not does_ret then
    type_error
      (List.nth body (List.length body - 1))
      ("missing return statement for " ^ fname);
  { frtyp = frtyp'; fname; args = args'; body = typed_body }

(* let type_class (tc : Tctxt.t) (cn : cdecl node) : Typed_ast.cdecl =  *)


let create_fn_ctxt (tc : Tctxt.t) (fns : fdecl node list) : Tctxt.t =
  let rec aux (tc : Tctxt.t) = function
    | fn :: t -> (
        match lookup_global_option fn.elt.fname tc with
        | Some _ ->
            type_error fn
              (Printf.sprintf "function with name %s already exists"
                 fn.elt.fname)
        | None ->
            let func_type = get_fdecl_type fn tc in
            let new_tc =
              Tctxt.add_global tc fn.elt.fname (convert_ty func_type)
            in
            aux new_tc t)
    | [] -> tc
  in
  aux tc fns

let create_class_ctxt (tc : Tctxt.t) (cns : cdecl node list) : Tctxt.t =
  let get_method_header { fname; frtyp; args; _ } : method_header =
    ( fname,
      convert_ret_ty frtyp,
      List.map (fun (t, name) -> (convert_ty t, name)) args )
  in
  let rec aux (tc : Tctxt.t) = function
    | cn :: t -> (
        match lookup_class_option cn.elt.cname tc with
        | Some _ ->
            type_error cn
              ("Class with name " ^ cn.elt.cname ^ " already exists.")
        | None ->
            let cname = cn.elt.cname in
            let fields =
              List.map
                (fun fn ->
                  match fn.elt with
                  | { fieldName; ftyp; init = Some init } ->
                      let _tinit, init_ty =
                        type_exp ~expected:(convert_ty ftyp) Tctxt.empty init
                      in
                      (fieldName, init_ty, true)
                  | { fieldName; ftyp; init = None } ->
                      (fieldName, convert_ty ftyp, false))
                cn.elt.fields
            in
            let method_headers =
              List.map (fun mn -> get_method_header mn.elt) cn.elt.methods
            in
            let new_tc = Tctxt.add_class tc cname fields method_headers in
            aux new_tc t)
    | [] -> tc
  in
  aux tc cns

let type_program (prog : Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  let (Prog (fns, cns)) = prog in
  let cc = create_class_ctxt Tctxt.empty cns in
  let fc = create_fn_ctxt cc fns in
  let fns_t = List.map (fun fn -> type_fn fc fn) fns in
  Prog (fns_t, [])

let type_prog (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program prog)
  with TypeError msg ->
    let err = Fmt.str "Type Error: %s" msg in
    Error (Core.Error.of_string err)
