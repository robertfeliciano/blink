open Ast
open Tctxt
open Type_stmt

(* open Type_exp *)
open Type_util
open Conversions
module Printer = Pprint_typed_ast

let type_fn (tc : Tctxt.t) (fn : fdecl node) : Typed_ast.fdecl =
  let { elt = { annotations = _; frtyp; fname; args; body }; loc = _ } = fn in
  let tc' =
    List.fold_left
      (fun acc (t, a) ->
        typecheck_ty fn acc t;
        add_local acc a (convert_ty t, false))
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

let type_proto (tc : Tctxt.t) (pn : proto node) : Typed_ast.proto =
  let { elt = { annotations; frtyp; fname; args }; loc = _ } = pn in
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, _) -> convert_ty ty) args in
  let annotations' =
    List.map
      (fun (i, en) ->
        let e' =
          match en with
          (* TODO create type_annotation and do specific things for that  *)
          | Some es -> Some (List.map (fun e -> type_exp tc e |> fst) es)
          | None -> None
        in
        (i.elt, e'))
      annotations
  in
  { annotations = annotations'; frtyp = frtyp'; fname; args = args' }

let type_field (tc : Tctxt.t) (fn : vdecl node) : Typed_ast.field =
  let { elt = vd; loc } = fn in
  let fieldName, fty_opt, en_opt, _const = vd in
  let stmt_n = { elt = Decl vd; loc } in
  let te, e_ty =
    match (fty_opt, en_opt) with
    | Some t, Some e -> type_exp ~expected:(convert_ty t) tc e
    | None, Some e -> type_exp tc e
    | Some t, None ->
        let e = create_default_init stmt_n tc t in
        (e, convert_ty t)
    | None, None -> type_error stmt_n "Must provide type or initial value."
  in
  { fieldName; ftyp = e_ty; init = te }

let type_class (tc : Tctxt.t) (cn : cdecl node) : Typed_ast.cdecl =
  let { elt = { annotations = _; cname; impls; fields; methods }; loc = _ } =
    cn
  in
  let () =
    match
      List.find_opt
        (fun ({ elt = fn; loc = _ } : fdecl node) -> fn.fname = cname)
        methods
    with
    | Some { elt = constructor; loc = _ } ->
        if constructor.frtyp = RetVoid then
          type_error cn "Constructor cannot return void."
        else ()
    | None -> ()
  in
  let tfields = List.map (type_field tc) fields in
  let globals' =
    List.map
      (fun (f : Typed_ast.field) -> (f.fieldName, (f.ftyp, false)))
      tfields
  in
  let tc' =
    {
      tc with
      locals = ("this", Typed_ast.(TRef (RClass cname), true)) :: tc.locals;
      globals = globals' @ tc.globals;
    }
  in
  let type_mthd method_node =
    let ({ elt = mthd; loc = _ } : fdecl node) = method_node in
    let tc' =
      if mthd.fname = cname then { tc with globals = globals' @ tc.globals }
      else tc'
    in
    type_fn tc' method_node
  in
  let tmethods = List.map type_mthd methods in
  { cname; impls; fields = tfields; methods = tmethods }

let create_proto_ctxt (tc : Tctxt.t) (pns : proto node list) : Tctxt.t =
  let rec aux (tc : Tctxt.t) : proto node list -> Tctxt.t = function
    | pn :: t -> (
        match lookup_global_option pn.elt.fname tc with
        | Some _ ->
            type_error pn
              (Printf.sprintf "function with name %s already exists"
                 pn.elt.fname)
        | None ->
            let func_type = get_proto_type pn tc in
            let new_tc =
              (*
          TODO insert with is_proto flag
          after lookup check if is_proto for fn ctxt -> yes then dont throw error
          *)
              Tctxt.add_global tc pn.elt.fname (convert_ty func_type, false)
            in
            aux new_tc t)
    | [] -> tc
  in
  aux tc pns

let create_fn_ctxt (tc : Tctxt.t) (fns : fdecl node list) : Tctxt.t =
  let rec aux (tc : Tctxt.t) : fdecl node list -> Tctxt.t = function
    | fn :: t -> (
        match lookup_global_option fn.elt.fname tc with
        | Some _ ->
            type_error fn
              (Printf.sprintf "function with name %s already exists"
                 fn.elt.fname)
        | None ->
            let func_type = get_fdecl_type fn tc in
            let new_tc =
              Tctxt.add_global tc fn.elt.fname (convert_ty func_type, false)
            in
            aux new_tc t)
    | [] -> tc
  in
  aux tc fns

let create_class_ctxt (tc : Tctxt.t) (cns : cdecl node list) : Tctxt.t =
  let get_method_header ({ fname; frtyp; args; _ } : fdecl) : method_header =
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
                  let { elt = _id, _ty_opt, en_opt, const; loc = _ } = fn in
                  (* TODO figure out how to not call type_field twice... *)
                  let fld = type_field tc fn in
                  (fld.fieldName, fld.ftyp, const, Option.is_some en_opt))
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
  let (Prog (fns, cns, pns)) = prog in
  let cc = create_class_ctxt Tctxt.empty cns in
  let pc = create_proto_ctxt cc pns in
  let fc = create_fn_ctxt pc fns in
  let typed_classes = List.map (type_class fc) cns in
  let typed_protos = List.map (type_proto fc) pns in
  let typed_funs = List.map (type_fn fc) fns in
  Prog (typed_funs, typed_classes, typed_protos)

let type_prog (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program prog)
  with TypeError msg ->
    let err = Fmt.str "Type Error: %s" msg in
    Error (Core.Error.of_string err)
