open Ast
open Tctxt
open Type_stmt
open Type_util
open Conversions
module Printer = Pprint_typed_ast

let type_annotations (tc : Tctxt.t) =
  List.map (fun (i, ens_opt) ->
      let e' =
        match ens_opt with
        | Some ens ->
            Some
              (List.map
                 (fun en ->
                   if is_const en then type_exp tc en None |> fst
                   else
                     type_error i
                       "Expected compile-constant or fully-typed lambda for \
                        annotation argument")
                 ens)
        | None -> None
      in
      (i.elt, e'))

let type_fn ?(enclosing_class : id option) (tc : Tctxt.t) (fn : fdecl node) :
    Typed_ast.fdecl =
  let { elt = { annotations; frtyp; fname; args; body }; loc = _ } = fn in
  let tc' =
    List.fold_left
      (fun acc (t, a) ->
        typecheck_ty fn acc t;
        add_local acc a (convert_ty t, false))
      tc args
  in
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, id) -> (convert_ty ty, id)) args in
  let _tc_final, typed_body, does_ret =
    type_block tc' frtyp' body false enclosing_class
  in
  let annotations' = type_annotations tc annotations in
  if frtyp' <> RetVoid && not does_ret then
    type_error
      (List.nth body (List.length body - 1))
      ("missing return statement for " ^ fname);
  {
    annotations = annotations';
    frtyp = frtyp';
    fname;
    args = args';
    body = typed_body;
  }

let type_proto (tc : Tctxt.t) (pn : proto node) : Typed_ast.proto =
  let { elt = { annotations; frtyp; fname; args }; loc = _ } = pn in
  let frtyp' = convert_ret_ty frtyp in
  let args' = List.map (fun (ty, _) -> convert_ty ty) args in
  let annotations' = type_annotations tc annotations in
  { annotations = annotations'; frtyp = frtyp'; fname; args = args' }

let type_field (tc : Tctxt.t) (cname : id) (fn : vdecl node) : Typed_ast.field =
  let { elt = vd; loc } = fn in
  let fieldName, fty_opt, en_opt, _const = vd in
  let stmt_n = { elt = Decl vd; loc } in
  match fty_opt with
  | Some (TRef (RFun _)) ->
      type_error stmt_n
        "Lambdas not allowed at class field level - please use function \
         instead."
  | _ -> (
      match en_opt with
      | Some { elt = Lambda _; loc = _ } | Some { elt = TypedLambda _; loc = _ }
        ->
          type_error stmt_n
            "Lambdas not allowed at class field level - please use function \
             instead."
      | _ ->
          ();
          let te, e_ty =
            match (fty_opt, en_opt) with
            | Some t, Some e ->
                type_exp ~expected:(convert_ty t) tc e (Some cname)
            | None, Some e -> type_exp tc e (Some cname)
            | Some t, None ->
                let e = create_default_init stmt_n tc t in
                (e, convert_ty t)
            | None, None ->
                type_error stmt_n "Must provide type or initial value."
          in
          { fieldName; ftyp = e_ty; init = te })

let type_class (tc : Tctxt.t) (cn : cdecl node) : Typed_ast.cdecl =
  let { elt = { annotations; cname; impls; fields; methods }; loc = _ } = cn in
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
  let tfields = List.map (type_field tc cname) fields in
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
    type_fn ~enclosing_class:cname tc' method_node
  in
  let tmethods = List.map type_mthd methods in
  let annotations' = type_annotations tc annotations in
  {
    annotations = annotations';
    cname;
    impls;
    fields = tfields;
    methods = tmethods;
  }

let create_proto_ctxt (tc : Tctxt.t) (pns : proto node list) : Tctxt.t =
  let rec aux (tc : Tctxt.t) : proto node list -> Tctxt.t = function
    | pn :: t -> (
        match lookup_proto_option pn.elt.fname tc with
        | Some _ ->
            type_error pn
              (Printf.sprintf "Function prototype with name %s already defined."
                 pn.elt.fname)
        | None ->
            let func_type = get_proto_type pn tc |> convert_ty in
            let externally_defined =
              List.exists
                (fun anno_n ->
                  let { elt = i; loc = _ }, _ = anno_n in
                  i = "C")
                pn.elt.annotations
            in
            let new_tc =
              Tctxt.add_proto tc pn.elt.fname (func_type, externally_defined)
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
              (Printf.sprintf "Function with name %s already defined."
                 fn.elt.fname)
        | None ->
            let func_type = get_fdecl_type fn tc |> convert_ty in
            let new_tc = Tctxt.add_global tc fn.elt.fname (func_type, false) in
            let new_tc' =
              Tctxt.add_proto new_tc fn.elt.fname (func_type, true)
            in
            aux new_tc' t)
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
                  let { elt = cid, _ty_opt, en_opt, const; loc = _ } = fn in
                  (* TODO figure out how to not call type_field twice... *)
                  let fld = type_field tc cid fn in
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

let check_undefined_protos tc =
  let unique_protos =
    List.fold_right
      (fun (id, data) acc ->
        if List.mem_assoc id acc then acc else (id, snd data) :: acc)
      tc.protos []
  in
  let undefined_protos =
    List.fold_left
      (fun acc (id, defined) -> if not defined then id :: acc else acc)
      [] unique_protos
  in
  if List.length undefined_protos > 0 then
    type_failure
      ("The following function prototypes are undefined:\n"
      ^ String.concat "\n" undefined_protos)

let type_program (prog : Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  let (Prog (fns, cns, pns)) = prog in
  let cc = create_class_ctxt Tctxt.empty cns in
  let pc = create_proto_ctxt cc pns in
  let fc = create_fn_ctxt pc fns in
  check_undefined_protos fc;
  let typed_classes = List.map (type_class fc) cns in
  let typed_protos = List.map (type_proto fc) pns in
  let typed_funs = List.map (type_fn fc) fns in
  Prog (typed_funs, typed_classes, typed_protos)

let type_prog (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program prog)
  with TypeError msg ->
    let err = Fmt.str "Type Error: %s" msg in
    Error (Core.Error.of_string err)
