open Ast
open Tctxt
open Type_stmt
open Type_util
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
  let { elt = { annotations; frtyp; fname; args; body; inline }; loc = _ } =
    fn
  in
  let args', frtyp' = validate_and_convert_signature fn tc args frtyp in
  let tc' =
    List.fold_left
      (fun acc (ty, id) -> add_local acc id (ty, false))
      tc args'
  in
  let _tc_final, typed_body, does_ret =
    type_block tc' frtyp' body false enclosing_class
  in
  let annotations' = type_annotations tc annotations in
  check_body_return_completeness fn frtyp' ~does_ret
    ~body_kind:("function " ^ fname);
  {
    annotations = annotations';
    frtyp = frtyp';
    fname;
    args = args';
    body = typed_body;
    inline;
  }

let type_proto (tc : Tctxt.t) (pn : proto node) : Typed_ast.proto =
  let { elt = { annotations; frtyp; fname; args }; loc = _ } = pn in
  let typed_args, frtyp' = validate_and_convert_signature pn tc args frtyp in
  let args' = List.map fst typed_args in
  let annotations' = type_annotations tc annotations in
  { annotations = annotations'; frtyp = frtyp'; fname; args = args' }

let type_field (tc : Tctxt.t) (cname : id) (fn : vdecl node) : Typed_ast.field =
  let { elt = vd; loc } = fn in
  let fieldName, fty_opt, en_opt, _const = vd in
  let stmt_n = { elt = Decl vd; loc } in
  match (fty_opt, en_opt) with
  | Some (TRef (RFun _)), _ ->
      type_error stmt_n
        "Lambdas not allowed at class field level - please use function \
         instead."
  | _, Some { elt = Lambda _ | TypedLambda _; loc = _ } ->
      type_error stmt_n
        "Lambdas not allowed at class field level - please use function instead."
  | Some ty, Some e ->
      let typed_ty = validate_and_convert_ty stmt_n tc ty in
      let te, e_ty = type_exp_as typed_ty tc e (Some cname) in
      { fieldName; ftyp = e_ty; init = te }
  | None, Some e ->
      let te, e_ty = type_exp tc e (Some cname) in
      { fieldName; ftyp = e_ty; init = te }
  | Some ty, None ->
      let typed_ty = validate_and_convert_ty stmt_n tc ty in
      let init = create_default_init stmt_n tc typed_ty in
      { fieldName; ftyp = typed_ty; init }
  | None, None -> type_error stmt_n "Must provide type or initial value."

let type_class (tc : Tctxt.t) (tfields : Typed_ast.field list) (cn : cdecl node)
    : Typed_ast.cdecl =
  let { elt = { annotations; cname; impls; methods; _ }; loc = _ } = cn in
  (match
     List.find_opt
       (fun ({ elt = fn; loc = _ } : fdecl node) -> fn.fname = cname)
       methods
   with
  | Some { elt = { frtyp = RetVoid; _ }; loc = _ } ->
      type_error cn "Constructor cannot return void."
  | Some _ | None -> ());
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
    let method_tc =
      if mthd.fname = cname then { tc with globals = globals' @ tc.globals }
      else tc'
    in
    type_fn ~enclosing_class:cname method_tc method_node
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
            let func_type =
              validate_and_convert_function_ty pn tc pn.elt.args pn.elt.frtyp
            in
            let externally_defined =
              List.exists
                (fun anno_n ->
                  let { elt = i; loc = _ }, _ = anno_n in
                  i = "C")
                pn.elt.annotations
            in
            let new_tc =
              Tctxt.set_proto tc pn.elt.fname (func_type, externally_defined)
            in
            aux new_tc t)
    | [] -> tc
  in
  aux tc pns

let create_fn_ctxt (tc : Tctxt.t) (fns : fdecl node list) : Tctxt.t =
  let reconcile_proto tc (fn : fdecl node) func_type =
    let fname = fn.elt.fname in
    match lookup_proto_option fname tc with
    | Some (proto_type, _) when not (equal_ty proto_type func_type) ->
        type_error fn
          ("Definition of " ^ fname ^ " has type " ^ Printer.show_ty func_type
         ^ ", but its prototype declares " ^ Printer.show_ty proto_type ^ ".")
    | Some _ | None -> Tctxt.set_proto tc fname (func_type, true)
  in
  let rec aux (tc : Tctxt.t) : fdecl node list -> Tctxt.t = function
    | fn :: t -> (
        match lookup_global_option fn.elt.fname tc with
        | Some _ ->
            type_error fn
              (Printf.sprintf "Function with name %s already defined."
                 fn.elt.fname)
        | None ->
            let func_type =
              validate_and_convert_function_ty fn tc fn.elt.args fn.elt.frtyp
            in
            let new_tc = Tctxt.add_global tc fn.elt.fname (func_type, false) in
            let new_tc' = reconcile_proto new_tc fn func_type in
            aux new_tc' t)
    | [] -> tc
  in
  aux tc fns

let create_class_name_ctxt (tc : Tctxt.t) (cns : cdecl node list) : Tctxt.t =
  List.fold_left
    (fun tc cn ->
      let cname = cn.elt.cname in
      match lookup_class_option cname tc with
      | Some _ -> type_error cn ("Class with name " ^ cname ^ " already exists.")
      | None -> Tctxt.add_class tc cname [] [])
    tc cns

let create_class_ctxt (tc : Tctxt.t) (cns : cdecl node list) :
    Tctxt.t * (cdecl node * Typed_ast.field list) list =
  let get_method_header (mn : fdecl node) : method_header =
    let ({ fname; frtyp; args; _ } : fdecl) = mn.elt in
    let typed_args, typed_ret =
      validate_and_convert_signature mn tc args frtyp
    in
    (fname, typed_ret, typed_args)
  in
  let rec aux (tc : Tctxt.t) typed_fields = function
    | cn :: t ->
        let cname = cn.elt.cname in
        let fields_with_types =
          List.map
            (fun field -> (field, type_field tc cname field))
            cn.elt.fields
        in
        let tfields = List.map snd fields_with_types in
        let fields =
          List.map
            (fun (field, (typed_field : Typed_ast.field)) ->
              let { elt = _, _ty_opt, init, const; loc = _ } = field in
              ( typed_field.fieldName,
                typed_field.ftyp,
                const,
                Option.is_some init ))
            fields_with_types
        in
        let method_headers = List.map get_method_header cn.elt.methods in
        let new_tc = Tctxt.set_class tc cname fields method_headers in
        aux new_tc ((cn, tfields) :: typed_fields) t
    | [] -> (tc, List.rev typed_fields)
  in
  aux tc [] cns

let check_undefined_protos tc =
  let undefined_protos =
    List.filter_map
      (fun (id, (_, defined)) -> if defined then None else Some id)
      tc.protos
  in
  match undefined_protos with
  | [] -> ()
  | _ ->
      type_failure
        ("The following function prototypes are undefined:\n"
        ^ String.concat "\n" undefined_protos)

let type_program ?(optimization_level = Util.Optimization_level.default)
    (prog : Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  let (Prog (fns, cns, pns)) = prog in
  let class_names = create_class_name_ctxt Tctxt.empty cns in
  let cc, classes_with_fields = create_class_ctxt class_names cns in
  let pc = create_proto_ctxt cc pns in
  let fc = create_fn_ctxt pc fns in
  check_undefined_protos fc;
  let typed_classes =
    List.map
      (fun (class_node, fields) -> type_class fc fields class_node)
      classes_with_fields
  in
  let typed_protos =
    List.filter_map
      (fun pn ->
        match lookup_global_option pn.elt.fname fc with
        | Some _ -> None
        | None -> Some (type_proto fc pn))
      pns
  in
  let typed_funs = List.map (type_fn fc) fns in
  Prog (optimization_level, typed_funs, typed_classes, typed_protos)

let type_prog ?(optimization_level = Util.Optimization_level.default)
    (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program ~optimization_level prog)
  with
  | TypeError msg ->
      let err = Fmt.str "Type Error: %s" msg in
      Error (Core.Error.of_string err)
  | exn ->
      let err =
        Fmt.str "Internal Typechecker Error: %s"
          (Printexc.to_string exn)
      in
      Error (Core.Error.of_string err)
