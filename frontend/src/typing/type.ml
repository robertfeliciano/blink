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
  let {
    elt = { annotations; frtyp; fname; args; body; inline; type_params = _ };
    loc = _;
  } =
    fn
  in
  let args', frtyp' = validate_and_convert_signature fn tc args frtyp in
  let tc' =
    List.fold_left (fun acc (ty, id) -> add_local acc id (ty, false)) tc args'
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
  let { elt = { annotations; frtyp; fname; args; type_params = _ }; loc = _ } =
    pn
  in
  let typed_args, frtyp' = validate_and_convert_signature pn tc args frtyp in
  let args' = List.map fst typed_args in
  let annotations' = type_annotations tc annotations in
  { annotations = annotations'; frtyp = frtyp'; fname; args = args' }

let type_field (tc : Tctxt.t) (cname : id) (fn : vdecl node) : Typed_ast.field =
  let { elt = vd; loc } = fn in
  let fieldName, fty_opt, en_opt, _const = vd in
  let stmt_n = { elt = Decl vd; loc } in
  match (fty_opt, en_opt) with
  | Some ty, None ->
      let ftyp = validate_and_convert_ty stmt_n tc ty in
      let init =
        match ftyp with
        | Typed_ast.TRef ty -> Typed_ast.Null ty
        | ty -> create_default_init stmt_n tc ty
      in
      { fieldName; ftyp; init }
  | Some (TRef (RFun _)), Some en when en.elt <> Null ->
      type_error stmt_n
        "Lambdas not allowed at class field level - please use function \
         instead."
  | _, Some { elt = Lambda _ | TypedLambda _; loc = _ } ->
      type_error stmt_n
        "Lambdas not allowed at class field level - please use function \
         instead."
  | _ ->
      let init, ftyp =
        type_variable_initializer stmt_n tc fty_opt en_opt (Some cname)
      in
      { fieldName; ftyp; init }

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
    match lookup_class_option cname tc with
    | Some (fields, _) ->
        List.map
          (fun (field, field_ty, is_const, _) -> (field, (field_ty, is_const)))
          fields
    | None -> type_error cn ("Class " ^ cname ^ " is undefined.")
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
    impls =
      List.map
        (function
          | TRef (RClass cn) -> resolved_class_name cn
          | _ -> type_error cn "Unresolved implemented interface.")
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
            let externally_defined = has_annotation "C" pn.elt.annotations in
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

let get_method_header (tc : Tctxt.t) (method_node : fdecl node) : method_header
    =
  if method_node.elt.type_params <> [] then
    type_error method_node "Generic methods are not supported.";
  let ({ fname; frtyp; args; _ } : fdecl) = method_node.elt in
  let typed_args, typed_ret =
    validate_and_convert_signature method_node tc args frtyp
  in
  (fname, typed_ret, typed_args)

let create_class_header_ctxt (tc : Tctxt.t) (cns : cdecl node list) : Tctxt.t =
  let get_field_header (field : vdecl node) =
    let { elt = field_name, ty, init, const; loc = _ } = field in
    match ty with
    | Some ty ->
        Some
          ( field_name,
            validate_and_convert_ty field tc ty,
            const,
            Option.is_some init )
    | None -> None
  in
  List.fold_left
    (fun tc cn ->
      let cname = cn.elt.cname in
      let fields = List.filter_map get_field_header cn.elt.fields in
      let methods = List.map (get_method_header tc) cn.elt.methods in
      Tctxt.set_class tc cname fields methods)
    tc cns

let create_class_ctxt (tc : Tctxt.t) (cns : cdecl node list) :
    Tctxt.t * (cdecl node * Typed_ast.field list) list =
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
        let method_headers = List.map (get_method_header tc) cn.elt.methods in
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

(* Resolution preserves exact C names. Reconcile shared declarations only after
   nominal class identities are resolved, using the ordinary signature rules.
   Do not deduplicate solely by spelling: LLVM would silently rename conflicts. *)
let reconcile_external_prototypes tc prototypes =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (prototype : proto node) ->
      let name = prototype.elt.fname in
      match Hashtbl.find_opt seen name with
      | None ->
          Hashtbl.add seen name prototype;
          true
      | Some previous ->
          if
            not
              (has_annotation "C" previous.elt.annotations
              && has_annotation "C" prototype.elt.annotations)
          then
            type_error prototype ("Duplicate function prototype " ^ name ^ ".");
          let signature node =
            validate_and_convert_function_ty node tc node.elt.args
              node.elt.frtyp
          in
          if not (equal_ty (signature previous) (signature prototype)) then
            type_error prototype
              ("Conflicting @C signatures for " ^ name ^ "; first declared at "
              ^ Util.Range.string_of_range previous.loc
              ^ ".");
          false)
    prototypes

(* Globals are resolved independently of source order. Initializers are typed
   with the referenced declarations' types before folding their values. *)
let type_globals tc declarations =
  let pending = Hashtbl.create (List.length declarations) in
  let complete = Hashtbl.create (List.length declarations) in
  let context = ref tc in
  let supported node = function
    | Typed_ast.(TBool | TInt _ | TFloat _ | TRef RString) -> ()
    | _ ->
        type_error node
          "Globals currently support only bool, integer, float, and string \
           types."
  in
  List.iter
    (fun (node : gdecl node) ->
      let name = node.elt.gname in
      if name = "main" then
        type_error node
          "Only the entry module may declare the Blink function main.";
      if
        Hashtbl.mem pending name
        || Option.is_some (lookup_option name tc)
        || Option.is_some (lookup_class_option name tc)
        || Option.is_some (lookup_generic_option name tc)
      then type_error node ("Duplicate declaration " ^ name ^ ".");
      Option.iter
        (fun ty -> supported node (validate_and_convert_ty node tc ty))
        node.elt.gtyp;
      Hashtbl.add pending name node)
    declarations;
  let invalid node =
    type_error node
      "Global initializer must be a literal, integer constant expression, or \
       constant global reference."
  in
  let rec resolve stack (node : gdecl node) =
    let { gname; gtyp; ginit; gconst } = node.elt in
    match Hashtbl.find_opt complete gname with
    | Some typed -> typed
    | None ->
        if List.mem gname stack then
          type_error node
            ("Cyclic global initializer dependency: "
            ^ String.concat " -> " (List.rev (gname :: stack))
            ^ ".");
        let stack = gname :: stack in
        let rec dependencies expression =
          match expression.elt with
          | Bool _ | Int _ | Float _ | Str _ -> ()
          | Id name -> (
              match Hashtbl.find_opt pending name with
              | Some dependency when dependency.elt.gconst ->
                  ignore (resolve stack dependency)
              | Some _ ->
                  type_error expression
                    ("Global initializer cannot read mutable global " ^ name
                   ^ ".")
              | None -> invalid expression)
          | Uop ((Neg | BNeg), value) -> dependencies value
          | Bop
              ( ( Add | Sub | Mul | Div | Mod | Pow | Shl | Lshr | Ashr | BAnd
                | BXor | BOr ),
                left,
                right ) ->
              dependencies left;
              dependencies right
          | _ -> invalid expression
        in
        Option.iter dependencies ginit;
        if gconst && Option.is_none ginit then
          type_error node "Constant globals require an initializer.";
        let stmt =
          { elt = Decl (gname, gtyp, ginit, gconst); loc = node.loc }
        in
        let initial, ty =
          type_variable_initializer stmt !context gtyp ginit None
        in
        supported node ty;
        let init_node = Option.value ginit ~default:(no_loc (Int Z.zero)) in
        let float_constant value float_ty =
          let value =
            match float_ty with
            | Typed_ast.Tf32 -> Int32.float_of_bits (Int32.bits_of_float value)
            | Typed_ast.Tf64 -> value
          in
          Typed_ast.Float (value, float_ty)
        in
        let unary_source source =
          match source.elt with Uop (_, value) -> value | _ -> source
        in
        let rec fold source = function
          | Typed_ast.(Bool _ | Int _ | Str _) as literal -> literal
          | Typed_ast.Float (value, float_ty) -> float_constant value float_ty
          | Typed_ast.Id (_, Typed_ast.TRef Typed_ast.RString) as reference ->
              (* Strings use pointer identity. Keep the reference so its static
                 initializer shares the defining global's backing storage. *)
              reference
          | Typed_ast.Id (name, _) ->
              (Hashtbl.find complete name).Typed_ast.ginit
          | Typed_ast.Cast (value, target) -> (
              match (fold source value, target) with
              | Typed_ast.Int (value, _), Typed_ast.TInt int_ty ->
                  fst (type_integer_constant source value int_ty)
              | Typed_ast.Int (value, _), Typed_ast.TFloat float_ty ->
                  let value = Z.to_float value in
                  if not (float_is_representable_in_ty value float_ty) then
                    type_error source
                      "Global initializer does not fit its floating-point type.";
                  float_constant value float_ty
              | Typed_ast.Float (value, _), Typed_ast.TFloat float_ty ->
                  if not (float_is_representable_in_ty value float_ty) then
                    type_error source
                      "Global initializer does not fit its floating-point type.";
                  float_constant value float_ty
              | _ -> invalid source)
          | Typed_ast.Uop (Typed_ast.Neg, value, Typed_ast.TFloat float_ty) -> (
              match fold (unary_source source) value with
              | Typed_ast.Float (value, _) -> float_constant (-.value) float_ty
              | _ -> invalid source)
          | Typed_ast.Uop (op, value, Typed_ast.TInt int_ty) ->
              let value = integer (unary_source source) value in
              let op =
                match op with
                | Typed_ast.Neg -> Neg
                | Typed_ast.BNeg -> BNeg
                | _ -> invalid source
              in
              integer_result source int_ty (Uop (op, value))
          | Typed_ast.Bop (op, left, right, Typed_ast.TInt int_ty) ->
              let op =
                match op with
                | Typed_ast.Add -> Add
                | Sub -> Sub
                | Mul -> Mul
                | Div -> Div
                | Mod -> Mod
                | Pow -> Pow
                | Shl -> Shl
                | Lshr -> Lshr
                | Ashr -> Ashr
                | BAnd -> BAnd
                | BXor -> BXor
                | BOr -> BOr
                | _ -> invalid source
              in
              let left_source, right_source =
                match source.elt with
                | Bop (_, left, right) -> (left, right)
                | _ -> (source, source)
              in
              integer_result source int_ty
                (Bop (op, integer left_source left, integer right_source right))
          | _ -> invalid source
        and integer source expression =
          match fold source expression with
          | Typed_ast.Int (value, _) -> { source with elt = Int value }
          | _ -> invalid source
        and integer_result source int_ty expression =
          let node = { source with elt = expression } in
          match eval_const_exp ~int_ty node with
          | Some value -> fst (type_integer_constant node value int_ty)
          | None -> invalid node
        in
        let typed : Typed_ast.gdecl =
          { gname; gtyp = ty; ginit = fold init_node initial; gconst }
        in
        Hashtbl.add complete gname typed;
        context := add_global !context gname (ty, gconst);
        typed
  in
  let globals = List.map (resolve []) declarations in
  (!context, globals)

let create_generic_ctxt ?(nominal_templates = []) tc fns pns =
  let seen = Hashtbl.create (List.length fns) in
  let rec validate_ty node = function
    | TParam _ -> ()
    | TRef (RDArray ty) -> validate_ty node ty
    | TRef (RGeneric (cn, ts)) -> (
        match List.assoc_opt (resolved_class_name cn) nominal_templates with
        | Some template
          when List.length (Generics.nominal_params template) = List.length ts
          ->
            List.iter (validate_ty node) ts
        | Some _ -> type_error cn "Wrong number of nominal type arguments."
        | None -> type_error cn "Unknown generic class or interface.")
    | TRef (RArray (ty, sz)) ->
        validate_array_length node sz;
        validate_ty node ty
    | TRef (RFun (args, ret)) ->
        List.iter (validate_ty node) args;
        validate_ret node ret
    | ty -> typecheck_ty node tc ty
  and validate_ret node = function
    | RetVoid -> ()
    | RetVal ty -> validate_ty node ty
  in
  let validate node params args ret annotations =
    Generics.validate_params node params args ret annotations;
    List.iter (fun (ty, _) -> validate_ty node ty) args;
    validate_ret node ret
  in
  List.iter
    (fun (fn : fdecl node) ->
      if Hashtbl.mem seen fn.elt.fname then
        type_error fn ("Function " ^ fn.elt.fname ^ " already defined.");
      Hashtbl.add seen fn.elt.fname fn;
      if fn.elt.type_params <> [] then (
        if fn.elt.fname = "main" then type_error fn "main cannot be generic.";
        validate fn fn.elt.type_params fn.elt.args fn.elt.frtyp
          fn.elt.annotations))
    fns;
  let seen_protos = Hashtbl.create (List.length pns) in
  List.iter
    (fun (pn : proto node) ->
      if pn.elt.type_params <> [] then (
        if Hashtbl.mem seen_protos pn.elt.fname then
          type_error pn "Duplicate generic prototype.";
        Hashtbl.add seen_protos pn.elt.fname ();
        validate pn pn.elt.type_params pn.elt.args pn.elt.frtyp
          pn.elt.annotations);
      match Hashtbl.find_opt seen pn.elt.fname with
      | Some fn when pn.elt.type_params <> [] || fn.elt.type_params <> [] ->
          if not (Generics.prototype_matches pn fn) then
            type_error pn
              ("Generic prototype does not match definition of " ^ pn.elt.fname
             ^ ".")
      | None when pn.elt.type_params <> [] ->
          type_error pn ("Undefined generic function " ^ pn.elt.fname ^ ".")
      | _ -> ())
    pns;
  let templates =
    List.filter_map
      (fun (fn : fdecl node) ->
        if fn.elt.type_params = [] then None else Some (fn.elt.fname, fn))
      fns
  in
  { tc with generics = Some (Generics.create ~nominal_templates templates) }

let generic_error_message loc msg =
  try type_error { elt = (); loc } msg with TypeError msg -> msg

let type_generic_instances tc =
  let state = Option.get tc.generics in
  let rec aux typed_funs =
    if Queue.is_empty state.pending then List.rev typed_funs
    else
      let instance = Queue.take state.pending in
      state.origins := instance.origins;
      let failure msg =
        let trace =
          List.map
            (fun (origin : Generics.origin) ->
              "  " ^ origin.name ^ " requested at "
              ^ Util.Range.string_of_range origin.loc)
            instance.origins
        in
        raise
          (TypeError (msg ^ "\nWhile specializing:\n" ^ String.concat "\n" trace))
      in
      let fn =
        try type_fn tc instance.fn with
        | TypeError msg -> failure msg
        | Generics.GenericError (loc, msg) | Generics.CannotInfer (loc, msg) ->
            failure (generic_error_message loc msg)
      in
      state.origins := [];
      aux (fn :: typed_funs)
  in
  aux []

let create_interface_name_ctxt tc declarations =
  List.fold_left
    (fun tc (i : idecl node) ->
      if
        Option.is_some (lookup_interface_option i.elt.iname tc)
        || Option.is_some (lookup_class_option i.elt.iname tc)
      then type_error i ("Duplicate interface or class " ^ i.elt.iname ^ ".");
      { tc with interfaces = (i.elt.iname, []) :: tc.interfaces })
    tc declarations

let create_interface_ctxt ?(register_names = true) tc declarations =
  let tc =
    if register_names then create_interface_name_ctxt tc declarations else tc
  in
  List.fold_left
    (fun tc (i : idecl node) ->
      let seen = Hashtbl.create 8 in
      let methods =
        List.map
          (fun (p : proto node) ->
            if Hashtbl.mem seen p.elt.fname then
              type_error p ("Duplicate interface method " ^ p.elt.fname ^ ".");
            Hashtbl.add seen p.elt.fname ();
            if p.elt.annotations <> [] then
              type_error p "Interface prototypes cannot have annotations.";
            let args, ret =
              validate_and_convert_signature p tc p.elt.args p.elt.frtyp
            in
            (p.elt.fname, ret, args))
          i.elt.protos
      in
      {
        tc with
        interfaces =
          (i.elt.iname, methods) :: List.remove_assoc i.elt.iname tc.interfaces;
      })
    tc declarations

let validate_implementations tc classes =
  List.fold_left
    (fun tc (c : cdecl node) ->
      let seen = Hashtbl.create 8 in
      let method_names = Hashtbl.create 8 in
      List.iter
        (fun (m : fdecl node) ->
          if Hashtbl.mem method_names m.elt.fname then
            type_error m ("Duplicate class method " ^ m.elt.fname ^ ".");
          Hashtbl.add method_names m.elt.fname ())
        c.elt.methods;
      let names =
        List.map
          (fun name ->
            let iname =
              match name with
              | TRef (RClass name) -> resolved_class_name name
              | _ -> type_error c "Unresolved implemented interface."
            in
            if Hashtbl.mem seen iname then
              type_error c ("Duplicate implemented interface " ^ iname ^ ".");
            Hashtbl.add seen iname ();
            let required =
              match lookup_interface_option iname tc with
              | Some methods -> methods
              | None -> type_error c ("Unknown interface " ^ iname ^ ".")
            in
            List.iter
              (fun (method_name, ret, args) ->
                match
                  List.find_opt
                    (fun (m : fdecl node) ->
                      m.elt.fname = method_name && m.elt.fname <> c.elt.cname)
                    c.elt.methods
                with
                | None ->
                    type_error c
                      ("Class " ^ c.elt.cname ^ " is missing interface method "
                     ^ method_name ^ " from " ^ iname ^ ".")
                | Some method_node ->
                    let actual_args, actual_ret =
                      validate_and_convert_signature method_node tc
                        method_node.elt.args method_node.elt.frtyp
                    in
                    if
                      not
                        (lists_equal_exact equal_ty (List.map fst args)
                           (List.map fst actual_args)
                        && equal_ret_ty ret actual_ret)
                    then
                      type_error method_node
                        ("Interface method " ^ method_name
                       ^ " signature does not match " ^ iname ^ "."))
              required;
            iname)
          c.elt.impls
      in
      { tc with implementations = (c.elt.cname, names) :: tc.implementations })
    tc classes

let create_nominal_ctxt tc =
  let state = Option.get tc.generics in
  let concrete_ty ty = resolve_interface_ty tc (Conversions.convert_ty ty) in
  let rec resolve_ty = function
    | TRef (RDArray ty) -> TRef (RDArray (resolve_ty ty))
    | TRef (RArray (ty, sz)) -> TRef (RArray (resolve_ty ty, sz))
    | TRef (RFun (args, ret)) ->
        TRef (RFun (List.map resolve_ty args, resolve_ret ret))
    | TRef (RClass cn) as ty -> (
        match
          List.assoc_opt (resolved_class_name cn) state.nominal_templates
        with
        | Some _ ->
            type_error cn "Generic class or interface requires type arguments."
        | None -> ty)
    | TRef (RGeneric (cn, args)) ->
        let origin = resolved_class_name cn in
        let template =
          match List.assoc_opt origin state.nominal_templates with
          | Some template -> template
          | None ->
              type_error cn
                ("Unknown generic class or interface " ^ origin ^ ".")
        in
        let params = Generics.nominal_params template in
        if List.length params <> List.length args then
          type_error cn "Wrong number of nominal type arguments.";
        let args = List.map resolve_ty args in
        let tys = List.map concrete_ty args in
        let name = Generics.nominal_name origin tys in
        let nominal_ty =
          TRef (RClass (unqualified_name { cn.elt.name with elt = name }))
        in
        (match Generics.lookup_nominal state name with
        | Some _ -> ()
        | None ->
            let previous = !(state.nominal_origins) in
            let display =
              origin ^ "<"
              ^ String.concat ", " (List.map Printer.show_ty tys)
              ^ ">"
            in
            let origins = previous @ [ display ] in
            if
              List.length origins > 128
              || List.length !(state.nominal_instances) >= 4096
            then
              type_error cn
                ("Generic nominal specialization limit exceeded: "
                ^ String.concat " -> " origins
                ^ ".");
            state.nominal_origins := origins;
            state.nominal_instances :=
              (name, (origin, tys)) :: !(state.nominal_instances);
            let bindings = List.map2 (fun p ty -> (p.elt, ty)) params tys in
            let ty ty = resolve_ty (Generics.subst_ty bindings ty) in
            (match template with
            | Generics.ClassTemplate source ->
                state.class_headers :=
                  (name, ([], [])) :: !(state.class_headers);
                let instance = Ast.map_cdecl_types ty source in
                let methods =
                  List.map
                    (fun (fn : fdecl node) ->
                      if fn.elt.fname = origin then
                        { fn with elt = { fn.elt with fname = name } }
                      else fn)
                    instance.elt.methods
                in
                let instance =
                  {
                    instance with
                    elt =
                      {
                        instance.elt with
                        type_params = [];
                        cname = name;
                        methods;
                      };
                  }
                in
                let headers = create_class_header_ctxt tc [ instance ] in
                let header = Option.get (List.assoc_opt name headers.classes) in
                state.class_headers :=
                  (name, header)
                  :: List.remove_assoc name !(state.class_headers);
                let implementations =
                  validate_implementations tc [ instance ]
                in
                let impls =
                  Option.value
                    (List.assoc_opt name implementations.implementations)
                    ~default:[]
                in
                state.implementations :=
                  (name, impls) :: !(state.implementations);
                state.nominal_classes := instance :: !(state.nominal_classes)
            | Generics.InterfaceTemplate source ->
                state.interface_headers :=
                  (name, []) :: !(state.interface_headers);
                let instance = Ast.map_idecl_types ty source in
                let instance =
                  {
                    instance with
                    elt = { instance.elt with type_params = []; iname = name };
                  }
                in
                let seen = Hashtbl.create 8 in
                let methods =
                  List.map
                    (fun (p : proto node) ->
                      if p.elt.type_params <> [] then
                        type_error p
                          "Generic interface methods are not supported.";
                      if p.elt.annotations <> [] then
                        type_error p
                          "Interface prototypes cannot have annotations.";
                      if Hashtbl.mem seen p.elt.fname then
                        type_error p "Duplicate interface method.";
                      Hashtbl.add seen p.elt.fname ();
                      let args, ret =
                        validate_and_convert_signature p tc p.elt.args
                          p.elt.frtyp
                      in
                      (p.elt.fname, ret, args))
                    instance.elt.protos
                in
                state.interface_headers :=
                  (name, methods)
                  :: List.remove_assoc name !(state.interface_headers);
                state.nominal_interfaces :=
                  instance :: !(state.nominal_interfaces));
            state.nominal_origins := previous);
        nominal_ty
    | ty -> ty
  and resolve_ret = function
    | RetVoid -> RetVoid
    | RetVal ty -> RetVal (resolve_ty ty)
  in
  state.resolve_ty <- resolve_ty;
  state.convert_ty <- (fun ty -> concrete_ty (resolve_ty ty));
  tc

let type_program ?(optimization_level = Util.Optimization_level.default)
    (prog : Ast.program) : Typed_ast.program =
  (* create global var ctxt *)
  let (Prog (imports, _)) = prog in
  (match imports with
  | import :: _ -> type_error import "Imports must be resolved before typing."
  | [] -> ());
  let fns, cns, pns, gds = Ast.partition_declarations prog in
  let fns = List.map Ast.bind_fdecl_params fns in
  let pns = List.map Ast.bind_proto_params pns in
  let cns = List.map Ast.bind_cdecl_params cns in
  let interfaces =
    List.map Ast.bind_idecl_params (Ast.interface_declarations prog)
  in
  let nominal_templates =
    List.filter_map
      (fun (cn : cdecl node) ->
        if cn.elt.type_params = [] then None
        else Some (cn.elt.cname, Generics.ClassTemplate cn))
      cns
    @ List.filter_map
        (fun (i : idecl node) ->
          if i.elt.type_params = [] then None
          else Some (i.elt.iname, Generics.InterfaceTemplate i))
        interfaces
  in
  let names = Hashtbl.create 16 in
  let bind_name node name =
    if Hashtbl.mem names name then
      type_error node ("Duplicate interface or class " ^ name ^ ".");
    Hashtbl.add names name ()
  in
  List.iter
    (fun (cn : cdecl node) ->
      bind_name cn cn.elt.cname;
      Generics.validate_param_names cn.elt.type_params;
      List.iter
        (fun (fn : fdecl node) ->
          if fn.elt.type_params <> [] then
            type_error fn "Generic methods are not supported.")
        cn.elt.methods)
    cns;
  List.iter
    (fun (i : idecl node) ->
      bind_name i i.elt.iname;
      Generics.validate_param_names i.elt.type_params;
      List.iter
        (fun (p : proto node) ->
          if p.elt.type_params <> [] then
            type_error p "Generic interface methods are not supported.")
        i.elt.protos)
    interfaces;
  let cns =
    List.filter (fun (cn : cdecl node) -> cn.elt.type_params = []) cns
  in
  let interfaces =
    List.filter (fun (i : idecl node) -> i.elt.type_params = []) interfaces
  in
  let class_names = create_class_name_ctxt Tctxt.empty cns in
  let class_names = create_interface_name_ctxt class_names interfaces in
  let class_names =
    create_generic_ctxt ~nominal_templates class_names fns pns
  in
  let class_names = create_nominal_ctxt class_names in
  let fns =
    List.filter (fun (fn : fdecl node) -> fn.elt.type_params = []) fns
  in
  let pns =
    List.filter (fun (pn : proto node) -> pn.elt.type_params = []) pns
  in
  let interface_ctxt =
    create_interface_ctxt ~register_names:false class_names interfaces
  in
  let state = Option.get interface_ctxt.generics in
  let class_names = interface_ctxt in
  ignore (create_nominal_ctxt class_names);
  let cns = List.map (Ast.map_cdecl_types state.resolve_ty) cns in
  let class_headers = create_class_header_ctxt class_names cns in
  let class_headers = validate_implementations class_headers cns in
  state.implementations :=
    class_headers.implementations @ !(state.implementations);
  let pns = reconcile_external_prototypes class_headers pns in
  let pc = create_proto_ctxt class_headers pns in
  let fc = create_fn_ctxt pc fns in
  check_undefined_protos fc;
  let fc, typed_globals = type_globals fc gds in
  let fc, classes_with_fields = create_class_ctxt fc cns in
  let typed_classes =
    List.map (fun (cn, fields) -> type_class fc fields cn) classes_with_fields
  in
  let typed_protos =
    List.filter_map
      (fun pn ->
        match lookup_global_option pn.elt.fname fc with
        | Some _ -> None
        | None -> Some (type_proto fc pn))
      pns
  in
  ignore (create_nominal_ctxt fc);
  let typed_funs = List.map (type_fn fc) fns in
  let generic_funs = ref [] in
  let nominal_classes = ref [] in
  let complete_classes = ref [] in
  let rec drain () =
    let new_funs = type_generic_instances fc in
    generic_funs := !generic_funs @ new_funs;
    let new_classes =
      List.filter
        (fun (cn : cdecl node) -> not (List.mem cn.elt.cname !complete_classes))
        !(state.nominal_classes)
    in
    List.iter
      (fun cn ->
        ignore (validate_implementations fc [ cn ]);
        complete_classes := cn.elt.cname :: !complete_classes;
        let context, fields = create_class_ctxt fc [ cn ] in
        let header = Option.get (List.assoc_opt cn.elt.cname context.classes) in
        state.class_headers :=
          (cn.elt.cname, header)
          :: List.remove_assoc cn.elt.cname !(state.class_headers);
        nominal_classes :=
          !nominal_classes @ [ type_class fc (snd (List.hd fields)) cn ])
      new_classes;
    if
      (not (Queue.is_empty state.pending))
      || List.length !complete_classes < List.length !(state.nominal_classes)
    then drain ()
  in
  drain ();
  Prog
    ( optimization_level,
      typed_funs @ !generic_funs,
      typed_classes @ !nominal_classes,
      typed_protos,
      typed_globals,
      List.map
        (fun (i : idecl node) ->
          (i.elt.iname, List.map (type_proto fc) i.elt.protos))
        (interfaces @ List.rev !(state.nominal_interfaces)) )

let type_prog ?(optimization_level = Util.Optimization_level.default)
    (prog : Ast.program) : (Typed_ast.program, Core.Error.t) result =
  try Ok (type_program ~optimization_level prog) with
  | Generics.GenericError (loc, msg) | Generics.CannotInfer (loc, msg) ->
      let err = Fmt.str "Type Error: %s" (generic_error_message loc msg) in
      Error (Core.Error.of_string err)
  | TypeError msg ->
      let err = Fmt.str "Type Error: %s" msg in
      Error (Core.Error.of_string err)
  | exn ->
      let err =
        Fmt.str "Internal Typechecker Error: %s" (Printexc.to_string exn)
      in
      Error (Core.Error.of_string err)
