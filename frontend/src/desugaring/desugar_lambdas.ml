open Desugared_ast
open Desugar_util

(** Replace a function value with its closure-struct representation. The caller
    is responsible for adding the returned declaration to the program. *)
let transform_ty (t : ty) (cs : cdecl list) : ty * cdecl option =
  (* TODO make this recursively check nested types *)
  match t with
  | TRef (RFun (arg_tys, rty) as r) ->
      let lname = mangle_lambda r in
      let lstruct_name = lambda_struct_name lname in
      (* just return cdecl corresponding to this lstruct instead of whole list *)
      let cd =
        match List.find_opt (fun c -> c.cname = lstruct_name) cs with
        | Some lcdecl -> lcdecl
        | None -> create_lambda_struct lstruct_name arg_tys rty
      in
      (TRef (RClass lstruct_name), Some cd)
  | _ -> (t, None)

let transform_ret_ty (rt : ret_ty) (cs : cdecl list) : ret_ty * cdecl option =
  match rt with
  | RetVal t ->
      let t', cd_opt = transform_ty t cs in
      (RetVal t', cd_opt)
  | RetVoid -> (RetVoid, None)

type lifted_lambda = {
  structs : cdecl list;
  function_decl : fdecl;
  setup : stmt list;
  value : id * ty;
  function_pointer : id;
  environment : id;
}

type lambda_converter = {
  closure_var : id;
  closure_ty : ty;
  function_pointer_ty : ty;
}

(** Project a field from the current closure value. The closure, rather than
    projections cached at its declaration, is the source of truth so
    assignments remain visible across control-flow joins. *)
let project_lambda_field converter suffix field_name field_ty =
  let projection = gensym (converter.closure_var ^ suffix) in
  ( Decl
      ( projection,
        field_ty,
        Proj
          ( Id (converter.closure_var, converter.closure_ty),
            field_name,
            field_ty ),
        true ),
    projection )

let project_lambda_converter converter =
  let function_pointer_decl, function_pointer =
    project_lambda_field converter "_fptr" "lambdaptr"
      converter.function_pointer_ty
  in
  let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
  let environment_decl, environment =
    project_lambda_field converter "_env" "envptr" i8_ptr
  in
  ([ function_pointer_decl; environment_decl ], function_pointer, environment)

let add_cdecl (cd : cdecl) (cs : cdecl list) : cdecl list =
  if List.exists (fun c -> c.cname = cd.cname) cs then cs else cd :: cs

let add_cdecls (new_cs : cdecl list) (old_cs : cdecl list) : cdecl list =
  List.fold_left (fun acc cd -> add_cdecl cd acc) old_cs new_cs

(** Closure conversion produces:
    - an environment struct containing captured values;
    - a lifted function whose first argument is an opaque environment pointer;
    - a closure value containing the environment and function pointers. *)
let rec lift_lambda (cs : cdecl list) (vname_opt : id option)
    (scope, args, rty, body) : lifted_lambda =
  let r = RFun (List.map snd args, rty) in
  let lty = TRef r in
  (* get the lambda struct we are desugaring down to *)
  let lstruct_ty, lstruct_cdecl_opt = transform_ty lty cs in
  let lstruct_cdecl =
    match lstruct_cdecl_opt with
    | Some l -> l
    | None ->
        desugar_error "impossible state - type is guaranteed to be a function"
  in
  (* our base lambda name - need to make it unique with syms *)
  let lname = mangle_lambda r in
  let sym = lambdasym lname in
  (* lifted unique lambda env struct name *)
  let lifted_lambda_scope = lambda_env_struct_name sym in
  (* set the fields of the env struct to the scope vars of the lambda *)
  let env_fields =
    List.map
      (fun (i, t) ->
        { prelude = []; fieldName = i; ftyp = t; init = create_default_init t })
      scope
  in
  (* create env struct *)
  let lambda_env =
    { cname = lifted_lambda_scope; fields = env_fields; annotations = [] }
  in
  (* the lambda must have been assigned to a variable after initial desugaring pass *)
  let vname = match vname_opt with Some v -> v | None -> gensym "lambda" in
  let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
  let vname_env = vname ^ ".env" in
  let env_ty = TRef (RClass lifted_lambda_scope) in
  let env_ptr_ty = create_ptr_to env_ty in
  let i8_name = vname_env ^ "i8" in
  let set_lambda_env_fields = List.map (fun (i, t) -> (i, Id (i, t))) scope in
  let lambda_env_decl =
    (* declare new lambda env struct instance *)
    Decl
      ( vname_env,
        i8_ptr,
        Cast (ObjInit (lifted_lambda_scope, set_lambda_env_fields), i8_ptr),
        true )
  in
  (* load the lambda scope variables from the lifted env struct *)
  (* this lifted env struct is passed to lambda (1) *)
  let captured_var_decls =
    List.map
      (fun (i, t) -> Decl (i, t, Proj (Id (vname_env, env_ty), i, t), false))
      scope
  in
  let body' =
    Decl (vname_env, env_ptr_ty, Cast (Id (i8_name, i8_ptr), env_ptr_ty), false)
    :: captured_var_decls
    @ body
  in
  let lifted_lambda_fname = lifted_lambda_name sym in
  let lifted_fn =
    {
      frtyp = rty;
      fname = lifted_lambda_fname;
      (* (1) add the env to the params *)
      args = (i8_ptr, i8_name) :: List.map (fun (i, t) -> (t, i)) args;
      body = body';
      annotations = [];
      inline = true;
    }
  in
  (* set the fields of the lambda struct *)
  let lambda_ptr_ty = create_ptr_to lty in
  let set_lambda_struct_fields =
    [
      ("envptr", Id (vname_env, i8_ptr));
      ("lambdaptr", Id (lifted_lambda_fname, lambda_ptr_ty));
    ]
  in
  let setup =
    [
      lambda_env_decl;
      Decl
        ( vname,
          lstruct_ty,
          ObjInit (lstruct_cdecl.cname, set_lambda_struct_fields),
          false );
    ]
  in
  {
    structs = [ lambda_env; lstruct_cdecl ];
    function_decl = lifted_fn;
    setup;
    value = (vname, lstruct_ty);
    function_pointer = lifted_lambda_fname;
    environment = vname_env;
  }

and lift_lambdas_from_list (lctxt : (id * lambda_converter) list)
    (vname_opt : id option) (cs_acc, fs_acc, stmts_acc) (e : exp) =
  let ncs, nfs, ns, _lambda_opt, _fptr_opt, ne, _env =
    lift_lambdas_from_exps cs_acc lctxt vname_opt e
  in
  ((ncs, nfs @ fs_acc, stmts_acc @ ns), ne)

and lift_partial_application cs lctxt vname_opt callee bound_args bound_types
    remaining_types rty =
  match callee with
  | PartialValue
      (PartialApply
        ( inner_callee,
          inner_bound_args,
          inner_bound_types,
          _inner_remaining_types,
          _inner_rty )) ->
      (* Chained partial applications capture the same original callee. Fold
         their bound prefixes together so lowering allocates one closure and
         one environment rather than an unreachable chain of wrappers. *)
      lift_partial_application cs lctxt vname_opt inner_callee
        (inner_bound_args @ bound_args)
        (inner_bound_types @ bound_types)
        remaining_types rty
  | _ ->
      lift_single_partial_application cs lctxt vname_opt callee bound_args
        bound_types remaining_types rty

and lift_single_partial_application cs lctxt vname_opt callee bound_args
    bound_types remaining_types rty =
  let full_arg_types = bound_types @ remaining_types in
  let original_fun_ty = TRef (RFun (full_arg_types, rty)) in
  let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
  let fptr_ty = create_ptr_to original_fun_ty in
  let prepare_indirect fptr_name env_name cs fs stmts =
    ( cs,
      fs,
      stmts,
      [ (fptr_name, fptr_ty); (env_name, i8_ptr) ],
      fptr_name,
      [ Id (env_name, i8_ptr) ] )
  in
  let callee_cs, callee_fs, callee_stmts, callee_scope, call_name, call_prefix =
    match callee with
    | PartialNamed name -> (
        match List.assoc_opt name lctxt with
        | Some converter ->
            let projection_stmts, function_pointer, environment =
              project_lambda_converter converter
            in
            prepare_indirect function_pointer environment cs [] projection_stmts
        | None -> (cs, [], [], [], name, []))
    | PartialMethod (receiver, method_name, class_name) ->
        let receiver_cs, receiver_fs, receiver_stmts, _, _, receiver', _ =
          lift_lambdas_from_exps cs lctxt None receiver
        in
        let receiver_ty = TRef (RClass class_name) in
        let receiver_name = gensym "partial_receiver" in
        let receiver_decl =
          Decl (receiver_name, receiver_ty, receiver', true)
        in
        let mangled_name =
          mangle_name ~enclosing_class:class_name method_name
            (receiver_ty :: full_arg_types)
            rty
        in
        ( receiver_cs,
          receiver_fs,
          receiver_stmts @ [ receiver_decl ],
          [ (receiver_name, receiver_ty) ],
          mangled_name,
          [ Id (receiver_name, receiver_ty) ] )
    | PartialValue value -> (
        let value_cs, value_fs, value_stmts, _, fptr_opt, value', env_opt =
          lift_lambdas_from_exps cs lctxt None value
        in
        match (fptr_opt, env_opt) with
        | Some fptr_name, Some env_name ->
            prepare_indirect fptr_name env_name value_cs value_fs value_stmts
        | _ ->
            let closure_ty, closure_decl_opt =
              transform_ty original_fun_ty value_cs
            in
            let value_cs =
              match closure_decl_opt with
              | Some declaration -> add_cdecl declaration value_cs
              | None -> value_cs
            in
            let closure_name = gensym "partial_callee" in
            let fptr_name = gensym "partial_fptr" in
            let env_name = gensym "partial_env" in
            let extraction_stmts =
              [
                Decl (closure_name, closure_ty, value', true);
                Decl
                  ( fptr_name,
                    fptr_ty,
                    Proj (Id (closure_name, closure_ty), "lambdaptr", fptr_ty),
                    true );
                Decl
                  ( env_name,
                    i8_ptr,
                    Proj (Id (closure_name, closure_ty), "envptr", i8_ptr),
                    true );
              ]
            in
            prepare_indirect fptr_name env_name value_cs value_fs
              (value_stmts @ extraction_stmts))
  in
  let (arg_cs, arg_fs, arg_stmts), bound_args' =
    List.fold_left_map
      (lift_lambdas_from_list lctxt None)
      (callee_cs, [], []) bound_args
  in
  let bound_cs, bound_decls, bound_scope, bound_ids =
    List.fold_left2
      (fun (cs_acc, decls, scope, ids) bound_ty bound_value ->
        let capture_ty, capture_decl_opt = transform_ty bound_ty cs_acc in
        let cs_acc =
          match capture_decl_opt with
          | Some declaration -> add_cdecl declaration cs_acc
          | None -> cs_acc
        in
        let name = gensym "partial_bound" in
        ( cs_acc,
          decls @ [ Decl (name, capture_ty, bound_value, true) ],
          scope @ [ (name, capture_ty) ],
          ids @ [ Id (name, capture_ty) ] ))
      (arg_cs, [], [], []) bound_types bound_args'
  in
  let remaining_args =
    List.map (fun ty -> (gensym "partial_arg", ty)) remaining_types
  in
  let remaining_ids =
    List.map (fun (name, ty) -> Id (name, ty)) remaining_args
  in
  let call_args = call_prefix @ bound_ids @ remaining_ids in
  let wrapper_body =
    match rty with
    | RetVoid -> [ SCall (call_name, call_args) ]
    | RetVal result_ty ->
        [ Ret (Some (Call (call_name, call_args, result_ty))) ]
  in
  let result =
    lift_lambda bound_cs vname_opt
      (callee_scope @ bound_scope, remaining_args, rty, wrapper_body)
  in
  let outer_cs = add_cdecls result.structs bound_cs in
  let final_cs, lifted_functions =
    lift_lambda_from_fdecl outer_cs result.function_decl
  in
  ( final_cs,
    callee_fs @ arg_fs @ lifted_functions,
    callee_stmts @ arg_stmts @ bound_decls @ result.setup,
    Some result.value,
    Some result.function_pointer,
    Id (fst result.value, snd result.value),
    Some result.environment )

and lift_lambdas_from_exps (cs : cdecl list)
    (lctxt : (id * lambda_converter) list) (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      let res = lift_lambda cs vname_opt (scope, args, rty, body) in
      let outer_cs = add_cdecls res.structs cs in
      (* A lifted lambda is an ordinary function declaration. Running it through
         the function-level pass recursively lifts any lambdas in its body and
         prepares lambda-valued parameters and return values in exactly the same
         way as a source-level function. *)
      let final_cs, lifted_fdecls =
        lift_lambda_from_fdecl outer_cs res.function_decl
      in
      ( final_cs,
        lifted_fdecls,
        res.setup,
        Some res.value,
        Some res.function_pointer,
        Id (fst res.value, snd res.value),
        Some res.environment )
  | PartialApply (callee, bound_args, bound_types, remaining_types, rty) ->
      lift_partial_application cs lctxt vname_opt callee bound_args bound_types
        remaining_types rty
  | Call (callee, es, ty) -> (
      let (ncs, nfs, nstmts), es' =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs, [], []) es
      in
      match List.assoc_opt callee lctxt with
      | Some cnv ->
          (* local lambda in lctxt *)
          let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
          let projection_stmts, function_pointer, environment =
            project_lambda_converter cnv
          in
          let transformed_call =
            Call (function_pointer, Id (environment, i8_ptr) :: es', ty)
          in
          ( ncs,
            nfs,
            nstmts @ projection_stmts,
            None,
            Some function_pointer,
            transformed_call,
            Some environment )
      | None -> (
          let base_call = Call (callee, es', ty) in
          match ty with
          | TRef (RFun (arg_tys, rty)) ->
              (* calling function that returns a lambda *)
              let tmp_v = "%fn" in
              let fptr_v = gensym (tmp_v ^ "_fptr") in
              let env_v = gensym (tmp_v ^ "_env") in
              let struct_ty, _ = transform_ty ty ncs in
              let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
              let l_ptr_ty = create_ptr_to (TRef (RFun (arg_tys, rty))) in

              let binding_stmts =
                [
                  Decl (tmp_v, struct_ty, base_call, true);
                  Decl
                    ( fptr_v,
                      l_ptr_ty,
                      Proj (Id (tmp_v, struct_ty), "lambdaptr", l_ptr_ty),
                      true );
                  Decl
                    ( env_v,
                      i8_ptr,
                      Proj (Id (tmp_v, struct_ty), "envptr", i8_ptr),
                      true );
                ]
              in
              ( ncs,
                nfs,
                nstmts @ binding_stmts,
                Some (tmp_v, struct_ty),
                Some fptr_v,
                Id (tmp_v, struct_ty),
                Some env_v )
          | _ -> (ncs, nfs, nstmts, None, None, base_call, None)))
  | Id (i, ty) as e -> (
      match List.assoc_opt i lctxt with
      | Some _ ->
          (* known lambda variable/parameter with converters in lctxt *)
          let ty', ncs_opt = transform_ty ty cs in
          let cs' =
            match ncs_opt with Some ncs -> add_cdecl ncs cs | None -> cs
          in
          (cs', [], [], None, None, Id (i, ty'), None)
      | None -> (
          match ty with
          | TRef (RFun (arg_tys, rty)) ->
              (*  global function being used as an rvalue *)
              let ty', ncs_opt = transform_ty ty cs in
              let cs' =
                match ncs_opt with Some ncs -> add_cdecl ncs cs | None -> cs
              in
              let struct_name =
                match ty' with TRef (RClass cn) -> cn | _ -> ""
              in

              (* thunk to box the global function *)
              let thunk_sym = lambdasym (i ^ "_thunk") in
              let thunk_name = lifted_lambda_name thunk_sym in
              let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in

              let arg_names =
                List.mapi (fun idx _ -> "a" ^ string_of_int idx) arg_tys
              in
              let thunk_args =
                (i8_ptr, gensym "unused_env") :: List.combine arg_tys arg_names
              in

              let call_exp =
                Call
                  ( i,
                    List.map2 (fun t n -> Id (n, t)) arg_tys arg_names,
                    match rty with RetVal t -> t | RetVoid -> TBool )
              in

              let thunk_fdecl =
                {
                  annotations = [];
                  frtyp = rty;
                  fname = thunk_name;
                  args = List.map (fun (t, n) -> (t, n)) thunk_args;
                  body =
                    [
                      (match rty with
                      | RetVoid ->
                          SCall
                            ( i,
                              List.map2 (fun t n -> Id (n, t)) arg_tys arg_names
                            )
                      | _ -> Ret (Some call_exp));
                    ];
                  inline = true;
                }
              in

              let tmp_v = gensym (i ^ "_val") in
              let env_v = gensym (i ^ "_env") in
              let l_ptr_ty = create_ptr_to ty in

              let stmts =
                [
                  (* global functions have no state, so env is Null *)
                  Decl (env_v, i8_ptr, Null i8_ptr, true);
                  Decl
                    ( tmp_v,
                      ty',
                      ObjInit
                        ( struct_name,
                          [
                            ("envptr", Id (env_v, i8_ptr));
                            ("lambdaptr", Id (thunk_name, l_ptr_ty));
                          ] ),
                      true );
                ]
              in

              ( cs',
                [ thunk_fdecl ],
                stmts,
                Some (tmp_v, ty'),
                Some thunk_name,
                Id (tmp_v, ty'),
                Some env_v )
          | _ -> (cs, [], [], None, None, e, None)))
  | Bop (b, e1, e2, ty) ->
      (* bop is never between lambdas, no need to transform_ty *)
      let cs1, fs1, ss1, _, _, e1', _ =
        lift_lambdas_from_exps cs lctxt None e1
      in
      let cs2, fs2, ss2, _, _, e2', _ =
        lift_lambdas_from_exps cs lctxt None e2
      in
      let cs' = add_cdecls cs2 (add_cdecls cs1 cs) in
      let fs' = fs1 @ fs2 in
      let ss = ss1 @ ss2 in
      (cs', fs', ss, None, None, Bop (b, e1', e2', ty), None)
  | Uop (u, e, ty) ->
      (* uop is never between lambdas, no need to transform_ty *)
      let ncs, nfs, ss, _, _, e', _ = lift_lambdas_from_exps cs lctxt None e in
      (add_cdecls ncs cs, nfs, ss, None, None, Uop (u, e', ty), None)
  | Index (coll, idx, ty) ->
      let ty', ncs_opt = transform_ty ty cs in
      let coll_cs, coll_fs, coll_ss, _, _, coll', _ =
        lift_lambdas_from_exps cs lctxt None coll
      in
      let idx_cs, idx_fs, idx_ss, _, _, idx', _ =
        lift_lambdas_from_exps cs lctxt None idx
      in
      let tcs =
        match ncs_opt with Some ncs -> add_cdecl ncs cs | None -> cs
      in
      let cs' = add_cdecls idx_cs (add_cdecls coll_cs tcs) in
      ( cs',
        coll_fs @ idx_fs,
        coll_ss @ idx_ss,
        None,
        None,
        Index (coll', idx', ty'),
        None )
  | Array (es, ty) ->
      let ty', nc_opt = transform_ty ty cs in
      let (ecs, efs, ess), es' =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs, [], []) es
      in
      let ncs = match nc_opt with Some nc -> add_cdecl nc cs | None -> cs in
      let cs' = add_cdecls ecs ncs in
      (cs', efs, ess, None, None, Array (es', ty'), None)
  | Cast _ as ce ->
      (* cannot cast lambdas *)
      (cs, [], [], None, None, ce, None)
  | Proj (e, i, ty) ->
      let ty', nc_opt = transform_ty ty cs in
      let ecs, efs, ess, _, _, e', _ = lift_lambdas_from_exps cs lctxt None e in
      let ncs = match nc_opt with Some nc -> add_cdecl nc cs | None -> cs in
      let cs' = add_cdecls ecs ncs in
      (cs', efs, ess, None, None, Proj (e', i, ty'), None)
  | ObjInit _ as oe ->
      (* no lambdas allowed as class fields *)
      (cs, [], [], None, None, oe, None)
  | (Bool _ | Int _ | Float _ | Str _ | Null _) as e ->
      (cs, [], [], None, None, e, None)

and lift_lambdas_from_stmt (cs : cdecl list) (fs : fdecl list)
    (lctxt : (id * lambda_converter) list) = function
  | Decl (vname, (TRef (RFun (args, rty)) as ty), e, const) ->
      (*  lift lambda from initialization exp *)
      let ncs, nfs, ns, _l, _fptr_opt, ne, _env_opt =
        lift_lambdas_from_exps cs lctxt (Some vname) e
      in
      (* update ty of decl if necessary *)
      let ty', dnc_opt = transform_ty ty ncs in
      let ns_with_decl =
        match ne with
        | Id (name, _) when name = vname -> ns
        | _ ->
            (* Box a global function when it initializes a local lambda value. *)
            ns @ [ Decl (vname, ty', ne, const) ]
      in
      let dcs = add_cdecls ncs cs in
      (* if this variable is a lambda, add it to our context for Call sites *)
      let nlctxt =
        ( vname,
          {
            closure_var = vname;
            closure_ty = ty';
            function_pointer_ty = create_ptr_to (TRef (RFun (args, rty)));
          } )
        :: lctxt
      in
      let cs' =
        match dnc_opt with Some dnc -> add_cdecl dnc dcs | None -> cs
      in
      (cs', nfs @ fs, nlctxt, ns_with_decl)
  | Decl (vname, ty, e, const) ->
      let cs', nfs, ns, _l, _fptr_opt, ne, _env_opt =
        lift_lambdas_from_exps cs lctxt (Some vname) e
      in
      (cs', nfs @ fs, lctxt, ns @ [ Decl (vname, ty, ne, const) ])
  | Ret rval -> (
      match rval with
      | Some e ->
          let cs', nfs, ns, _l, _fptr_opt, e', _env =
            lift_lambdas_from_exps cs lctxt None e
          in
          (cs', nfs @ fs, lctxt, ns @ [ Ret (Some e') ])
      | None -> (cs, fs, lctxt, [ Ret rval ]))
  | SCall (i, es) ->
      let (ecs, efs, ess), es' =
        List.fold_left_map (lift_lambdas_from_list lctxt None) (cs, [], []) es
      in
      let call_name, call_args, projection_stmts =
        match List.assoc_opt i lctxt with
        | Some converter ->
            let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
            let projection_stmts, function_pointer, environment =
              project_lambda_converter converter
            in
            (function_pointer, Id (environment, i8_ptr) :: es', projection_stmts)
        | None -> (i, es', [])
      in
      ( ecs,
        efs @ fs,
        lctxt,
        ess @ projection_stmts @ [ SCall (call_name, call_args) ] )
  | If (e, tb, eb) ->
      let ecs, efs, ess, _l, _fptr_opt, e', _env =
        lift_lambdas_from_exps cs lctxt None e
      in
      let tbcs, tbfs, tb' = lift_lambda_from_block cs lctxt tb in
      let ebcs, ebfs, eb' = lift_lambda_from_block tbcs lctxt eb in
      let cs' = add_cdecls ebcs (add_cdecls tbcs (add_cdecls ecs cs)) in
      (cs', efs @ tbfs @ ebfs @ fs, lctxt, ess @ [ If (e', tb', eb') ])
  | While (e, b) ->
      let ecs, efs, ess, _l, _, e', _ =
        lift_lambdas_from_exps cs lctxt None e
      in
      let cs' = add_cdecls ecs cs in
      let bcs, bfs, b' = lift_lambda_from_block cs' lctxt b in
      (add_cdecls bcs cs', efs @ bfs @ fs, lctxt, ess @ [ While (e', b') ])
  | Free es ->
      let final_cs, final_fs, all_setup_stmts, desugared_exps =
        List.fold_left
          (fun (cs_acc, fs_acc, stmts_acc, exps_acc) e ->
            let ncs, nfs, ns, _l_opt, _f_ptr_opt, e', env_opt =
              lift_lambdas_from_exps cs_acc lctxt None e
            in
            let ns, env_opt =
              match e with
              | Id (name, _) -> (
                  match List.assoc_opt name lctxt with
                  | Some converter ->
                      let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
                      let environment_decl, environment =
                        project_lambda_field converter "_env" "envptr" i8_ptr
                      in
                      (ns @ [ environment_decl ], Some environment)
                  | None -> (ns, env_opt))
              | _ -> (ns, env_opt)
            in
            let final_es =
              match env_opt with
              | Some env_ptr ->
                  let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
                  Id (env_ptr, i8_ptr) :: e' :: exps_acc
              | _ -> e' :: exps_acc
            in
            (ncs, fs_acc @ nfs, stmts_acc @ ns, final_es))
          (cs, fs, [], []) es
      in

      (final_cs, final_fs, lctxt, all_setup_stmts @ [ Free desugared_exps ])
  | Assn (l, r, ty) ->
      let lcs, lfs, lss, _, _, l', _ = lift_lambdas_from_exps cs lctxt None l in
      let rcs, rfs, rss, _, _fptr_opt, r', _env_opt =
        lift_lambdas_from_exps cs lctxt None r
      in

      let ty', tc_opt = transform_ty ty cs in
      let cs' = add_cdecls rcs (add_cdecls lcs cs) in
      let cs' = match tc_opt with Some tc -> add_cdecl tc cs' | None -> cs' in

      (cs', lfs @ rfs @ fs, lctxt, lss @ rss @ [ Assn (l', r', ty') ])
  | (Break | Continue) as s -> (cs, fs, lctxt, [ s ])

and lift_lambda_from_block cs lctxt block =
  let final_cs, lifted_fs, _, new_block =
    List.fold_left
      (fun (curr_cs, curr_fs, curr_lctxt, block_acc) stmt ->
        let next_cs, next_fs, next_lctxt, new_stmts =
          lift_lambdas_from_stmt curr_cs curr_fs curr_lctxt stmt
        in
        (next_cs, next_fs, next_lctxt, block_acc @ new_stmts))
      (cs, [], lctxt, []) block
  in
  (final_cs, lifted_fs, new_block)

and lift_lambda_from_fdecl (cs : cdecl list) (f : fdecl) :
    cdecl list * fdecl list =
  (* process arguments *)
  let lctxt_initial, args_transformed, cs_args =
    List.fold_right
      (fun (t, i) (lctxt_acc, args_acc, cs_acc) ->
        match t with
        | TRef (RFun (arg_tys, rty)) ->
            (* convert lambda type to struct *)
            let t', cd_opt = transform_ty t cs_acc in
            let cs' =
              match cd_opt with
              | Some cd -> add_cdecl cd cs_acc
              | None -> cs_acc
            in

            let lambda_ptr_ty = create_ptr_to (TRef (RFun (arg_tys, rty))) in
            ( ( i,
                {
                  closure_var = i;
                  closure_ty = t';
                  function_pointer_ty = lambda_ptr_ty;
                } )
              :: lctxt_acc,
              (t', i) :: args_acc,
              cs' )
        | _ -> (lctxt_acc, (t, i) :: args_acc, cs_acc))
      f.args ([], [], cs)
  in

  (* return type transformation *)
  let frtyp', cd_ret_opt = transform_ret_ty f.frtyp cs_args in
  let cs_ret =
    match cd_ret_opt with Some cd -> add_cdecl cd cs_args | None -> cs_args
  in

  let final_cs, lifted_fs, transformed_body =
    lift_lambda_from_block cs_ret lctxt_initial f.body
  in

  let new_fdecl =
    { f with frtyp = frtyp'; args = args_transformed; body = transformed_body }
  in

  (final_cs, new_fdecl :: lifted_fs)
