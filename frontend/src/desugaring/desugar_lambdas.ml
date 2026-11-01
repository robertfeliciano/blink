open Desugared_ast
open Desugar_util

let add_cdecl (cd : cdecl) (cs : cdecl list) : cdecl list =
  if List.exists (fun c -> c.cname = cd.cname) cs then cs else cd :: cs

let add_cdecls (new_cs : cdecl list) (old_cs : cdecl list) : cdecl list =
  List.fold_left (fun acc cd -> add_cdecl cd acc) old_cs new_cs

(** Convert function values wherever they occur in runtime aggregates. Raw
    function pointers keep their pointer representation, with converted value
    types in their signatures. Return every required closure declaration. *)
let rec transform_ty (t : ty) (cs : cdecl list) : ty * cdecl list =
  match t with
  | TRef (RFun (arg_tys, rty) as r) ->
      let lstruct_name = lambda_struct_name (mangle_lambda r) in
      let args, arg_cs = transform_tys arg_tys cs in
      let ret, ret_cs = transform_ret_ty rty (add_cdecls arg_cs cs) in
      let cd =
        match List.find_opt (fun c -> c.cname = lstruct_name) cs with
        | Some cd -> cd
        | None -> create_lambda_struct lstruct_name args ret
      in
      (TRef (RClass lstruct_name), add_cdecl cd (add_cdecls ret_cs arg_cs))
  | TTuple elements ->
      let elements, declarations = transform_tys elements cs in
      (TTuple elements, declarations)
  | TRef (RArray (element, size)) ->
      let element, declarations = transform_ty element cs in
      (TRef (RArray (element, size)), declarations)
  | TRef (RPtr (TRef (RFun (args, ret)))) ->
      let args, arg_cs = transform_tys args cs in
      let ret, ret_cs = transform_ret_ty ret (add_cdecls arg_cs cs) in
      (TRef (RPtr (TRef (RFun (args, ret)))), add_cdecls ret_cs arg_cs)
  | TRef (RPtr inner) ->
      let inner, declarations = transform_ty inner cs in
      (TRef (RPtr inner), declarations)
  | _ -> (t, [])

and transform_tys types cs =
  let declarations, types =
    List.fold_left_map
      (fun declarations ty ->
        let ty, more = transform_ty ty (add_cdecls declarations cs) in
        (add_cdecls more declarations, ty))
      [] types
  in
  (types, declarations)

and transform_ret_ty (rt : ret_ty) (cs : cdecl list) : ret_ty * cdecl list =
  match rt with
  | RetVal t ->
      let t, declarations = transform_ty t cs in
      (RetVal t, declarations)
  | RetVoid -> (RetVoid, [])

let closure_pointer_ty cs args ret =
  fst (transform_ty (create_ptr_to (TRef (RFun (args, ret)))) cs)

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
    projections cached at its declaration, is the source of truth so assignments
    remain visible across control-flow joins. *)
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

let lower_call_result cs fs stmts ty make_call =
  match ty with
  | TRef (RFun (arg_tys, rty)) ->
      let struct_ty, declaration = transform_ty ty cs in
      let cs = add_cdecls declaration cs in
      let value = gensym "returned_closure" in
      let function_pointer = gensym "returned_fptr" in
      let environment = gensym "returned_env" in
      let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
      let function_pointer_ty = closure_pointer_ty cs arg_tys rty in
      let setup =
        [
          Decl (value, struct_ty, make_call struct_ty, true);
          Decl
            ( function_pointer,
              function_pointer_ty,
              Proj (Id (value, struct_ty), "lambdaptr", function_pointer_ty),
              true );
          Decl
            ( environment,
              i8_ptr,
              Proj (Id (value, struct_ty), "envptr", i8_ptr),
              true );
        ]
      in
      ( cs,
        fs,
        stmts @ setup,
        Some (value, struct_ty),
        Some function_pointer,
        Id (value, struct_ty),
        Some environment )
  | _ ->
      let ty, declarations = transform_ty ty cs in
      (add_cdecls declarations cs, fs, stmts, None, None, make_call ty, None)

(** Closure conversion produces:
    - an environment struct containing captured values;
    - a lifted function whose first argument is an opaque environment pointer;
    - a closure value containing the environment and function pointers. *)
let rec lift_lambda ?captured_values (cs : cdecl list) (vname_opt : id option)
    (scope, args, rty, body) : lifted_lambda =
  (* Function captures are closure values in memory, not raw RFun fields. Keep
     their original types on the lifted local declarations so the normal
     function-level pass installs closure call converters for these locals. *)
  let cs, env_fields =
    List.fold_left_map
      (fun cs (name, capture_ty) ->
        let field_ty, declaration = transform_ty capture_ty cs in
        let cs = add_cdecls declaration cs in
        ( cs,
          {
            prelude = [];
            fieldName = name;
            ftyp = field_ty;
            init = create_default_init field_ty;
          } ))
      cs scope
  in
  let r = RFun (List.map snd args, rty) in
  let lty = TRef r in
  (* get the lambda struct we are desugaring down to *)
  let lstruct_ty, lstruct_declarations = transform_ty lty cs in
  let lstruct_cdecl =
    match lstruct_ty with
    | TRef (RClass name) ->
        List.find (fun cd -> cd.cname = name) lstruct_declarations
    | _ -> desugar_error "Expected a converted closure type."
  in
  (* our base lambda name - need to make it unique with syms *)
  let lname = mangle_lambda r in
  let sym = lambdasym lname in
  (* lifted unique lambda env struct name *)
  let lifted_lambda_scope = lambda_env_struct_name sym in
  (* set the fields of the env struct to the scope vars of the lambda *)
  (* create env struct *)
  let lambda_env =
    {
      cname = lifted_lambda_scope;
      fields = env_fields;
      annotations = [];
      impls = [];
    }
  in
  (* the lambda must have been assigned to a variable after initial desugaring pass *)
  let vname = match vname_opt with Some v -> v | None -> gensym "lambda" in
  let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
  let vname_env = vname ^ ".env" in
  let env_ty = TRef (RClass lifted_lambda_scope) in
  let env_ptr_ty = create_ptr_to env_ty in
  let i8_name = vname_env ^ "i8" in
  let set_lambda_env_fields =
    match captured_values with
    | Some values -> values
    | None -> List.map (fun (i, t) -> (i, Id (i, t))) scope
  in
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
  let lambda_ptr_ty = fst (transform_ty (create_ptr_to lty) cs) in
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
    structs = add_cdecl lambda_env (add_cdecls lstruct_declarations cs);
    function_decl = lifted_fn;
    setup;
    value = (vname, lstruct_ty);
    function_pointer = lifted_lambda_fname;
    environment = vname_env;
  }

and lift_ordered_exps cs lctxt vname_opt expressions =
  let (cs, fs), operands =
    List.fold_left_map
      (fun (cs, fs) expression ->
        let cs, more_fs, setup, _, _, value, _ =
          lift_lambdas_from_exps cs lctxt vname_opt expression
        in
        ((cs, fs @ more_fs), (setup, value)))
      (cs, []) expressions
  in
  let setup, values = sequence_expressions operands in
  ((cs, fs, setup), values)

and lift_lambdas_from_exps (cs : cdecl list)
    (lctxt : (id * lambda_converter) list) (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      (* Box global function captures using the same path as other function
         rvalues; already-local closures remain the existing closure value. *)
      let (cs, capture_fs, capture_stmts), captured_values =
        List.fold_left_map
          (fun (cs, fs, stmts) (name, ty) ->
            let cs, new_fs, setup, _, _, value, _ =
              lift_lambdas_from_exps cs lctxt None (Id (name, ty))
            in
            ((cs, fs @ new_fs, stmts @ setup), (name, value)))
          (cs, [], []) scope
      in
      let res =
        lift_lambda ~captured_values cs vname_opt (scope, args, rty, body)
      in
      let outer_cs = add_cdecls res.structs cs in
      (* A lifted lambda is an ordinary function declaration. Running it through
         the function-level pass recursively lifts any lambdas in its body and
         prepares lambda-valued parameters and return values in exactly the same
         way as a source-level function. *)
      let final_cs, lifted_fdecls =
        lift_lambda_from_fdecl outer_cs res.function_decl
      in
      ( final_cs,
        capture_fs @ lifted_fdecls,
        capture_stmts @ res.setup,
        Some res.value,
        Some res.function_pointer,
        Id (fst res.value, snd res.value),
        Some res.environment )
  | InterfaceCast (receiver, cname, iname) ->
      let ncs, nfs, ns, _, _, receiver, _ =
        lift_lambdas_from_exps cs lctxt None receiver
      in
      (ncs, nfs, ns, None, None, InterfaceCast (receiver, cname, iname), None)
  | InterfaceCall (receiver, slot, args, ty) ->
      let (ncs, nfs, ns), values =
        lift_ordered_exps cs lctxt None (receiver :: args)
      in
      let receiver, args =
        match values with
        | receiver :: args -> (receiver, args)
        | [] -> assert false
      in
      lower_call_result ncs nfs ns ty (fun ty ->
          InterfaceCall (receiver, slot, args, ty))
  | Call (callee, es, ty) -> (
      let (ncs, nfs, nstmts), es' = lift_ordered_exps cs lctxt vname_opt es in
      match List.assoc_opt callee lctxt with
      | Some cnv ->
          (* local lambda in lctxt *)
          let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
          let projection_stmts, function_pointer, environment =
            project_lambda_converter cnv
          in
          lower_call_result ncs nfs (projection_stmts @ nstmts) ty (fun ty ->
              Call (function_pointer, Id (environment, i8_ptr) :: es', ty))
      | None ->
          lower_call_result ncs nfs nstmts ty (fun ty -> Call (callee, es', ty))
      )
  | Id (i, ty) -> (
      match List.assoc_opt i lctxt with
      | Some _ ->
          (* known lambda variable/parameter with converters in lctxt *)
          let ty', ncs_opt = transform_ty ty cs in
          let cs' = add_cdecls ncs_opt cs in
          (cs', [], [], None, None, Id (i, ty'), None)
      | None -> (
          match ty with
          | TRef (RFun (arg_tys, rty)) ->
              (*  global function being used as an rvalue *)
              let ty', ncs_opt = transform_ty ty cs in
              let cs' = add_cdecls ncs_opt cs in
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
              let runtime_args, _ = transform_tys arg_tys cs' in
              let runtime_ret, _ = transform_ret_ty rty cs' in
              let thunk_args =
                (i8_ptr, gensym "unused_env")
                :: List.combine runtime_args arg_names
              in

              let call_exp =
                Call
                  ( i,
                    List.map2 (fun t n -> Id (n, t)) runtime_args arg_names,
                    match runtime_ret with RetVal t -> t | RetVoid -> TBool )
              in

              let thunk_fdecl =
                {
                  annotations = [];
                  frtyp = runtime_ret;
                  fname = thunk_name;
                  args = List.map (fun (t, n) -> (t, n)) thunk_args;
                  body =
                    [
                      (match rty with
                      | RetVoid ->
                          SCall
                            ( i,
                              List.map2
                                (fun t n -> Id (n, t))
                                runtime_args arg_names )
                      | _ -> Ret (Some call_exp));
                    ];
                  inline = true;
                }
              in

              let tmp_v = gensym (i ^ "_val") in
              let env_v = gensym (i ^ "_env") in
              let l_ptr_ty = fst (transform_ty (create_ptr_to ty) cs') in

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
          | _ ->
              let ty, declarations = transform_ty ty cs in
              (add_cdecls declarations cs, [], [], None, None, Id (i, ty), None)
          ))
  | Bop (b, e1, e2, ty) ->
      let (cs, fs, setup), values =
        lift_ordered_exps cs lctxt None [ e1; e2 ]
      in
      let e1, e2 =
        match values with [ e1; e2 ] -> (e1, e2) | _ -> assert false
      in
      (cs, fs, setup, None, None, Bop (b, e1, e2, ty), None)
  | Conditional (cond, (true_block, when_true), (false_block, when_false), ty)
    ->
      let cond_cs, cond_fs, cond_stmts, _, _, cond', _ =
        lift_lambdas_from_exps cs lctxt None cond
      in
      let lift_branch branch_cs block value =
        let block_cs, block_fs, branch_lctxt, block' =
          List.fold_left
            (fun (curr_cs, curr_fs, curr_lctxt, block_acc) stmt ->
              let next_cs, next_fs, next_lctxt, new_stmts =
                lift_lambdas_from_stmt curr_cs curr_fs curr_lctxt stmt
              in
              (next_cs, next_fs, next_lctxt, block_acc @ new_stmts))
            (branch_cs, [], lctxt, []) block
        in
        let value_cs, value_fs, value_stmts, _, _, value', _ =
          lift_lambdas_from_exps block_cs branch_lctxt None value
        in
        (value_cs, block_fs @ value_fs, (block' @ value_stmts, value'))
      in
      let true_cs, true_fs, true_branch =
        lift_branch cond_cs true_block when_true
      in
      let false_cs, false_fs, false_branch =
        lift_branch true_cs false_block when_false
      in
      let result_ty, result_cdecl = transform_ty ty false_cs in
      let final_cs = add_cdecls result_cdecl false_cs in
      ( final_cs,
        cond_fs @ true_fs @ false_fs,
        cond_stmts,
        None,
        None,
        Conditional (cond', true_branch, false_branch, result_ty),
        None )
  | Uop (u, e, ty) ->
      (* uop is never between lambdas, no need to transform_ty *)
      let ncs, nfs, ss, _, _, e', _ = lift_lambdas_from_exps cs lctxt None e in
      (add_cdecls ncs cs, nfs, ss, None, None, Uop (u, e', ty), None)
  | Index (coll, idx, ty) ->
      let (cs, fs, setup), values =
        lift_ordered_exps cs lctxt None [ coll; idx ]
      in
      let coll, idx =
        match values with [ coll; idx ] -> (coll, idx) | _ -> assert false
      in
      let ty, declarations = transform_ty ty cs in
      ( add_cdecls declarations cs,
        fs,
        setup,
        None,
        None,
        Index (coll, idx, ty),
        None )
  | Array (es, ty, on_heap) ->
      let ty', nc_opt = transform_ty ty cs in
      let (ecs, efs, ess), es' = lift_ordered_exps cs lctxt vname_opt es in
      let ncs = add_cdecls nc_opt cs in
      let cs' = add_cdecls ecs ncs in
      (cs', efs, ess, None, None, Array (es', ty', on_heap), None)
  | Tuple (elements, ty) ->
      let ty, declarations = transform_ty ty cs in
      let (cs, fs, setup), elements =
        lift_ordered_exps (add_cdecls declarations cs) lctxt None elements
      in
      (cs, fs, setup, None, None, Tuple (elements, ty), None)
  | TupleProj (tuple, index, ty) ->
      let cs, fs, setup, _, _, tuple, _ =
        lift_lambdas_from_exps cs lctxt None tuple
      in
      lower_call_result cs fs setup ty (fun ty -> TupleProj (tuple, index, ty))
  | Cast (value, ty) ->
      let cs, fs, setup, _, _, value, _ =
        lift_lambdas_from_exps cs lctxt None value
      in
      let ty, declarations = transform_ty ty cs in
      (add_cdecls declarations cs, fs, setup, None, None, Cast (value, ty), None)
  | InterfaceProj (e, slot, ty) ->
      let ty', nc_opt = transform_ty ty cs in
      let ecs, efs, ess, _, _, e', _ = lift_lambdas_from_exps cs lctxt None e in
      let ncs = add_cdecls nc_opt cs in
      let cs' = add_cdecls ecs ncs in
      (cs', efs, ess, None, None, InterfaceProj (e', slot, ty'), None)
  | Proj (e, i, ty) ->
      let ty', nc_opt = transform_ty ty cs in
      let ecs, efs, ess, _, _, e', _ = lift_lambdas_from_exps cs lctxt None e in
      let ncs = add_cdecls nc_opt cs in
      let cs' = add_cdecls ecs ncs in
      (cs', efs, ess, None, None, Proj (e', i, ty'), None)
  | ObjInit (name, fields) ->
      let names, values = List.split fields in
      let (cs, fs, setup), values = lift_ordered_exps cs lctxt None values in
      ( cs,
        fs,
        setup,
        None,
        None,
        ObjInit (name, List.combine names values),
        None )
  | Null ty ->
      let ty, declarations = transform_ty ty cs in
      (add_cdecls declarations cs, [], [], None, None, Null ty, None)
  | (Bool _ | Int _ | Float _ | Str _) as e -> (cs, [], [], None, None, e, None)

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
            function_pointer_ty = closure_pointer_ty ncs args rty;
          } )
        :: lctxt
      in
      let cs' = add_cdecls dnc_opt dcs in
      (cs', nfs @ fs, nlctxt, ns_with_decl)
  | Decl (vname, ty, e, const) ->
      let cs', nfs, ns, _l, _fptr_opt, ne, _env_opt =
        lift_lambdas_from_exps cs lctxt (Some vname) e
      in
      let ty, declarations = transform_ty ty cs' in
      ( add_cdecls declarations cs',
        nfs @ fs,
        lctxt,
        ns @ [ Decl (vname, ty, ne, const) ] )
  | Ret rval -> (
      match rval with
      | Some e ->
          let cs', nfs, ns, _l, _fptr_opt, e', _env =
            lift_lambdas_from_exps cs lctxt None e
          in
          (cs', nfs @ fs, lctxt, ns @ [ Ret (Some e') ])
      | None -> (cs, fs, lctxt, [ Ret rval ]))
  | InterfaceSCall (receiver, slot, args, ret) ->
      let (ncs, nfs, ns), values =
        lift_ordered_exps cs lctxt None (receiver :: args)
      in
      let receiver, args =
        match values with
        | receiver :: args -> (receiver, args)
        | [] -> assert false
      in
      let ret, declaration = transform_ret_ty ret ncs in
      let ncs = add_cdecls declaration ncs in
      (ncs, nfs @ fs, lctxt, ns @ [ InterfaceSCall (receiver, slot, args, ret) ])
  | SCall (i, es) ->
      let (ecs, efs, ess), es' = lift_ordered_exps cs lctxt None es in
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
        projection_stmts @ ess @ [ SCall (call_name, call_args) ] )
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
      (add_cdecls bcs cs', efs @ bfs @ fs, lctxt, [ lower_while ess e' b' ])
  | Free es ->
      let final_cs, final_fs, all_setup_stmts, rev_exps =
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
                  e' :: Id (env_ptr, i8_ptr) :: exps_acc
              | _ -> e' :: exps_acc
            in
            (ncs, fs_acc @ nfs, stmts_acc @ ns, final_es))
          (cs, fs, [], []) es
      in

      (* Restore source order, freeing each closure's environment first. *)
      (final_cs, final_fs, lctxt, all_setup_stmts @ [ Free (List.rev rev_exps) ])
  | Assn (l, r, ty) ->
      let lcs, lfs, lss, _, _, l', _ = lift_lambdas_from_exps cs lctxt None l in
      let rcs, rfs, rss, _, _fptr_opt, r', _env_opt =
        lift_lambdas_from_exps cs lctxt None r
      in

      let ty', tc_opt = transform_ty ty cs in
      let cs' = add_cdecls rcs (add_cdecls lcs cs) in
      let cs' = add_cdecls tc_opt cs' in

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
            let cs' = add_cdecls cd_opt cs_acc in

            let lambda_ptr_ty = closure_pointer_ty cs' arg_tys rty in
            ( ( i,
                {
                  closure_var = i;
                  closure_ty = t';
                  function_pointer_ty = lambda_ptr_ty;
                } )
              :: lctxt_acc,
              (t', i) :: args_acc,
              cs' )
        | _ ->
            let t, declarations = transform_ty t cs_acc in
            (lctxt_acc, (t, i) :: args_acc, add_cdecls declarations cs_acc))
      f.args ([], [], cs)
  in

  (* return type transformation *)
  let frtyp', cd_ret_opt = transform_ret_ty f.frtyp cs_args in
  let cs_ret = add_cdecls cd_ret_opt cs_args in

  let final_cs, lifted_fs, transformed_body =
    lift_lambda_from_block cs_ret lctxt_initial f.body
  in

  let new_fdecl =
    { f with frtyp = frtyp'; args = args_transformed; body = transformed_body }
  in

  (final_cs, new_fdecl :: lifted_fs)
