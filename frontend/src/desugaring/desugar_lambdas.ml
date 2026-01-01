open Desugared_ast
open Desugar_util
(* open Pprint_desugared_ast *)

(* transforms lambda types into their corresponding struct types
  also adds the new lambda-structs to the cdecl list
*)
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

type lifted_lambda_result = {
  lambda_structs : cdecl list; (* (env_struct, lstruct_cdecl) *)
  lifted_fdecl : fdecl; (* The actual lifted function *)
  setup_stmts : stmt list; (* Decls for env and lambda structs *)
  lambda_var : id * ty; (* The struct instance (vname, lstruct_ty) *)
  fn_ptr_var : id * ty; (* The temporary function pointer variable *)
  env_var : id * ty (* The env variable *);
}

type lambda_converter = { fptr_var : id; env_var : id }

let add_cdecl (cd : cdecl) (cs : cdecl list) : cdecl list =
  if List.exists (fun c -> c.cname = cd.cname) cs then cs else cd :: cs

let add_cdecls (new_cs : cdecl list) (old_cs : cdecl list) : cdecl list =
  List.fold_left (fun acc cd -> add_cdecl cd acc) old_cs new_cs

let rec lift_lambda (cs : cdecl list) (vname_opt : id option)
    (scope, args, rty, body) : lifted_lambda_result =
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
  let body' =
    Decl (vname_env, env_ptr_ty, Cast (Id (i8_name, i8_ptr), env_ptr_ty), false)
    :: List.fold_left
         (fun acc (i, t) ->
           Decl (i, t, Proj (Id (vname_env, env_ty), i, t), false) :: acc)
         body scope
  in
  let lifted_lambda_fname = lifted_lambda_name sym in
  let lifted_fn =
    {
      frtyp = rty;
      fname = lifted_lambda_fname;
      (* (1) add the env to the params *)
      args = (i8_ptr, i8_name) :: List.map (fun (i, t) -> (t, i)) args;
      body = body';
      (* TODO desugar lambdas in body of lambdas..*)
      annotations = [];
    }
  in
  (* set the fields of the lambda struct *)
  let lambda_ptr_ty = create_ptr_to lty in
  let new_lambda_fn = vname ^ ".fun" in
  let set_lambda_struct_fields =
    [
      ("envptr", Id (vname_env, i8_ptr));
      ("lambdaptr", Id (lifted_lambda_fname, lambda_ptr_ty));
    ]
  in
  let s =
    [
      lambda_env_decl;
      (* 
        declare new lambda struct instance
        need to use the original variable name to store the struct 
        so we do not need to modify passing lamdbas and returning lambdas
      *)
      Decl
        ( vname,
          lstruct_ty,
          ObjInit (lstruct_cdecl.cname, set_lambda_struct_fields),
          false );
      (* 
        we will store the function pointer in a new variable
        this way we can just substitute lambda names with this when calling
      *)
      Decl
        ( new_lambda_fn,
          lambda_ptr_ty,
          Proj (Id (vname, lstruct_ty), "lambdaptr", lambda_ptr_ty),
          true );
    ]
  in
  (* 
    let a = 10;
    let f: [i32; i32] -> i32 = fn [a](x,y) { return x+y*a; };
    let r = f(2, 3);

    turns into...

    class Env0.lambda_i32_i32__i32 {
      let a: i32 = 0;
    }

    fun Lifted0.lambda_i32_i32__i32("f.env": i8*, x: i32, y: i32) => i32 {
      let a = "f.env".a;
      return x+y*a;
    }

    let a = 10;
    const "f.env": i8* = ( i8* ) new Env0.lambda_i32_i32__i32 { a = a };
    let f: Struct.lambda_i32_i32__i32 = new Struct.lambda_i32_i32__i32 { 
      envptr: i8* = "f.env",
      lambdaptr: (i32, i32 -> i32)* = Lifted0.lambda_i32_i32__i32
    };

    let %tmp_f_fun1 = f.lambdaptr;

    let r = %tmp_f_fun1(2, 3);  
  *)

  (* 
      return the new cdecls (env and lambda structs),
      the new fdecl,
      the required statements for declaring the desugared variables,
      the lambda struct instance bound to the original lambda name,
      and the variable storing the function pointer to the lifted lambda
  *)
  {
    lambda_structs = lambda_env :: [ lstruct_cdecl ];
    lifted_fdecl = lifted_fn;
    setup_stmts = s;
    lambda_var = (vname, lstruct_ty);
    fn_ptr_var = (new_lambda_fn, lambda_ptr_ty);
    env_var = (vname_env, env_ty);
  }

and lift_lambdas_from_list (lctxt : (id * lambda_converter) list)
    (vname_opt : id option) (cs_acc, fs_acc, stmts_acc) (e : exp) =
  (* lift lambdas from the current expression *)
  let ncs, nfs, ns, _lambda_opt, _fptr_opt, ne, _env =
    lift_lambdas_from_exps cs_acc lctxt vname_opt e
  in
  (* collect new functions and statements *)
  let updated_fs = match nfs with [] -> fs_acc | _ -> nfs @ fs_acc in
  (* return the new state and the transformed expression *)
  ((ncs, updated_fs, stmts_acc @ ns), ne)

and lift_lambdas_from_exps (cs : cdecl list)
    (lctxt : (id * lambda_converter) list) (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      let res = lift_lambda cs vname_opt (scope, args, rty, body) in
      ( add_cdecls res.lambda_structs cs,
        [ res.lifted_fdecl ],
        res.setup_stmts,
        Some res.lambda_var,
        Some (fst res.fn_ptr_var),
        Id (fst res.lambda_var, snd res.lambda_var),
        Some (fst res.env_var) )
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
          let transformed_call =
            Call (cnv.fptr_var, Id (cnv.env_var, i8_ptr) :: es', ty)
          in
          ( ncs,
            nfs,
            nstmts,
            None,
            Some cnv.fptr_var,
            transformed_call,
            Some cnv.env_var )
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
      | Some cnv ->
          (* known lambda variable/parameter with converters in lctxt *)
          let ty', ncs_opt = transform_ty ty cs in
          let cs' =
            match ncs_opt with Some ncs -> add_cdecl ncs cs | None -> cs
          in
          (cs', [], [], None, Some cnv.fptr_var, Id (i, ty'), Some cnv.env_var)
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
  | Decl (vname, (TRef (RFun (args, rty)) as ty), e, _const) ->
      (*  lift lambda from initialization exp *)
      let ncs, nfs, ns, _l, fptr_opt, ne, env_opt =
        lift_lambdas_from_exps cs lctxt (Some vname) e
      in
      let fptr, env, extra_stmts =
        match (fptr_opt, env_opt) with
        | Some f, Some e -> (f, e, [])
        | _ ->
            let f_name = gensym (vname ^ "_fptr") in
            let e_name = gensym (vname ^ "_env") in
            let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
            let l_ptr_ty = create_ptr_to (TRef (RFun (args, rty))) in
            ( f_name,
              e_name,
              [
                Decl (f_name, l_ptr_ty, Proj (ne, "lambdaptr", l_ptr_ty), true);
                Decl (e_name, i8_ptr, Proj (ne, "envptr", i8_ptr), true);
              ] )
      in
      let dcs = add_cdecls ncs cs in
      (* update ty of decl if necessary *)
      let _ty', dnc_opt = transform_ty ty ncs in
      (* if this variable is a lambda, add it to our context for Call sites *)
      let nlctxt = (vname, { fptr_var = fptr; env_var = env }) :: lctxt in
      let cs' =
        match dnc_opt with Some dnc -> add_cdecl dnc dcs | None -> cs
      in
      (cs', nfs @ fs, nlctxt, ns @ extra_stmts)
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
      (ecs, efs @ fs, lctxt, ess @ [ SCall (i, es') ])
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
      let rcs, rfs, rss, _, fptr_opt, r', env_opt =
        lift_lambdas_from_exps cs lctxt None r
      in

      let ty', tc_opt = transform_ty ty cs in
      let cs' = add_cdecls rcs (add_cdecls lcs cs) in
      let cs' = match tc_opt with Some tc -> add_cdecl tc cs' | None -> cs' in

      (* update pointers of LHS if we are tracking it *)
      let extra_assns =
        match l' with
        | Id (name, _) -> (
            match (List.assoc_opt name lctxt, fptr_opt, env_opt) with
            | Some cnv, Some f_new, Some e_new ->
                let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
                (* always same ty when re-assigning *)
                let f_ptr_ty = create_ptr_to ty in
                [
                  Assn
                    (Id (cnv.fptr_var, f_ptr_ty), Id (f_new, f_ptr_ty), f_ptr_ty);
                  Assn (Id (cnv.env_var, i8_ptr), Id (e_new, i8_ptr), i8_ptr);
                ]
            | _ -> [])
        | _ -> []
      in

      ( cs',
        lfs @ rfs @ fs,
        lctxt,
        lss @ rss @ [ Assn (l', r', ty') ] @ extra_assns )
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

let lift_lambda_from_fdecl (cs : cdecl list) (f : fdecl) :
    cdecl list * fdecl list =
  (* process arguments *)
  let lctxt_initial, args_transformed, unpack_stmts, cs_args =
    List.fold_right
      (fun (t, i) (lctxt_acc, args_acc, stmts_acc, cs_acc) ->
        match t with
        | TRef (RFun (arg_tys, rty)) ->
            (* convert lambda type to struct *)
            let t', cd_opt = transform_ty t cs_acc in
            let cs' =
              match cd_opt with
              | Some cd -> add_cdecl cd cs_acc
              | None -> cs_acc
            in

            (* create local variables to hold the unpacked fptr and env *)
            let fptr_name = gensym (i ^ "_fptr") in
            let env_name = gensym (i ^ "_env") in
            let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
            let lambda_ptr_ty = create_ptr_to (TRef (RFun (arg_tys, rty))) in

            let p_fptr =
              Decl
                ( fptr_name,
                  lambda_ptr_ty,
                  Proj (Id (i, t'), "lambdaptr", lambda_ptr_ty),
                  true )
            in
            let p_env =
              Decl (env_name, i8_ptr, Proj (Id (i, t'), "envptr", i8_ptr), true)
            in

            ( (i, { fptr_var = fptr_name; env_var = env_name }) :: lctxt_acc,
              (t', i) :: args_acc,
              p_fptr :: p_env :: stmts_acc,
              cs' )
        | _ -> (lctxt_acc, (t, i) :: args_acc, stmts_acc, cs_acc))
      f.args ([], [], [], cs)
  in

  (* return type transformation *)
  let frtyp', cd_ret_opt = transform_ret_ty f.frtyp cs_args in
  let cs_ret =
    match cd_ret_opt with Some cd -> add_cdecl cd cs_args | None -> cs_args
  in

  (* lift lambdas from body, prefixing the body with our unpack statements *)
  let body_with_unpacks = unpack_stmts @ f.body in
  let final_cs, lifted_fs, transformed_body =
    lift_lambda_from_block cs_ret lctxt_initial body_with_unpacks
  in

  let new_fdecl =
    { f with frtyp = frtyp'; args = args_transformed; body = transformed_body }
  in

  (final_cs, new_fdecl :: lifted_fs)
