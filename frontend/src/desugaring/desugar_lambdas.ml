open Desugared_ast
open Desugar_util

(* transforms lambda types into their corresponding struct types
  also adds the new lambda-structs to the cdecl list
*)
let transform_ty (t : ty) (cs_acc : cdecl list) : ty * cdecl list * id option =
  match t with
  | TRef (RFun (arg_tys, rty) as r) ->
      let lname = mangle_lambda r in
      (*  TODO might have to check on this *)
      let lstruct_name = lambda_struct_name lname in
      let new_cs_acc =
        if List.exists (fun c -> c.cname = lstruct_name) cs_acc then cs_acc
        else
          let lstruct = create_lambda_struct lstruct_name arg_tys rty in
          lstruct :: cs_acc
      in
      (TRef (RClass lstruct_name), new_cs_acc, Some lstruct_name)
  | _ -> (t, cs_acc, None)

let transform_ret_ty (rt : ret_ty) (cs_acc : cdecl list) : ret_ty * cdecl list =
  match rt with
  | RetVal t ->
      let t', cs', _ = transform_ty t cs_acc in
      (RetVal t', cs')
  | RetVoid -> (RetVoid, cs_acc)

(* transforms lambda arguments and return types into their corresponding struct types *)
let check_fdecls (fs : fdecl list) (cs : cdecl list) : fdecl list * cdecl list =
  let final_cs, updated_fs =
    List.fold_left_map
      (fun curr_cs (f : fdecl) ->
        let frtyp', ret_cs = transform_ret_ty f.frtyp curr_cs in
        let args', args_cs =
          List.fold_right
            (fun (t, i) (args_acc, cs_acc) ->
              let t', cs_acc', _ = transform_ty t cs_acc in
              ((t', i) :: args_acc, cs_acc'))
            f.args ([], ret_cs)
        in
        let f' = { f with frtyp = frtyp'; args = args' } in
        (args_cs, f'))
      cs fs
  in
  (updated_fs, final_cs)

let rec lift_lambda (cs : cdecl list) (_fs : fdecl list) (vname_opt : id option)
    (scope, args, rty, body) =
  let r = RFun (List.map snd args, rty) in
  let lty = TRef r in
  (* create the lambda struct class, add it to cdecls, get name of class *)
  let lstruct_ty, cs', lstruct_name_opt = transform_ty lty cs in
  let lstruct_cdecl =
    match cs' with h :: _ -> h | [] -> desugar_error "impossible state"
  in
  let lstruct_name =
    match lstruct_name_opt with
    | Some l -> l
    | None -> desugar_error "impossible state"
  in
  let lname = mangle_lambda r in
  (* get the lifted lambda function name and scope struct name *)
  let sym = lambdasym lname in
  let lifted_lambda_fname = lifted_lambda_name sym in
  let lifted_lambda_scope = lambda_env_struct_name sym in
  let vname = match vname_opt with Some v -> v | None -> gensym "lambda" in
  (* set the fields of the env struct to the scope vars *)
  let env_fields =
    List.map
      (fun (i, t) ->
        { prelude = []; fieldName = i; ftyp = t; init = create_default_init t })
      scope
  in
  (* create env struct and add to cdecls *)
  let lambda_env =
    { cname = lifted_lambda_scope; fields = env_fields; annotations = [] }
  in
  (* let cs' = lambda_env :: cs' in *)
  let vname_env = vname ^ ".env" in
  let vname_env_ty = TRef (RClass lifted_lambda_scope) in
  (* load variables from the scope into the lambda's body 
        this will have to be passed to the lambda function too...
      *)
  let load_from_scope =
    List.map
      (fun (i, t) ->
        Decl (i, t, Proj (Id (vname_env, vname_env_ty), i, t), false))
      scope
  in
  let lenv = (vname_env_ty, vname_env) in
  let lifted_fn =
    {
      frtyp = rty;
      fname = lifted_lambda_fname;
      (* add the env to the params, first arg passed will have to be this lambda env *)
      args = lenv :: List.map (fun (i, t) -> (t, i)) args;
      body = load_from_scope @ body;
      (* TODO desugar lambdas in body of lambdas..*)
      annotations = [];
    }
  in
  (* add lifted lambda function to fdecl list *)
  (* let fs' = lifted_fn :: fs in *)
  let set_lambda_env_fields = List.map (fun (i, t) -> (i, Id (i, t))) scope in
  (* set the fields of the lambda struct *)
  let lambda_ptr_ty = create_ptr_to lty in
  let new_lambda_fn = gensym vname ^ "_struct" in
  let set_lambda_struct_fields =
    [
      ( "envptr",
        Cast (Id (vname_env, vname_env_ty), create_ptr_to (TInt (TSigned Ti8)))
      );
      ("lambdaptr", Id (lifted_lambda_fname, lambda_ptr_ty));
    ]
  in
  let s =
    [
      (* declare new lambda env struct instance *)
      Decl
        ( vname_env,
          vname_env_ty,
          ObjInit (lifted_lambda_scope, set_lambda_env_fields),
          true );
      (* declare new lambda struct instance *)
      Decl
        ( new_lambda_fn,
          lstruct_ty,
          ObjInit (lstruct_name, set_lambda_struct_fields),
          true );
      Decl
        ( vname,
          lambda_ptr_ty,
          Proj (Id (new_lambda_fn, lstruct_ty), "lambdaptr", lambda_ptr_ty),
          true );
    ]
    (* let f: [i32, i32] -> f64 = fn [a](x,y) {...} *)
  in
  (* return the new cdecl, new fdecl 
      the required statements for declaring the lifted lambda structs,
      the new variable holding the lambda struct,
      the lambda struct instance,
      and variable storing the lambda pointer
  *)
  ( (lambda_env, lstruct_cdecl),
    lifted_fn,
    s,
    Id (vname, lstruct_ty),
    Id (new_lambda_fn, lambda_ptr_ty) )

and lift_lambdas_from_list lctxt vname_opt =
 fun (cs_acc, fs_acc, stmts_acc) e ->
  let ncs, nfs, ns, de', _ =
    lift_lambdas_from_exps cs_acc fs_acc lctxt vname_opt e
  in
  ((ncs, nfs, stmts_acc @ ns), de')

and lift_lambdas_from_exps (cs : cdecl list) (fs : fdecl list) (lctxt : id list)
    (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      let (lenv, lstruct), fs', s, nid, lfun =
        lift_lambda cs fs vname_opt (scope, args, rty, body)
      in
      (lenv :: lstruct :: cs, fs' :: fs, s, nid, Some lfun)
  | Call (e, es, ty) ->
      let cs', fs', s, de, _lfun =
        lift_lambdas_from_exps cs fs lctxt vname_opt e
      in
      let (cs'', fs'', s'), des =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs', fs', s) es
      in
      (* 
      if we are calling a lambda, `de` now contains the name of the variable 
        which stores the lambda struct instance
      `de` is now Id (vname, lstruct_ty)
      but `lfun` is a variable containing the function pointer
      we instead need to call this function pointer.
      so we do this: 
        `lambda(x, y)`
        -> `...setting lambda environment struct...`
        -> `let lstruct = new Lambda { envptr = ..., lambdaptr = ...}`
        -> `let lfun = lstruct.lambdaptr;
        -> `lfun(lstruct.envptr, x, y)`      
      *)
      (cs'', fs'', s', Call (de, des, ty), None)
  | Array (es, t) ->
      let (cs', fs', s), des =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs, fs, []) es
      in
      (cs', fs', s, Array (des, t), None)
  | Cast (e, t) ->
      let cs', fs', s, de, _ = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs', fs', s, Cast (de, t), None)
  | Proj (e, i, t) ->
      let cs', fs', s, de, _ = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs', fs', s, Proj (de, i, t), None)
  | ObjInit (i, inits) ->
      let (cs', fs', s), inits' =
        List.fold_left_map
          (fun (cs_acc, fs_acc, stmts_acc) (field_id, e) ->
            let ncs, nfs, ns, de, _ =
              lift_lambdas_from_exps cs_acc fs_acc lctxt vname_opt e
            in
            ((ncs, nfs, stmts_acc @ ns), (field_id, de)))
          (cs, fs, []) inits
      in
      (cs', fs', s, ObjInit (i, inits'), None)
  | Id (i, t) -> (
      (* check if id is in lctxt *)
      let t', cs', lstruct_opt = transform_ty t cs in
      match lstruct_opt with
      | Some ls -> (cs', fs, [], Id (ls, t'), Some (Id (ls, t')))
      | None -> (cs', fs, [], Id (i, t'), None))
  | Bool b -> (cs, fs, [], Bool b, None)
  | Int (s, it) -> (cs, fs, [], Int (s, it), None)
  | Float (f, ft) -> (cs, fs, [], Float (f, ft), None)
  | Str s -> (cs, fs, [], Str s, None)
  | Bop (b, l, r, t) ->
      let lcs, lfs, ls, le, _ =
        lift_lambdas_from_exps cs fs lctxt vname_opt l
      in
      let rcs, rfs, rs, re, _ =
        lift_lambdas_from_exps lcs lfs lctxt vname_opt r
      in
      (rcs, rfs, ls @ rs, Bop (b, le, re, t), None)
  | Uop (u, e, t) ->
      let cs, fs, s, e', _ = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs, fs, s, Uop (u, e', t), None)
  | Index (ec, ei, t) ->
      let eccs, ecfs, ecs, ec', _ =
        lift_lambdas_from_exps cs fs lctxt vname_opt ec
      in
      let eics, eifs, eis, ei', _ =
        lift_lambdas_from_exps eccs ecfs lctxt vname_opt ei
      in
      (eics, eifs, ecs @ eis, Index (ec', ei', t), None)
  | Null t -> (cs, fs, [], Null t, None)

and lift_lambdas_from_stmt (cs : cdecl list) (fs : fdecl list) (lctxt : id list)
    = function
  | Decl (vname, ty, e, _const) ->
      (* Lift lambdas from the initialization expression *)
      let ncs, nfs, ns, _ne, _ =
        lift_lambdas_from_exps cs fs lctxt (Some vname) e
      in
      (* Update type if it was a function type *)
      let _ty', ncs', _ = transform_ty ty ncs in
      (* If this variable is a lambda, add it to our context for Call sites *)
      let nlctxt =
        match ty with TRef (RFun _) -> vname :: lctxt | _ -> lctxt
      in
      (ncs', nfs, nlctxt, ns)
  | Assn (e1, e2, ty) ->
      let cs1, fs1, ns1, ne1, _ = lift_lambdas_from_exps cs fs lctxt None e1 in
      let cs2, fs2, ns2, ne2, _ =
        lift_lambdas_from_exps cs1 fs1 lctxt None e2
      in
      let ty', cs3, _ = transform_ty ty cs2 in
      (cs3, fs2, lctxt, ns1 @ ns2 @ [ Assn (ne1, ne2, ty') ])
  | Ret (Some e) ->
      let cs', fs', ns, _ne, _ = lift_lambdas_from_exps cs fs lctxt None e in
      (cs', fs', lctxt, ns @ [ Ret (Some e) ])
  | SCall (e, es) -> (
      (* Transform the call expression (which handles the lambda-indirect-call logic) *)
      let cs', fs', ns, ne, _ =
        lift_lambdas_from_exps cs fs lctxt None
          (Call (e, es, TBool (* dummy *)))
      in
      (* Unwrap the result back into an SCall *)
      match ne with
      | Call (e_new, es_new, _) ->
          (cs', fs', lctxt, ns @ [ SCall (e_new, es_new) ])
      | _ -> desugar_error "Unexpected expression transformation in SCall")
  | If (cond, b1, b2) ->
      let cs', fs', ns, ncond, _ =
        lift_lambdas_from_exps cs fs lctxt None cond
      in
      let cs1, fs1, nb1 = lift_lambda_from_block cs' fs' lctxt b1 in
      let cs2, fs2, nb2 = lift_lambda_from_block cs1 fs1 lctxt b2 in
      (cs2, fs2, lctxt, ns @ [ If (ncond, nb1, nb2) ])
  | While (cond, b) ->
      let cs', fs', ns, ncond, _ =
        lift_lambdas_from_exps cs fs lctxt None cond
      in
      let cs1, fs1, nb = lift_lambda_from_block cs' fs' lctxt b in
      (cs1, fs1, lctxt, ns @ [ While (ncond, nb) ])
  | (Break | Continue | Ret None) as s -> (cs, fs, lctxt, [ s ])
  | Free es ->
      let (cs', fs', ns), nes =
        List.fold_left_map
          (fun (c, f, s) e ->
            let c', f', s', ne, _ = lift_lambdas_from_exps c f lctxt None e in
            ((c', f', s @ s'), ne))
          (cs, fs, []) es
      in
      (cs', fs', lctxt, ns @ [ Free nes ])

and lift_lambda_from_block cs fs lctxt block =
  let final_cs, final_fs, _, new_block =
    List.fold_left
      (fun (curr_cs, curr_fs, curr_lctxt, block_acc) stmt ->
        let next_cs, next_fs, next_lctxt, new_stmts =
          lift_lambdas_from_stmt curr_cs curr_fs curr_lctxt stmt
        in
        (next_cs, next_fs, next_lctxt, block_acc @ new_stmts))
      (cs, fs, lctxt, []) block
  in
  (final_cs, final_fs, new_block)

(* and lift_lambdas_from_stmt (cs : cdecl list) (fs : fdecl list) (lctxt : id list)
    = function
  | Decl (vname, (TRef (RFun _)), e, _) ->
    (* TODO create function that just stores fptr of another function *)
      let cs', fs', ss, _, _ = lift_lambdas_from_exps cs fs lctxt (Some vname) e in
      let lctxt = vname :: lctxt in
      cs', fs', ss, lctxt
  | _ -> desugar_error "hi"

let lift_lambda_from_fdecl (cs : cdecl list) (fs : fdecl) =
  let lctxt =
    List.fold_left
      (fun lctxt (t, i) ->
        match t with TRef (RFun _) -> (i, t) :: lctxt | _ -> lctxt)
      [] fs.args
  in
  let new_fs = [] in
  let _ = List.fold_left
    (fun acc _a -> fst acc, 1::(snd acc)) (cs, new_fs) fs.body in
    let _ = List.map2 *)
let lift_lambda_from_fdecl (cs : cdecl list) (f : fdecl) :
    cdecl list * fdecl list * fdecl =
  (* 1. Initialize lctxt from arguments that are functions *)
  let lctxt_initial =
    List.fold_left
      (fun acc (t, i) -> match t with TRef (RFun _) -> i :: acc | _ -> acc)
      [] f.args
  in

  (* 2. Process the body statements *)
  let final_cs, new_lifted_fs, transformed_body =
    lift_lambda_from_block cs [] lctxt_initial f.body
  in

  (* 3. Transform the return type of the function itself *)
  let frtyp', cs_after_ret = transform_ret_ty f.frtyp final_cs in

  (* 4. Transform the argument types of the function itself *)
  let args', cs_final =
    List.fold_right
      (fun (t, i) (args_acc, cs_acc) ->
        let t', cs_acc', _ = transform_ty t cs_acc in
        ((t', i) :: args_acc, cs_acc'))
      f.args ([], cs_after_ret)
  in

  (* Return the updated class declarations, all new lifted functions, and the cleaned-up original function *)
  ( cs_final,
    new_lifted_fs,
    { f with frtyp = frtyp'; args = args'; body = transformed_body } )

(* in lctxt *)
(* desugar_error "hi" *)

(* *)
(* 
  add any lambdas in the args to the lambda ctxt
  for each line: 
    if rhs of decl or assn is lambda type:
      add lhs id to lctxt
      create new lambda scope struct
      create new lambda struct if need to
      change assn/decl to struct type

    if callee of function call is in lambda ctxt: 
      do a method call of the function ptr

    if return is of a lambda type: 
      return lambda struct instead
      creating structs as needed
 *)
