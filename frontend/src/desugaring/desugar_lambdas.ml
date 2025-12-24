open Desugared_ast
open Desugar_util

(* transforms lambda types into their corresponding struct types
  also adds the new lambda-structs to the cdecl list
*)
let transform_ty (t : ty) (cs_acc : cdecl list) : ty * cdecl list * id option =
  match t with
  | TRef (RFun (arg_tys, rty) as r) ->
      let lname = mangle_lambda r in
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

let rec lift_lambda (cs : cdecl list) (fs : fdecl list) (vname_opt : id option)
    (scope, args, rty, body) =
  let r = RFun (List.map snd args, rty) in
  let lty = TRef r in
  (* create the lambda struct class, add it to cdecls, get name of class *)
  let lstruct_ty, cs', lstruct_name_opt = transform_ty lty cs in
  let lstruct_name =
    match lstruct_name_opt with
    | Some l -> l
    | None -> desugar_error "impossible state"
  in
  let lname = mangle_lambda r in
  (* get the lifted lambda function name and scope struct name *)
  let lifted_lambda_fname = lifted_lambda_name lname in
  let lifted_lambda_scope = lambda_env_struct_name lname in
  let vname =
    match vname_opt with Some v -> gensym v | None -> gensym "anon.lambda"
  in
  (* set the fields of the env struct to the scope vars *)
  let env_fields =
    List.map
      (fun (i, t) ->
        { prelude = []; fieldName = i; ftyp = t; init = Id (i, t) })
      scope
  in
  (* create env struct and add to cdecls *)
  let lambda_env =
    { cname = lifted_lambda_scope; fields = env_fields; annotations = [] }
  in
  let cs' = lambda_env :: cs' in
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
  let fs' = lifted_fn :: fs in
  (* set the fields of the lambda struct *)
  let set_lambda_struct_fields =
    [
      ( "envptr",
        Cast (Id (vname_env, vname_env_ty), create_ptr_to (TInt (TSigned Ti8)))
      );
      ("lambdaptr", Id (lifted_lambda_fname, create_ptr_to lty));
    ]
  in
  let s =
    [
      (* declare new lambda env struct instance *)
      Decl
        ( vname_env,
          vname_env_ty,
          ObjInit (lifted_lambda_scope, [ (* already with default init *) ]),
          true );
      (* declare new lambda struct instance *)
      Decl
        ( vname,
          lstruct_ty,
          ObjInit (lstruct_name, set_lambda_struct_fields),
          true );
    ]
    (* let f: [i32, i32] -> f64 = fn [a](x,y) {...} *)
  in
  (* return the updated cdecls, fdecls, 
      the required statements for declaring the lifted lambda structs
      and the new variable holding the lambda struct *)
  (cs', fs', s, Id (vname, lstruct_ty))

and lift_lambdas_from_list lctxt vname_opt =
 fun (cs_acc, fs_acc, stmts_acc) e ->
  let ncs, nfs, ns, de' =
    lift_lambdas_from_exps cs_acc fs_acc lctxt vname_opt e
  in
  ((ncs, nfs, stmts_acc @ ns), de')

and lift_lambdas_from_exps (cs : cdecl list) (fs : fdecl list) (lctxt : id list)
    (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      lift_lambda cs fs vname_opt (scope, args, rty, body)
  | Call (e, es, ty) ->
      let cs', fs', s, de = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      let (cs'', fs'', s'), des =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs', fs', s) es
      in
      (cs'', fs'', s', Call (de, des, ty))
  | Array (es, t) ->
      let (cs', fs', s), des =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs, fs, []) es
      in
      (cs', fs', s, Array (des, t))
  | Cast (e, t) ->
      let cs', fs', s, de = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs', fs', s, Cast (de, t))
  | Proj (e, i, t) ->
      let cs', fs', s, de = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs', fs', s, Proj (de, i, t))
  | ObjInit (i, inits) ->
      let (cs', fs', s), inits' =
        List.fold_left_map
          (fun (cs_acc, fs_acc, stmts_acc) (field_id, e) ->
            let ncs, nfs, ns, de =
              lift_lambdas_from_exps cs_acc fs_acc lctxt vname_opt e
            in
            ((ncs, nfs, stmts_acc @ ns), (field_id, de)))
          (cs, fs, []) inits
      in
      (cs', fs', s, ObjInit (i, inits'))
  | Id _ -> failwith "soon"
  | Bool b -> (cs, fs, [], Bool b)
  | Int (s, it) -> (cs, fs, [], Int (s, it))
  | Float (f, ft) -> (cs, fs, [], Float (f, ft))
  | Str s -> cs, fs, [], Str s
  | Bop (b, l, r, t) ->
      let lcs, lfs, ls, le = lift_lambdas_from_exps cs fs lctxt vname_opt l in
      let rcs, rfs, rs, re = lift_lambdas_from_exps lcs lfs lctxt vname_opt r in
      (rcs, rfs, ls @ rs, Bop (b, le, re, t))
  | Uop (u, e, t) ->
      let cs, fs, s, e' = lift_lambdas_from_exps cs fs lctxt vname_opt e in
      (cs, fs, s, Uop (u, e', t))
  | Index (ec, ei, t) ->
      let eccs, ecfs, ecs, ec' =
        lift_lambdas_from_exps cs fs lctxt vname_opt ec
      in
      let eics, eifs, eis, ei' =
        lift_lambdas_from_exps eccs ecfs lctxt vname_opt ei
      in
      (eics, eifs, ecs @ eis, Index (ec', ei', t))
  | Null t -> cs, fs, [], Null t

and lift_lambdas_from_stmt (cs: cdecl list) (fs : fdecl list) (lctxt : id list) = 


(* and lift_lambdas_from_stmt (cs : cdecl list) (lambda_ctxt : id list) = function
  | Assn (_, Lambda _, TRef (RFun (_, _))) -> desugar_error "n"
  | Decl (_, TRef (RFun (_, _)), Lambda _, _) -> desugar_error "n"
  | Ret (Some (Lambda _)) -> desugar_error "n"
  | SCall (Lambda _, _) -> desugar_error "n"
  | Assn (_, Id (l, _), TRef (RFun (_, _))) when List.mem l lambda_ctxt ->
      desugar_error "n"
  | Decl (_, TRef (RFun (_, _)), Id (l, _), _) when List.mem l lambda_ctxt ->
      desugar_error "n"
  | Ret (Some (Id (l, TRef (RFun (_, _))))) when List.mem l lambda_ctxt ->
      desugar_error "n"
  | SCall (Id (l, _), _) when List.mem l lambda_ctxt -> desugar_error "n"
  | If (_cond, b1, b2) ->
      (*  TODO need to desugar lambdas possibly present in cond *)
      List.concat_map (lift_lambdas_from_stmt cs lambda_ctxt) b1
      @ List.concat_map (lift_lambdas_from_stmt cs lambda_ctxt) b2
  | While (_cond, b) ->
      (*  TODO need to desugar lambdas possibly present in cond *)
      List.concat_map (lift_lambdas_from_stmt cs lambda_ctxt) b
  | Free _ -> desugar_error "n"
  | s -> [ s ] *)

let lift_lambdas_from_block (b : block) (cs : cdecl list) =
  List.concat_map (fun s -> lift_lambdas_from_stmt cs [] s) b

let desugar_lambdas (fs : fdecl list) (cs : cdecl list) =
  let lctxt = [] in
  lctxt
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
