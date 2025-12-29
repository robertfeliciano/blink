open Desugared_ast
open Desugar_util
(* open Pprint_desugared_ast *)

(* transforms lambda types into their corresponding struct types
  also adds the new lambda-structs to the cdecl list
*)
let transform_ty (t : ty) (cs : cdecl list) : ty * cdecl option =
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
}

type lambda_converter = {
  fptr_var : id; 
  env_var : id;
}

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
  let new_lambda_fn = gensym (vname ^ "_fun") in
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
  }

and lift_lambdas_from_list (lctxt : (id * lambda_converter) list) (vname_opt : id option)
    (cs_acc, fs_acc, stmts_acc) (e : exp) =
  (* 1. Lift lambdas from the current expression *)
  let ncs, nf_opt, ns, _lambda_opt, _fptr_opt, ne =
    lift_lambdas_from_exps cs_acc lctxt vname_opt e
  in
  (* 3. Collect new functions and statements *)
  let updated_fs =
    match nf_opt with Some nf -> nf :: fs_acc | None -> fs_acc
  in
  (* Return the new state and the transformed expression *)
  ((ncs, updated_fs, stmts_acc @ ns), ne)

and lift_lambdas_from_exps (cs : cdecl list) (lctxt : (id * lambda_converter) list)
    (vname_opt : id option) = function
  | Lambda (scope, args, rty, body) ->
      let res = lift_lambda cs vname_opt (scope, args, rty, body) in
      ( res.lambda_structs @ cs,
        Some res.lifted_fdecl,
        res.setup_stmts,
        Some res.lambda_var,
        Some res.fn_ptr_var,
        Id (fst res.lambda_var, snd res.lambda_var) )
  | Call (callee, es, ty) -> (
    (* TODO update signature to return list fdecls instead of option, return nfs *)
      let (ncs, _nfs, nstmts), es' =
        List.fold_left_map
          (lift_lambdas_from_list lctxt vname_opt)
          (cs, [], []) es
      in
      (* get fptr from context if we are calling a lambda variable *)
      match List.assoc_opt callee lctxt with
      | Some cnv ->
        let i8_ptr = create_ptr_to (TInt (TSigned Ti8)) in
        let transformed_call = Call (cnv.fptr_var, Id (cnv.env_var, i8_ptr)::es', ty) in
          (ncs, None, nstmts, None, None, transformed_call)
      | None -> 
          (ncs, None, nstmts, None, None, (Call (callee, es', ty))))
  | e -> (cs, None, [], None, None, e)
  (* | _ -> failwith "burp" *)

and lift_lambdas_from_stmt (cs : cdecl list) (fs : fdecl list)
    (lctxt : (id * lambda_converter) list) = function
  | Decl (vname, (TRef (RFun _) as ty), e, _const) ->
      (*  lift lambda from initialization exp *)
      let ncs, nf, ns, _l, fptr_opt, _ =
        lift_lambdas_from_exps cs lctxt (Some vname) e
      in
      let fptr =
        match fptr_opt with
        | Some fptr -> fst fptr
        | None ->
            desugar_error
              "Guaranteed to get a function pointer when rhs is \
               lambda/function type"
      in
      (* update ty of decl if necessary *)
      let _ty', dnc_opt = transform_ty ty ncs in
      (* if this variable is a lambda, add it to our context for Call sites *)
      let nlctxt = (vname, {fptr_var = fptr; env_var = "f.env"}) :: lctxt in
      let cs' =
        match dnc_opt with
        | Some dnc ->
            if List.exists (fun c -> c.cname = dnc.cname) ncs then ncs
            else dnc :: (ncs @ cs)
        | None -> cs
      in
      (cs', (match nf with Some f -> f :: fs | None -> fs), nlctxt, ns)
  | Ret rval -> (
      match rval with
      | Some e ->
          let cs', nf, ns, _l, _fptr_opt, e' = lift_lambdas_from_exps cs lctxt None e in
          (cs', (match nf with Some f -> f :: fs | None -> fs), lctxt, ns @ [Ret (Some e')])
      | None -> (cs, fs, lctxt, [ Ret rval ]))
  | s -> (cs, fs, lctxt, [ s ])
(* | _ -> failwith "hi" *)

and lift_lambda_from_block cs lctxt block =
  let final_cs, lifted_fs, _, new_block =
    List.fold_left
      (fun (curr_cs, curr_fs, curr_lctxt, block_acc) stmt ->
        (* Printf.printf "%s\n" (show_stmt stmt); *)
        let next_cs, next_fs, next_lctxt, new_stmts =
          lift_lambdas_from_stmt curr_cs curr_fs curr_lctxt stmt
        in
        (next_cs, next_fs, next_lctxt, block_acc @ new_stmts))
      (cs, [], lctxt, []) block
  in
  (final_cs, lifted_fs, new_block)

let lift_lambda_from_fdecl (cs : cdecl list) (f : fdecl) :
    cdecl list * fdecl list =
  let lctxt_initial, args', cs_args =
    List.fold_right
      (fun (t, i) (lambda_ctxt, args_acc, cs_acc) ->
        match t with
        | TRef (RFun _) -> (
            (* TODO get env and function ptr at beginning of function and add to lctxt*)
            let t', cd_opt = transform_ty t cs_acc in
            ( lambda_ctxt,
              (t', i) :: args_acc,
              match cd_opt with Some cd -> cd :: cs_acc | None -> cs_acc ))
        | _ -> (lambda_ctxt, (t, i) :: args_acc, cs_acc))
      f.args ([], [], cs)
  in

  let frtyp', cd_ret_opt = transform_ret_ty f.frtyp cs_args in
  let cs_ret =
    match cd_ret_opt with Some cd -> cd :: cs_args | None -> cs_args
  in

  (* lift lambdas from body of function *)
  let final_cs, lifted_fs, transformed_body =
    lift_lambda_from_block cs_ret lctxt_initial f.body
  in

  let new_fdecl =
    { f with frtyp = frtyp'; args = args'; body = transformed_body }
  in

  (final_cs, new_fdecl :: lifted_fs)
