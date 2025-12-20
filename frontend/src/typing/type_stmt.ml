open Ast
open Type_util
open Conversions
module Printer = Pprint_typed_ast
module Methods = Util.Constants.Methods

(*
I wish there was an easy way to separate this into two separate files for exp and stmts.
The mutual recursion introduced by lambda expressions has forced me to combine everything
into one file. 
*)

let rec type_stmt (tc : Tctxt.t) (frtyp : Typed_ast.ret_ty) (stmt_n : stmt node)
    (in_loop : bool) : Tctxt.t * Typed_ast.stmt * bool =
  let { elt = stmt; loc = _ } = stmt_n in
  match stmt with
  | Decl (_, None, None, _) ->
      type_error stmt_n "Must provide type or initial value."
  | Decl (i, Some ty, None, const) ->
      let e = create_default_init stmt_n tc ty in
      let e_ty = convert_ty ty in
      let tc' = Tctxt.add_local tc i (e_ty, const) in
      (tc', Typed_ast.Decl (i, e_ty, e, const), false)
  | Decl (i, None, Some en, const) ->
      let te, e_ty = type_exp tc en in
      let tc', resolved_ty = (Tctxt.add_local tc i (e_ty, const), e_ty) in
      (tc', Typed_ast.Decl (i, resolved_ty, te, const), false)
  | Decl (i, Some given_ty_ast, Some en, const) ->
      let given_ty = convert_ty given_ty_ast in
      let te, e_ty = type_exp ~expected:given_ty tc en in
      let tc', resolved_ty =
        match (e_ty, given_ty, te) with
        | Typed_ast.TInt _, Typed_ast.TInt given_num_ty, Typed_ast.Int (n, _) ->
            if fits_in_int_ty n given_num_ty then
              (Tctxt.add_local tc i (given_ty, const), given_ty)
            else
              type_error stmt_n
                ("Integer literal " ^ Z.to_string n ^ " does not fit in type "
               ^ Printer.show_ty given_ty)
        | ( Typed_ast.TFloat _,
            Typed_ast.TFloat given_float_ty,
            Typed_ast.Float (f, _) ) ->
            if fits_in_float_ty f given_float_ty then
              (Tctxt.add_local tc i (given_ty, const), given_ty)
            else type_error stmt_n "Float literal out of range for f32"
        | Typed_ast.TInt _, Typed_ast.TFloat given_float_ty, Typed_ast.Int (n, _)
          ->
            (* trying to auto upcast int to float *)
            if int_in_float n given_float_ty then
              (Tctxt.add_local tc i (given_ty, const), given_ty)
            else
              type_error stmt_n
                ("Integer literal " ^ Z.to_string n
               ^ " does not fit in float type " ^ Printer.show_ty given_ty)
        (* note - we will not auto downcast, i.e. `let x: u8 = 12.14` is not valid *)
        | _ ->
            if given_ty = e_ty then
              (Tctxt.add_local tc i (given_ty, const), given_ty)
            else
              type_error stmt_n
                ("Provided type " ^ show_ty given_ty_ast
               ^ " does not match inferred type " ^ Printer.show_ty e_ty)
      in
      (tc', Typed_ast.Decl (i, resolved_ty, te, const), false)
  | LambdaDecl _ -> type_lambda tc stmt_n
  | Assn (lhs, op, rhs) ->
      (match lhs.elt with
      | Id i -> (
          match Tctxt.lookup_option i tc with
          | Some (_, is_const) ->
              if is_const then
                type_error stmt_n
                  "Attempting to assign to a variable marked as constant."
          | None -> type_error stmt_n ("variable " ^ i ^ " is not defined"))
      | Proj _ | Index _ -> ()
      | _ -> type_error lhs "cannot assign to this expression");
      let tlhs, lhsty = type_exp tc lhs in
      let trhs, _rhsty = type_exp ~expected:lhsty tc rhs in
      (tc, Typed_ast.Assn (tlhs, convert_aop op, trhs, lhsty), false)
  | Ret expr ->
      let te_opt =
        match (expr, frtyp) with
        | Some e, RetVal r_ty ->
            let te, expr_ty = type_exp ~expected:r_ty tc e in
            if not (equal_ty expr_ty r_ty) then
              type_error stmt_n
                ("Expected function return type " ^ Printer.show_ty r_ty
               ^ ", found " ^ Printer.show_ty expr_ty ^ ".")
            else Some te
        | Some e, RetVoid ->
            type_error stmt_n
              ("Expected function return type void, found " ^ show_exp e.elt
             ^ ".")
        | None, RetVal r_ty ->
            type_error stmt_n
              ("Expected function return type " ^ Printer.show_ty r_ty
             ^ ", found void.")
        | None, RetVoid -> None
      in
      (tc, Typed_ast.Ret te_opt, true)
  | SCall ({ elt = Proj (obj, mth); loc = _ }, args) -> (
      match type_method (Proj (obj, mth)) args false tc with
      | Error msg -> type_error stmt_n msg
      | Ok (Proj (tobj, _, cname, t), arg_types, typed_args, RetVoid) ->
          ( tc,
            Typed_ast.(
              SCall (Proj (tobj, mth, cname, t), typed_args, arg_types, RetVoid)),
            false )
      | Ok (Proj (tobj, _, cname, t), arg_types, typed_args, ret_ty) ->
          type_warning stmt_n "Ignoring non-void function";
          ( tc,
            Typed_ast.(
              SCall (Proj (tobj, mth, cname, t), typed_args, arg_types, ret_ty)),
            false )
      | _ -> type_error stmt_n "Unreachable state.")
  | SCall (f, args) ->
      let typed_callee, typ = type_exp tc f in
      let arg_types, typed_args, ret =
        match type_func args typ false tc with
        | Error msg -> type_error stmt_n msg
        | Ok (arg_types, typed_args, RetVoid) ->
            (arg_types, typed_args, Typed_ast.RetVoid)
        | Ok (arg_types, typed_args, t) ->
            type_warning stmt_n "Ignoring non-void function";
            (arg_types, typed_args, t)
      in
      (tc, Typed_ast.SCall (typed_callee, typed_args, arg_types, ret), false)
  | If (cond, then_branch, else_branch) ->
      let tcond, cond_ty = type_exp ~expected:TBool tc cond in
      if cond_ty <> Typed_ast.TBool then
        type_error cond "if condition must be bool";
      let _tc_then, t_then, if_ret = type_block tc frtyp then_branch in_loop in
      let _tc_else, t_else, else_ret =
        type_block tc frtyp else_branch in_loop
      in
      (tc, Typed_ast.If (tcond, t_then, t_else), if_ret && else_ret)
  | While (cond, body) ->
      let tcond, cond_ty = type_exp ~expected:TBool tc cond in
      if cond_ty <> Typed_ast.TBool then
        type_error cond "while condition must be bool";
      let _tc_while, t_body, while_ret = type_block tc frtyp body true in
      (tc, Typed_ast.While (tcond, t_body), while_ret)
  | For (i_node, (start, fin, incl), step_opt, body) ->
      let tstart, start_ty = type_exp tc start in
      let tfin, fin_ty = type_exp tc fin in
      if not (is_number start_ty) then
        type_error start "Expected number type for left bound.";
      if not (is_number fin_ty) then
        type_error fin "Expected number type for right bound.";
      if not (equal_ty start_ty fin_ty) then
        type_error fin
          ("Expected right bound to have type " ^ Printer.show_ty start_ty
         ^ " so as to match left bound.");
      let t_step, s_ty =
        match step_opt with
        | Some s ->
            let ts, s_ty = type_exp tc s in
            if not (equal_ty s_ty start_ty) then
              type_error s
                ("Expected type " ^ Printer.show_ty start_ty
               ^ " so as to match bounds.");
            (ts, s_ty)
        | None -> (default_step start_ty stmt_n, Typed_ast.TInt (TSigned Ti32))
      in
      let tc_loop = Tctxt.add_local tc i_node.elt (start_ty, false) in
      let _tc_body, t_body, for_ret = type_block tc_loop frtyp body true in
      ( tc,
        Typed_ast.For (i_node.elt, tstart, tfin, incl, t_step, s_ty, t_body),
        for_ret )
  | ForEach (i_node, iter_exp, body) ->
      let titer, iter_ty = type_exp tc iter_exp in
      let elem_ty =
        match iter_ty with
        | Typed_ast.(TRef (RClass cls)) -> (
            let lookup mthd = Tctxt.lookup_method_option cls mthd tc in
            match (lookup Methods.iterate, lookup Methods.hasNext) with
            | Some (RetVal r, _), Some (RetVal b, _) when b = TBool -> r
            | _ ->
                type_error iter_exp
                  ("Class " ^ cls ^ " must implement " ^ Methods.iterate
                 ^ " and " ^ Methods.hasNext ^ "."))
        | Typed_ast.(TRef (RArray (t, _))) -> t
        | Typed_ast.(TRef RString) -> Typed_ast.(TInt (TSigned Ti8))
        | _ ->
            type_error iter_exp
              "For-loop must iterate over an array, string, or iterable class."
      in
      let tc_loop = Tctxt.add_local tc i_node.elt (elem_ty, false) in
      let _tc_body, t_body, for_ret = type_block tc_loop frtyp body true in
      (tc, Typed_ast.ForEach (i_node.elt, titer, iter_ty, t_body), for_ret)
  | Break ->
      if not in_loop then type_error stmt_n "break can only be used inside loop"
      else (tc, Typed_ast.Break, false)
  | Continue ->
      if not in_loop then
        type_error stmt_n "continue can only be used inside loop"
      else (tc, Typed_ast.Continue, false)
  | Free ens ->
      let tes =
        List.fold_left
          (fun acc en ->
            let te, ety = type_exp tc en in
            match ety with
            | Typed_ast.TRef (RClass _) -> te :: acc
            (* other TRefs are on the stack or global (strings) not suited for deletion *)
            | _ -> type_error en "Expected reference type for freeing!")
          [] ens
      in
      (tc, Typed_ast.Free tes, false)

and type_exp ?(expected : Typed_ast.ty option) (tc : Tctxt.t) (e : Ast.exp node)
    : Typed_ast.exp * Typed_ast.ty =
  let { elt = e'; loc = _ } = e in
  match e' with
  | Bool b ->
      check_expected_ty expected TBool e;
      (Typed_ast.Bool b, Typed_ast.TBool)
  | Int i -> (
      match expected with
      | Some (TInt target_ty) ->
          if fits_in_int_ty i target_ty then
            (Typed_ast.Int (i, target_ty), TInt target_ty)
          else
            type_error e
              ("Integer literal " ^ Z.to_string i ^ " does not fit in type "
              ^ Printer.show_ty (TInt target_ty))
      | Some t -> unexpected_ty t e "integer"
      | _ ->
          let inferred_ty = infer_integer_ty i e in
          (Typed_ast.Int (i, inferred_ty), TInt inferred_ty))
  | Float f ->
      let target_ty =
        match expected with
        | Some (TFloat target_ty) -> target_ty
        | Some t -> unexpected_ty t e "float"
        | None -> Typed_ast.Tf64
      in
      if fits_in_float_ty f target_ty then
        (Float (f, target_ty), TFloat target_ty)
      else
        type_error e
          ("Float literal " ^ Float.to_string f ^ " does not fit in type "
          ^ Printer.show_ty (TFloat target_ty))
  | Null ->
      let t =
        match expected with
        | Some (TRef ty) -> ty
        | Some _ ->
            type_error e "Null only allowed to reference types, not primitives"
        | None -> type_error e "Expected type for null"
      in
      (Typed_ast.Null t, TRef t)
  | Str s ->
      check_expected_ty expected (TRef RString) e;
      Typed_ast.(Str s, TRef RString)
  | Id i -> (
      match Tctxt.lookup_option i tc with
      | Some (t, _) ->
          check_expected_ty expected t e;
          (Id (i, t), t)
      | None -> type_error e ("variable " ^ i ^ " is not defined"))
  | Call ({ elt = Proj (obj, mth); loc = _ }, args) -> (
      match type_method (Proj (obj, mth)) args true tc with
      | Ok (Proj (tobj, _, cname, t), arg_types, typed_args, RetVal rt) ->
          check_expected_ty expected rt e;
          Typed_ast.
            (Call (Proj (tobj, mth, cname, t), typed_args, arg_types, rt), rt)
      | Error msg -> type_error e msg
      | _ -> type_error e "Unreachable state.")
  | Call (f, args) -> (
      let typed_callee, typ = type_exp tc f in
      match type_func args typ true tc with
      | Error msg -> type_error e msg
      | Ok (arg_types, typed_args, RetVal rt) ->
          check_expected_ty expected rt e;
          (Typed_ast.Call (typed_callee, typed_args, arg_types, rt), rt)
      | _ -> type_error e "Unreachable state.?")
  | Bop (binop, e1, e2) -> (
      let te1, lty = type_exp tc e1 in
      let te2, rty = type_exp tc e2 in
      match eval_const_exp e with
      | Some ev ->
          let inferred_int_ty = infer_integer_ty ev e in
          (Typed_ast.Int (ev, inferred_int_ty), TInt inferred_int_ty)
      | None ->
          let binop' = convert_binop binop in
          let res_ty =
            match binop with
            | Eqeq | Neq | Gt | Gte | Lt | Lte ->
                if equal_ty lty rty then Typed_ast.TBool
                else
                  type_error e
                    "== or != used with non type-compatible arguments"
            | And | Or | Xor ->
                if lty = Typed_ast.TBool && rty = Typed_ast.TBool then
                  Typed_ast.TBool
                else type_error e "boolean operator used on non-bool arguments"
            | At -> type_error e "@ not yet supported."
            | _ ->
                let args_valid = all_numbers [ lty; rty ] in
                (if not args_valid then
                   type_error e "using binary operator on non-number types"
                 else if not (equal_ty lty rty) then
                   type_error e
                     ("no operator between " ^ Printer.show_ty lty ^ " and "
                    ^ Printer.show_ty rty)
                 else
                   match binop with
                   | Pow ->
                       if not @@ is_float lty then
                         type_error e1 "Base of pow must be float type."
                       else if not @@ is_float rty then
                         type_error e2 "Exponent of pow must be float type."
                   | _ -> ());
                meet_number e (lty, rty)
          in
          (Typed_ast.Bop (binop', te1, te2, res_ty), res_ty))
  | Uop (unop, e1) ->
      let te1, ety = type_exp tc e1 in
      let unop' = convert_unop unop in
      let res_ty =
        match (unop, ety) with
        | Neg, t when is_number t -> (
            match expected with
            | Some (TInt (TUnsigned _)) ->
                type_error e "Cannot assign negative number to unsigned int."
            | _ -> t)
        | BNeg, t when is_number t -> t
        | Not, t when t = TBool -> TBool
        | _, t ->
            type_error e ("bad operand type, received " ^ Printer.show_ty t)
      in
      (Typed_ast.Uop (unop', te1, res_ty), res_ty)
  | Index (e_iter, e_idx) ->
      let t_iter, iter_ty = type_exp tc e_iter in
      let ty_of_array =
        match iter_ty with
        | Typed_ast.(TRef (RArray (arr_ty', _sz))) -> arr_ty'
        | t ->
            type_error e
              ("cannot index non-array type, recieved " ^ Printer.show_ty t)
      in
      check_expected_ty expected ty_of_array e;
      let t_idx, idx_ty = type_exp tc e_idx in
      let _ =
        match idx_ty with
        | Typed_ast.TInt _ -> ()
        | _ -> type_error e "index must be integer type"
      in
      (Typed_ast.Index (t_iter, t_idx, ty_of_array, iter_ty), ty_of_array)
  | Array _ -> type_array expected tc e
  | Cast (e, t) ->
      let te, e_ty = type_exp tc e in
      let tty = convert_ty t in
      check_expected_ty expected tty e;
      if subtype tc e_ty tty then (Typed_ast.Cast (te, tty), tty)
      else
        type_error e
          ("Cannot cast " ^ Printer.show_exp te ^ " which has type "
         ^ Printer.show_ty e_ty ^ " to type " ^ Printer.show_ty tty ^ ".")
  | Proj (ec, f) -> (
      let tec, e_ty = type_exp tc ec in
      match e_ty with
      | Typed_ast.(TRef (RClass cid)) -> (
          match Tctxt.lookup_field_option cid f tc with
          | Some (fty, _) ->
              check_expected_ty expected fty e;
              (Typed_ast.Proj (tec, f, cid, fty), fty)
          | None -> type_error ec ("Class " ^ cid ^ " has no member field " ^ f)
          )
      | _ -> type_error ec "Must project field of a class.")
  | Lambda _ | TypedLambda _ -> type_error e "not supported yet"
  | ObjInit ({ elt = cname; loc = cloc }, inits) ->
      let cfields, _methods =
        match Tctxt.lookup_class_option cname tc with
        | Some c -> c
        | None ->
            type_error
              { elt = cname; loc = cloc }
              ("Class  " ^ cname ^ " not found.")
      in
      check_expected_ty expected (TRef (RClass cname)) e;
      let initializes_field field =
        List.exists
          (fun ({ elt = fname; loc = _ }, _init) -> fname = field)
          inits
      in
      let missing_fields =
        List.filter_map
          (fun (fname, _, _, has_default) ->
            if not (initializes_field fname || has_default) then Some fname
            else None)
          cfields
      in
      if missing_fields <> [] then
        type_error e
          ("Missing fields in " ^ cname ^ " initialization: "
          ^ String.concat ", " missing_fields);
      let field_set = Hashtbl.create (List.length cfields) in
      let type_field_inits (fname_node, init) =
        let { elt = fname; loc = _ } = fname_node in
        if Hashtbl.mem field_set fname then
          type_error fname_node ("Already initialized field " ^ fname);
        Hashtbl.add field_set fname ();
        try
          let _, fty, _, _ =
            List.find (fun (fieldName, _, _, _) -> fieldName = fname) cfields
          in
          let tinit, _init_ty = type_exp ~expected:fty tc init in
          (fname, tinit)
        with Not_found ->
          type_error e
            ("Class " ^ cname ^ " does not contain member field " ^ fname)
      in
      let typed_inits = List.map type_field_inits inits in
      (Typed_ast.ObjInit (cname, typed_inits), Typed_ast.(TRef (RClass cname)))

and type_func (args : exp node list) (ftyp : Typed_ast.ty) (from_exp : bool)
    (tc : Tctxt.t) :
    (Typed_ast.ty list * Typed_ast.exp list * Typed_ast.ret_ty, string) result =
  let typecheck_args arg_types =
    ( arg_types,
      List.map2
        (fun aty a ->
          let te, ty = type_exp tc a in
          if equal_ty ty aty then te
          else
            let err_msg =
              "Invalid argument type for `" ^ show_exp a.elt ^ "`. Expected "
              ^ Printer.show_ty aty ^ ", got " ^ Printer.show_ty ty ^ "."
            in
            raise (TypeError err_msg))
        arg_types args )
  in
  match ftyp with
  | TRef (RFun (arg_types, RetVal rt_ty)) -> (
      try
        let arg_types, typed_args = typecheck_args arg_types in
        Ok (arg_types, typed_args, RetVal rt_ty)
      with
      | TypeError msg -> Error msg
      | Invalid_argument _ -> Error "invalid number of arguments supplied")
  | TRef (RFun (arg_types, RetVoid)) ->
      if from_exp then Error "assigning void function return type to variable."
      else
        let arg_types, typed_args = typecheck_args arg_types in
        Ok (arg_types, typed_args, RetVoid)
  | _ -> Error "attempted to call a non-function type."

and type_method (proj : exp) (args : exp node list) (from_exp : bool)
    (tc : Tctxt.t) :
    ( Typed_ast.exp * Typed_ast.ty list * Typed_ast.exp list * Typed_ast.ret_ty,
      string )
    result =
  match proj with
  | Proj (obj, mth) -> (
      let tobj, obj_ty = type_exp tc obj in
      match obj_ty with
      | Typed_ast.(TRef (RClass cid)) -> (
          match Tctxt.lookup_method_option cid mth tc with
          | Some (rt, argheaders) -> (
              let argtypes = List.map fst argheaders in
              let temp_func = Typed_ast.(TRef (RFun (argtypes, rt))) in
              match type_func args temp_func from_exp tc with
              | Error msg -> Error msg
              | Ok (arg_types, typed_args, rt) ->
                  Ok
                    ( Typed_ast.Proj
                        ( tobj,
                          mth,
                          cid,
                          TRef (RFun (List.map fst argheaders, rt)) ),
                      arg_types,
                      typed_args,
                      rt ))
          | None -> Error ("Class " ^ cid ^ " has no member method " ^ mth))
      | _ -> Error "Attempting to call method of non-class type.")
  | _ -> Error "Attemping to call method of non-class type."

and type_array (expected : Typed_ast.ty option) (tc : Tctxt.t) (en : exp node) :
    Typed_ast.exp * Typed_ast.ty =
  match (en.elt, expected) with
  | Array [], None ->
      type_error en
        "Could not infer type of empty array. Please provide type annotation."
  | Array [], Some (TRef (RArray (elt_ty, len))) ->
      let ty_of_array = Typed_ast.TRef (RArray (elt_ty, len)) in
      (Typed_ast.Array ([], ty_of_array), ty_of_array)
  | Array [], Some other ->
      type_error en
        ("Expected " ^ Printer.show_ty other ^ " but got empty array.")
  | Array (h :: t), exp_opt ->
      let exp_ty, exp_len =
        match exp_opt with
        | Some (TRef (RArray (ety, elen))) ->
            check_expected_ty expected (TRef (RArray (ety, elen))) en;
            (Some ety, Some elen)
        | _ -> (None, None)
      in
      let th, h_ty =
        match exp_ty with
        | Some ety -> type_exp ~expected:ety tc h
        | None -> type_exp tc h
      in
      let typed_elems =
        List.map
          (fun elem ->
            (* now that the head of the array has been typechecked 
             we can just check the rest of the array must match the head's type *)
            let te, ty = type_exp ~expected:h_ty tc elem in
            if equal_ty ty h_ty then te
            else
              type_error elem
                ("Array element type mismatch. Expected " ^ Printer.show_ty h_ty
               ^ " but got " ^ Printer.show_ty ty))
          t
      in
      let all_elems = th :: typed_elems in
      let len = List.length all_elems in
      (match exp_len with
      | Some elen when elen <> len ->
          type_error en
            ("Array length mismatch. Expected " ^ Int.to_string elen
           ^ " but got " ^ Int.to_string len)
      | _ -> ());
      let arr_ty = Typed_ast.(TRef (RArray (h_ty, len))) in
      (match exp_opt with
      | Some exp when not (equal_ty arr_ty exp) ->
          type_error en
            ("Array type mismatch. Expected " ^ Printer.show_ty exp
           ^ " but got " ^ Printer.show_ty arr_ty)
      | _ -> ());
      (Typed_ast.Array (all_elems, arr_ty), arr_ty)
  | _ -> type_error en "Somehow reached unreachable state."

and type_lambda (tc : Tctxt.t) (stmt_n : stmt node) =
  let { elt = stmt; loc = _ } = stmt_n in
  match stmt with
  | LambdaDecl (lname, None, { elt = Lambda _; _ }) ->
      type_error lname "Must include type defn of lambda on either LHS or RHS."
  | LambdaDecl (lname, Some t, { elt = TypedLambda (args, ret, body); loc }) ->
      let arg_types, ret_typ =
        match t with
        | RFun (arg_types, ret_typ) -> (arg_types, ret_typ)
        | _ -> type_error lname "Must specify function type for untyped lambda."
      in
      let all_types_match =
        try
          List.for_all2
            (fun ltyp (_, rtyp) -> equal_ty (convert_ty ltyp) (convert_ty rtyp))
            arg_types args
          && equal_ret_ty (convert_ret_ty ret_typ) (convert_ret_ty ret)
        with Invalid_argument _ ->
          type_error lname "LHS and RHS type specs must have same length."
      in
      if not all_types_match then
        type_error lname "Given LHS and RHS lambda types do not match."
      else
        type_lambda tc
          {
            elt =
              LambdaDecl
                (lname, None, { elt = TypedLambda (args, ret, body); loc });
            loc = stmt_n.loc;
          }
  | LambdaDecl (lname, Some t, { elt = Lambda (arg_ids, body); loc }) ->
      let arg_types, ret =
        match t with
        | RFun (arg_types, ret_typ) -> (arg_types, ret_typ)
        | _ -> type_error lname "Must specify function type for lambda."
      in
      let new_args =
        try List.combine arg_ids arg_types
        with Invalid_argument _ ->
          type_error lname "LHS types and RHS ids must have same length."
      in
      type_lambda tc
        {
          elt =
            LambdaDecl
              (lname, None, { elt = TypedLambda (new_args, ret, body); loc });
          loc = stmt_n.loc;
        }
  | LambdaDecl (lname, None, { elt = TypedLambda (args, ret, body); _ }) ->
      let converted_args = List.map (fun (i, t) -> (i, convert_ty t)) args in
      let converted_ret = convert_ret_ty ret in
      let lambda_tc =
        List.fold_left
          (fun tc' (i, t) -> Tctxt.add_local tc' i (t, false))
          tc converted_args
      in
      let _, typed_body, _ = type_block lambda_tc converted_ret body false in
      let lambda_type =
        Typed_ast.RFun (List.map snd converted_args, converted_ret)
      in
      let typed_lambda =
        Typed_ast.Lambda (converted_args, converted_ret, typed_body)
      in
      let tc' = Tctxt.add_local tc lname.elt (TRef lambda_type, false) in
      (tc', Typed_ast.LambdaDecl (lname.elt, lambda_type, typed_lambda), false)
  | _ -> type_error stmt_n "Even more impossible state"

and create_default_init (stmt_n : stmt node) (tc : Tctxt.t) = function
  | TBool -> Typed_ast.Bool false
  | TInt it -> Typed_ast.Int (Z.of_int 0, convert_int_ty it)
  | TFloat ft -> Typed_ast.Float (0.0, convert_float_ty ft)
  | TRef RString -> Typed_ast.Str ""
  | TRef (RClass cname) ->
      let default_constructor = cname in
      let constructor =
        match Tctxt.lookup_method_option cname default_constructor tc with
        | Some (RetVal rt, _) ->
            Typed_ast.Call (Id (default_constructor, rt), [], [], rt)
        | Some (_, _) ->
            type_error stmt_n
              ("Default constructor for " ^ cname ^ " cannot return void.")
        | None ->
            type_error stmt_n
              ("Must provide a default constructor for " ^ cname ^ " class.")
      in
      constructor
  | TRef (RFun _) -> type_error stmt_n "Default functions not allowed."
  | TRef (RGeneric _) -> type_error stmt_n "Generic default init to come some"
  | TRef (RArray (t, sz)) ->
      let sz' = Z.to_int sz in
      let lst = List.init sz' (fun _ -> create_default_init stmt_n tc t) in
      let t' = Typed_ast.TRef (RArray (convert_ty t, sz')) in
      Typed_ast.Array (lst, t')

and type_block (tc : Tctxt.t) (frtyp : Typed_ast.ret_ty)
    (stmts : stmt node list) (in_loop : bool) :
    Tctxt.t * Typed_ast.stmt list * bool =
  let tc_new, rev_stmts, does_ret =
    List.fold_left
      (fun (tc_acc, tstmts, does_ret) s ->
        if does_ret then
          type_error s "Dead code, function already returns before this.";
        let tc', tstmt, ret = type_stmt tc_acc frtyp s in_loop in
        (tc', tstmt :: tstmts, ret))
      (tc, [], false) stmts
  in
  (tc_new, List.rev rev_stmts, does_ret)
