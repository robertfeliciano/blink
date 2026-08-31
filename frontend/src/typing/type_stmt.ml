open Ast
open Type_util
open Conversions
module Printer = Pprint_typed_ast
module Methods = Util.Constants.Methods

type typed_projection = {
  exp : Typed_ast.exp;
  ty : Typed_ast.ty;
  is_const : bool;
}

(*
I wish there was an easy way to separate this into two separate files for exp and stmts.
The mutual recursion introduced by lambda expressions has forced me to combine everything
into one file. 
*)

let var_exists stmt_n i tc =
  match Tctxt.lookup_local_option i tc with
  | Some _ -> type_error stmt_n ("Variable " ^ i ^ " already exists.")
  | None -> ()

let type_integer_constant ?(description = "Constant expression result") node
    value int_ty =
  let ty = Typed_ast.TInt int_ty in
  if fits_in_int_ty value int_ty then (Typed_ast.Int (value, int_ty), ty)
  else
    type_error node
      (description ^ " " ^ Z.to_string value ^ " does not fit in "
     ^ Printer.show_ty ty ^ ".")

let validate_assignment_operator stmt_n op lhs_ty =
  let require_number () =
    if not (is_number lhs_ty) then
      type_error stmt_n
        ("Assignment operator " ^ show_aop op ^ " requires a numeric target, got "
       ^ Printer.show_ty lhs_ty ^ ".")
  in
  let require_integer () =
    if not (is_integer lhs_ty) then
      type_error stmt_n
        ("Assignment operator " ^ show_aop op
       ^ " requires an integer target, got " ^ Printer.show_ty lhs_ty ^ ".")
  in
  match op with
  | Eq -> ()
  | PluEq | MinEq | TimEq | DivEq -> require_number ()
  | PowEq ->
      if not (is_float lhs_ty) then
        type_error stmt_n
          ("Assignment operator " ^ show_aop op
         ^ " requires a floating-point target, got " ^ Printer.show_ty lhs_ty
         ^ ".")
  | ModEq | ShlEq | LShrEq | AShrEq | BXorEq | BAndEq | BOrEq ->
      require_integer ()
  | AtEq -> type_error stmt_n "Assignment operator AtEq is not supported."

let rec type_stmt (enclosing_class : id option) (tc : Tctxt.t)
    (frtyp : Typed_ast.ret_ty) (stmt_n : stmt node) (in_loop : bool) :
    Tctxt.t * Typed_ast.stmt * bool =
  let { elt = stmt; loc = _ } = stmt_n in
  match stmt with
  | Decl (_, None, None, _) ->
      type_error stmt_n "Must provide type or initial value."
  | Decl (i, Some ty, None, const) ->
      var_exists stmt_n i tc;
      let e_ty = validate_and_convert_ty stmt_n tc ty in
      let e = create_default_init stmt_n tc e_ty in
      let tc' = Tctxt.add_local tc i (e_ty, const) in
      (tc', Typed_ast.Decl (i, e_ty, e, const), false)
  | Decl (i, None, Some en, const) ->
      var_exists stmt_n i tc;
      let te, e_ty = type_exp tc en enclosing_class in
      let tc', resolved_ty = (Tctxt.add_local tc i (e_ty, const), e_ty) in
      (tc', Typed_ast.Decl (i, resolved_ty, te, const), false)
  | Decl (i, Some given_ty_ast, Some en, const) ->
      var_exists stmt_n i tc;
      let given_ty = validate_and_convert_ty stmt_n tc given_ty_ast in
      let te, _ = type_exp_as given_ty tc en enclosing_class in
      let tc' = Tctxt.add_local tc i (given_ty, const) in
      (tc', Typed_ast.Decl (i, given_ty, te, const), false)
  | Assn (lhs, op, rhs) ->
      let tlhs, lhsty = type_lvalue tc lhs enclosing_class in
      validate_assignment_operator stmt_n op lhsty;
      let trhs, _ = type_exp_as lhsty tc rhs enclosing_class in
      (tc, Typed_ast.Assn (tlhs, convert_aop op, trhs, lhsty), false)
  | Ret expr ->
      let te_opt =
        match (expr, frtyp) with
        | Some e, RetVal r_ty ->
            let te, _ = type_exp_as r_ty tc e enclosing_class in
            Some te
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
      match type_method_app (Proj (obj, mth)) args false tc enclosing_class with
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
      let typed_callee, typ = type_exp tc f enclosing_class in
      let arg_types, typed_args, ret =
        match type_func_app args typ false tc enclosing_class with
        | Error msg -> type_error stmt_n msg
        | Ok (arg_types, typed_args, RetVoid) ->
            (arg_types, typed_args, Typed_ast.RetVoid)
        | Ok (arg_types, typed_args, t) ->
            type_warning stmt_n "Ignoring non-void function";
            (arg_types, typed_args, t)
      in
      (tc, Typed_ast.SCall (typed_callee, typed_args, arg_types, ret), false)
  | If (cond, then_branch, else_branch) ->
      let tcond, _ = type_exp_as Typed_ast.TBool tc cond enclosing_class in
      let _tc_then, t_then, if_ret =
        type_block tc frtyp then_branch in_loop enclosing_class
      in
      let _tc_else, t_else, else_ret =
        type_block tc frtyp else_branch in_loop enclosing_class
      in
      (tc, Typed_ast.If (tcond, t_then, t_else), if_ret && else_ret)
  | While (cond, body) ->
      let tcond, _ = type_exp_as Typed_ast.TBool tc cond enclosing_class in
      let _tc_while, t_body, _body_returns =
        type_block tc frtyp body true enclosing_class
      in
      (* A while body may execute zero times, so a return in the body does not
         guarantee that control cannot fall through the loop. *)
      (tc, Typed_ast.While (tcond, t_body), false)
  | For (i_node, (start, fin, incl), step_opt, body) ->
      let tstart, start_ty = type_exp tc start enclosing_class in
      let tfin, fin_ty = type_exp tc fin enclosing_class in
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
            let ts, s_ty = type_exp tc s enclosing_class in
            if not (equal_ty s_ty start_ty) then
              type_error s
                ("Expected type " ^ Printer.show_ty start_ty
               ^ " so as to match bounds.");
            (ts, s_ty)
        | None -> (default_step start_ty stmt_n, start_ty)
      in
      let tc_loop = Tctxt.add_local tc i_node.elt (start_ty, false) in
      let _tc_body, t_body, _body_returns =
        type_block tc_loop frtyp body true enclosing_class
      in
      ( tc,
        Typed_ast.For (i_node.elt, tstart, tfin, incl, t_step, s_ty, t_body),
        false )
  | ForEach (i_node, iter_exp, body) ->
      let titer, iter_ty = type_exp tc iter_exp enclosing_class in
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
      let _tc_body, t_body, _body_returns =
        type_block tc_loop frtyp body true enclosing_class
      in
      (tc, Typed_ast.ForEach (i_node.elt, titer, iter_ty, t_body), false)
  | Break ->
      if not in_loop then type_error stmt_n "break can only be used inside loop"
      else (tc, Typed_ast.Break, false)
  | Continue ->
      if not in_loop then
        type_error stmt_n "continue can only be used inside loop"
      else (tc, Typed_ast.Continue, false)
  | Free ens ->
      let type_free_exp en =
        let te, ety = type_exp tc en enclosing_class in
        match ety with
        | Typed_ast.TRef _ -> te
        (* Other values live on the stack and are not suited for deletion. *)
        | _ -> type_error en "Expected reference type for freeing!"
      in
      let tes = List.map type_free_exp ens in
      (tc, Typed_ast.Free tes, false)

and type_lvalue (tc : Tctxt.t) (lhs : Ast.exp node)
    (enclosing_class : id option) : Typed_ast.exp * Typed_ast.ty =
  match lhs.elt with
  | Id id -> (
      match Tctxt.lookup_option id tc with
      | Some (_, true) ->
          type_error lhs "Attempting to assign to a constant binding."
  | Some _ -> type_exp tc lhs enclosing_class
      | None -> type_error lhs ("variable " ^ id ^ " is not defined"))
  | Proj (obj, field) ->
      let projection = type_projection None tc lhs obj field enclosing_class in
      if projection.is_const then
        type_error lhs ("Attempting to assign to constant field " ^ field ^ ".")
      else (projection.exp, projection.ty)
  | Index _ ->
      (* Const references are shallow: their elements remain mutable. *)
      type_exp tc lhs enclosing_class
  | _ -> type_error lhs "cannot assign to this expression"

and type_exp ?(expected : Typed_ast.ty option) (tc : Tctxt.t) (e : Ast.exp node)
    (enclosing_class : id option) : Typed_ast.exp * Typed_ast.ty =
  let { elt = e'; loc = _ } = e in
  match e' with
  | Bool b ->
      check_expected_ty expected TBool e;
      (Typed_ast.Bool b, Typed_ast.TBool)
  | Int i -> (
      match expected with
      | Some (TInt target_ty) ->
          type_integer_constant ~description:"Integer literal" e i target_ty
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
      if float_is_representable_in_ty f target_ty then
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
      let handle_global_or_field i t tc e enclosing_class =
        match enclosing_class with
        | Some cn ->
            let fields, _ =
              match Tctxt.lookup_class_option cn tc with
              | Some class_info -> class_info
              | None -> type_error e ("Enclosing class " ^ cn ^ " is undefined")
            in
            let is_field = List.exists (fun (f, _, _, _) -> f = i) fields in

            if is_field then
              match Tctxt.lookup_local_option "this" tc with
              | Some ((TRef (RClass cname) as clazz), _) ->
                  Typed_ast.(Proj (Id ("this", clazz), i, cname, t), t)
              | _ -> type_error e "Field access found but 'this' is undefined"
            else (Id (i, t), t)
        | None -> (Id (i, t), t)
      in
      match Tctxt.lookup_local_option i tc with
      | Some (t, _) ->
          check_expected_ty expected t e;
          (Typed_ast.Id (i, t), t)
      | None -> (
          match Tctxt.lookup_global_option i tc with
          | Some (t, _) -> handle_global_or_field i t tc e enclosing_class
          | None -> (
              match Tctxt.lookup_proto_option i tc with
              | Some (t, _) -> (Id (i, t), t)
              | None -> type_error e ("variable " ^ i ^ " is not defined"))))
  | Call ({ elt = Proj (obj, mth); loc = _ }, args) -> (
      match type_method_app (Proj (obj, mth)) args true tc enclosing_class with
      | Ok (Proj (tobj, _, cname, t), arg_types, typed_args, RetVal rt) ->
          check_expected_ty expected rt e;
          Typed_ast.
            (Call (Proj (tobj, mth, cname, t), typed_args, arg_types, rt), rt)
      | Error msg -> type_error e msg
      | _ -> type_error e "Unreachable state.")
  | Call (f, args) -> (
      let typed_callee, typ = type_exp tc f enclosing_class in
      match type_func_app args typ true tc enclosing_class with
      | Error msg -> type_error e msg
      | Ok (arg_types, typed_args, RetVal rt) ->
          check_expected_ty expected rt e;
          (Typed_ast.Call (typed_callee, typed_args, arg_types, rt), rt)
      | _ -> type_error e "Unreachable state.?")
  | Bop (binop, e1, e2) -> (
      let te1, lty = type_exp tc e1 enclosing_class in
      let te2, rty = type_exp tc e2 enclosing_class in
      let promote te from_ty to_ty =
        if equal_ty from_ty to_ty then te else Typed_ast.Cast (te, to_ty)
      in
      let promote_numbers () =
        if is_number lty && is_number rty then
          let operand_ty = meet_number e (lty, rty) in
          (promote te1 lty operand_ty, promote te2 rty operand_ty, operand_ty)
        else
          type_error e
            ("Numeric operator cannot be applied to " ^ Printer.show_ty lty
           ^ " and " ^ Printer.show_ty rty ^ ".")
      in
      let te1', te2', operand_ty, res_ty =
        match binop with
        | Eqeq | Neq when is_number lty && is_number rty ->
            let te1', te2', operand_ty = promote_numbers () in
            (te1', te2', operand_ty, Typed_ast.TBool)
        | Eqeq | Neq ->
            if equal_ty lty rty then (te1, te2, lty, Typed_ast.TBool)
            else
              type_error e
                ("Cannot compare " ^ Printer.show_ty lty ^ " with "
               ^ Printer.show_ty rty ^ ".")
        | Gt | Gte | Lt | Lte ->
            let te1', te2', operand_ty = promote_numbers () in
            (te1', te2', operand_ty, Typed_ast.TBool)
        | And | Or | Xor ->
            if equal_ty lty Typed_ast.TBool && equal_ty rty Typed_ast.TBool then
              (te1, te2, Typed_ast.TBool, Typed_ast.TBool)
            else type_error e "Boolean operator used on non-bool arguments."
        | Shl | Lshr | Ashr | BAnd | BOr | BXor ->
            if is_integer lty && is_integer rty then
              let te1', te2', operand_ty = promote_numbers () in
              (te1', te2', operand_ty, operand_ty)
            else type_error e "Bitwise operator used on non-integer arguments."
        | Pow ->
            if is_float lty && is_float rty then
              let te1', te2', operand_ty = promote_numbers () in
              (te1', te2', operand_ty, operand_ty)
            else type_error e "Pow operands must both have float type."
        | Mod ->
            if is_integer lty && is_integer rty then
              let te1', te2', operand_ty = promote_numbers () in
              (te1', te2', operand_ty, operand_ty)
            else type_error e "Modulo operands must both have integer type."
        | Add | Sub | Mul | Div ->
            let te1', te2', operand_ty = promote_numbers () in
            (te1', te2', operand_ty, operand_ty)
        | At -> type_error e "@ not yet supported."
      in
      match (eval_const_exp e, operand_ty) with
      | Some ev, Typed_ast.TInt int_ty ->
          let resolved_int_ty =
            match expected with Some (TInt ty) -> ty | _ -> int_ty
          in
          let typed_constant, constant_ty =
            type_integer_constant e ev resolved_int_ty
          in
          check_expected_ty expected constant_ty e;
          (typed_constant, constant_ty)
      | _ ->
          check_expected_ty expected res_ty e;
          ( Typed_ast.Bop (convert_binop binop, te1', te2', res_ty),
            res_ty ))
  | Uop (unop, e1) ->
      let te1, ety = type_exp tc e1 enclosing_class in
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
      let t_iter, iter_ty = type_exp tc e_iter enclosing_class in
      let ty_of_array =
        match iter_ty with
        | Typed_ast.(TRef (RArray (arr_ty', _sz))) -> arr_ty'
        | t ->
            type_error e
              ("cannot index non-array type, recieved " ^ Printer.show_ty t)
      in
      check_expected_ty expected ty_of_array e;
      let t_idx, idx_ty = type_exp tc e_idx enclosing_class in
      let _ =
        match idx_ty with
        | Typed_ast.TInt _ -> ()
        | _ -> type_error e "index must be integer type"
      in
      (Typed_ast.Index (t_iter, t_idx, ty_of_array, iter_ty), ty_of_array)
  | Array _ -> type_array enclosing_class expected tc e
  | Cast (ec, t) ->
      let te, e_ty = type_exp tc ec enclosing_class in
      let tty = validate_and_convert_ty e tc t in
      check_expected_ty expected tty ec;
      (match t with
      | TRef (RFun _) -> type_error e "Cannot cast functions/lambdas."
      | _ -> ());
      if subtype tc e_ty tty then (Typed_ast.Cast (te, tty), tty)
      else
        type_error ec
          ("Cannot cast " ^ Printer.show_exp te ^ " which has type "
         ^ Printer.show_ty e_ty ^ " to type " ^ Printer.show_ty tty ^ ".")
  | Proj (ec, f) ->
      let projection = type_projection expected tc e ec f enclosing_class in
      (projection.exp, projection.ty)
  | Lambda (scope, arg_ids, body) -> (
      (* TODO get better node for errors from parser *)
      let local_tc, t_scope = type_lambda_scope tc scope in
      let tc' = { tc with locals = local_tc } in
      match expected with
      | Some t ->
          let arg_types, ret =
            match t with
            | TRef (RFun (arg_types, ret_typ)) -> (arg_types, ret_typ)
            | _ -> type_error e "Must specify function type for lambda"
          in
          let new_args =
            match map2_exact (fun id ty -> (id, ty)) arg_ids arg_types with
            | Some args -> args
            | None ->
                type_error e "LHS types and RHS ids must have same length."
          in
          create_typed_lambda e tc' new_args ret body enclosing_class t_scope
      | None -> type_error e "Must specify variable type for untyped lambda.")
  | TypedLambda (scope, args, rhs_ret, body) -> (
      let local_tc, t_scope = type_lambda_scope tc scope in
      let tc' = { tc with locals = local_tc } in
      let rhs_args =
        List.map
          (fun (i, t) -> (i, validate_and_convert_ty e tc t))
          args
      in
      let rhs_ret = validate_and_convert_ret_ty e tc rhs_ret in
      match expected with
      | Some t ->
          let lhs_arg_types, lhs_ret =
            match t with
            | TRef (RFun (arg_types, ret_typ)) -> (arg_types, ret_typ)
            | _ -> type_error e "Invalid type specified for lambda"
          in
          if
            not
              (lists_equal_exact
                 (fun t1 (_, t2) -> equal_ty t1 t2)
                 lhs_arg_types rhs_args)
          then
            type_error e "LHS and RHS types must match exactly.";
          if not (equal_ret_ty lhs_ret rhs_ret) then
            type_error e "LHS and RHS types must match exactly.";
          create_typed_lambda e tc' rhs_args rhs_ret body enclosing_class t_scope
      | None ->
          create_typed_lambda e tc' rhs_args rhs_ret body enclosing_class t_scope)
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
        match
          List.find_opt (fun (fieldName, _, _, _) -> fieldName = fname) cfields
        with
        | Some (_, fty, _, _) ->
            let tinit, _init_ty = type_exp_as fty tc init enclosing_class in
            (fname, tinit)
        | None ->
            type_error fname_node
              ("Class " ^ cname ^ " does not contain member field " ^ fname)
      in
      let typed_inits = List.map type_field_inits inits in
      (Typed_ast.ObjInit (cname, typed_inits), Typed_ast.(TRef (RClass cname)))

and type_projection (expected : Typed_ast.ty option) (tc : Tctxt.t)
    (projection : Ast.exp node) (obj : Ast.exp node) (field : id)
    (enclosing_class : id option) : typed_projection =
  let typed_obj, obj_ty = type_exp tc obj enclosing_class in
  match obj_ty with
  | Typed_ast.TRef (RClass class_id) -> (
      match Tctxt.lookup_field_option class_id field tc with
      | Some (field_ty, is_const) ->
          check_expected_ty expected field_ty projection;
          {
            exp = Typed_ast.Proj (typed_obj, field, class_id, field_ty);
            ty = field_ty;
            is_const;
          }
      | None ->
          type_error projection
            ("Class " ^ class_id ^ " has no member field " ^ field))
  | _ -> type_error obj "Must project field of a class."

and type_exp_as (expected : Typed_ast.ty) (tc : Tctxt.t) (e : Ast.exp node)
    (enclosing_class : id option) : Typed_ast.exp * Typed_ast.ty =
  let promote te actual =
    match (te, expected) with
    | Typed_ast.Int (value, _), Typed_ast.TInt int_ty ->
        type_integer_constant e value int_ty
    | _ when equal_ty actual expected -> (te, expected)
    | _ when is_number actual && is_number expected ->
        let promoted = meet_number e (actual, expected) in
        if equal_ty promoted expected then
          (Typed_ast.Cast (te, expected), expected)
        else
          type_error e
            ("Cannot implicitly promote " ^ Printer.show_ty actual ^ " to "
           ^ Printer.show_ty expected ^ ".")
    | _ ->
        type_error e
          ("Expected " ^ Printer.show_ty expected ^ " but got "
         ^ Printer.show_ty actual ^ ".")
  in
  match (e.elt, expected) with
  | Int n, Typed_ast.TFloat float_ty ->
      let te, _actual = type_exp tc e enclosing_class in
      if int_is_exactly_representable_in_float_ty n float_ty then
        (Typed_ast.Cast (te, expected), expected)
      else
        type_error e
          ("Integer literal " ^ Z.to_string n ^ " cannot be represented exactly as "
         ^ Printer.show_ty expected ^ ".")
  | (Int _ | Float _ | Null | Array _ | Lambda _ | TypedLambda _), _ ->
      type_exp ~expected tc e enclosing_class
  | _ ->
      let te, actual = type_exp tc e enclosing_class in
      promote te actual

and type_func_app (args : exp node list) (ftyp : Typed_ast.ty) (from_exp : bool)
    (tc : Tctxt.t) (enclosing_class : id option) :
    (Typed_ast.ty list * Typed_ast.exp list * Typed_ast.ret_ty, string) result =
  let typecheck_args arg_types =
    let expected_count = List.length arg_types in
    let actual_count = List.length args in
    if expected_count <> actual_count then
      Error
        ("invalid number of arguments supplied: expected "
        ^ Int.to_string expected_count ^ " but got " ^ Int.to_string actual_count)
    else
      match
        map2_exact
          (fun aty a ->
            let te, _ = type_exp_as aty tc a enclosing_class in
            te)
          arg_types args
      with
      | Some typed_args -> Ok typed_args
      | None -> Error "failed to pair function arguments"
  in
  match ftyp with
  | TRef (RFun (_, RetVoid)) when from_exp ->
      Error "assigning void function return type to variable."
  | TRef (RFun (arg_types, ret_ty)) -> (
      match typecheck_args arg_types with
      | Ok typed_args -> Ok (arg_types, typed_args, ret_ty)
      | Error _ as error -> error)
  | _ -> Error "attempted to call a non-function type."

and type_method_app (proj : exp) (args : exp node list) (from_exp : bool)
    (tc : Tctxt.t) (enclosing_class : id option) :
    ( Typed_ast.exp * Typed_ast.ty list * Typed_ast.exp list * Typed_ast.ret_ty,
      string )
    result =
  match proj with
  | Proj (obj, mth) -> (
      let tobj, obj_ty = type_exp tc obj enclosing_class in
      match obj_ty with
      | Typed_ast.(TRef (RClass cid)) -> (
          match Tctxt.lookup_method_option cid mth tc with
          | Some (rt, argheaders) -> (
              let argtypes = List.map fst argheaders in
              let temp_func = Typed_ast.(TRef (RFun (argtypes, rt))) in
              match
                type_func_app args temp_func from_exp tc enclosing_class
              with
              | Error msg -> Error msg
              | Ok (arg_types, typed_args, rt) ->
                  Ok
                    ( Typed_ast.Proj
                        ( tobj,
                          mth,
                          cid,
                          TRef (RFun (argtypes, rt)) ),
                      arg_types,
                      typed_args,
                      rt ))
          | None -> Error ("Class " ^ cid ^ " has no member method " ^ mth))
      | _ -> Error "Attempting to call method of non-class type.")
  | _ -> Error "Attemping to call method of non-class type."

and type_array (enclosing_class : id option) (expected : Typed_ast.ty option)
    (tc : Tctxt.t) (en : exp node) : Typed_ast.exp * Typed_ast.ty =
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
            (Some ety, Some elen)
        | Some other ->
            type_error en
              ("Expected " ^ Printer.show_ty other ^ " but got an array.")
        | None -> (None, None)
      in
      let th, h_ty =
        match exp_ty with
        | Some ety -> type_exp_as ety tc h enclosing_class
        | None -> type_exp tc h enclosing_class
      in
      let typed_elems =
        List.map
          (fun elem ->
            (* now that the head of the array has been typechecked 
             we can just check the rest of the array must match the head's type *)
            let te, _ = type_exp_as h_ty tc elem enclosing_class in
            te)
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
      (Typed_ast.Array (all_elems, arr_ty), arr_ty)
  | _ -> type_error en "Somehow reached unreachable state."

and create_default_init (stmt_n : stmt node) (tc : Tctxt.t) = function
  | Typed_ast.TBool -> Typed_ast.Bool false
  | Typed_ast.TInt it -> Typed_ast.Int (Z.of_int 0, it)
  | Typed_ast.TFloat ft -> Typed_ast.Float (0.0, ft)
  | Typed_ast.TRef RString -> Typed_ast.Str ""
  | Typed_ast.TRef (RClass cname) ->
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
  | Typed_ast.TRef (RFun _) -> type_error stmt_n "Default functions not allowed."
  | Typed_ast.TRef (RArray (t, sz)) as array_ty ->
      let elems = List.init sz (fun _ -> create_default_init stmt_n tc t) in
      Typed_ast.Array (elems, array_ty)

and type_block (tc : Tctxt.t) (frtyp : Typed_ast.ret_ty)
    (stmts : stmt node list) (in_loop : bool) (enclosing_class : id option) :
    Tctxt.t * Typed_ast.stmt list * bool =
  let tc_new, rev_stmts, does_ret =
    List.fold_left
      (fun (tc_acc, tstmts, does_ret) s ->
        if does_ret then
          type_error s "Dead code, function already returns before this.";
        let tc', tstmt, ret =
          type_stmt enclosing_class tc_acc frtyp s in_loop
        in
        (tc', tstmt :: tstmts, ret))
      (tc, [], false) stmts
  in
  (tc_new, List.rev rev_stmts, does_ret)

and create_typed_lambda (lambda_node : exp node) (tc : Tctxt.t)
    (args : (id * Typed_ast.ty) list) (ret : Typed_ast.ret_ty) (body : block)
    (enclosing_class : id option) (scope : Typed_ast.exp list) =
  let ltc =
    List.fold_left (fun tc' (i, t) -> Tctxt.add_local tc' i (t, false)) tc args
  in
  let _, t_body, does_ret = type_block ltc ret body false enclosing_class in
  check_body_return_completeness lambda_node ret ~does_ret ~body_kind:"lambda";
  let lambda_typ = Typed_ast.(TRef (RFun (List.map snd args, ret))) in
  let t_lambda = Typed_ast.Lambda (scope, args, ret, t_body) in
  (t_lambda, lambda_typ)

and type_lambda_scope (tc : Tctxt.t) (scope : exp node list) =
  let locals', scope' =
    List.fold_left
      (fun (locals, scope_acc) en ->
        let { elt = v; loc = _ } = en in
        match v with
        | Id i -> (
            match Tctxt.lookup_option i tc with
            | Some (t, _) ->
                let tid = Typed_ast.Id (i, t) in
                let tc_entry = (i, (t, false)) in
                (tc_entry :: locals, tid :: scope_acc)
            | None -> type_error en ("Variable " ^ i ^ "is not defined."))
        | _ -> type_error en "Only support ids for lambda scope now.")
      ([], []) scope
  in
  (locals', scope')
