open Ast
open Tctxt
open Type_exp
open Type_util
open Conversions
module Printer = Pprint_typed_ast

let rec type_stmt (tc : Tctxt.t) (frtyp : Typed_ast.ret_ty) (stmt_n : stmt node)
    (in_loop : bool) : Tctxt.t * Typed_ast.stmt * bool =
  let { elt = stmt; loc = _ } = stmt_n in
  match stmt with
  | Decl (i, None, en, const) ->
      let te, e_ty = type_exp tc en in
      let tc', resolved_ty = (add_local tc i e_ty, e_ty) in
      (tc', Typed_ast.Decl (i, resolved_ty, te, const), false)
  | Decl (i, Some given_ty_ast, en, const) ->
      let given_ty = convert_ty given_ty_ast in
      let te, e_ty = type_exp ~expected:given_ty tc en in
      let tc', resolved_ty =
        match (e_ty, given_ty, te) with
        | Typed_ast.TInt _, Typed_ast.TInt given_num_ty, Typed_ast.Int (n, _) ->
            if fits_in_int_ty n given_num_ty then
              (add_local tc i given_ty, given_ty)
            else
              type_error stmt_n
                ("Integer literal " ^ Z.to_string n ^ " does not fit in type "
               ^ Printer.show_ty given_ty)
        | ( Typed_ast.TFloat _,
            Typed_ast.TFloat given_float_ty,
            Typed_ast.Float (f, _) ) ->
            if fits_in_float_ty f given_float_ty then
              (add_local tc i given_ty, given_ty)
            else type_error stmt_n "Float literal out of range for f32"
        | Typed_ast.TInt _, Typed_ast.TFloat given_float_ty, Typed_ast.Int (n, _)
          ->
            (* trying to auto upcast int to float *)
            if int_in_float n given_float_ty then
              (add_local tc i given_ty, given_ty)
            else
              type_error stmt_n
                ("Integer literal " ^ Z.to_string n
               ^ " does not fit in float type " ^ Printer.show_ty given_ty)
        (* note - we will not auto downcast, i.e. `let x: u8 = 12.14` is not valid *)
        | _ ->
            if given_ty = e_ty then (add_local tc i given_ty, given_ty)
            else
              type_error stmt_n
                ("Provided type " ^ show_ty given_ty_ast
               ^ " does not match inferred type " ^ Printer.show_ty e_ty)
      in
      (tc', Typed_ast.Decl (i, resolved_ty, te, const), false)
  | Assn (lhs, op, rhs) ->
      (match lhs.elt with
      | Id _ -> ()
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
      | Ok (Proj (tobj, _), typed_args, RetVal _) ->
          type_warning stmt_n "Ignoring non-void function";
          (tc, Typed_ast.(SCall (Proj (tobj, mth), typed_args)), false)
      | Ok (Proj (tobj, _), typed_args, _) ->
          (tc, Typed_ast.(SCall (Proj (tobj, mth), typed_args)), false)
      | _ -> type_error stmt_n "Unreachable state.")
  | SCall (f, args) ->
      let typed_callee, typ = type_exp tc f in
      let typed_args =
        match type_func args typ false tc with
        | Error msg -> type_error stmt_n msg
        | Ok (typed_args, RetVal _) ->
            type_warning stmt_n "Ignoring non-void function";
            typed_args
        | Ok (typed_args, _) -> typed_args
      in
      (tc, Typed_ast.SCall (typed_callee, typed_args), false)
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
  | For (i_node, iter_exp, step_opt, body) ->
      let titer, iter_ty = type_exp tc iter_exp in
      let elem_ty =
        match iter_ty with
        | Typed_ast.(TRef (RArray (t, _))) -> t
        | Typed_ast.(TRef RString) -> Typed_ast.(TInt (TSigned Ti8))
        | Typed_ast.(TRef (RRange (t1, _t2))) -> t1
        | _ ->
            type_error iter_exp
              "for loop must iterate over an array, string, or range"
      in
      let t_step =
        match (iter_ty, step_opt) with
        | Typed_ast.(TRef (RRange _)), Some step_exp ->
            let ts, step_ty = type_exp tc step_exp in
            if is_number step_ty then ts
            else type_error step_exp "for loop step must be an integer"
        | Typed_ast.(TRef (RRange _)), None ->
            Typed_ast.(Int (Z.of_int 1, TSigned Ti32))
        | _, Some _ ->
            type_error iter_exp
              "step is only allowed when iterating over ranges"
        | _, None ->
            (* no step needed for strings/arrays *)
            Typed_ast.(Int (Z.of_int 1, TSigned Ti32))
        (* ignored *)
      in
      let tc_loop = add_local tc i_node.elt elem_ty in
      let _tc_body, t_body, for_ret = type_block tc_loop frtyp body true in
      (tc, Typed_ast.For (i_node.elt, titer, t_step, t_body), for_ret)
  | Break ->
      if not in_loop then type_error stmt_n "break can only be used inside loop"
      else (tc, Typed_ast.Break, false)
  | Continue ->
      if not in_loop then
        type_error stmt_n "continue can only be used inside loop"
      else (tc, Typed_ast.Continue, false)

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
