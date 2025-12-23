open Desugar_util
open Conversions
module Methods = Util.Constants.Methods
module Typed = Typing.Typed_ast
module Type_stmt = Typing.Type_stmt
module D = Desugared_ast
module A = Ast

let base_op = function
  | Typed.PluEq -> D.Add
  | Typed.MinEq -> D.Sub
  | Typed.TimEq -> D.Mul
  | Typed.DivEq -> D.Div
  | Typed.AtEq -> D.At
  | Typed.PowEq -> D.Pow
  | Typed.ModEq -> D.Mod
  | Typed.ShlEq -> D.Shl
  | Typed.LShrEq -> D.Lshr
  | Typed.AShrEq -> D.Ashr
  | Typed.BXorEq -> D.BXor
  | Typed.BAndEq -> D.BAnd
  | Typed.BOrEq -> D.BOr
  | Typed.Eq -> desugar_error "unreachable state"

let rec desugar_stmt (stmt : Typed.stmt) : D.stmt list =
  match stmt with
  | Assn (lhs, op, rhs, t) when op <> Eq ->
      let op' = base_op op in
      let lhs_stmts, lhs' = desugar_exp lhs in
      let rhs_stmts, rhs' = desugar_exp rhs in
      let t' = convert_ty t in
      let bop = D.Bop (op', lhs', rhs', t') in
      lhs_stmts @ rhs_stmts @ [ Assn (lhs', bop, t') ]
  | Assn (lhs, _eq, rhs, t) ->
      let lhs_stmts, lhs' = desugar_exp lhs in
      let rhs_stmts, rhs' = desugar_exp rhs in
      let t' = convert_ty t in
      lhs_stmts @ rhs_stmts @ [ Assn (lhs', rhs', t') ]
  | If (cond, b1, b2) ->
      let cstmts, cond' = desugar_exp cond in
      let then_block = desugar_block b1 in
      let else_block = desugar_block b2 in
      cstmts @ [ If (cond', then_block, else_block) ]
  | For (iter, start, fin, incl, step, ty, body) ->
      let s_stmts, start' = desugar_exp start in
      let f_stmts, fin' = desugar_exp fin in
      let st_stmts, step' = desugar_exp step in
      let prelude = s_stmts @ f_stmts @ st_stmts in
      let step_name = "%step" in
      let ty = convert_ty ty in
      let step_ = D.Id (step_name, ty) in
      let iter_ = D.Id (iter, ty) in
      let step_decl = D.Decl (step_name, ty, step', true) in
      let iter_decl = D.Decl (iter, ty, start', false) in
      let zero = get_zero ty in
      let up_to = if incl then D.Lte else Lt in
      let down_to = if incl then D.Gte else Gt in
      let create_cond cmp = D.Bop (cmp, iter_, fin', TBool) in
      let body' = desugar_block body in
      let step_dec =
        D.If
          ( Bop (Lt, step_, zero, TBool),
            [
              While
                ( create_cond down_to,
                  body' @ [ Assn (iter_, Bop (Sub, iter_, step_, ty), ty) ] );
            ],
            [] )
      in
      let step_inc =
        D.If
          ( Bop (Gt, step_, zero, TBool),
            [
              While
                ( create_cond up_to,
                  body' @ [ Assn (iter_, Bop (Add, iter_, step_, ty), ty) ] );
            ],
            [ step_dec ] )
      in
      let zero_check =
        D.If
          ( Bop (Eqeq, step_, zero, TBool),
            [
              SCall
                ( Id ("exit", TRef (RFun ([], RetVoid))),
                  [ Int ("1", TSigned Ti32) ] );
            ],
            [ step_inc ] )
      in
      prelude @ [ step_decl; iter_decl; zero_check ]
  | ForEach (iter, collection, of_ty, body) ->
      let coll_stmts, coll' = desugar_exp collection in
      (* let cond = D.Call (Proj (coll', Methods.hasNext, TBool), [], TBool) in *)
      let cond = D.Call (Id (Methods.hasNext, TBool), [], TBool) in
      let of_ty = convert_ty of_ty in
      let set_iter =
        D.Assn
          ( Id (iter, of_ty),
            Call (Id (Methods.iterate, of_ty), [ coll' ], of_ty),
            (* Call (Proj (coll', Methods.iterate, of_ty), [], of_ty), *)
            TBool )
      in
      let body' = set_iter :: desugar_block body in
      coll_stmts @ [ While (cond, body') ]
  | While (cond, body) ->
      let cstmts, cond' = desugar_exp cond in
      let body' = desugar_block body in
      cstmts @ [ While (cond', body') ]
  | SCall (Proj (inst, pname, cname, t), args, types, ret) ->
      let istmts, inst' = desugar_exp inst in
      let dtypes, dret = (List.map convert_ty types, convert_ret_ty ret) in
      let args_stmts, args' = List.map desugar_exp args |> flatten in
      let mangled = mangle_name ~enclosing_class:cname pname dtypes dret in
      istmts @ args_stmts
      @ [ SCall (Id (mangled, convert_ty t), inst' :: args') ]
  | SCall (fn, args, tys, _ret) -> (
      let tys' = List.map convert_ty tys in
      let sf, fn' = desugar_exp fn in
      let sa, args' = List.map desugar_exp args |> flatten in
      match fn' with
      | D.Id (fname, t) -> sf @ sa @ [ SCall (Id (fname, t), args') ]
      | _ ->
          let fn_store = gensym "Fn" in
          let fn_ty = D.TRef (RFun (tys', RetVoid)) in
          let tmp_decl = D.Decl (fn_store, fn_ty, fn', false) in
          sf @ [ tmp_decl ] @ sa @ [ SCall (Id (fn_store, fn_ty), args') ])
  | Decl v ->
      let estmts, v' = desugar_vdecl v in
      estmts @ [ Decl v' ]
  | Ret eo -> (
      match eo with
      | None -> [ Ret None ]
      | Some e ->
          let estmts, e' = desugar_exp e in
          estmts @ [ Ret (Some e') ])
  | Break -> [ Break ]
  | Continue -> [ Continue ]
  | Free es ->
      let sa, des = List.map desugar_exp es |> flatten in
      sa @ [ Free des ]

and desugar_vdecl (id, ty, e, is_const) : D.stmt list * D.vdecl =
  (* vdecls may also produce pre-statements now *)
  let estmts, e' = desugar_exp e in
  (estmts, (id, convert_ty ty, e', is_const))

and desugar_exp (e : Typed.exp) : D.stmt list * D.exp =
  match e with
  | Bool b -> ([], D.Bool b)
  | Int (i, ity) -> ([], D.Int (Z.to_string i, convert_int_ty ity))
  | Float (f, fty) -> ([], D.Float (f, convert_float_ty fty))
  | Null t -> ([], D.Null (TRef (convert_ref_ty t)))
  | Str s -> ([], D.Str s)
  | Id (id, t) -> ([], D.Id (id, convert_ty t))
  | Bop (op, e1, e2, ty) ->
      let s1, e1' = desugar_exp e1 in
      let s2, e2' = desugar_exp e2 in
      (s1 @ s2, D.Bop (convert_binop op, e1', e2', convert_ty ty))
  | Uop (op, e, ty) ->
      let s, e' = desugar_exp e in
      (s, D.Uop (convert_unop op, e', convert_ty ty))
  | Index (arr, idx, ty, iter_ty) ->
      let sa, arr' =
        if is_lvalue arr then
          let stmts_arr, d_arr = desugar_exp arr in
          let lvalue_typ = convert_ty iter_ty in
          let tmp_id = gensym "lval" in
          let tmp_decl = D.Decl (tmp_id, lvalue_typ, d_arr, true) in
          (stmts_arr @ [ tmp_decl ], D.Id (tmp_id, lvalue_typ))
        else desugar_exp arr
      in
      let si, idx' = desugar_exp idx in
      (sa @ si, D.Index (arr', idx', convert_ty ty))
  | Array (elems, ty) ->
      let ss, elems' = List.map desugar_exp elems |> flatten in
      (ss, D.Array (elems', convert_ty ty))
  | Cast (e, ty) ->
      let s, e' = desugar_exp e in
      (s, D.Cast (e', convert_ty ty))
  | Proj (inst, field, cname, t) ->
      let s, inst' =
        if is_lvalue inst then
          let stmts_inst, d_inst = desugar_exp inst in
          let lvalue_typ = D.TRef (RClass cname) in
          let tmp_id = gensym "lval" in
          let tmp_decl = D.Decl (tmp_id, lvalue_typ, d_inst, true) in
          (stmts_inst @ [ tmp_decl ], D.Id (tmp_id, lvalue_typ))
        else desugar_exp inst
      in
      (s, D.Proj (inst', field, convert_ty t))
  | ObjInit (cn, fields) ->
      let stmts, fields' =
        List.fold_left
          (fun (stmts, pairs) (fid, e) ->
            let sn, e' = desugar_exp e in
            (sn :: stmts, (fid, e') :: pairs))
          ([], []) fields
      in
      (List.flatten stmts, D.ObjInit (cn, fields'))
  | Call (Proj (inst, pname, cname, t), args, tys, ty) ->
      (* desugar instance method call: inst.method(a,b) → method(inst,a,b) *)
      let si, inst' = desugar_exp inst in
      let sa, args' = List.map desugar_exp args |> flatten in
      let dtys, dty = (List.map convert_ty tys, convert_ty ty) in
      let mangled_name =
        mangle_name ~enclosing_class:cname pname dtys (RetVal dty)
      in
      ( si @ sa,
        D.Call (D.Id (mangled_name, convert_ty t), inst' :: args', convert_ty ty)
      )
  | Call (fn, args, tys, ty) -> (
      let ty' = convert_ty ty in
      let tys' = List.map convert_ty tys in

      let sf, fn' = desugar_exp fn in
      let sa, args' = List.map desugar_exp args |> flatten in
      match fn' with
      | D.Id (fname, _t) -> (sf @ sa, D.Call (Id (fname, ty'), args', ty'))
      | _ ->
          (* will expand chained calls: 
        ```
        let x = func(args)(moreArgs)
        ```
        becomes
        ```
        let tmpFn0 = func(args)
        let x = tmpFn0(moreArgs)
        ```
        and something like: 
        ```
        let y = arr[12](args)
        ```
        becomes
        ```
        let tmpFn1 = arr[12]
        let y = tmpFn1(args)
        ```

        we dont have to mangle these as they are locally defined functions, i.e. lambdas in a way
        *)
          let fn_store = gensym "Fn" in
          let fn_ty = D.TRef (RFun (tys', RetVal ty')) in
          let tmp_decl = D.Decl (fn_store, fn_ty, fn', false) in
          (sf @ [ tmp_decl ] @ sa, D.Call (Id (fn_store, fn_ty), args', ty')))
  | Lambda (scope, args, ret_ty, body) ->
      (* TODO desugar lambdas to structs and function pointers *)
      let converted_args = List.map (fun (i, t) -> (i, convert_ty t)) args in
      let converted_ret = convert_ret_ty ret_ty in
      let desugared_body = desugar_block body in
      let ls, scope' = List.map desugar_exp scope |> flatten in
      (ls, Lambda (scope', converted_args, converted_ret, desugared_body))

and desugar_block (b : Typed.block) : D.block = List.concat_map desugar_stmt b
