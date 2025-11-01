open Desugar_util
open Desugar_exp
open Conversions
module Methods = Util.Constants.Methods
module Typed = Typing.Typed_ast
module D = Desugared_ast

let base_op = function
  | Typed.PluEq -> D.Add
  | Typed.MinEq -> D.Sub
  | Typed.TimEq -> D.Mul
  | Typed.DivEq -> D.Div
  | Typed.AtEq -> D.At
  | Typed.PowEq -> D.Pow
  | Typed.ModEq -> D.Mod
  | Typed.Eq -> desugar_error "unreachable state"

(* vdecls may also produce pre-statements now *)
let desugar_vdecl (id, ty, e, is_const) : D.stmt list * D.vdecl =
  let estmts, e' = desugar_exp e in
  (estmts, (id, convert_ty ty, e', is_const))

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
      let step_ = D.Id "%step" in
      let iter_ = D.Id iter in
      let ty = convert_ty ty in
      let step_assn = D.Assn (step_, step', ty) in
      let iter_assn = D.Assn (iter_, start', ty) in
      let zero = get_zero ty in
      let up_to = if incl then D.Lte else Lt in
      let down_to = if incl then D.Gte else Gt in
      let create_cond cmp = D.Bop (cmp, step_, fin', TBool) in
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
            [ SCall (Id "panic", []) ],
            [ step_inc ] )
      in
      prelude @ [ step_assn; iter_assn; zero_check ]
  | ForEach (iter, collection, of_ty, body) ->
      let coll_stmts, coll' = desugar_exp collection in
      let cond = D.Call (Proj (coll', Methods.hasNext), [], TBool) in
      let set_iter =
        D.Assn
          ( Id iter,
            Call (Proj (coll', Methods.iterate), [], convert_ty of_ty),
            TBool )
      in
      let body' = set_iter :: desugar_block body in
      coll_stmts @ [ While (cond, body') ]
  | While (cond, body) ->
      let cstmts, cond' = desugar_exp cond in
      let body' = desugar_block body in
      cstmts @ [ While (cond', body') ]
  | SCall (Proj (inst, pname, cname), args, types) ->
      let istmts, inst' = desugar_exp inst in
      let args_stmts, args' = List.map desugar_exp args |> flatten in
      let mangled = mangle_name ~enclosing_class:cname pname types in
      istmts @ args_stmts @ [ SCall (Id mangled, inst' :: args') ]
  | SCall (fn, args, tys) -> (
      let tys' = List.map convert_ty tys in

      let sf, fn' = desugar_exp fn in
      let sa, args' = List.map desugar_exp args |> flatten in
      match fn' with
      | D.Id fname ->
          let mangled_name = mangle_name fname tys in
          sf @ sa @ [ SCall (Id mangled_name, args') ]
      | _ ->
          let fn_store = gensym "Fn" in
          let fn_ty = D.TRef (RFun (tys', RetVoid)) in
          let tmp_decl = D.Decl (fn_store, fn_ty, fn', false) in
          sf @ [ tmp_decl ] @ sa @ [ SCall (Id fn_store, args') ])
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
  | _ -> desugar_error "todo lambdadecl"

and desugar_block (b : Typed.block) : D.block = List.concat_map desugar_stmt b
