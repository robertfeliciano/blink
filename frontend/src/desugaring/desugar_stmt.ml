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

let desugar_vdecl (id, ty, e, is_const) : D.vdecl =
  (id, convert_ty ty, desugar_exp e, is_const)

let rec desugar_stmt (stmt : Typed.stmt) : D.stmt list =
  match stmt with
  | Assn (lhs, op, rhs, t) when op <> Eq ->
      let op' = base_op op in
      let lhs' = desugar_exp lhs in
      let t' = convert_ty t in
      let rhs' = D.Bop (op', lhs', desugar_exp rhs, t') in
      [ Assn (lhs', rhs', t') ]
  | Assn (lhs, _eq, rhs, t) ->
      let lhs' = desugar_exp lhs in
      let t' = convert_ty t in
      let rhs' = desugar_exp rhs in
      [ Assn (lhs', rhs', t') ]
  | If (cond, b1, b2) ->
      let desugared_then = desugar_block b1 in
      let desugared_else = desugar_block b2 in
      [ D.If (desugar_exp cond, desugared_then, desugared_else) ]
  | For (iter, start, fin, incl, step, ty, body) ->
      let step = desugar_exp step in
      let start = desugar_exp start in
      let fin = desugar_exp fin in
      let step_ = D.Id "%step" in
      let iter_ = D.Id iter in
      let ty = convert_ty ty in
      let step_assn = D.Assn (step_, step, ty) in
      let iter_assn = D.Assn (iter_, start, ty) in
      let zero = get_zero ty in
      let up_to = if incl then D.Lte else D.Lt in
      let down_to = if incl then D.Gte else D.Gt in
      let create_cond comparator = D.Bop (comparator, step_, fin, TBool) in
      let desugared_body = desugar_block body in
      (* TODO if step is a constant known at compile time then figure out direction here *)
      let step_dec =
        D.If
          ( Bop (Lt, step_, zero, TBool),
            [
              While
                ( create_cond down_to,
                  desugared_body
                  @ [ Assn (iter_, Bop (Sub, iter_, step_, ty), ty) ] );
            ],
            [] )
      in
      let step_inc =
        D.If
          ( Bop (Gt, step_, zero, TBool),
            [
              While
                ( create_cond up_to,
                  desugared_body
                  @ [ Assn (iter_, Bop (Add, iter_, step_, ty), ty) ] );
            ],
            [ step_dec ] )
        (* else if, decrement *)
      in
      (* TODO make sure there is a runtime panic function to be called *)
      let zero_check =
        D.If
          ( Bop (Eqeq, step_, zero, TBool),
            [ SCall (Id "panic", []) ],
            [ step_inc ] )
      in
      (* 
    let %step = step
    let iter = start
    if %step == 0 then panic
    else if %step > 0 then we are going up
    else if %step < 0 then we are going down
    *)
      [ step_assn; iter_assn; zero_check ]
  | ForEach (iter, collection, of_ty, body) ->
      let coll = desugar_exp collection in
      let cond = D.Call (Proj (coll, Methods.hasNext), [], TBool) in
      let set_iter =
        D.Assn
          ( Id iter,
            Call (Proj (coll, Methods.iterate), [], convert_ty of_ty),
            TBool )
      in
      let desugared_body = set_iter :: desugar_block body in
      [ While (cond, desugared_body) ]
  | While (cond, body) ->
      let desugared_body = desugar_block body in
      [ While (desugar_exp cond, desugared_body) ]
  | SCall (Proj (inst, pname, cname), args, types) ->
      let inst' = desugar_exp inst in
      let args' = List.map desugar_exp args in
      let mangled_name = mangle_name ~enclosing_class:cname pname types in
      [ SCall (Id mangled_name, inst' :: args') ]
  | Decl v -> [ D.Decl (desugar_vdecl v) ]
  | Ret eo -> [ D.Ret (Option.map desugar_exp eo) ]
  | Break -> [ D.Break ]
  | Continue -> [ D.Continue ]
  | SCall (fn, args, _types) ->
      let desugared_fn = desugar_exp fn in
      (* TODO get function name out of fn exp *)
      (*  probably will need desugar_exp to return a list of stmts as well to store temp functions *)
      (* let mangled_name = mangle_name fn types in *)
      [ D.SCall (desugared_fn, List.map desugar_exp args) ]

and desugar_block (b : Typed.block) : D.block =
  List.map desugar_stmt b |> List.flatten
