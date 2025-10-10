open Typing.Typed_ast
open Desugar_util
open Desugar_exp

let base_op = function
  | PluEq -> Add
  | MinEq -> Sub
  | TimEq -> Mul
  | DivEq -> Div
  | AtEq -> At
  | PowEq -> Pow
  | ModEq -> Mod
  | Eq -> desugar_error "unreachable state"

let rec desugar_stmt (stmt : stmt) : stmt =
  match stmt with
  (* dont forget to desugar all exps *)
  | Assn (lhs, op, rhs, t) when op <> Eq ->
      let op' = base_op op in
      let rhs' = Bop (op', desugar_exp lhs, desugar_exp rhs, t) in
      Assn (lhs, Eq, rhs', t)
  | If (cond, b1, b2) ->
      let desugared_then = List.map desugar_stmt b1 in
      let desugared_else = List.map desugar_stmt b2 in
      If (desugar_exp cond, desugared_then, desugared_else)
  | While (cond, body) ->
      let desugared_body = List.map desugar_stmt body in
      While (desugar_exp cond, desugared_body)
  | SCall (Proj (inst, pname), args) ->
      let inst' = desugar_exp inst in
      let args' = List.map desugar_exp args in
      SCall (Id pname, inst' :: args')
  | _ -> stmt
