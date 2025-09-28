open Typing.Typed_ast
open Desugar_util

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
  | Assn (lhs, op, rhs, t) when op <> Eq ->
      let op' = base_op op in
      let rhs' = Bop (op', lhs, rhs, t) in
      Assn (lhs, Eq, rhs', t)
  | If (cond, b1, b2) -> 
      let desugared_then = List.map desugar_stmt b1 in 
      let desugared_else = List.map desugar_stmt b2 in 
      If (cond, desugared_then, desugared_else)
  | While (cond, body) -> 
      let desugared_body = List.map desugar_stmt body in 
      While (cond, desugared_body)
  | _ -> stmt
