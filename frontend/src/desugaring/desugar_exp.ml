open Typing.Typed_ast

let rec desugar_exp e =
  match e with
  | Bop (op, e1, e2, t) -> Bop (op, desugar_exp e1, desugar_exp e2, t)
  | Uop (op, e, t) -> Uop (op, desugar_exp e, t)
  | Call (Proj (inst, pname), args, ty) ->
      let inst' = desugar_exp inst in
      let args' = List.map desugar_exp args in
      Call (Id pname, inst' :: args', ty)
  | Call (fn, args, ty) -> Call (desugar_exp fn, List.map desugar_exp args, ty)
  | Proj (inst, field) -> Proj (desugar_exp inst, field)
  | _ -> e
