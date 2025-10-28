open Typing.Typed_ast
open Conversions
module D = Desugared_ast

let rec desugar_exp (e : exp) : D.exp =
  match e with
  | Bool b -> D.Bool b
  | Int (i, ity) -> D.Int (i, convert_int_ty ity)
  | Float (f, fty) -> D.Float (f, convert_float_ty fty)
  | Str s -> D.Str s
  | Id id -> D.Id id
  | Bop (op, e1, e2, ty) ->
      D.Bop (convert_binop op, desugar_exp e1, desugar_exp e2, convert_ty ty)
  | Uop (op, e, ty) ->
      D.Uop (convert_unop op, desugar_exp e, convert_ty ty)
  | Index (arr, idx, ty) ->
      D.Index (desugar_exp arr, desugar_exp idx, convert_ty ty)
  | Array (elems, ty, sz) ->
      D.Array (List.map desugar_exp elems, convert_ty ty, sz)
  | Cast (e, ty) -> D.Cast (desugar_exp e, convert_ty ty)
  | Proj (inst, field) -> D.Proj (desugar_exp inst, field)
  | ObjInit (cn, fields) ->
      D.ObjInit
        (cn, List.map (fun (fid, e) -> (fid, desugar_exp e)) fields)
  | Call (Proj (inst, pname), args, ty) ->
      (* desugar instance method call: inst.meth(a,b) → meth(inst,a,b) *)
      let inst' = desugar_exp inst in
      let args' = List.map desugar_exp args in
      D.Call (D.Id pname, inst' :: args', convert_ty ty)
  | Call (fn, args, ty) ->
      D.Call (desugar_exp fn, List.map desugar_exp args, convert_ty ty)
  