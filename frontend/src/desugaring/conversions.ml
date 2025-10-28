open Typing.Typed_ast
module D = Desugared_ast

(** Base type conversions *)

let convert_sint = function
  | Ti8 -> D.Ti8
  | Ti16 -> D.Ti16
  | Ti32 -> D.Ti32
  | Ti64 -> D.Ti64
  | Ti128 -> D.Ti128

let convert_uint = function
  | Tu8 -> D.Tu8
  | Tu16 -> D.Tu16
  | Tu32 -> D.Tu32
  | Tu64 -> D.Tu64
  | Tu128 -> D.Tu128

let convert_float_ty = function Tf32 -> D.Tf32 | Tf64 -> D.Tf64

let convert_int_ty = function
  | TSigned s -> D.TSigned (convert_sint s)
  | TUnsigned u -> D.TUnsigned (convert_uint u)

let rec convert_ret_ty = function
  | RetVoid -> D.RetVoid
  | RetVal t -> D.RetVal (convert_ty t)

and convert_ty = function
  | TBool -> D.TBool
  | TInt it -> D.TInt (convert_int_ty it)
  | TFloat ft -> D.TFloat (convert_float_ty ft)
  | TRef r -> D.TRef (convert_ref_ty r)

and convert_ref_ty = function
  | RString -> D.RString
  | RArray (t, sz) -> D.RArray (convert_ty t, sz)
  | RClass cn -> D.RClass cn
  | RFun (args, ret) ->
      D.RFun (List.map convert_ty args, convert_ret_ty ret)

let convert_unop = function Neg -> D.Neg | Not -> D.Not

let convert_binop = function
  | Add -> D.Add
  | Sub -> D.Sub
  | Mul -> D.Mul
  | Div -> D.Div
  | At -> D.At
  | Mod -> D.Mod
  | Pow -> D.Pow
  | Eqeq -> D.Eqeq
  | Neq -> D.Neq
  | Lt -> D.Lt
  | Lte -> D.Lte
  | Gt -> D.Gt
  | Gte -> D.Gte
  | And -> D.And
  | Or -> D.Or

