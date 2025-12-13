let convert_sint (s : Ast.sint) : Typed_ast.sint =
  match s with
  | Ast.Ti8 -> Typed_ast.Ti8
  | Ast.Ti16 -> Typed_ast.Ti16
  | Ast.Ti32 -> Typed_ast.Ti32
  | Ast.Ti64 -> Typed_ast.Ti64
  | Ast.Ti128 -> Typed_ast.Ti128

let convert_uint (u : Ast.uint) : Typed_ast.uint =
  match u with
  | Ast.Tu8 -> Typed_ast.Tu8
  | Ast.Tu16 -> Typed_ast.Tu16
  | Ast.Tu32 -> Typed_ast.Tu32
  | Ast.Tu64 -> Typed_ast.Tu64
  | Ast.Tu128 -> Typed_ast.Tu128

let convert_float_ty (f : Ast.float_ty) : Typed_ast.float_ty =
  match f with Ast.Tf32 -> Typed_ast.Tf32 | Ast.Tf64 -> Typed_ast.Tf64

let convert_int_ty (i : Ast.int_ty) : Typed_ast.int_ty =
  match i with
  | Ast.TSigned s -> Typed_ast.TSigned (convert_sint s)
  | Ast.TUnsigned u -> Typed_ast.TUnsigned (convert_uint u)

let rec convert_ret_ty (rt : Ast.ret_ty) : Typed_ast.ret_ty =
  match rt with
  | Ast.RetVoid -> Typed_ast.RetVoid
  | Ast.RetVal t -> Typed_ast.RetVal (convert_ty t)

and convert_ty (t : Ast.ty) : Typed_ast.ty =
  match t with
  | Ast.TBool -> Typed_ast.TBool
  | Ast.TInt it -> Typed_ast.TInt (convert_int_ty it)
  | Ast.TFloat ft -> Typed_ast.TFloat (convert_float_ty ft)
  | Ast.TRef r -> Typed_ast.TRef (convert_ref_ty r)

and convert_ref_ty (r : Ast.ref_ty) : Typed_ast.ref_ty =
  match r with
  | Ast.RString -> Typed_ast.RString
  | Ast.RArray (t, sz) -> Typed_ast.RArray (convert_ty t, Z.to_int sz)
  | Ast.RClass cn -> Typed_ast.RClass cn
  | Ast.RFun (tl, rt) ->
      Typed_ast.RFun (List.map convert_ty tl, convert_ret_ty rt)
  | Ast.RGeneric _ -> failwith "bleh"

let convert_unop : Ast.unop -> Typed_ast.unop = function
  | Ast.Neg -> Typed_ast.Neg
  | Ast.Not -> Typed_ast.Not
  | Ast.BNeg -> Typed_ast.BNeg

let convert_binop : Ast.binop -> Typed_ast.binop = function
  | Ast.Add -> Typed_ast.Add
  | Ast.Sub -> Typed_ast.Sub
  | Ast.Mul -> Typed_ast.Mul
  | Ast.Div -> Typed_ast.Div
  | Ast.At -> Typed_ast.At
  | Ast.Mod -> Typed_ast.Mod
  | Ast.Pow -> Typed_ast.Pow
  | Ast.Eqeq -> Typed_ast.Eqeq
  | Ast.Neq -> Typed_ast.Neq
  | Ast.Lt -> Typed_ast.Lt
  | Ast.Lte -> Typed_ast.Lte
  | Ast.Gt -> Typed_ast.Gt
  | Ast.Gte -> Typed_ast.Gte
  | Ast.And -> Typed_ast.And
  | Ast.Or -> Typed_ast.Or
  | Ast.Xor -> Typed_ast.Xor
  | Ast.Shl -> Typed_ast.Shl
  | Ast.Lshr -> Typed_ast.Lshr
  | Ast.Ashr -> Typed_ast.Ashr
  | Ast.BXor -> Typed_ast.BXor
  | Ast.BAnd -> Typed_ast.BAnd
  | Ast.BOr -> Typed_ast.BOr

let convert_aop : Ast.aop -> Typed_ast.aop = function
  | Ast.Eq -> Typed_ast.Eq
  | Ast.PluEq -> Typed_ast.PluEq
  | Ast.MinEq -> Typed_ast.MinEq
  | Ast.TimEq -> Typed_ast.TimEq
  | Ast.DivEq -> Typed_ast.DivEq
  | Ast.AtEq -> Typed_ast.AtEq
  | Ast.PowEq -> Typed_ast.PowEq
  | Ast.ModEq -> Typed_ast.ModEq
  | Ast.ShlEq -> Typed_ast.ShlEq
  | Ast.LShrEq -> Typed_ast.LShrEq
  | Ast.AShrEq -> Typed_ast.AShrEq
  | Ast.BXorEq -> Typed_ast.BXorEq
  | Ast.BAndEq -> Typed_ast.BAndEq
  | Ast.BOrEq -> Typed_ast.BOrEq
