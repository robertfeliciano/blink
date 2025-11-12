open Typing.Pprint_typed_ast
module T = Typing.Typed_ast
module D = Desugared_ast

exception DesugarError of string

let desugar_error err = raise (DesugarError err)

let flatten pairs =
  let stmts, exps = List.split pairs in
  (List.flatten stmts, exps)

let counter = ref 0

let gensym sfx =
  let n = !counter in
  incr counter;
  "%tmp" ^ sfx ^ Int.to_string n

let get_zero (ty : D.ty) : D.exp =
  match ty with
  | TInt some_int_ty -> Int ("0", some_int_ty)
  | TFloat some_float_ty -> Float (0.0, some_float_ty)
  | _ ->
      desugar_error
        "this should have ben caught earlier, but we expect numbers in the \
         bounds and step of a loop"

let mangle_int = function
  | T.TSigned s -> show_sint s
  | T.TUnsigned u -> show_uint u

let mangle_float = function T.Tf32 -> "f32" | Tf64 -> "f64"
let len_and_name e = Printf.sprintf "%d%s" (String.length e) e

let rec mangle_ty = function
  | T.TBool -> "b"
  | T.TInt i -> mangle_int i
  | T.TFloat f -> mangle_float f
  | T.TRef (RClass cname) -> len_and_name cname
  | T.TRef RString -> "str"
  | T.TRef (RArray (t, sz)) -> show_ty t ^ "x" ^ Int64.to_string sz
  | T.TRef (RFun (tys, r)) -> (
      String.concat "_" (List.map mangle_ty tys)
      ^
      match r with
      | RetVoid -> "__void"
      | RetVal t -> Printf.sprintf "__%s" (show_ty t))

let mangle_name ?(enclosing_class : T.id option) (fname : T.id)
    (tys : T.ty list) (rtyp : T.ret_ty) : T.id =
  let base = "_Z" in
  let mangled_class =
    match enclosing_class with
    | Some cname -> "N" ^ len_and_name cname
    | None -> ""
  in
  let mangled_fun = len_and_name fname in
  let base = base ^ mangled_class ^ mangled_fun in
  let base = if Option.is_some enclosing_class then base ^ "E" else base in
  let base =
    match tys with
    | [] -> base ^ "v"
    | _ -> base ^ String.concat "" (List.map mangle_ty tys)
  in
  base ^ match rtyp with RetVoid -> "__void" | RetVal t -> mangle_ty t
