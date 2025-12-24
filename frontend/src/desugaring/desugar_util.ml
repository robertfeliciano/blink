open Pprint_desugared_ast
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
  "%tmp_" ^ sfx ^ Int.to_string n

let desugar_annotation ((i, el) : T.annotation) =
  match (i, el) with
  (* 
  TODO desugar complex annotations (getters/things with exps inside)
  only thing passed to backend should be basic @<id> annotations
  *)
  | id, Some _ -> id
  | id, None -> id

let is_lvalue = function T.Index _ | T.Proj _ -> true | _ -> false

let get_zero (ty : D.ty) : D.exp =
  match ty with
  | TInt some_int_ty -> Int ("0", some_int_ty)
  | TFloat some_float_ty -> Float (0.0, some_float_ty)
  | _ ->
      desugar_error
        "this should have been caught earlier, but we expect numbers in the \
         bounds and step of a loop"

let mangle_int = function
  | D.TSigned s -> show_sint s
  | D.TUnsigned u -> show_uint u

let mangle_float = function D.Tf32 -> "f32" | Tf64 -> "f64"
let len_and_name e = Printf.sprintf "%d%s" (String.length e) e

let mangle_ret_ty (t : D.ret_ty) =
  match t with
  | RetVoid -> "__void"
  | RetVal t -> Printf.sprintf "__%s" (show_ty t)

let rec mangle_ty = function
  | D.TBool -> "b"
  | D.TInt i -> mangle_int i
  | D.TFloat f -> mangle_float f
  | D.TRef (RClass cname) -> len_and_name cname
  | D.TRef RString -> "str"
  | D.TRef (RArray (t, sz)) -> show_ty t ^ "x" ^ Int.to_string sz
  | D.TRef (RFun (tys, r)) ->
      String.concat "_" (List.map mangle_ty tys) ^ mangle_ret_ty r
  | D.TRef (RPtr t) -> show_ty t ^ "*"

let mangle_name ?(enclosing_class : D.id option) (fname : D.id)
    (tys : D.ty list) (rtyp : D.ret_ty) : D.id =
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
  base ^ mangle_ret_ty rtyp

let mangle_lambda (t : D.ref_ty) =
  match t with
  | RFun (arg_tys, ret_ty) ->
      "lambda"
      ^ String.concat "_" (List.map mangle_ty arg_tys)
      ^ mangle_ret_ty ret_ty
  | _ -> desugar_error "impossible state"

(* TODO update gensym to use diff pattern for lambdas *)

let lambdasym sfx =
  let n = !counter in
  incr counter;
  sfx ^ "anon" ^ Int.to_string n

let lifted_lambda_name (lname : string) = lambdasym "Lifted." ^ lname
let lambda_env_struct_name (lname : string) = lambdasym "Env." ^ lname
let lambda_struct_name (lname : string) = "Struct." ^ lname
let create_ptr_to t = D.TRef (RPtr t)

let create_lambda_struct (cname : string) (arg_tys : D.ty list) (rty : D.ret_ty)
    : D.cdecl =
  let env_ty = create_ptr_to (TInt (TSigned Ti8)) in
  (* using generic i8* for env -> LLVM GEP uses opaque ptr *)
  let fptr_ty = create_ptr_to (TRef (RFun (arg_tys, rty))) in
  let env_field : D.field =
    { prelude = []; fieldName = "envptr"; ftyp = env_ty; init = Null env_ty }
  in
  let fptr_field : D.field =
    {
      prelude = [];
      fieldName = "lambdaptr";
      ftyp = fptr_ty;
      init = Null fptr_ty;
    }
  in
  { cname; fields = [ env_field; fptr_field ]; annotations = [] }
