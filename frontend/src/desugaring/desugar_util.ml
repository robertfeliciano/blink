open Pprint_desugared_ast
module T = Typing.Typed_ast
module D = Desugared_ast

exception DesugarError of string

let desugar_error err = raise (DesugarError err)

let flatten pairs =
  let stmts, exps = List.split pairs in
  (List.flatten stmts, exps)

let lower_while setup condition body =
  match setup with
  | [] -> D.While (condition, body)
  | _ ->
      (* Condition setup belongs to every check, including checks reached by
         continue. Use the existing loop and break constructs to preserve it. *)
      let stop = D.If (D.Uop (D.Not, condition, D.TBool), [ D.Break ], []) in
      D.While (D.Bool true, setup @ (stop :: body))

let counter = ref 0

let gensym sfx =
  let n = !counter in
  incr counter;
  "%tmp_" ^ sfx ^ Int.to_string n

let expression_ty = function
  | D.Bool _ -> D.TBool
  | Int (_, int_ty) -> D.TInt int_ty
  | Float (_, float_ty) -> D.TFloat float_ty
  | Str _ -> D.TRef RString
  | ObjInit (name, _) -> D.TRef (RClass name)
  | Lambda (_, args, ret, _) -> D.TRef (RFun (List.map snd args, ret))
  | Id (_, ty)
  | Call (_, _, ty)
  | Bop (_, _, _, ty)
  | Uop (_, _, ty)
  | Tuple (_, ty)
  | TupleProj (_, _, ty)
  | Array (_, ty, _)
  | Cast (_, ty)
  | Proj (_, _, ty)
  | Index (_, _, ty)
  | Conditional (_, _, _, ty)
  | InterfaceCall (_, _, _, ty)
  | InterfaceProj (_, _, ty) ->
      ty
  | InterfaceCast (_, _, name) -> D.TRef (RInterface name)
  | Null ty -> ty

(* A later operand's setup must not run before an earlier operand's value.
   Keep ordinary expression trees intact when no setup is hoisted. *)
let sequence_expressions operands =
  let _, setup, values =
    List.fold_right
      (fun (prefix, value) (later_setup, setup, values) ->
        let prefix, value =
          if later_setup then
            let name = gensym "operand" in
            let ty = expression_ty value in
            (prefix @ [ D.Decl (name, ty, value, true) ], D.Id (name, ty))
          else (prefix, value)
        in
        (later_setup || prefix <> [], prefix @ setup, value :: values))
      operands (false, [], [])
  in
  (setup, values)

let desugar_annotation ((i, el) : T.annotation) =
  match (i, el) with
  (* 
  TODO desugar complex annotations (getters/things with exps inside)
  only thing passed to backend should be basic @<id> annotations
  *)
  | id, Some _ -> id
  | id, None -> id

let is_lvalue = function
  (* Tuple slots are read-only, but references projected from them must be
     evaluated once when used as the receiver of a mutable field or index. *)
  | T.Index _ | T.Proj _ | T.InterfaceProj _ | T.TupleProj _ -> true
  | _ -> false

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
  | D.TTuple ts ->
      "tuple"
      ^ String.concat "" (List.map (fun ty -> len_and_name (show_ty ty)) ts)
  | D.TRef (RClass cname) -> len_and_name cname
  | D.TRef (RInterface name) -> "I" ^ len_and_name name
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
      "lambda_"
      ^ String.concat "_" (List.map mangle_ty arg_tys)
      ^ mangle_ret_ty ret_ty
  | _ -> desugar_error "impossible state"

let lambdasym sfx =
  let n = !counter in
  incr counter;
  Int.to_string n ^ "." ^ sfx

let lifted_lambda_name (lname : string) = "Lifted" ^ lname
let lambda_env_struct_name (lname : string) = "Env" ^ lname
let lambda_struct_name (lname : string) = "Struct." ^ lname
let create_ptr_to t = D.TRef (RPtr t)

let create_lambda_struct (cname : string) (arg_tys : D.ty list) (rty : D.ret_ty)
    : D.cdecl =
  let env_ty = create_ptr_to (TInt (TSigned Ti8)) in
  (* using generic i8* for env -> LLVM GEP uses opaque ptr anyway *)
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
  { cname; fields = [ env_field; fptr_field ]; annotations = []; impls = [] }

let rec create_default_init t =
  match t with
  | D.TBool -> D.Bool false
  | D.TInt it -> D.Int ("0", it)
  | D.TFloat ft -> D.Float (0.0, ft)
  | D.TTuple ts -> D.Tuple (List.map create_default_init ts, t)
  | D.TRef RString -> D.Str ""
  | D.TRef (RClass _cname) ->
      (* let default_constructor = cname in *)
      Null t
      (* D.Call (Id (default_constructor, TRef (RClass cname)), [], TRef (RClass cname)) *)
  | D.TRef (RFun _) -> desugar_error "Default functions not allowed."
  (* | D.TRef (RGeneric _) -> type_error stmt_n "Generic default init to come some" *)
  | D.TRef (RPtr _) | D.TRef (RInterface _) -> Null t
  | D.TRef (RArray (t, sz)) ->
      let lst = List.init sz (fun _ -> create_default_init t) in
      let t' = D.TRef (RArray (t, sz)) in
      D.Array (lst, t', false)
