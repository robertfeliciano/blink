open Desugared_ast
open Pprint_desugared_ast


exception DesugarError of string

let desugar_error err = raise (DesugarError err)

let get_zero ty =
  match ty with
  | TInt some_int_ty -> Int (Z.of_int 0, some_int_ty)
  | TFloat some_float_ty -> Float (0.0, some_float_ty)
  | _ ->
      desugar_error
        "this should have ben caught earlier, but we expect numbers in the \
         bounds and step of a loop"


let mangle_int = function
  | TSigned s -> show_sint s
  | TUnsigned u -> show_uint u

let mangle_float = function Tf32 -> "f32" | Tf64 -> "f64"

let mangle_name ?(enclosing_class : id option) (fname : id) (tys : ty list) : id
    =
  let len_and_name e = Printf.sprintf "%d%s" (String.length e) e in
  let base = "_Z" in
  let mangled_class =
    match enclosing_class with
    | Some cname -> "N" ^ len_and_name cname
    | None -> ""
  in
  let mangled_fun = len_and_name fname in
  let base = base ^ mangled_class ^ mangled_fun in
  let base = if Option.is_some enclosing_class then base ^ "E" else base in
  match tys with
  | [] -> base ^ "v"
  | _ ->
      base
      ^ String.concat ""
          (List.map
             (fun t ->
               let mangled_ty =
                 match t with
                 | TBool -> "b"
                 | TInt i -> mangle_int i
                 | TFloat f -> mangle_float f
                 | TRef (RClass cname) -> len_and_name cname
                 | TRef RString -> "str"
                 | TRef (RArray (t, sz)) -> show_ty t ^ "x" ^ (Int64.to_string sz)
                 | TRef (RFun (_, _)) ->
                  failwith "passing lambda functions not supported yet"
               in
               mangled_ty)
             tys)