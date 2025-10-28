open Desugared_ast

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
