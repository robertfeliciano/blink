module T = Typed_ast
module Printer = Pprint_typed_ast

let length_and_name name = string_of_int (String.length name) ^ name

let rec encode_ty ty =
  match ty with
  | T.TBool -> "b"
  | T.TInt _ | T.TFloat _ -> Printer.show_ty ty
  | T.TRef T.RString -> "s"
  | T.TRef (T.RClass cn) -> "c" ^ length_and_name cn
  | T.TRef (T.RInterface name) -> "i" ^ length_and_name name
  | T.TRef (T.RArray (ty, sz)) ->
      "a" ^ string_of_int sz ^ "_" ^ length_and_name (encode_ty ty)
  | T.TRef (T.RFun (args, ret)) ->
      "f"
      ^ string_of_int (List.length args)
      ^ "_"
      ^ String.concat ""
          (List.map (fun ty -> length_and_name (encode_ty ty)) args)
      ^ encode_ret ret

and encode_ret = function
  | T.RetVoid -> "v"
  | T.RetVal ty -> "r" ^ length_and_name (encode_ty ty)

let validate_params fail params =
  let seen = Hashtbl.create (List.length params) in
  List.iter
    (fun (param : Ast.type_param) ->
      let p = param.pname in
      if Hashtbl.mem seen p.elt then
        fail p ("Duplicate type parameter " ^ p.elt ^ ".");
      Hashtbl.add seen p.elt ())
    params

let validate_array_length fail node size =
  if Z.lt size Z.zero then fail node "negative length specified"
  else if not (Z.fits_int size) then
    fail node "array length is too large for this target"

let rec validate_array_lengths fail node = function
  | Ast.TRef (RArray (ty, size)) ->
      validate_array_length fail node size;
      validate_array_lengths fail node ty
  | Ast.TRef (RGeneric (_, args)) ->
      List.iter (validate_array_lengths fail node) args
  | Ast.TRef (RFun (args, ret)) -> (
      List.iter (validate_array_lengths fail node) args;
      match ret with
      | RetVoid -> ()
      | RetVal ty -> validate_array_lengths fail node ty)
  | _ -> ()

(* Functions and nominal types enforce the same explicit interface relation.
   Keep inferred/explicit arguments concrete; a bound never changes inference. *)
let check_constraints ~fail ~convert_ty ~implements node params args =
  let bindings = List.map2 (fun p ty -> (p.Ast.pname.elt, ty)) params args in
  let substitute =
    Ast.substitute_type_params (fun p -> List.assoc p.Ast.elt bindings)
  in
  List.iter2
    (fun p actual ->
      match p.Ast.bound with
      | None -> ()
      | Some bound ->
          let required = convert_ty (substitute bound.elt) in
          let actual = convert_ty actual in
          let satisfies =
            match (actual, required) with
            | T.TRef (T.RInterface a), T.TRef (T.RInterface b) -> a = b
            | T.TRef (T.RClass c), T.TRef (T.RInterface i) -> implements c i
            | _ -> false
          in
          if not satisfies then
            fail node
              ("Type argument " ^ Printer.show_ty actual ^ " for " ^ p.pname.elt
             ^ " does not satisfy interface constraint "
             ^ Printer.show_ty required ^ " (declared at "
              ^ Util.Range.string_of_range bound.loc
              ^ ")."))
    params args
