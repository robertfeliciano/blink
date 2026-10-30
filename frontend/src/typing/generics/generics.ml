open Ast
module T = Typed_ast
module Printer = Pprint_typed_ast

exception GenericError of Range.t * string
exception CannotInfer of Range.t * string

let generic_error node message = raise (GenericError (node.loc, message))

type origin = { loc : Range.t; name : string }
type instance = { fn : fdecl node; ty : T.ty; origins : origin list }

type t = {
  templates : (id * fdecl node) list;
  instances : (id * instance) list ref;
  pending : instance Queue.t;
  origins : origin list ref;
  nominals : Nominal_generics.t;
}

let create nominals templates =
  {
    nominals;
    templates;
    instances = ref [];
    pending = Queue.create ();
    origins = ref [];
  }

let subst_ty bindings ty =
  Ast.substitute_type_params
    (fun p ->
      match List.assoc_opt p.elt bindings with
      | Some ty -> Conversions.convert_source_ty ty
      | None ->
          raise
            (CannotInfer
               ( p.loc,
                 "Cannot infer type parameter " ^ p.elt
                 ^ "; provide a type annotation." )))
    ty

let unresolved bindings =
  Ast.exists_type_param (fun p -> not (List.mem_assoc p.elt bindings))

let check_array_length node sz actual =
  if not (Z.equal sz actual) then
    generic_error node "Array length mismatch during generic inference."

let check_function_arity node args actual =
  if List.length args <> List.length actual then
    generic_error node "Function arity mismatch during generic inference."

(* Only generic positions constrain inference. Ordinary positions still use the
   existing argument conversion rules after the signature is concrete. *)
let rec match_ty ~convert_ty ~nominals ~implementations ?(fill = false) node
    bindings ty actual =
  match (ty, actual) with
  | TParam p, actual -> (
      match List.assoc_opt p.elt !bindings with
      | None -> bindings := (p.elt, actual) :: !bindings
      | Some _ when fill -> ()
      | Some previous when previous = actual -> ()
      | Some previous ->
          generic_error node
            ("Conflicting types for " ^ p.elt ^ ": " ^ Printer.show_ty previous
           ^ " and " ^ Printer.show_ty actual ^ "."))
  | TRef (RGeneric (name, args)), T.TRef (T.RClass actual | T.RInterface actual)
    -> (
      let owner = Ast.show_qualified_name name in
      let candidates = actual :: implementations actual in
      let matches =
        List.filter_map
          (fun candidate ->
            match Nominal_generics.arguments nominals candidate with
            | Some (template, actual_args) when template = owner ->
                Some actual_args
            | _ -> None)
          candidates
      in
      match matches with
      | [ actual_args ] ->
          if List.length args <> List.length actual_args then
            generic_error node "Generic type arity mismatch during inference.";
          List.iter2
            (fun ty actual ->
              match_ty ~convert_ty ~nominals ~implementations ~fill node
                bindings ty (convert_ty actual))
            args actual_args
      | [] ->
          generic_error node
            "Generic class or interface specialization mismatch."
      | _ -> generic_error node "Ambiguous generic interface implementation.")
  | TRef (RArray (ty, sz)), T.TRef (T.RArray (actual, len)) ->
      check_array_length node sz (Z.of_int len);
      match_ty ~convert_ty ~nominals ~implementations ~fill node bindings ty
        actual
  | TRef (RFun (args, ret)), T.TRef (T.RFun (actual_args, actual_ret)) ->
      check_function_arity node args actual_args;
      List.iter2
        (match_ty ~convert_ty ~nominals ~implementations ~fill node bindings)
        args actual_args;
      match_ret_ty ~convert_ty ~nominals ~implementations ~fill node bindings
        ret actual_ret
  | _ -> ()

and match_ret_ty ~convert_ty ~nominals ~implementations ?(fill = false) node
    bindings ret actual =
  match (ret, actual) with
  | RetVal ty, T.RetVal actual ->
      match_ty ~convert_ty ~nominals ~implementations ~fill node bindings ty
        actual
  | RetVoid, T.RetVoid -> ()
  | _ -> generic_error node "Function return mismatch during generic inference."

(* A generic callback can connect still-unbound parameters in its receiver's
   signature. Each side keeps its own bindings, even when parameter names match.
   Only concrete types enter either set of bindings. *)
let match_patterns ~nominals ~implementations ~convert_ty node bindings ty
    callback_ty =
  let callback_bindings = ref [] in
  let rec match_pair ty callback_ty =
    if not (unresolved !bindings ty) then
      match_ty ~convert_ty ~nominals ~implementations node callback_bindings
        callback_ty
        (convert_ty (subst_ty !bindings ty))
    else if not (unresolved !callback_bindings callback_ty) then
      match_ty ~convert_ty ~nominals ~implementations node bindings ty
        (convert_ty (subst_ty !callback_bindings callback_ty))
    else
      match (ty, callback_ty) with
      | TRef (RGeneric (name, args)), TRef (RGeneric (other, other_args))
        when Ast.show_qualified_name name = Ast.show_qualified_name other ->
          List.iter2 match_pair args other_args
      | TRef (RArray (ty, sz)), TRef (RArray (callback_ty, callback_sz)) ->
          check_array_length node sz callback_sz;
          match_pair ty callback_ty
      | TRef (RFun (args, ret)), TRef (RFun (callback_args, callback_ret)) -> (
          check_function_arity node args callback_args;
          List.iter2 match_pair args callback_args;
          match (ret, callback_ret) with
          | RetVal ty, RetVal callback_ty -> match_pair ty callback_ty
          | RetVoid, RetVoid -> ()
          | _ ->
              generic_error node
                "Function return mismatch during generic inference.")
      | _ -> ()
  in
  let rec aux () =
    let previous = List.length !bindings + List.length !callback_bindings in
    match_pair ty callback_ty;
    if List.length !bindings + List.length !callback_bindings > previous then
      aux ()
  in
  aux ()

let validate_params node params args ret annotations =
  Generic_types.validate_params generic_error params;
  if params <> [] && has_annotation "C" annotations then
    generic_error node "Generic @C functions are not supported.";
  let uses i = Ast.exists_type_param (fun p -> p.elt = i) in
  let uses_ret i = function RetVoid -> false | RetVal ty -> uses i ty in
  List.iter
    (fun p ->
      if
        not
          (List.exists (fun (ty, _) -> uses p.elt ty) args || uses_ret p.elt ret)
      then
        generic_error p
          ("Type parameter " ^ p.elt ^ " must appear in the function signature."))
    params

let length_and_name = Generic_types.length_and_name
let encode_ty = Generic_types.encode_ty

let request ~convert_ty ~convert_ret_ty state (node : 'a Ast.node)
    (fn : fdecl node) bindings =
  let tys =
    List.map
      (fun p -> convert_ty (subst_ty bindings (TParam p)))
      fn.elt.type_params
  in
  let fname =
    "_BLG"
    ^ length_and_name fn.elt.fname
    ^ "_"
    ^ String.concat "" (List.map (fun ty -> length_and_name (encode_ty ty)) tys)
  in
  match List.assoc_opt fname !(state.instances) with
  | Some instance -> instance
  | None ->
      let name =
        fn.elt.fname ^ "<"
        ^ String.concat ", " (List.map Printer.show_ty tys)
        ^ ">"
      in
      let origins = !(state.origins) @ [ { loc = node.loc; name } ] in
      if List.length origins > 128 || List.length !(state.instances) >= 4096
      then
        generic_error node
          ("Generic specialization limit exceeded: "
          ^ String.concat " -> " (List.map (fun origin -> origin.name) origins)
          );
      let fn = map_fdecl_types (subst_ty bindings) fn in
      let fn = Nominal_generics.normalize_function state.nominals fn in
      let fn = { fn with elt = { fn.elt with fname; type_params = [] } } in
      let args = List.map (fun (ty, _) -> convert_ty ty) fn.elt.args in
      let ty = T.TRef (T.RFun (args, convert_ret_ty fn.elt.frtyp)) in
      let instance = { fn; ty; origins } in
      state.instances := (fname, instance) :: !(state.instances);
      Queue.add instance state.pending;
      instance

let prototype_matches (pn : proto node) (fn : fdecl node) =
  let canonical params =
    let rec ty = function
      | TParam p ->
          let rec index n = function
            | [] -> -1
            | q :: qs -> if p.elt = q.elt then n else index (n + 1) qs
          in
          TParam (no_loc (string_of_int (index 0 params)))
      | TRef (RGeneric (name, args)) ->
          TRef
            (RGeneric
               ( unqualified_name (no_loc (show_qualified_name name)),
                 List.map ty args ))
      | TRef (RClass cn) ->
          TRef (RClass (unqualified_name (no_loc (show_qualified_name cn))))
      | TRef (RArray (t, sz)) -> TRef (RArray (ty t, sz))
      | TRef (RFun (args, ret)) -> TRef (RFun (List.map ty args, ret_ty ret))
      | t -> t
    and ret_ty = function RetVoid -> RetVoid | RetVal t -> RetVal (ty t) in
    (ty, ret_ty)
  in
  let pty, pret = canonical pn.elt.type_params in
  let fty, fret = canonical fn.elt.type_params in
  List.length pn.elt.type_params = List.length fn.elt.type_params
  && List.map (fun (ty, _) -> pty ty) pn.elt.args
     = List.map (fun (ty, _) -> fty ty) fn.elt.args
  && pret pn.elt.frtyp = fret fn.elt.frtyp
