open Ast
open Type_util
open Conversions
module Printer = Pprint_typed_ast

let rec type_exp ?(expected : Typed_ast.ty option) (tc : Tctxt.t)
    (e : Ast.exp node) : Typed_ast.exp * Typed_ast.ty =
  let { elt = e'; loc = _ } = e in
  match e' with
  | Bool b -> (Typed_ast.Bool b, Typed_ast.TBool)
  | Int i -> (
      match expected with
      | Some (TInt target_ty) ->
          if fits_in_int_ty i target_ty then
            (Typed_ast.Int (i, target_ty), TInt target_ty)
          else
            type_error e
              ("Integer literal " ^ Z.to_string i ^ " does not fit in type "
              ^ Printer.show_ty (TInt target_ty))
      | _ ->
          let inferred_ty = infer_integer_ty i e in
          (Typed_ast.Int (i, inferred_ty), TInt inferred_ty))
  | Float f ->
      let target_ty =
        match expected with
        | Some (TFloat target_ty) -> target_ty
        | None -> Typed_ast.Tf64
        | _ -> type_error e "Will not cast float to non-float."
      in
      if fits_in_float_ty f target_ty then
        (Float (f, target_ty), TFloat target_ty)
      else
        type_error e
          ("Float literal " ^ Float.to_string f ^ " does not fit in type "
          ^ Printer.show_ty (TFloat target_ty))
  | Str s ->
      let ty = Typed_ast.(TRef RString) in
      (Typed_ast.Str s, ty)
  | Id i -> (
      match Tctxt.lookup_option i tc with
      | Some t -> (Id i, t)
      | None -> type_error e ("variable " ^ i ^ " is not defined"))
  | Call ({ elt = Proj (obj, mth); loc = _ }, args) -> (
      match type_method (Proj (obj, mth)) args true tc with
      | Ok (Proj (tobj, _), typed_args, RetVal rt) ->
          Typed_ast.(Call (Proj (tobj, mth), typed_args, rt), rt)
      | Error msg -> type_error e msg
      | _ -> type_error e "Unreachable state.")
  | Call (f, args) -> (
      let typed_callee, typ = type_exp tc f in
      match type_func args typ true tc with
      | Error msg -> type_error e msg
      | Ok (typed_args, RetVal rt) ->
          (Typed_ast.Call (typed_callee, typed_args, rt), rt)
      | _ -> type_error e "Unreachable state.?")
  | Bop (binop, e1, e2) -> (
      let te1, lty = type_exp tc e1 in
      let te2, rty = type_exp tc e2 in
      match eval_const_exp e with
      | Some ev -> (Typed_ast.Int (ev, TSigned Ti32), TInt (TSigned Ti32))
      | None ->
          let binop' = convert_binop binop in
          let res_ty =
            match binop with
            | Eqeq | Neq | Gt | Gte | Lt | Lte ->
                if equal_ty lty rty then Typed_ast.TBool
                else
                  type_error e
                    "== or != used with non type-compatible arguments"
            | And | Or ->
                if lty = Typed_ast.TBool && rty = Typed_ast.TBool then
                  Typed_ast.TBool
                else type_error e "&& or || used on non-bool arguments"
            | At -> type_error e "@ not yet supported."
            | _ ->
                let args_valid = all_numbers [ lty; rty ] && equal_ty rty lty in
                if not args_valid then
                  type_error e "using binary operator on non-number types"
                else meet_number e (lty, rty)
          in
          (Typed_ast.Bop (binop', te1, te2, res_ty), res_ty))
  | Uop (unop, e1) ->
      let te1, ety = type_exp tc e1 in
      let unop' = convert_unop unop in
      let res_ty =
        match (unop, ety) with
        | Neg, t when is_number t -> (
            match expected with
            | Some (TInt (TUnsigned _)) ->
                type_error e "Cannot assign negative number to unsigned int."
            | _ -> t)
        | Not, t when t = TBool -> TBool
        | _ -> type_error e "bad operand type"
      in
      (Typed_ast.Uop (unop', te1, res_ty), res_ty)
  | Index (e_iter, e_idx) ->
      let t_iter, iter_ty = type_exp tc e_iter in
      let arr_ty =
        match iter_ty with
        | Typed_ast.(TRef (RArray (arr_ty', _sz))) -> arr_ty'
        | _ -> type_error e "cannot index non-array type"
      in
      let t_idx, idx_ty = type_exp tc e_idx in
      let _ =
        match idx_ty with
        | Typed_ast.TInt _ -> ()
        | _ -> type_error e "index must be integer type"
      in
      (Typed_ast.Index (t_iter, t_idx, arr_ty), arr_ty)
  | Array _ -> type_array expected tc e
  | Cast (e, t) ->
      let te, e_ty = type_exp tc e in
      let tty = convert_ty t in
      if subtype tc e_ty tty then (Typed_ast.Cast (te, tty), tty)
      else
        type_error e
          ("Cannot cast " ^ Printer.show_exp te ^ " which has type "
         ^ Printer.show_ty e_ty ^ " to type " ^ Printer.show_ty tty ^ ".")
  | Range (el, er, incl) ->
      let tel, el_ty = type_exp tc el in
      let ter, er_ty = type_exp tc er in
      if all_numbers [ el_ty; er_ty ] then
        (* TODO figure out if we should check if bounds are mixed floats/ints or not *)
        ( Typed_ast.Range (tel, ter, incl),
          Typed_ast.TRef (Typed_ast.RRange (el_ty, er_ty)) )
      else type_error e "Range must have numeric bounds..."
  | Proj (ec, f) -> (
      let tec, e_ty = type_exp tc ec in
      match e_ty with
      | Typed_ast.(TRef (RClass cid)) -> (
          match Tctxt.lookup_field_option cid f tc with
          | Some fty -> (Typed_ast.Proj (tec, f), fty)
          | None -> type_error ec ("Class " ^ cid ^ " has no member field " ^ f)
          )
      | _ -> type_error ec "Must project field of a class.")

  | ObjCons (_cname, _args) -> type_error e "object constructor not allowed yet"
    (* lookup class
      get constructor (just cname)
      check constructor args against given args
      basically same as call  
    *)

and type_func (args : exp node list) (ftyp : Typed_ast.ty) (from_exp : bool)
    (tc : Tctxt.t) : (Typed_ast.exp list * Typed_ast.ret_ty, string) result =
  let typecheck_args arg_types =
    List.map2
      (fun aty a ->
        let te, ty = type_exp tc a in
        if equal_ty ty aty then te
        else
          let err_msg =
            "Invalid argument type for `" ^ show_exp a.elt ^ "`. Expected "
            ^ Printer.show_ty aty ^ ", got " ^ Printer.show_ty ty ^ "."
          in
          raise (TypeError err_msg))
      arg_types args
  in
  match ftyp with
  | TRef (RFun (arg_types, RetVal rt_ty)) -> (
      try
        let typed_args = typecheck_args arg_types in
        Ok (typed_args, RetVal rt_ty)
      with
      | TypeError msg -> Error msg
      | Invalid_argument _ -> Error "invalid number of arguments supplied")
  | TRef (RFun (arg_types, RetVoid)) ->
      if from_exp then Error "assigning void function return type to variable."
      else
        let typed_args = typecheck_args arg_types in
        Ok (typed_args, RetVoid)
  | _ -> Error "attempted to call a non-function type."

and type_method (proj : exp) (args : exp node list) (from_exp : bool)
    (tc : Tctxt.t) :
    (Typed_ast.exp * Typed_ast.exp list * Typed_ast.ret_ty, string) result =
  match proj with
  | Proj (obj, mth) -> (
      let tobj, obj_ty = type_exp tc obj in
      match obj_ty with
      | Typed_ast.(TRef (RClass cid)) -> (
          match Tctxt.lookup_method_option cid mth tc with
          | Some (rt, argheaders) -> (
              let argtypes = List.map (fun (t, _) -> t) argheaders in
              let temp_func = Typed_ast.(TRef (RFun (argtypes, rt))) in
              match type_func args temp_func from_exp tc with
              | Error msg -> Error msg
              | Ok (typed_args, rt) ->
                  Ok (Typed_ast.Proj (tobj, mth), typed_args, rt))
          | None -> Error ("Class " ^ cid ^ " has no member method " ^ mth))
      | _ -> Error "Attempting to call method of non-class type.")
  | _ -> Error "Attemping to call method of non-class type."

and type_array (expected : Typed_ast.ty option) (tc : Tctxt.t) (en : exp node) :
    Typed_ast.exp * Typed_ast.ty =
  match (en.elt, expected) with
  | Array [], None ->
      type_error en
        "Could not infer type of empty array. Please provide type annotation."
  | Array [], Some (TRef (RArray (elt_ty, len))) ->
      (Typed_ast.Array ([], elt_ty, len), TRef (RArray (elt_ty, len)))
  | Array [], Some other ->
      type_error en
        ("Expected " ^ Printer.show_ty other ^ " but got empty array.")
  | Array (h :: t), exp_opt ->
      let exp_elt, exp_len =
        match exp_opt with
        | Some (TRef (RArray (ety, elen))) -> (Some ety, Some elen)
        | _ -> (None, None)
      in
      let th, h_ty =
        match exp_elt with
        | Some ety -> type_exp ~expected:ety tc h
        | None -> type_exp tc h
      in
      let typed_elems =
        List.map
          (fun elem ->
            let te, ty =
              match exp_elt with
              | Some ety -> type_exp ~expected:ety tc elem
              | None -> type_exp tc elem
            in
            if equal_ty ty h_ty then te
            else
              type_error elem
                ("Array element type mismatch. Expected " ^ Printer.show_ty h_ty
               ^ " but got " ^ Printer.show_ty ty))
          t
      in
      let all_elems = th :: typed_elems in
      let len = Int64.of_int (List.length all_elems) in
      (match exp_len with
      | Some elen when elen <> len ->
          type_error en
            ("Array length mismatch. Expected " ^ Int64.to_string elen
           ^ " but got " ^ Int64.to_string len)
      | _ -> ());
      let arr_ty = Typed_ast.(TRef (RArray (h_ty, len))) in
      (match exp_opt with
      | Some exp when not (equal_ty arr_ty exp) ->
          type_error en
            ("Array type mismatch. Expected " ^ Printer.show_ty exp
           ^ " but got " ^ Printer.show_ty arr_ty)
      | _ -> ());
      (Typed_ast.Array (all_elems, h_ty, len), arr_ty)
  | _ -> type_error en "Somehow reached unreachable state."
