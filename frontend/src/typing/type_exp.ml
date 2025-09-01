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
  | Call (f, args) -> (
      let typed_callee, typ = type_exp tc f in
      match typ with
      | TRef (RFun (arg_types, RetVal rt)) -> (
          try
            let typed_args =
              List.map2
                (fun aty a ->
                  let te, ty = type_exp tc a in
                  if equal_ty ty aty then te
                  else
                    let err_msg =
                      "Invalid argument type for `" ^ show_exp a.elt
                      ^ "`. expected " ^ Printer.show_ty aty ^ ", got "
                      ^ Printer.show_ty ty ^ "."
                    in
                    type_error e err_msg)
                arg_types args
            in
            (Typed_ast.Call (typed_callee, typed_args, rt), rt)
          with Invalid_argument _ ->
            type_error e "invalid number of arguments supplied")
      | TRef (RFun (_, RetVoid)) ->
          type_error e "assigning void function return type to variable."
      | _ -> type_error e "attempted to call a non-function type.")
  | Bop (binop, e1, e2) -> (
      let te1, lty = type_exp tc e1 in
      let te2, rty = type_exp tc e2 in
      let _x = expected in
      match eval_const_exp e with
      | Some ev -> (Typed_ast.Int (ev, TSigned Ti32), TInt (TSigned Ti32))
      | None ->
          let binop' = convert_binop binop in
          let res_ty =
            match binop with
            | Eqeq | Neq | Gt | Gte | Lt | Lte ->
                if subtype tc lty rty && subtype tc rty lty then Typed_ast.TBool
                else
                  type_error e
                    "== or != used with non type-compatible arguments"
            | And | Or ->
                if lty = Typed_ast.TBool && rty = Typed_ast.TBool then
                  Typed_ast.TBool
                else type_error e "&& or || used on non-bool arguments"
            | At -> type_error e "@ not yet supported."
            | _ ->
                let args_valid =
                  subtype tc lty rty || all_numbers [ lty; rty ]
                in
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
  (* TODO actually type cast *)
  | Cast (_e, _t) -> (Typed_ast.Bool true, Typed_ast.TBool)
  | Range (el, er, incl) ->
      let tel, el_ty = type_exp tc el in
      let ter, er_ty = type_exp tc er in
      if all_numbers [ el_ty; er_ty ] then
        (* TODO figure out if we should check if bounds are mixed floats/ints or not *)
        ( Typed_ast.Range (tel, ter, incl),
          Typed_ast.TRef (Typed_ast.RRange (el_ty, er_ty)) )
      else type_error e "Range must have numeric bounds..."

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
