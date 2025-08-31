open Ast
open Type_util
open Conversions

module Printer = Pprint_typed_ast

let rec type_exp ?(expected: Typed_ast.ty option) (tc: Tctxt.t) (e: Ast.exp node) : (Typed_ast.exp * Typed_ast.ty) = 
  let {elt=e';loc=_} = e in 
  match e' with
  | Bool b -> (Typed_ast.Bool b, Typed_ast.TBool)

  | Int i ->
    (match expected with
     | Some (TInt target_ty) ->
         if fits_in_ty i target_ty then
           (Typed_ast.Int (i, target_ty), TInt target_ty)
         else type_error e ("Integer literal " ^ Z.to_string i ^ " does not fit in type " ^ Printer.show_ty (TInt target_ty))
     | _ ->
         let inferred_ty = infer_integer_ty i e in
         (Typed_ast.Int (i, inferred_ty), TInt inferred_ty))

  | Float f -> let ty = Typed_ast.(TFloat Tf64) in 
    (Typed_ast.(Float (f, Tf64)), ty)

  | Str s -> let ty = Typed_ast.(TRef RString) in 
    (Typed_ast.Str s, ty)

  | Id i -> (match Tctxt.lookup_option i tc with 
    | Some t -> (Id i, t)
    | None -> type_error e ("variable " ^ i ^ " is not defined"))

  | Call (f, args) -> 
    (match snd @@ type_exp tc f with 
    | TRef (RFun (arg_types, RetVal rt)) -> 
      (try (
        let typed_callee, _ = type_exp tc f in 
        let typed_args = List.map2 
            (fun aty a -> 
              let te, ty = type_exp tc a in 
              if (subtype tc ty aty) then 
                te 
              else 
                let err_msg = "Invalid argument type for `" ^ show_exp a.elt 
                ^ "`. expected " ^ (Printer.show_ty aty) 
                ^ ", got " ^ (Printer.show_ty ty) ^ "." in
                type_error e err_msg
            ) arg_types args in 
        (Typed_ast.Call (typed_callee,  typed_args, rt), rt)
      ) with Invalid_argument _ -> type_error e "invalid number of arguments supplied")
    | TRef (RFun (_, RetVoid)) -> type_error e "assigning void function return type to variable."
    | _ -> type_error e "attempted to call a non-function type." )

  | Bop (binop, e1, e2) -> 
    let te1, lty = type_exp tc e1 in 
    let te2, rty = type_exp tc e2 in 
    (match eval_const_exp e with 
    | Some ev -> (Typed_ast.Int (ev, (TSigned Ti32)), TInt (TSigned Ti32)) (* will adjust later to expected_ty *)
    | None -> 
      let binop' = convert_binop binop in 
      let res_ty = (match binop with 
      | Eqeq | Neq | Gt | Gte | Lt | Lte -> 
        if (subtype tc lty rty) && (subtype tc rty lty) then Typed_ast.TBool
        else type_error e "== or != used with non type-compatible arguments"
      | And | Or -> 
        if lty = Typed_ast.TBool && rty = Typed_ast.TBool then Typed_ast.TBool 
          else type_error e "&& or || used on non-bool arguments"
      | At -> type_error e "@ not yet supported."
      | _ -> 
        let args_valid = subtype tc lty rty || all_numbers [lty ; rty] in
        if not args_valid then 
          type_error e "using binary operator on non-number types"
        else 
          meet_number e (lty, rty)
      ) in Typed_ast.Bop(binop', te1, te2, res_ty), res_ty)

  | Uop (unop, e1) -> 
    let te1, ety = type_exp tc e1 in 
    let unop' = convert_unop unop in 
    let res_ty = (match unop, ety with 
      | Neg, t when is_number t -> t
      | Not, t when t  = TBool -> TBool
      | _ -> type_error e "bad type for neg or not operator")
    in Typed_ast.Uop(unop', te1, res_ty), res_ty

  | Index (e_iter, e_idx) -> 
    let t_iter, iter_ty = type_exp tc e_iter in 
    let arr_ty = (match iter_ty with 
      | Typed_ast.(TRef RArray (arr_ty', _sz)) -> arr_ty'
      | _ -> type_error e "cannot index non-array type") in
    let t_idx, idx_ty = type_exp tc e_idx in 
    let _ = (match idx_ty with 
      | Typed_ast.TInt _ -> ()
      | _ -> type_error e "index must be integer type") in 
    Typed_ast.Index(t_iter, t_idx, arr_ty), arr_ty

  | Array _ -> type_array expected tc e
  
  | Range (el, er, incl) -> 
    let tel, el_ty = type_exp tc el in 
    let ter, er_ty = type_exp tc er in 
    if all_numbers [el_ty ; er_ty] then 
      (* TODO figure out if we should check if bounds are mixed floats/ints or not *)
      Typed_ast.Range (tel, ter, incl), Typed_ast.TRef (Typed_ast.RRange (el_ty, er_ty))
    else  
      type_error e "Range must have numeric bounds..."

and type_array (expected: Typed_ast.ty option) (tc: Tctxt.t) (en: exp node) : (Typed_ast.exp * Typed_ast.ty) = 
  match en.elt, expected with 
  | Array [], None -> 
    type_error en "Could not infer type of empty array. Please provide type annotation."
  | Array [], Some given_ty -> 
    (Typed_ast.(Array ([], given_ty, 0L)),
    Typed_ast.(TRef (RArray (given_ty, 0L))))
  | Array (h::t), _ -> 
    let th, h_ty = type_exp tc h in 
    let typed_elems = 
      List.map (
        fun elem -> 
          let te, ty = type_exp tc elem in 
          if subtype tc ty h_ty then te
          else
            type_error elem
              ("Array element type mismatch. Expected "
              ^ Printer.show_ty h_ty
              ^ " but got "
              ^ Printer.show_ty ty))
      t
    in
    let all_elems = th :: typed_elems in
    let len = Int64.of_int (List.length all_elems) in
    let arr_ty = Typed_ast.TRef (Typed_ast.RArray (h_ty, len)) in
    if Option.is_some expected && (Option.get expected) != arr_ty then 
      type_error en
              ("Array element type mismatch. Expected "
              ^ Printer.show_ty (Option.get expected)
              ^ " but got "
              ^ Printer.show_ty arr_ty)
  else 
    (Typed_ast.Array (all_elems, h_ty, len), arr_ty)
  | _ -> type_error en "Somehow reached unreachable state."
  (* | Array ens -> 
    (match expected with 
      | None when List.length ens = 0 -> 
        type_error en "Could not infer type of empty array. Please provide type annotation."
      | None -> 
        let el, tys = List.map (fun elem -> type_exp tc elem) ens |> List.split in  
        let expected_ty = List.hd tys in
        let common_ty = 
          List.fold_left 
          (fun acc t -> 
            if subtype tc t acc 
              then acc 
            else 
              type_error en ("Array elements must have compatible types. Expected type %s" ^ Printer.show_ty expected_ty))
          expected_ty 
        in 
        (* placeholder *)
        (Typed_ast.(Array ([], Typed_ast.TFloat Tf32, 0L)),
        Typed_ast.(TRef (RArray (TInt (TSigned Ti32), 0L))))
      | Some _ -> 
        (* placeholder *)
        (Typed_ast.(Array ([], Typed_ast.TFloat Tf32, 0L)),
        Typed_ast.(TRef (RArray (TInt (TSigned Ti32), 0L))))
    )
  | _ -> type_error en "somehow reached unreachable state" *)
  (* match expected with 
  | None -> 
    if List.length ens = 0
      then type_error
  | Some _v -> 
    (Typed_ast.(Array ([], TInt (TSigned Ti32), 0L)),
    Typed_ast.(TRef (RArray (TInt (TSigned Ti32), 0L)))) *)

  (* if List.length ens = 0 then 
    (Typed_ast.(Array ([], TInt (TSigned Ti32), 0L)),
    Typed_ast.(TRef (RArray (TInt (TSigned Ti32), 0L))))
  else
    let el, tys = List.map (fun en -> type_exp tc en) ens |> List.split in 
    let expected_ty = List.hd tys in 
    let common_ty = 
      List.fold_left 
        (fun acc t -> 
          if subtype tc t acc then 
            acc
          else 
            type_error (List.hd ens) ("Array elements must have compatible types. Expected type %s" ^ Printer.show_ty expected_ty)
        ) expected_ty (List.tl tys)
    in
    let size = List.length ens in
    (Typed_ast.Array (el, common_ty, Int64.of_int size),
    Typed_ast.(TRef (RArray (common_ty, Int64.of_int size)))) *)