open Desugared_ast
open Desugar_util

(* transforms lambda types into their corresponding struct types
  also adds the new lambda-structs to the cdecl list
*)
let transform_ty (t : ty) (cs_acc : cdecl list) : ty * cdecl list =
  match t with
  | TRef (RFun (arg_tys, rty) as r) ->
      let lname = mangle_lambda r in
      let lstruct_name = lambda_struct_name lname in
      let new_cs_acc =
        if List.exists (fun c -> c.cname = lstruct_name) cs_acc then cs_acc
        else
          let lstruct = create_lambda_struct lstruct_name arg_tys rty in
          lstruct :: cs_acc
      in
      (TRef (RClass lstruct_name), new_cs_acc)
  | _ -> (t, cs_acc)

let transform_ret_ty (rt : ret_ty) (cs_acc : cdecl list) : ret_ty * cdecl list =
  match rt with
  | RetVal t ->
      let t', cs' = transform_ty t cs_acc in
      (RetVal t', cs')
  | RetVoid -> (RetVoid, cs_acc)

(* transforms lambda arguments and return types into their corresponding struct types *)
let check_fdecls (fs : fdecl list) (cs : cdecl list) : fdecl list * cdecl list =
  List.fold_left
    (fun ((acc_fs : fdecl list), curr_cs) (f : fdecl) ->
      let frtyp', cs_after_ret = transform_ret_ty f.frtyp curr_cs in

      let args', final_cs =
        List.fold_right
          (fun (t, i) (args_acc, cs_acc) ->
            let t', cs_acc' = transform_ty t cs_acc in
            ((t', i) :: args_acc, cs_acc'))
          f.args ([], cs_after_ret)
      in

      let f' = { f with frtyp = frtyp'; args = args' } in
      (f' :: acc_fs, final_cs))
    ([], cs) fs
  |> fun (final_fs, final_cs) -> (List.rev final_fs, final_cs)
