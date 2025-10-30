open Typing.Typed_ast
open Conversions
open Desugar_util
module D = Desugared_ast

let rec desugar_exp (e : exp) : D.stmt list * D.exp =
  match e with
  | Bool b -> ([], D.Bool b)
  | Int (i, ity) -> ([], D.Int (i, convert_int_ty ity))
  | Float (f, fty) -> ([], D.Float (f, convert_float_ty fty))
  | Str s -> ([], D.Str s)
  | Id id -> ([], D.Id id)
  | Bop (op, e1, e2, ty) ->
      let s1, e1' = desugar_exp e1 in
      let s2, e2' = desugar_exp e2 in
      (s1 @ s2, D.Bop (convert_binop op, e1', e2', convert_ty ty))
  | Uop (op, e, ty) ->
      let s, e' = desugar_exp e in
      (s, D.Uop (convert_unop op, e', convert_ty ty))
  | Index (arr, idx, ty) ->
      let sa, arr' = desugar_exp arr in
      let si, idx' = desugar_exp idx in
      (sa @ si, D.Index (arr', idx', convert_ty ty))
  | Array (elems, ty, sz) ->
      let ss, elems' = List.map desugar_exp elems |> flatten in
      (ss, D.Array (elems', convert_ty ty, sz))
  | Cast (e, ty) ->
      let s, e' = desugar_exp e in
      (s, D.Cast (e', convert_ty ty))
  | Proj (inst, field, _cname) ->
      let s, inst' = desugar_exp inst in
      (s, D.Proj (inst', field))
  | ObjInit (cn, fields) ->
      let stmts, fields' =
        List.fold_left
          (fun (stmts, pairs) (fid, e) ->
            let sn, e' = desugar_exp e in
            (sn :: stmts, (fid, e') :: pairs))
          ([], []) fields
      in
      (List.flatten stmts, D.ObjInit (cn, fields'))
  | Call (Proj (inst, pname, cname), args, tys, ty) ->
      (* desugar instance method call: inst.method(a,b) → method(inst,a,b) *)
      let si, inst' = desugar_exp inst in
      let sa, args' = List.map desugar_exp args |> flatten in
      let mangled_name = mangle_name ~enclosing_class:cname pname tys in
      (si @ sa, D.Call (D.Id mangled_name, inst' :: args', convert_ty ty))
  | Call (fn, args, tys, ty) -> (
      let ty' = convert_ty ty in
      let tys' = List.map convert_ty tys in

      let sf, fn' = desugar_exp fn in
      let sa, args' = List.map desugar_exp args |> flatten in
      match fn' with
      | D.Id fname ->
          let mangled_name = mangle_name fname tys in
          (sf @ sa, D.Call (Id mangled_name, args', ty'))
      | _ ->
          (* will expand chained calls: 
        ```
        let x = func(args)(moreArgs)
        ```
        becomes
        ```
        let tmpFn0 = func(args)
        let x = tmpFn0(moreArgs)
        ```
        and something like: 
        ```
        let y = arr[12](args)
        ```
        becomes
        ```
        let tmpFn1 = arr[12]
        let y = tmpFn1(args)
        ```

        we dont have to mangle these as they are locally defined functions, i.e. lambdas in a way
        *)
          let fn_store = gensym "Fn" in
          let fn_ty = D.TRef (RFun (tys', RetVal ty')) in
          let tmp_decl = D.Decl (fn_store, fn_ty, fn', false) in
          (sf @ [ tmp_decl ] @ sa, D.Call (Id fn_store, args', ty')))
