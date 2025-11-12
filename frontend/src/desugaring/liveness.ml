
open Desugared_ast

module StringSet = Set.Make (String)

type result = { reads : StringSet.t; writes : StringSet.t }

let empty_result = { reads = StringSet.empty; writes = StringSet.empty }

let union_result r1 r2 =
  {
    reads = StringSet.union r1.reads r2.reads;
    writes = StringSet.union r1.writes r2.writes;
  }

(* -------------------------------------------------------------------------- *)
(* Core recursive analysis *)

let rec analyze_lambda (params : (id * ty) list) (body : block) : result =
  let locals =
    List.fold_left (fun s (p, _) -> StringSet.add p s) StringSet.empty params
  in
  analyze_block locals body

and analyze_exp (locals : StringSet.t) (is_lhs : bool) (e : exp) : result =
  match e with
  | Bool _ | Int _ | Float _ | Str _ -> empty_result
  | Id x ->
      if StringSet.mem x locals then empty_result
      else if is_lhs then { reads = StringSet.empty; writes = StringSet.singleton x }
      else { reads = StringSet.singleton x; writes = StringSet.empty }
  | Call (fn, args, _) ->
      List.fold_left
        (fun acc a -> union_result acc (analyze_exp locals false a))
        (analyze_exp locals false fn)
        args
  | Bop (_, a, b, _) ->
      union_result
        (analyze_exp locals false a)
        (analyze_exp locals false b)
  | Uop (_, x, _) -> analyze_exp locals false x
  | Index (base, idx, _) ->
      let rbase = analyze_exp locals is_lhs base in
      let ridx = analyze_exp locals false idx in
      union_result rbase ridx
  | Array (elems, _, _) ->
      List.fold_left
        (fun acc e -> union_result acc (analyze_exp locals false e))
        empty_result elems
  | Cast (e, _) -> analyze_exp locals false e
  | Proj (base, _) -> analyze_exp locals is_lhs base
  | ObjInit (_, fields) ->
      List.fold_left
        (fun acc (_, ex) -> union_result acc (analyze_exp locals false ex))
        empty_result fields
  | Lambda (params, _ret, body) ->
      let nested = analyze_lambda params body in
      let reads = StringSet.filter (fun id -> not (StringSet.mem id locals)) nested.reads in
      let writes = StringSet.filter (fun id -> not (StringSet.mem id locals)) nested.writes in
      { reads; writes }

and analyze_block (locals_init : StringSet.t) (blk : block) : result =
  let rec loop locals acc = function
    | [] -> acc
    | stmt :: rest ->
        let r =
          match stmt with
          | Assn (lhs, rhs, _) ->
              union_result
                (analyze_exp locals true lhs)
                (analyze_exp locals false rhs)
          | LambdaDecl (_id, _, init_exp)
          | Decl (_id, _, init_exp, _) ->
              analyze_exp locals false init_exp
          | Ret maybe_e ->
              (match maybe_e with
               | None -> empty_result
               | Some e -> analyze_exp locals false e)
          | SCall (fn, args) ->
              List.fold_left
                (fun a x -> union_result a (analyze_exp locals false x))
                (analyze_exp locals false fn)
                args
          | If (cond, then_blk, else_blk) ->
              union_result
                (analyze_exp locals false cond)
                (union_result
                   (analyze_block locals then_blk)
                   (analyze_block locals else_blk))
          | While (cond, body_blk) ->
              union_result
                (analyze_exp locals false cond)
                (analyze_block locals body_blk)
          | Break | Continue -> empty_result
        in
        let locals' =
          match stmt with
          | Decl (id, _, _, _) | LambdaDecl (id, _, _) -> StringSet.add id locals
          | _ -> locals
        in
        loop locals' (union_result acc r) rest
  in
  loop locals_init empty_result blk

(* Entry point *)
let free_vars_of_lambda (params : (id * ty) list) (body : block) : result =
  analyze_lambda params body
