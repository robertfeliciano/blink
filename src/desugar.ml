open Ast

let rec desugar_block (statement: stmt node) : stmt node list = 
  let node_creator elem = {elt=elem; loc=statement.loc} in
  match statement.elt with
  | Assn(lhs, op, rhs) -> 
    if op <> Eq then 
      let base_op = match op with 
        | PluEq -> Add
        | MinEq -> Sub
        | TimEq -> Mul
        | DivEq -> Div
        | AtEq  -> At
        | PowEq -> Pow
        | ModEq -> Mod
        | _ -> failwith "unsupported assignment operator"
      in let new_rhs = no_loc @@ Bop(base_op, lhs, rhs)
    in [node_creator @@ Assn(lhs, Eq, new_rhs)]
    else [statement]
  | For(iterator, iterable, step_opt, body) -> 
    let desugared_loop = 
      match iterable.elt with 
      | Range(left, right, incl) -> 
        let iterator_decl = no_loc @@ Decl(iterator.elt, Some TFloat, left, false) in 
        let iterator_node = no_loc @@ Id iterator.elt in
        let step = Option.value step_opt ~default:(no_loc @@ Int 1L) in 
        let incr_iterator = no_loc @@ Assn(iterator_node, PluEq, step) in
        let new_body = List.map desugar_block @@ body @ [incr_iterator] |> List.concat in
        let create_while = fun comp1 comp2 -> While (no_loc @@ Bop ((if incl then comp1 else comp2), iterator_node, right), new_body) in
        let forward_while = create_while Lte Lt |> no_loc in 
        let backward_while = create_while Gte Gt |> no_loc in 
        let if_else = no_loc @@ If (no_loc @@ Bop (Lt, left, right), [forward_while], [backward_while])
        in [iterator_decl; if_else]
      | _ -> [statement]
      in desugared_loop
  | _ -> [statement] (* | _ -> statement we are iterating over a list, like: for elem in lst... *)

let desugar_func (f: fdecl node) : fdecl node =
  let desugared_block = List.map desugar_block f.elt.body |> List.concat in 
  f.elt.body <- desugared_block; f

let desugar_prog = function
 | Gfdecl f -> Gfdecl (desugar_func f)
 | other -> other

let desugar ast = List.map desugar_prog ast