open Ast

let base_op = function
  | PluEq, _ -> Add
  | MinEq, _ -> Sub
  | TimEq, _ -> Mul
  | DivEq, _ -> Div
  | AtEq, _ -> At
  | PowEq, _ -> Pow
  | ModEq, _ -> Mod
  | _, loc ->
      let _, (s, e), _ = loc in
      failwith
      @@ Printf.sprintf "unsupported assignment operator at [%d, %d]" s e

let rec depends_on_iterator right_bound iterator_id =
  match right_bound.elt with
  | Id id -> id = iterator_id
  | Call (_func, args) ->
      List.exists (fun arg -> depends_on_iterator arg iterator_id) args
  | Bop (_binop, lhs, rhs) ->
      depends_on_iterator lhs iterator_id || depends_on_iterator rhs iterator_id
  | Index (indexable, idx) ->
      depends_on_iterator indexable iterator_id
      || depends_on_iterator idx iterator_id
  | _ -> false

let rec desugar_stmt (statement : stmt node) : stmt node list =
  let node_creator elem = { elt = elem; loc = statement.loc } in

  match statement.elt with
  | Assn (lhs, op, rhs) when op <> Eq ->
      let base_op = base_op (op, statement.loc) in
      let new_rhs = no_loc @@ Bop (base_op, lhs, rhs) in
      [ node_creator @@ Assn (lhs, Eq, new_rhs) ]
  | For (iterator, iterable, step_opt, body) -> (
      let desugar_range left right incl =
        let iterator_decl =
          no_loc @@ Decl (iterator.elt, Some TFloat, left, false)
        in
        let iterator_node = no_loc @@ Id iterator.elt in

        let can_lift_right_bound =
          not @@ depends_on_iterator right iterator.elt
        in
        let lifted_right_bound = no_loc @@ Decl ("_right", None, right, true) in
        let lifted_reference = no_loc @@ Id "_right" in
        let useable_right_bound =
          if can_lift_right_bound then lifted_reference else right
        in

        let step = Option.value step_opt ~default:(no_loc @@ Int 1L) in
        let incr_iterator = no_loc @@ Assn (iterator_node, PluEq, step) in

        let new_body =
          List.concat (List.map desugar_stmt @@ body @ [ incr_iterator ])
        in
        let create_while comp1 comp2 =
          While
            ( no_loc
              @@ Bop
                   ( (if incl then comp1 else comp2),
                     iterator_node,
                     useable_right_bound ),
              new_body )
        in
        let forward_while = no_loc @@ create_while Lte Lt in
        let backward_while = no_loc @@ create_while Gte Gt in

        let if_else =
          no_loc
          @@ If
               ( no_loc @@ Bop (Lt, left, useable_right_bound),
                 [ forward_while ],
                 [ backward_while ] )
        in

        if can_lift_right_bound then
          [ iterator_decl; lifted_right_bound; if_else ]
        else [ iterator_decl; if_else ]
      in
      match iterable.elt with
      | Range (left, right, incl) -> desugar_range left right incl
      | _ -> [ statement ])
  | While (cond, body) ->
      let new_body = List.concat (List.map desugar_stmt body) in
      [ node_creator @@ While (cond, new_body) ]
  (* TODO think about if i want to desugar the exp node in a vdecl... like: let a = for x in 1..10 {...}*)
  | _ -> [ statement ]

let desugar_func (f : fdecl node) : fdecl node =
  let desugared_block = List.map desugar_stmt f.elt.body |> List.concat in
  f.elt.body <- desugared_block;
  f

let desugar_prog = function
  | Gfdecl f -> Gfdecl (desugar_func f)
  | other -> other

let desugar ast = List.map desugar_prog ast
