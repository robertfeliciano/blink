open Typing.Typed_ast
open Desugar_util
open Desugar_exp
module Methods = Util.Constants.Methods

let base_op = function
  | PluEq -> Add
  | MinEq -> Sub
  | TimEq -> Mul
  | DivEq -> Div
  | AtEq -> At
  | PowEq -> Pow
  | ModEq -> Mod
  | Eq -> desugar_error "unreachable state"

let rec desugar_stmt (stmt : stmt) : stmt list =
  match stmt with
  | Assn (lhs, op, rhs, t) when op <> Eq ->
      let op' = base_op op in
      let rhs' = Bop (op', desugar_exp lhs, desugar_exp rhs, t) in
      [ Assn (lhs, Eq, rhs', t) ]
  | If (cond, b1, b2) ->
      let desugared_then = List.map desugar_stmt b1 |> List.flatten in
      let desugared_else = List.map desugar_stmt b2 |> List.flatten in
      [ If (desugar_exp cond, desugared_then, desugared_else) ]
  | For (iter, start, fin, incl, step, ty, body) ->
      let step_ = Id "%step" in
      let step_assn = Assn (step_, Eq, step, ty) in
      let iter_assn = Assn (Id iter, Eq, start, ty) in
      let zero = get_zero ty in
      let up_to = if incl then Lte else Lt in
      let down_to = if incl then Gte else Gt in
      let create_cond comparator = Bop (comparator, step_, fin, TBool) in
      let desugared_body = List.map desugar_stmt body |> List.flatten in
      (* TODO if step is a constant known at compile time then figure out direction here *)
      let step_dec =
        If
          ( Bop (Lt, step_, zero, TBool),
            [
              While
                ( create_cond down_to,
                  desugared_body
                  @ [ Assn (Id iter, Eq, Bop (Sub, Id iter, step, ty), ty) ] );
            ],
            [] )
      in
      let step_inc =
        If
          ( Bop (Gt, step_, zero, TBool),
            [
              While
                ( create_cond up_to,
                  desugared_body
                  @ [ Assn (Id iter, Eq, Bop (Add, Id iter, step, ty), ty) ] );
            ],
            [ step_dec ] )
        (* else if, decrement *)
      in
      (* TODO make sure there is a runtime panic function to be called *)
      let zero_check =
        If
          ( Bop (Eqeq, step_, zero, TBool),
            [ SCall (Id "panic", []) ],
            [ step_inc ] )
      in
      (* 
    let %step = step
    let iter = start
    if %step == 0 then panic
    else if %step > 0 then we are going up
    else if %step < 0 then we are going down
    *)
      [ step_assn; iter_assn; zero_check ]
  | ForEach (iter, collection, of_ty, body) ->
      let cond = Call (Proj (collection, Methods.hasNext), [], TBool) in
      let set_iter =
        Assn
          ( Id iter,
            Eq,
            Call (Proj (collection, Methods.iterate), [], of_ty),
            TBool )
      in
      let desugared_body =
        set_iter :: (List.map desugar_stmt body |> List.flatten)
      in
      [ While (cond, desugared_body) ]
  | While (cond, body) ->
      let desugared_body = List.map desugar_stmt body |> List.flatten in
      [ While (desugar_exp cond, desugared_body) ]
  | SCall (Proj (inst, pname), args) ->
      let inst' = desugar_exp inst in
      let args' = List.map desugar_exp args in
      [ SCall (Id pname, inst' :: args') ]
  | _ -> [ stmt ]
