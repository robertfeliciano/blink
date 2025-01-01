open Ast

let desugar_func (f: fdecl node) : fdecl node =
  let replacer (statement: stmt node) : stmt node = 
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
          (* TODO add modulo here *)
          | _ -> failwith "should never happen"
        in let new_rhs = node_creator @@ Bop (base_op, lhs, rhs)
      in node_creator @@ Assn(lhs, Eq, new_rhs)
      else statement
    | _ -> statement
  in f.elt.body <- List.map replacer f.elt.body; f

(* | Assn(lhs, op, rhs) -> sprintf "%s %s %s" (string_of_exp lhs) (string_of_aop op) (string_of_exp rhs)
| Decl(name, typ, rhs, const) -> sprintf "%s%s: %s := %s" (if const then "const " else "") (name) (
  match typ with
  | Some t -> string_of_type t
  | None -> "inferred"
) (string_of_exp rhs)
| Ret v -> sprintf "return %s" (if is_some v then get v |> string_of_exp else "")
| SCall (func, arglist) -> sprintf "%s(%s)" (string_of_exp func) (string_of_list string_of_exp arglist)
| If (cond, tbr, ebr) -> sprintf "if %s \n\t%s\n\telse %s\n" (string_of_exp cond) (string_of_body tbr) (string_of_body ebr)
| For (iter, range, step, b) -> sprintf "for %s in %s by %s \n\t%s\n" (iter.elt) (string_of_exp range) (value step ~default:1L |> Int64.to_string) (string_of_body b)
| While (cond, b) ->  sprintf "while %s \n\t%s\n" (string_of_exp cond) (string_of_body b)
| Break -> "break"
| Continue -> "continue" *)

let desugar_prog = function
 | Gfdecl f -> Gfdecl (desugar_func f)
 | other -> other

let desugar ast = List.map desugar_prog ast