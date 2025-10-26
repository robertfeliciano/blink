open Typing.Typed_ast
open Desugar_stmt

let desugar_method (m : fdecl) (cname : id) : Desugared_ast.fdecl =
  let new_args = (TRef (RClass cname), "this") :: m.args in
  let desugared_body = List.map desugar_stmt m.body |> List.flatten in
  { frtyp = m.frtyp; fname = m.fname; args = new_args; body = desugared_body }

let desugar_class (c : cdecl) : Desugared_ast.fdecl list * Desugared_ast.cdecl =
  let desugared_methods =
    List.map (fun m -> desugar_method m c.cname) c.methods
  in
  (desugared_methods, { cname = c.cname; fields = c.fields })
