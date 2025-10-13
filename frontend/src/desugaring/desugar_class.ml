open Typing.Typed_ast
open Desugar_stmt

let desugar_method (m : fdecl) (cname : id) =
  let new_args = (TRef (RClass cname), "this") :: m.args in
  let desugared_body = List.map desugar_stmt m.body in
  { frtyp = m.frtyp; fname = m.fname; args = new_args; body = desugared_body }

let desugar_class (c : cdecl) =
  let methods = c.methods in
  let desugared_methods =
    List.map (fun m -> desugar_method m c.cname) methods
  in
  (desugared_methods, { c with methods = [] })
