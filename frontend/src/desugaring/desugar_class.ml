open Typing.Typed_ast
open Desugar_exp
open Desugar_stmt
open Conversions
module D = Desugared_ast

let desugar_method (m : fdecl) (cname : id) : D.fdecl =
  let desugared_body = List.map desugar_stmt m.body |> List.flatten in
  let desugared_args = (D.TRef (RClass cname), "this") :: List.map (fun (t,i) -> (convert_ty t, i)) m.args in 
  { frtyp = convert_ret_ty m.frtyp; fname = m.fname; args = desugared_args; body = desugared_body }


let desugar_fields fs : D.field list = 
  List.map (fun { fieldName ; ftyp ; init } -> D.{ fieldName=fieldName ; ftyp=convert_ty ftyp ; init=desugar_exp init}) fs

let desugar_class (c : cdecl) : D.fdecl list * D.cdecl =
  let desugared_methods =
    List.map (fun m -> desugar_method m c.cname) c.methods
  in
  (desugared_methods, { cname = c.cname; fields = desugar_fields c.fields })
