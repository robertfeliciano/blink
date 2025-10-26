open Desugared_ast
module PT = Typing.Pprint_typed_ast

let show_field = PT.show_field

let show_cdecl { cname; fields } =
  Printf.sprintf "cdecl{name=%s; fields=[\n%s\n]}"
    cname
    (String.concat "\n" (List.map PT.show_field fields))

let show_desugared_program (Prog (fns, cns)) =
  let cns_str = String.concat "\n" (List.map show_cdecl cns) in
  let fns_str = String.concat "\n" (List.map PT.show_fdecl fns) in
  Printf.sprintf "program{\nclasses=[\n%s\n];\nfunctions=[\n%s\n]\n}" cns_str fns_str
