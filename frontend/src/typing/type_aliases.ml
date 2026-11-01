open Ast
open Type_util

(* Expand after module resolution and parameter binding, but before nominal
   specialization. Aliases never introduce an identity into the typed AST. *)
let expand (Prog (imports, declarations)) =
  let aliases = Hashtbl.create 16 in
  List.iter
    (fun item ->
      match item.elt.declaration with
      | TypeAlias alias ->
          let name = alias.elt.aname in
          if Hashtbl.mem aliases name.elt then
            type_error name ("Duplicate type alias " ^ name.elt ^ ".");
          Hashtbl.add aliases name.elt alias
      | _ -> ())
    declarations;
  List.iter
    (fun item ->
      let name =
        match item.elt.declaration with
        | Function fn -> Some fn.elt.fname
        | Prototype pn -> Some pn.elt.fname
        | Class cn -> Some cn.elt.cname
        | Interface i -> Some i.elt.iname
        | Global gn -> Some gn.elt.gname
        | TypeAlias _ -> None
      in
      Option.iter
        (fun name ->
          if Hashtbl.mem aliases name then
            type_error item ("Duplicate declaration " ^ name ^ "."))
        name)
    declarations;
  let complete = Hashtbl.create (Hashtbl.length aliases) in
  let rec ty stack = function
    | TTuple ts -> TTuple (List.map (ty stack) ts)
    | TRef (RClass name) as t -> (
        match Hashtbl.find_opt aliases (show_qualified_name name) with
        | Some alias -> resolve stack name alias
        | None -> t)
    | TRef (RGeneric (name, args)) ->
        if Hashtbl.mem aliases (show_qualified_name name) then
          type_error name
            ("Type alias " ^ show_qualified_name name
           ^ " does not accept type arguments.");
        TRef (RGeneric (name, List.map (ty stack) args))
    | TRef (RArray (element, size)) -> TRef (RArray (ty stack element, size))
    | TRef (RFun (args, ret)) ->
        let ret =
          match ret with
          | RetVoid -> RetVoid
          | RetVal value -> RetVal (ty stack value)
        in
        TRef (RFun (List.map (ty stack) args, ret))
    | t -> t
  and resolve stack use alias =
    let name = alias.elt.aname.elt in
    if List.mem name stack then
      type_error use
        ("Cyclic type alias: "
        ^ String.concat " -> " (List.rev (name :: stack))
        ^ ".");
    match Hashtbl.find_opt complete name with
    | Some t -> t
    | None ->
        let expanded = ty (name :: stack) alias.elt.target.elt in
        Hashtbl.add complete name expanded;
        expanded
  in
  let expand = ty [] in
  let map item =
    let declaration =
      match item.elt.declaration with
      | TypeAlias alias ->
          TypeAlias
            {
              alias with
              elt =
                {
                  alias.elt with
                  target =
                    {
                      alias.elt.target with
                      elt = resolve [] (unqualified_name alias.elt.aname) alias;
                    };
                };
            }
      | Function fn -> Function (map_fdecl_types expand fn)
      | Prototype pn -> Prototype (map_proto_types expand pn)
      | Class cn -> Class (map_cdecl_types expand cn)
      | Interface i -> Interface (map_idecl_types expand i)
      | Global gn ->
          Global
            {
              gn with
              elt =
                {
                  gn.elt with
                  gtyp = Option.map expand gn.elt.gtyp;
                  ginit = Option.map (map_exp_types expand) gn.elt.ginit;
                };
            }
    in
    { item with elt = { item.elt with declaration } }
  in
  try Prog (imports, List.map map declarations)
  with InvalidObjectType loc ->
    type_error { elt = (); loc } "Object construction requires a class type."
