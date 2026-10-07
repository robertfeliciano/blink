open Ast
(** Resolve files in their own lexical scope, then combine declarations. Only
    ordinary names cross into typing/desugaring; the native FFI is unchanged. *)

open Module_model
module Names = Set.Make (String)

type kind = Function_kind | Class_kind
type symbol = { identity : string; kind : kind; public : bool }

type scope = {
  source : source;
  declarations : (string, symbol) Hashtbl.t;
  aliases : (string, module_id) Hashtbl.t;
}

exception Resolution_error of diagnostic

let fail (node : 'a node) message =
  raise (Resolution_error { loc = node.loc; message })

let declaration_info = function
  | Function fn -> (fn.elt.fname, Function_kind, fn.elt.annotations)
  | Prototype pn -> (pn.elt.fname, Function_kind, pn.elt.annotations)
  | Class cn -> (cn.elt.cname, Class_kind, cn.elt.annotations)

let make_scope entry source =
  let declarations = Hashtbl.create 16 in
  let seen = Hashtbl.create 16 in
  let (Prog (imports, items)) = source.program in
  List.iter
    (fun item ->
      let decl = item.elt.declaration in
      let name, kind, annotations = declaration_info decl in
      let is_external =
        match decl with
        | Prototype _ -> has_annotation "C" annotations
        | Function _ when has_annotation "C" annotations ->
            fail item
              "@C is only supported on prototypes; define a Blink wrapper \
               instead."
        | _ -> false
      in
      if
        name = "main" && (source.id <> entry || kind = Class_kind || is_external)
      then
        fail item "Only the entry module may declare the Blink function main.";
      let identity =
        if is_external || name = "main" then name
        else
          match Module_symbols.encode ~owner:source.id ~name with
          | Ok identity -> identity
          | Error diagnostic -> raise (Resolution_error diagnostic)
      in
      let public = Option.is_some item.elt.export_loc in
      (match Hashtbl.find_opt seen name with
      | None -> Hashtbl.add seen name [ decl ]
      | Some [ previous ] -> (
          (* Only one prototype + definition pair may share a local name. *)
          match (previous, decl) with
          | (Prototype p, Function _ | Function _, Prototype p)
            when not (has_annotation "C" p.elt.annotations) ->
              Hashtbl.replace seen name [ previous; decl ]
          | _ -> fail item ("Duplicate declaration " ^ name ^ "."))
      | Some _ -> fail item ("Duplicate declaration " ^ name ^ "."));
      let public =
        public
        ||
        match Hashtbl.find_opt declarations name with
        | Some previous -> previous.public
        | None -> false
      in
      Hashtbl.replace declarations name { identity; kind; public })
    items;
  let aliases = Hashtbl.create (List.length imports) in
  List.iter
    (fun import ->
      let alias =
        Option.value import.elt.alias ~default:import.elt.path.elt.name
      in
      if Hashtbl.mem aliases alias.elt then
        fail alias ("Duplicate import alias " ^ alias.elt ^ ".");
      if Hashtbl.mem declarations alias.elt then
        fail alias
          ("Import alias " ^ alias.elt ^ " conflicts with a declaration.");
      let owner =
        List.map (fun part -> part.elt) (name_components import.elt.path)
      in
      Hashtbl.add aliases alias.elt owner)
    imports;
  { source; declarations; aliases }

let resolve graph : (program, diagnostic) result =
  try
    let scopes = List.map (make_scope graph.entry) graph.dependency_order in
    let by_id = Hashtbl.create (List.length scopes) in
    List.iter (fun scope -> Hashtbl.add by_id scope.source.id scope) scopes;
    let resolve_scope scope =
      let local_symbol node name =
        match Hashtbl.find_opt scope.declarations name with
        | Some symbol -> symbol
        | None ->
            fail node
              ("Unknown name " ^ name ^ " in module "
              ^ String.concat "." scope.source.id
              ^ ".")
      in
      let imported_symbol node alias name =
        let owner =
          match Hashtbl.find_opt scope.aliases alias with
          | Some owner -> owner
          | None -> fail node ("Unknown module alias " ^ alias ^ ".")
        in
        let target =
          match Hashtbl.find_opt by_id owner with
          | Some target -> target
          | None ->
              fail node ("Module was not loaded: " ^ String.concat "." owner)
        in
        match Hashtbl.find_opt target.declarations name with
        | None ->
            fail node
              ("Module " ^ String.concat "." owner ^ " has no member " ^ name
             ^ ".")
        | Some symbol when not symbol.public ->
            fail node
              ("Member " ^ alias ^ "." ^ name
             ^ " is private; add export to its declaration.")
        | Some symbol -> symbol
      in
      let bind node locals name =
        if Hashtbl.mem scope.aliases name then
          fail node ("Binding " ^ name ^ " conflicts with an import alias.");
        Names.add name locals
      in
      let class_name name =
        let symbol =
          match name.elt.qualifiers with
          | [] -> local_symbol name name.elt.name.elt
          | [ alias ] -> imported_symbol name alias.elt name.elt.name.elt
          | _ ->
              fail name
                "Class types must use a direct import alias (alias.Class)."
        in
        if symbol.kind <> Class_kind then
          fail name (show_qualified_name name ^ " is not a class.");
        {
          name with
          elt =
            {
              qualifiers = [];
              name = { name.elt.name with elt = symbol.identity };
            };
        }
      in
      let rec ty = function
        | TRef (RClass name) -> TRef (RClass (class_name name))
        | TRef (RArray (element, size)) -> TRef (RArray (ty element, size))
        | TRef (RFun (args, ret)) -> TRef (RFun (List.map ty args, ret_ty ret))
        | TRef (RGeneric (name, args)) ->
            TRef (RGeneric (name, List.map ty args))
        | primitive -> primitive
      and ret_ty = function
        | RetVoid -> RetVoid
        | RetVal value -> RetVal (ty value)
      in
      let rec exp implicit locals node =
        let recurse = exp implicit locals in
        let elt =
          match node.elt with
          | Id name when Names.mem name locals -> Id name
          | Id name when Hashtbl.mem scope.aliases name ->
              fail node
                ("Module alias " ^ name
               ^ " is not a value; select an exported member.")
          | Id name ->
              let symbol = local_symbol node name in
              if symbol.kind = Class_kind then
                fail node
                  (name
                 ^ " is a class, not a function or value; use new or a typed \
                    default initializer.");
              Id symbol.identity
          | Proj ({ elt = Id alias; _ }, member)
            when Hashtbl.mem scope.aliases alias ->
              let symbol = imported_symbol node alias member in
              if symbol.kind = Class_kind then
                fail node
                  (alias ^ "." ^ member
                 ^ " is a class, not a function or value; use new or a typed \
                    default initializer.");
              Id symbol.identity
          | Proj (base, member) -> Proj (recurse base, member)
          | Call (callee, args) -> Call (recurse callee, List.map recurse args)
          | Bop (op, left, right) -> Bop (op, recurse left, recurse right)
          | Uop (op, value) -> Uop (op, recurse value)
          | Index (base, index) -> Index (recurse base, recurse index)
          | Array values -> Array (List.map recurse values)
          | Cast (value, target) -> Cast (recurse value, ty target)
          | ObjInit (name, fields) ->
              ObjInit
                ( class_name name,
                  List.map (fun (field, value) -> (field, recurse value)) fields
                )
          | Conditional (test, yes, no) ->
              Conditional (recurse test, recurse yes, recurse no)
          | Lambda (captures, args, body) ->
              let captures = List.map recurse captures in
              let locals = lambda_locals implicit node captures args in
              Lambda (captures, args, block implicit locals body)
          | TypedLambda (captures, args, ret, body) ->
              let captures = List.map recurse captures in
              let locals =
                lambda_locals implicit node captures (List.map fst args)
              in
              TypedLambda
                ( captures,
                  List.map (fun (name, value) -> (name, ty value)) args,
                  ret_ty ret,
                  block implicit locals body )
          | (Bool _ | Int _ | Float _ | Str _ | Null) as literal -> literal
        in
        { node with elt }
      and lambda_locals implicit node captures args =
        (* Only captures and parameters are lexical locals. Implicit class
           fields remain in typing's globals; typing still requires this. *)
        let locals =
          List.fold_left
            (fun locals capture ->
              match capture.elt with
              | Id name -> Names.add name locals
              | _ -> locals)
            implicit captures
        in
        List.fold_left (bind node) locals args
      and vdecl implicit locals (name, annotation, init, const) =
        ( name,
          Option.map ty annotation,
          Option.map (exp implicit locals) init,
          const )
      and block implicit locals stmts =
        match stmts with
        | [] -> []
        | node :: rest ->
            let recurse = exp implicit locals in
            let elt, next =
              match node.elt with
              | Decl ((name, _, _, _) as declaration) ->
                  ( Decl (vdecl implicit locals declaration),
                    bind node locals name )
              | Assn (left, op, right) ->
                  (Assn (recurse left, op, recurse right), locals)
              | Ret value -> (Ret (Option.map recurse value), locals)
              | SCall (callee, args) ->
                  (SCall (recurse callee, List.map recurse args), locals)
              | If (test, yes, no) ->
                  ( If
                      ( recurse test,
                        block implicit locals yes,
                        block implicit locals no ),
                    locals )
              | While (test, body) ->
                  (While (recurse test, block implicit locals body), locals)
              | ForEach (name, values, body) ->
                  ( ForEach
                      ( name,
                        recurse values,
                        block implicit (bind name locals name.elt) body ),
                    locals )
              | For (name, (start, stop, inclusive), step, body) ->
                  ( For
                      ( name,
                        (recurse start, recurse stop, inclusive),
                        Option.map recurse step,
                        block implicit (bind name locals name.elt) body ),
                    locals )
              | Free values -> (Free (List.map recurse values), locals)
              | Break -> (Break, locals)
              | Continue -> (Continue, locals)
            in
            { node with elt } :: block implicit next rest
      in
      let annotations values =
        List.map
          (fun (name, args) ->
            (name, Option.map (List.map (exp Names.empty Names.empty)) args))
          values
      in
      let args values =
        List.map (fun (value, name) -> (ty value, name)) values
      in
      let fn implicit locals identity node =
        let value : fdecl = node.elt in
        let locals =
          List.fold_left
            (fun locals (_, name) -> bind node locals name)
            locals value.args
        in
        {
          node with
          elt =
            {
              value with
              fname = identity;
              annotations = annotations value.annotations;
              args = args value.args;
              frtyp = ret_ty value.frtyp;
              body = block implicit locals value.body;
            };
        }
      in
      let (Prog (_, declarations)) = scope.source.program in
      List.map
        (fun node ->
          let name, _, _ = declaration_info node.elt.declaration in
          let identity = (local_symbol node name).identity in
          let declaration =
            match node.elt.declaration with
            | Function value ->
                Function (fn Names.empty Names.empty identity value)
            | Prototype value ->
                List.iter
                  (fun (_, name) -> ignore (bind value Names.empty name))
                  value.elt.args;
                Prototype
                  {
                    value with
                    elt =
                      {
                        fname = identity;
                        annotations = annotations value.elt.annotations;
                        args = args value.elt.args;
                        frtyp = ret_ty value.elt.frtyp;
                      };
                  }
            | Class value ->
                let fields =
                  List.fold_left
                    (fun locals field ->
                      let name, _, _, _ = field.elt in
                      bind field locals name)
                    Names.empty value.elt.fields
                in
                let methods =
                  List.map
                    (fun (method_node : fdecl node) ->
                      let constructor = method_node.elt.fname = name in
                      let locals =
                        if constructor then fields else Names.add "this" fields
                      in
                      fn fields locals
                        (if constructor then identity else method_node.elt.fname)
                        method_node)
                    value.elt.methods
                in
                Class
                  {
                    value with
                    elt =
                      {
                        value.elt with
                        cname = identity;
                        annotations = annotations value.elt.annotations;
                        fields =
                          List.map
                            (fun field ->
                              {
                                field with
                                elt = vdecl Names.empty Names.empty field.elt;
                              })
                            value.elt.fields;
                        methods;
                      };
                  }
          in
          { node with elt = { declaration; export_loc = None } })
        declarations
    in
    Ok (Prog ([], List.concat_map resolve_scope scopes))
  with Resolution_error diagnostic -> Error diagnostic
