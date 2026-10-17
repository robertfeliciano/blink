open Ast

exception Error of Range.t * string

(* A function instance may discover a new nominal type after headers have been frozen.
   Keep discovered source instances, then rebuild typing contexts. No partially
   typed declarations or function caches survive this boundary. *)
exception New_instances

type template =
  | Class_template of cdecl node
  | Interface_template of idecl node

let parameters = function
  | Class_template cn -> cn.elt.cparams
  | Interface_template i -> i.elt.iparams

let kind = function
  | Class_template _ -> "class"
  | Interface_template _ -> "interface"

type instance = {
  name : id;
  template : id;
  args : ty list;
  mutable declaration : top_level_decl option;
  origin : Range.t;
  chain : id list;
}

type t = {
  templates : (id * template) list;
  instances : (id, instance) Hashtbl.t;
  pending : instance Queue.t;
  mutable ordered : instance list;
}

let fail node message = raise (Error (node.loc, message))

let create classes interfaces =
  let seen = Hashtbl.create 16 in
  let register node name =
    if Hashtbl.mem seen name then
      fail node
        ("Duplicate interface or class " ^ name ^ "; name already exists.");
    Hashtbl.add seen name ()
  in
  List.iter
    (fun (cn : cdecl node) ->
      register cn cn.elt.cname;
      Generic_types.validate_params fail cn.elt.cparams;
      if cn.elt.cparams <> [] && has_annotation "C" cn.elt.annotations then
        fail cn "Generic @C classes are not supported.";
      List.iter
        (fun (fn : fdecl node) ->
          if fn.elt.type_params <> [] then
            fail fn "Generic methods are not supported.")
        cn.elt.methods)
    classes;
  List.iter
    (fun (i : idecl node) ->
      register i i.elt.iname;
      Generic_types.validate_params fail i.elt.iparams;
      let members = Hashtbl.create 8 in
      let member node name =
        if Hashtbl.mem members name then
          fail node ("Duplicate interface member " ^ name ^ ".");
        Hashtbl.add members name ()
      in
      List.iter (fun (f : ifield node) -> member f f.elt.fieldName) i.elt.fields;
      List.iter
        (fun (p : proto node) ->
          member p p.elt.fname;
          if p.elt.type_params <> [] then
            fail p "Generic interface methods are not supported.";
          if p.elt.annotations <> [] then
            fail p "Interface prototypes cannot have annotations.")
        i.elt.protos)
    interfaces;
  {
    templates =
      List.filter_map
        (fun (cn : cdecl node) ->
          if cn.elt.cparams = [] then None
          else Some (cn.elt.cname, Class_template cn))
        classes
      @ List.filter_map
          (fun (i : idecl node) ->
            if i.elt.iparams = [] then None
            else Some (i.elt.iname, Interface_template i))
          interfaces;
    instances = Hashtbl.create 16;
    pending = Queue.create ();
    ordered = [];
  }

let arguments state name =
  Option.map
    (fun instance -> (instance.template, instance.args))
    (Hashtbl.find_opt state.instances name)

let dependent = Ast.exists_type_param (fun _ -> true)

let template state name args =
  match List.assoc_opt (show_qualified_name name) state.templates with
  | None ->
      fail name
        ("Not a generic class or interface: " ^ show_qualified_name name ^ ".")
  | Some template ->
      if List.length (parameters template) <> List.length args then
        fail name
          (Printf.sprintf "Generic %s %s expects %d type arguments, got %d."
             (kind template) (show_qualified_name name)
             (List.length (parameters template))
             (List.length args));
      template

let rec normalize ?(chain = []) state = function
  | TRef (RGeneric (name, args)) ->
      let owner = template state name args in
      List.iter (Generic_types.validate_array_lengths fail name) args;
      let args = List.map (normalize ~chain state) args in
      if List.exists dependent args then TRef (RGeneric (name, args))
      else
        let identity =
          (if kind owner = "class" then "_BLC" else "_BLI")
          ^ Generic_types.length_and_name (show_qualified_name name)
          ^ "_"
          ^ String.concat ""
              (List.map
                 (fun ty ->
                   Generic_types.length_and_name
                     (Generic_types.encode_ty (Conversions.convert_ty ty)))
                 args)
        in
        if not (Hashtbl.mem state.instances identity) then (
          if List.length chain >= 128 || Hashtbl.length state.instances >= 4096
          then
            fail name
              ("Generic nominal specialization limit exceeded: "
              ^ String.concat " -> "
                  (List.rev (show_qualified_name name :: chain)));
          let instance =
            {
              name = identity;
              template = show_qualified_name name;
              args;
              declaration = None;
              origin = name.loc;
              chain = show_qualified_name name :: chain;
            }
          in
          Hashtbl.add state.instances identity instance;
          state.ordered <- instance :: state.ordered;
          Queue.add instance state.pending);
        TRef
          (RClass
             {
               name with
               elt =
                 {
                   qualifiers = [];
                   name = { name.elt.name with elt = identity };
                 };
             })
  | TRef (RClass name) as ty ->
      (match List.assoc_opt (show_qualified_name name) state.templates with
      | Some owner ->
          fail name
            ("Generic " ^ kind owner ^ " " ^ show_qualified_name name
           ^ " requires type arguments.")
      | None -> ());
      ty
  | TRef (RArray (ty, size)) -> TRef (RArray (normalize ~chain state ty, size))
  | TRef (RFun (args, ret)) ->
      TRef
        (RFun
           ( List.map (normalize ~chain state) args,
             normalize_ret ~chain state ret ))
  | ty -> ty

and normalize_ret ?(chain = []) state = function
  | RetVoid -> RetVoid
  | RetVal ty -> RetVal (normalize ~chain state ty)

let drain state =
  while not (Queue.is_empty state.pending) do
    let instance = Queue.take state.pending in
    let owner = List.assoc instance.template state.templates in
    let bindings =
      List.map2 (fun p ty -> (p.elt, ty)) (parameters owner) instance.args
    in
    let substitute =
      substitute_type_params (fun p -> List.assoc p.elt bindings)
    in
    let normalize = normalize ~chain:instance.chain state in
    instance.declaration <-
      Some
        (match owner with
        | Class_template cn ->
            let cn = map_cdecl_types substitute cn in
            let cn =
              {
                cn with
                elt =
                  {
                    cn.elt with
                    cname = instance.name;
                    cparams = [];
                    methods =
                      List.map
                        (fun (fn : fdecl node) ->
                          if fn.elt.fname = instance.template then
                            {
                              fn with
                              elt = { fn.elt with fname = instance.name };
                            }
                          else fn)
                        cn.elt.methods;
                  };
              }
            in
            Class (map_cdecl_types normalize cn)
        | Interface_template i ->
            let i = map_idecl_types substitute i in
            Interface
              (map_idecl_types normalize
                 {
                   i with
                   elt = { i.elt with iname = instance.name; iparams = [] };
                 }))
  done

let normalize_function state fn =
  let count = Hashtbl.length state.instances in
  let fn = map_fdecl_types (normalize state) fn in
  if Hashtbl.length state.instances <> count then (
    drain state;
    raise New_instances);
  fn

let prepare state (Prog (imports, declarations)) =
  drain state;
  let declarations =
    List.filter_map
      (fun (item : top_level node) ->
        let declaration =
          match item.elt.declaration with
          | Class cn when cn.elt.cparams <> [] -> None
          | Class cn -> Some (Class (map_cdecl_types (normalize state) cn))
          | Function fn ->
              let body = fn.elt.body in
              let fn =
                if fn.elt.type_params = [] then
                  map_fdecl_types (normalize state) fn
                else
                  let mapped =
                    map_fdecl_types (normalize state)
                      { fn with elt = { fn.elt with body = [] } }
                  in
                  { mapped with elt = { mapped.elt with body } }
              in
              Some (Function fn)
          | Prototype pn ->
              Some (Prototype (map_proto_types (normalize state) pn))
          | Global gn ->
              Some
                (Global
                   {
                     gn with
                     elt =
                       {
                         gn.elt with
                         gtyp = Option.map (normalize state) gn.elt.gtyp;
                         ginit =
                           Option.map
                             (map_exp_types (normalize state))
                             gn.elt.ginit;
                       };
                   })
          | Interface i when i.elt.iparams <> [] -> None
          | Interface i ->
              Some (Interface (map_idecl_types (normalize state) i))
        in
        Option.map
          (fun declaration -> { item with elt = { item.elt with declaration } })
          declaration)
      declarations
  in
  drain state;
  let instances =
    List.rev_map
      (fun instance ->
        {
          elt =
            { declaration = Option.get instance.declaration; export_loc = None };
          loc = instance.origin;
        })
      state.ordered
  in
  Prog (imports, declarations @ instances)

let display_names state message =
  List.fold_left
    (fun message instance ->
      let display =
        instance.template ^ "<"
        ^ String.concat ", "
            (List.map
               (fun ty -> Pprint_typed_ast.show_ty (Conversions.convert_ty ty))
               instance.args)
        ^ ">"
      in
      Core.String.substr_replace_all message ~pattern:instance.name
        ~with_:display)
    message state.ordered
