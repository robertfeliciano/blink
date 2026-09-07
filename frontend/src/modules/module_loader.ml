(** File parsing and canonical path lookup. Graph traversal remains a stub.
    Relative roots use the caller's working directory; absolute roots make
    lookup independent of it. Symlinks escaping their root are rejected. *)
open Module_model

let filesystem_error loc action f =
  try f () with
  | Sys_error message -> Error { loc; message = action ^ ": " ^ message }
  | Unix.Unix_error (error, operation, path) ->
      Error { loc; message = Printf.sprintf "%s: %s (%s %s)"
        action (Unix.error_message error) operation path }

let readable_file filename =
  if (Unix.stat filename).Unix.st_kind <> Unix.S_REG then
    raise (Sys_error (filename ^ " is not a regular file"));
  Unix.access filename [ Unix.R_OK ]

let canonical_directory path =
  let path = Unix.realpath path in
  if (Unix.stat path).Unix.st_kind <> Unix.S_DIR then
    raise (Sys_error (path ^ " is not a directory"));
  path

let parse_source ~id ~filename : (source, diagnostic) result =
  let filename =
    if Filename.is_relative filename then Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  let loc = Util.Range.mk_range filename (1, 1) (1, 1) in
  filesystem_error loc ("Cannot parse module " ^ filename) (fun () ->
    readable_file filename;
    let channel = open_in_bin filename in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let lexbuf = Lexing.from_channel channel in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      match Parsing.Parse.parse_prog_diagnostic lexbuf with
      | Ok program -> Ok { id; filename; program }
      | Error error -> Error { loc = error.loc; message = error.message }))

let valid_component value =
  let alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  String.length value > 0 && alpha value.[0]
  && String.for_all (fun c -> alpha c || (c >= '0' && c <= '9')
      || c = '_' || c = '\'') value

let within_root root path =
  let prefix = if root = Filename.dir_sep then root else root ^ Filename.dir_sep in
  String.starts_with ~prefix path

let resolve_path (config : config) (import : import) : (string, diagnostic) result =
  let parts = Ast.name_components import.elt.path in
  match List.find_opt (fun part -> not (valid_component part.Ast.elt)) parts with
  | Some part -> Error { loc = part.loc; message = "Invalid module path component" }
  | None ->
      let names = List.map (fun part -> part.Ast.elt) parts in
      let root_and_parts =
        match names with
        | "std" :: rest ->
            (match config.stdlib_root, rest with
            | _, [] -> Error { loc = import.loc; message = "Expected a module after std" }
            | None, _ -> Error { loc = import.loc; message = "Standard-library root is not configured" }
            | Some root, _ -> Ok (root, rest))
        | _ -> Ok (config.project_root, names)
      in
      match root_and_parts with
      | Error error -> Error error
      | Ok (root, parts) ->
          let candidate = List.fold_left Filename.concat root parts ^ ".bl" in
          filesystem_error import.loc ("Cannot resolve module at " ^ candidate) (fun () ->
            let root = canonical_directory root in
            let filename = Unix.realpath candidate in
            if not (within_root root filename) then
              Error { loc = import.loc; message = "Module path escapes configured root: " ^ candidate }
            else (
              readable_file filename;
              Ok filename))

(** Prepare the inputs to DFS without traversing any imports. The returned
    roots and source filename are canonical absolute paths. Entry identity is
    derived from its canonical path relative to the project root. *)
let prepare_entry (config : config) ~entry_filename :
    (config * source, diagnostic) result =
  let loc = Util.Range.mk_range entry_filename (1, 1) (1, 1) in
  filesystem_error loc ("Cannot prepare entry module " ^ entry_filename) (fun () ->
    let config =
      { project_root = canonical_directory config.project_root;
        stdlib_root = Option.map canonical_directory config.stdlib_root }
    in
    let filename = Unix.realpath entry_filename in
    if not (within_root config.project_root filename) then
      Error { loc; message = "Entry module is outside the project root: " ^ filename }
    else if not (Filename.check_suffix filename ".bl") then
      Error { loc; message = "Entry module must have a .bl extension: " ^ filename }
    else (
      readable_file filename;
      let prefix_length = String.length config.project_root
        + (if config.project_root = Filename.dir_sep then 0 else 1) in
      let relative = String.sub filename prefix_length (String.length filename - prefix_length) in
      let id = String.split_on_char '/' (Filename.chop_suffix relative ".bl") in
      if not (List.for_all valid_component id) then
        Error { loc; message = "Entry path must consist of module identifiers: " ^ relative }
      else if List.hd id = "std" then
        Error { loc; message = "The std module root is reserved for the standard library" }
      else
        match parse_source ~id ~filename with
        | Error error -> Error error
        | Ok entry -> Ok (config, entry)))

let load (config : config) ~entry_filename : (graph, diagnostic) result =
  match prepare_entry config ~entry_filename with
  | Error error -> Error error
  | Ok (config, entry) ->
      let Ast.Prog (imports, _) = entry.program in
      (* TODO(modules-05): Implement DFS here. config has absolute roots;
         entry is already parsed, with its canonical filename and module id;
         imports are its located import declarations in source order.
         Create fresh visiting/visited state and seed it with entry, so DFS
         does not reparse the entry. Resolve dependencies with resolve_path
         config and parse them with parse_source. Cycle detection, recursion,
         and dependency-first output are intentionally left for you. *)
      ignore (config, imports);
      Error {
        loc = Util.Range.mk_range entry.filename (1, 1) (1, 1);
        message = "Entry module parsed; implement TODO(modules-05) DFS to build the graph";
      }
