open OUnit2

let contains text substring =
  let rec loop index =
    index + String.length substring <= String.length text
    && (String.sub text index (String.length substring) = substring
       || loop (index + 1))
  in
  loop 0

let assert_contains ~substring text =
  assert_bool
    (Printf.sprintf "Expected %S in:\n%s" substring text)
    (contains text substring)

let parse_module (id, text) : Modules.Module_model.source =
  let filename = String.concat "/" id ^ ".bl" in
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  match Parsing.Parse.parse_prog_diagnostic lexbuf with
  | Ok program -> { id; filename; program }
  | Error diagnostic -> assert_failure diagnostic.message

let resolve sources =
  let graph : Modules.Module_model.graph =
    { entry = [ "main" ]; dependency_order = List.map parse_module sources }
  in
  Modules.Module_resolver.resolve graph

let resolved sources =
  match resolve sources with
  | Ok program -> program
  | Error diagnostic -> assert_failure diagnostic.message

let assert_resolves sources = ignore (resolved sources)

let assert_resolution_error ~substring sources =
  match resolve sources with
  | Ok _ -> assert_failure "Expected name resolution to fail"
  | Error diagnostic -> assert_contains ~substring diagnostic.message

let assert_types sources =
  match Typing.Type.type_prog (resolved sources) with
  | Ok _ -> ()
  | Error error -> assert_failure (Core.Error.to_string_hum error)

let write_sources root files =
  let rec mkdir directory =
    if not (Sys.file_exists directory) then (
      mkdir (Filename.dirname directory);
      Unix.mkdir directory 0o700)
  in
  List.iter
    (fun (relative, contents) ->
      let filename = Filename.concat root relative in
      mkdir (Filename.dirname filename);
      let channel = open_out_bin filename in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel contents))
    files
