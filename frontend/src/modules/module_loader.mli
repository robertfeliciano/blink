(** File parsing and canonical path lookup. Graph traversal remains a stub.
    Roots are interpreted relative to the caller's working directory if relative.
    Pass absolute roots for lookup independent of the working directory.
    Lookup rejects symlinks escaping their designated root. *)

val parse_source :
  id:Module_model.module_id ->
  filename:string ->
  (Module_model.source, Module_model.diagnostic) result

val resolve_path :
  Module_model.config ->
  Module_model.import ->
  (string, Module_model.diagnostic) result

val load :
  Module_model.config ->
  entry_filename:string ->
  (Module_model.graph, Module_model.diagnostic) result
