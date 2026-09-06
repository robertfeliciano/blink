(** Module discovery and parsing. Every stub returns an explicit Error. *)

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
