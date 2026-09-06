(** One authoritative encoding for module-owned declaration identities.
    Do not call this for main or an external C linker symbol. *)
val encode :
  owner:Module_model.module_id ->
  name:string ->
  (string, Module_model.diagnostic) result
