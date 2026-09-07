(** One authoritative encoding for module-owned declaration identities.
    Do not call this for main or an external C linker symbol. *)
open Module_model

(* TODO(modules-06): Encode component lengths under a compiler-reserved prefix.
   Keep display names separately for diagnostics. C prototypes retain their
   exact external names; reconcile compatible duplicates across modules. *)
let encode ~owner:(_ : module_id) ~name:(_ : string) :
    (string, diagnostic) result =
  pending "06"
