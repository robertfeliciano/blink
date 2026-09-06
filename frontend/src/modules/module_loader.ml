open Module_model

(* TODO(modules-03): Extend lexer/parser, then use Parsing.Parse.parse_prog here.
   Open each file with exception-safe cleanup and set pos_fname to its path.
   Add the parsing library dependency only when this implementation needs it. *)
let parse_source ~id:(_ : module_id) ~filename:(_ : string) :
    (source, diagnostic) result =
  pending "03"

(* TODO(modules-04): Resolve std.* exclusively under stdlib_root; resolve other
   names under project_root. Canonicalize identity, preserve useful display
   paths, and attach lookup failures to the importing declaration's range. *)
let resolve_path (_ : config) (_ : import) : (string, diagnostic) result =
  pending "04"

(* TODO(modules-05): Traverse imports with per-compilation visiting/visited
   states. Parse once per canonical file, report cycle edges, and return stable
   dependency-first order. No process-global cache and no source concatenation. *)
let load (_ : config) ~entry_filename:(_ : string) :
    (graph, diagnostic) result =
  pending "05"
