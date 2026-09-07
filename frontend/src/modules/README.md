# Module scaffold

Start with [the ordered implementation guide](../../../../docs/modules.md).
The `module_system` library builds independently of the native backend. It is
deliberately not a dependency of `Compiler` yet. `parse_source` and `resolve_path`
are implemented; graph traversal and name resolution still return pending errors.

Module interfaces are inferred from the `.ml` files, matching the main frontend
libraries. There are no separate `.mli` files; helper definitions are therefore
visible to other OCaml modules as well.

- `module_model.ml`: provisional contracts and diagnostics.
- `module_loader.ml`: parsing, path lookup, dependency graph.
- `module_symbols.ml`: one encoding rule for internal declaration names.
- `module_resolver.ml`: export filtering and scoped name resolution before
  existing typing.

Sections 01–04 are complete: located qualified names/import/export metadata,
source-order preservation, syntax parsing, file parsing, and canonical lookup.
`Module_model.import` aliases the authoritative Ast type. Recursive imports and
export visibility enforcement await graph loading and name resolution.

TODO(modules-10): Wire the completed loader and resolver into Compiler and the
CLI only after the isolated tests pass; see the guide for API compatibility.

TODO(modules-11): Add an independent OUnit suite and multi-file native tests.
The planned input files under docs/module-fixtures are not active tests yet.

TODO(modules-12): Add the installed standard-library root and std.io only once
ordinary source modules work. Importing a declaration does not link a runtime.
