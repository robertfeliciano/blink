# Module implementation

The `blink.modules` library builds independently of the native backend and is
used by `Compiler.compile_file` before the existing typing/lowering pipeline.

`prepare_entry` prepares canonical roots and the parsed entry module. `load`
performs DFS, parses shared dependencies once, detects cycles and ambiguous
module identities, and returns deterministic dependency-first source order.
State is fresh for every load call.

Module interfaces are inferred from the `.ml` files, matching the main frontend
libraries. There are no separate `.mli` files; helper definitions are therefore
visible to other OCaml modules as well.

- `module_model.ml`: compile-time contracts and diagnostics.
- `module_loader.ml`: parsing, path lookup, dependency graph.
- `module_symbols.ml`: one encoding rule for internal declaration names.
- `module_resolver.ml`: export filtering and scoped name resolution before
  existing typing.

All twelve module steps are implemented. The resolver indexes each file's
declarations and direct imports, rewrites names with lexical scope and explicit
export checks, then combines files. Functions, classes, interfaces, and globals
use reserved length-coded identities; `main` and external C prototypes retain
exact names. Error display
decodes internal identities without global state. Compatible shared C prototypes
are deduplicated by the existing type checker, after nominal names resolve.

Imports select either a module namespace (`import helper as h;`) or a nonempty
list of exported declarations (`import answer, Box, from helper;`). Named lists
allow a trailing comma. The source AST keeps the located names and module path;
the loader follows that same path for both forms. The resolver validates every
named member eagerly, using the same export lookup as qualified projections.
Import bindings share one namespace with module aliases and cannot conflict
with top-level declarations. Locals may shadow named imports, as they do module
declarations, while namespace aliases remain reserved within the file.
Named imports reuse original declaration identities without re-exporting them
or binding an implicit module alias. No import metadata reaches typing or the
native bridge. `from` is reserved by the lexer.

Top-level global variables use `gdecl` nodes and the same export filtering and
symbol encoding as functions. A qualified read or assignment is rewritten to
the defining module's identity, so every importer accesses one shared storage
location. Typing resolves constant initializer dependencies, including forward
references, before checking class defaults and function bodies. Initializers
are folded to scalar or string literals, preserving string constant references
so aliases share their backing storage; runtime module initialization remains
unsupported. Unlike imports, global declarations cross the native FFI as the
fifth field of the desugared program and become LLVM globals with internal
linkage. Source `export` does not imply external linker visibility.

The CLI/wrapper accept `-module-root` and `-stdlib-root`. The default project
root is the entry directory; standard-library roots are explicit. `runtime/stdlib/io.bl`
exports `println` over private libc `puts`; a custom C++ runtime is a follow-up.

AST/loader/resolver suites run independently without libbackend.a. The native
module suite tests multi-file calls/classes/interfaces/closures/globals, O0/O2, C symbols,
stdlib output, failures, and the wrapper from outside the project directory.

Generic function type parameters are bound in the source AST before resolution.
The resolver preserves those bindings while resolving template bodies in their
defining module, including private helper references. Exported generic functions
use the same module identities and visibility rules as ordinary functions.
Typing later specializes the resolved templates and shares each instance across
all importers in the combined program; generic metadata stays in the frontend.

Generic class and interface parameters bind before module resolution as well.
Type applications may name either kind; object construction requires a class,
and `impl` applications require an interface. Template signatures and implementation
arguments resolve in their defining module. The shared nominal specialization
coordinator gives each concrete class or interface one identity across importers.
