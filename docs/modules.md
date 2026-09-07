# Implementing modules in Blink

This branch implements module syntax and individual-file lookup/parsing.
Recursive import compilation is not enabled: the `frontend/src/modules/` library
is independently buildable but its loader is not called by the compiler.
Unfinished graph/resolver entry points return descriptive `Error` values.
The `.bl` files in `docs/module-fixtures/` describe the first acceptance case;
they are not registered as runnable examples or tests yet.

Current checkpoint: sections 01–04 are complete. Qualified names retain each
component's range plus the whole name's range. Imports, exported functions and
classes, qualified class types, and qualified object initializers parse. File
lookup canonicalizes paths, isolates stdlib lookup, and rejects symlink escapes.
Typing still rejects unresolved imports and qualified class names; section 05
graph construction and section 07 name resolution must precede their compilation.

Work through `TODO(modules-01)` through `TODO(modules-12)` in order. The matching
markers in the scaffold point back here. Add tests as each behavior is built;
step 11 completes the integration coverage rather than postponing all testing.

## 01 — Settle the module contract

**Owner:** `module_model.ml`. **Depends on:** nothing.

Agreed first-version rules (enforcement is implemented in later steps):

- One file is one module. Module identity comes from the path relative to a
  configured root; there is no `module` declaration in source.
- `import app.geometry;` introduces alias `geometry` in the importing file.
  `import app.geometry as shapes;` introduces `shapes` instead.
- Use qualified access (`shapes.area(...)`) and qualified class types
  (`shapes.Circle`). An import does not copy names into the local namespace.
- A top-level function, function prototype, or class is visible to importers
  only when its declaration starts with `export`.
  Unmarked declarations remain usable throughout their defining module.
- `export` applies only to top-level declarations. It cannot mark imports,
  methods, fields, parameters, local declarations, or statements.
- Imports themselves are not re-exported. Access is through direct imports
  only, even when the imported module exported the referenced declaration.
- Reject duplicate aliases, even if they refer to the same module. Reject
  declarations or local bindings that reuse an import alias, with a useful
  diagnostic. This is a deliberate MVP restriction that removes ambiguity
  between module access and object projection.
- Reserve `std` as the standard-library root. `std.io` resolves exclusively
  under the configured standard-library directory, never to a project file.
- Reject import cycles. Accept shared dependencies and load each only once.
- Only the entry module may declare `main`; an imported `main` is an error.
  `main` does not need `export`. Do not otherwise change Blink's entry-point
  signature rules in this feature.
- No runtime module initialization, wildcard imports, re-exports, package
  downloads, separate compilation, or persistent compiler cache in this MVP.

For the first fixture, the project root is `docs/module-fixtures` and the entry
is `main.bl`. Later provide `-module-root DIR`; by default use the entry file's
directory, not the process working directory. Under this rule `app.geometry`
maps to `<root>/app/geometry.bl`. Define the standard-library root to contain
`io.bl`, so `std.io` maps to `<stdlib-root>/io.bl` (strip the `std` segment).

**Done when:** these rules are understood well enough to write positive and
negative tests without choosing a different interpretation in each phase.

Representative declarations are:

```blink
fun private_helper(value: i32) => i32 { return value + 1; }

export fun calculate(value: i32) => i32 {
  return private_helper(value);
}

export class Result {
  let value: i32;
}
```

An importer may use `module.calculate` and `module.Result`; it must receive a
private-member diagnostic for `module.private_helper`. An exported class
exposes its existing Blink-visible constructor, methods, and fields.
Member-level visibility is a later feature. Global variables are outside this
module feature because Blink does not currently implement them.

## 02 — Add source AST representations

Implemented, including qualified type and object-initializer names.
`Ast.program` is now `Prog of import node list * top_level node list`.
`top_level.export_loc` is `None` for private declarations or `Some range` for
explicit exports, preserving the keyword location without a redundant boolean.
`Ast.partition_declarations` supplies the existing typechecker's header groups.
`Module_model.source` carries imports only through its `program`, avoiding two
copies that could diverge. Direct typing of unresolved imports reports an error.

Run the focused AST/typing checks from `frontend/` with
`dune exec test/module_ast_tests.exe`. These tests also construct import/export
metadata directly to exercise representation and typing boundaries independently.

**Files:** `frontend/src/ast/ast.ml`, AST printers, `module_model.ml`.

Introduce an import record in `Ast`, with located path components and an
optional located alias. Represent top-level declarations with a wrapper that
records the located declaration and whether `export` was present. This keeps
visibility on a module-owned declaration rather than adding an `exported` field
to `fdecl`, which is also reused for class methods. Include functions,
prototypes, and classes in the top-level variant.
The scaffold's simpler import record is provisional: replace it with an alias
to the AST definition so parsing and loading do not maintain competing schemas.

Extend `Ast.program` to carry imports and top-level declarations, then update
every constructor/pattern in the frontend and tests. Leave the currently unused
`gdecl` type untouched; globals are outside this feature. Search all `Prog`
uses, distinguishing Ast, Typed_ast, and Desugared_ast. Import/export metadata
should disappear before the backend boundary.

Qualified type names use `qualified_name = { qualifiers : id node list;
name : id node }`, wrapped in a source node. `RClass`, `ObjInit`, and import paths
all reuse it. A required final name makes empty paths unrepresentable.
`unqualified_name`, `name_components`, and `unqualified_id` provide shared
construction/access operations; `show_qualified_name` joins components for
display only. The future resolver supplies internal unqualified identities
before typed AST conversion; typed/desugared class representations are unchanged.

**Done when:** existing AST construction tests compile, source printers show
imports, export modifiers, and qualified types, and source ranges identify both
the declaration and its `export` keyword.

## 03 — Parse imports and individual source files

Implemented. `Module_loader.parse_source` reads a regular, readable file with
exception-safe channel cleanup and retains its filename in every source range.
`Parsing.Parse.parse_prog_diagnostic` exposes a structured range/message result;
the existing `parse_prog` API still returns `Core.Error.t` for existing callers.

**Files:** `lexer.mll`, `parser.mly`, `parse.ml`, `module_loader.ml`, its Dune file,
and `frontend/test/test_parsing.ml`. **Depends on:** 02.

`IMPORT` and `EXPORT` are enabled in the lexer and parser. `AS` is shared with
casts. Parse `import path.parts;` and `import path.parts as alias;` at
top level. The MVP should require imports before other declarations. Reject
empty paths, trailing dots, non-identifiers, missing semicolons, and imports
inside functions. Check that existing casts still parse unchanged.

Parse `export` as an optional prefix on the whole top-level declaration, before
annotations and modifiers:

```blink
export fun answer() => i32 { return 42; }
export inline fun square(value: f64) => f64 { return value * value; }
export @C fun puts(text: string) => i32;
export class Box { let value: i32; }
```

Reject `export import`, `export` without a declaration, duplicate `export`, and
exported methods, fields, or local functions. `export let` and `export const`
are invalid because globals are outside this feature. Do not treat `export` as
an annotation: visibility is a language rule needed before annotation lowering.

Parse qualified class names in annotations, return types, arrays, constructor
calls and object initializers as applicable to the existing grammar. Expression
access may initially parse as `Proj`; the resolver will distinguish module
qualification from instance projection. Do not let modules become values.

Dots extend the qualified type in a cast. Write `(value as geo.Circle).radius`
to project a field after casting. Explicit grammar precedence implements this
rule without unresolved shift/reduce conflicts.

Fill `parse_source` using `Parsing.Parse.parse_prog` and add `parsing` to the
new library's dependencies. Set `lex_curr_p.pos_fname` to the opened filename,
and close the channel on success and exception. Improve parse errors to include
the filename; the existing wrapper often reports only line and column.
Translate filesystem and parser failures into the shared diagnostic form.

**Done when:** imports, exported declarations, and qualified types parse;
invalid forms fail at useful locations; and parsing an imported file retains
that file's identity.

## 04 — Implement deterministic module lookup

Implemented by `Module_loader.resolve_path`. It returns the canonical absolute
filename, or an import-located error containing the attempted path. In-root
symlinks are accepted; escaping symlinks, directories, missing/unreadable files,
invalid path components, and missing stdlib configuration are rejected. Absolute
configured roots make lookup independent of the caller's working directory.
Relative roots are supported relative to that directory at call time; the
future compilation entry point should freeze roots once before graph traversal.
Two logical names resolving to one file produce the same canonical filename;
detecting that graph-level identity ambiguity remains part of section 05.

Run `dune exec test/module_loader_tests.exe` from `frontend/` for syntax,
qualified-name, file parsing, and temporary-filesystem lookup tests.

**Owner:** `module_loader.resolve_path`. **Depends on:** 01–03.

Join validated identifier components under the designated root and append
`.bl`. Never interpret dotted module names as arbitrary filesystem paths.
Resolve roots to absolute paths once per compilation. Canonicalize files for
cache identity, while keeping readable paths for diagnostics. If two logical
module names resolve through symlinks to one file, reject the ambiguity instead
of giving its classes two identities. Define and test whether symlinks escaping
the configured root are permitted; recommended MVP policy is to reject them.

Report missing files, unavailable stdlib root, non-file targets and unreadable
files at the import declaration, including the attempted path. Avoid using
global `chdir`; that would affect output locations and nested imports.

**Done when:** changing the caller's working directory does not change an
explicitly rooted import, and all failure cases have deterministic diagnostics.

## 05 — Load the dependency graph

**Owner:** `module_loader.load`. **Depends on:** 03–04.

Use a fresh table for each compilation, keyed by canonical file identity.
Maintain unseen, visiting, and visited states. On a visiting dependency, report
the cycle chain and the source location of the closing import edge. On a
visited dependency, reuse it. Parse once and append a source to the output
after visiting its dependencies; preserve import source order for deterministic
results. Record the entry identity separately from dependency order.

Do not cache failures or trees across separate compilations. A long-lived test
process may change a source file between compilations. Collect dependency
information without concatenating text, which would destroy source locations.

**Done when:** direct cycles, indirect cycles and diamond dependencies have
tests, and repeated compilation does not leak loader state.

## 06 — Give declarations stable internal identities

**Owner:** `module_symbols.encode`. **Depends on:** 05.

Use a compiler-reserved prefix plus length-prefixed components for ordinary
module-owned functions and classes. Test separator ambiguities such as
`a.bc` versus `ab.c`. Check the existing `desugar_util.ml` naming conventions
before choosing a prefix; source identifiers must not collide with generated
names. Preserve source spellings alongside identities for diagnostics.

Keep the entry's `main` symbol exactly `main`. Existing `@C` prototypes must
keep their exact external linker name. Across imported modules, deduplicate
compatible declarations of the same C symbol and reject conflicting signatures
or incompatible definitions before generating LLVM. Do not assume LLVM will
merge them: duplicate creation may silently rename symbols. Use the existing
typed type equality for signature comparison after nominal names resolve.

This step does not require `@extern("symbol")`: a Blink wrapper in `std.io`
can call an existing `@C fun blink_rt_v1_io_println(...)` prototype.

**Done when:** identically named functions and classes in different modules are
distinct, one module's identity is stable across importers, and C names remain
exact.

## 07 — Resolve names with lexical scope

**Owner:** `module_resolver.resolve`. **Depends on:** 05–06.

Build an export table containing only declarations marked `export` and an alias
table for each importing file. Keep a separate complete local-declaration table
so private items remain usable inside their module. Resolve bodies using only
that module's declarations and direct imports.
Only after resolution combine declarations into an ordinary source program.
Flattening first would accidentally allow access to unimported declarations.

Walk all expressions and statements, tracking parameters, ordered local
bindings, nested blocks, loop variables, lambda parameters and captures.
Resolve annotation arguments, field defaults, method bodies, partial
applications and ternary expressions too. Preserve instance field/member
spellings; qualify class identities and top-level symbols. Ordinary local
shadowing of function names must continue to work. Module aliases follow the
no-shadowing MVP rule from step 01.

Resolve `alias.member` against exports when its base is an alias; otherwise
leave it as ordinary object projection for typing. Validate member existence
and kind: a class used as a function/value is not automatically a valid call.
When a requested member exists but is not exported, report that it is private;
do not misleadingly report an unknown member.
Resolve qualified class names consistently in signatures, casts, object
initialization and constructor references. Imports are compile-time names and
cannot be passed as arguments or assigned to variables.

If source and internal identities need more structure, introduce a narrow
resolved representation or accompanying lookup metadata. Avoid copying the
entire AST hierarchy without a concrete need. The scaffold's `Ast.program`
return type is a starting point, not a requirement to lose diagnostic names.

**Done when:** alias/member/private-item errors are located correctly, local
variables are not renamed accidentally, and unexported declarations and
transitive imports do not leak names.

## 08 — Integrate resolved names with typing

**Files:** `typing/type.ml`, `type_stmt.ml`, `tctxt.ml`, conversion/printing
helpers and typing tests. **Depends on:** 07.

Reuse existing signature collection and type rules. A class's nominal identity
must include its defining module: `left.Box` and `right.Box` are different
types even if their fields match. Preserve constructors, `this`, fields and
method lookup when class identities change. Reject imported `main` before
flattening and reconcile shared external signatures here if step 06 deferred it.

Use original display names and ranges in diagnostics; users should not have to
decode compiler mangling. An imported file's error must cite its own source.
Existing single-file programs must still type-check without imports.

**Done when:** cross-module calls and classes work, incompatible nominal types
fail, and partial application and local shadowing retain existing semantics.

## 09 — Audit lowering, the bridge and LLVM symbols

**Files:** `desugaring/desugar*.ml`, backend bridge declarations and
`backend/src/codegen/{decl,exp,stmt,generator}.cpp`. **Depends on:** 08.

The intended boundary is a combined, resolved program: imports and export flags
disappear before desugaring. Existing function/class string identities should
be sufficient at the bridge. Inspect every declaration and use site, including
method mangling, constructors, lifted closures, and external calls. Qualify
classes before applying the existing method-name encoder; do not invent a second
method-mangling rule in the resolver.

Native tests must cover statement calls, expression calls and partially applied
imported functions. Check emitted symbols, exact C names and a single entry
`main`. Validate LLVM IR at O0. If a desugared field/constructor really changes,
update its positional C++ converter and add native bridge coverage together.

**Done when:** the backend emits valid IR without knowing filesystem/module
structure, and separate modules cannot collide in LLVM's symbol table.

## 10 — Connect the CLI and compiler pipeline

**Files:** `frontend/src/compiler.ml`, `blink.ml`, `frontend/src/dune`, `compile`.
**Depends on:** 03–09.

Add the module library dependency only now. Provide a file-based compiler entry
that loads and resolves the graph, then invokes the existing typing/desugaring
pipeline once. Preserve the lexbuf-based API for existing callers/tests; an
in-memory input with imports needs explicit origin/root configuration or a
clear error. It must not silently choose roots from the process directory.

Share the post-resolution pipeline rather than duplicating its phases. Thread
optimization/debug options through both entry points. Explain whether source
debug output shows individual parsed modules or the combined resolved tree.
Ensure compile errors return a nonzero process status, not only printed errors.

Add documented `-module-root` and `-stdlib-root` options and update `compile`'s
argument validation/forwarding to accept them. Keep one root input file and one
LLVM output module. Do not invoke separate backend compilations for imports;
they currently write the same output filename.

**Done when:** command-line compilation works from a different directory with
explicit roots, wrapper options work, and existing callers keep their behavior.

## 11 — Complete regression coverage and the first milestone

**Files:** new `frontend/test/test_modules.ml`, Dune test definitions, existing
parser/typing/desugaring suites, native test support and e2e suite.
**Depends on:** test incrementally; finish after 10.

Create an independently runnable OUnit suite depending on the module library,
parser, AST and test filesystem helpers. It should not need libbackend.a for
path/graph/resolver checks. The scaffold deliberately adds no always-passing
tests of its pending errors; tests should exercise implemented behavior.

Extend native fixture support to write multiple source files under one temporary
project root. Run each native case in its own directory and place an upper
bound on process runtime where appropriate. Include:

- The supplied main/helper fixture: expected native exit code 42.
- Aliases, nested module paths, diamond imports and stable output order.
- Same-spelling functions/classes in different modules, and shadowed locals.
- Private functions/classes rejected through imports but usable inside their
  defining module; corresponding exported items accessible to importers.
- Qualified class types, constructors, methods and imported partial calls.
- Invalid `export` placement on imports, methods, fields, locals and statements.
- Missing files, syntax/type failures in imports, invalid members and aliases.
- Import cycles, duplicate aliases, imported main and unimported references.
- Compatible/incompatible duplicated C prototypes and unchanged C symbols.
- Repeated compilations, including changing a dependency between runs.
- O0 and optimized native execution, plus module IR verification.

When the fixture works, move/copy it into the active example/test layout and
remove its unsupported-syntax notice. Update documentation accordingly.

## 12 — Add the first standard-library module

**Files:** eventual `stdlib/io.bl`, runtime build/linking files and README.
**Depends on:** 11; C++ runtime work is a separate next milestone.

Supply a standard-library root through CLI/configuration and later installation
metadata; avoid embedding this worktree's absolute path. Use `import std.io;`
and `io.println(...)` with a Blink wrapper around an exact `@C` declaration.
Mark the wrapper `export` and leave the raw runtime declaration unexported:

```blink
@C fun blink_rt_v1_io_println(text: string) => i32;

export fun println(text: string) => i32 {
  return blink_rt_v1_io_println(text);
}
```

Import resolution makes declarations available to typing, but does not provide
their implementations. A C++ runtime archive still needs to be built and linked
by `compile` and native tests. Keep that archive independent of LLVM/OCaml.

After this milestone, consider visibility before a broad public standard
library, followed by selective imports and package configuration. Separate
compilation can come later without changing the source import syntax.

## Working on the scaffold

The worktree is on `cdx/modules`, rebased onto `4cb44bf` (`master`). It lives
at `/home/robert/projects/blink/build/worktrees/modules`; treat this directory
as a real checkout, even though its parent is named `build`. Do not delete it
as a generated artifact. Existing `make clean` does not target this location.
The plan scaffold is committed on the branch; inspect the worktree status before
starting implementation so later edits are easy to distinguish.

From this worktree's `frontend/`, build just the new library:

```sh
dune build src/modules/module_system.cma
dune fmt
```

For completed compiler changes, follow the repository validation workflow from
the worktree root:

```sh
make
make test-unit
make test
```

Build artifacts are per-worktree. The new library needs the configured OCaml
dependencies but no native backend archive. Full compiler/native suites require
the backend and LLVM 16 toolchain. Inspect formatter output before accepting
unrelated formatting changes. Find all implementation markers with:

```sh
rg -n 'TODO\(modules-[0-9]+\)' frontend/src/modules
```
