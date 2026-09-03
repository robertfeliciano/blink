# AGENTS.md

## Purpose

This file is the working guide for agents and contributors changing Blink.
It describes the repository as it exists on this branch.
Keep it aligned with the build files and implementation when the layout changes.

## Project overview

Blink is a small ahead-of-time compiled programming language.
The compiler is split between an OCaml frontend and a C++/LLVM backend.
Source programs use the `.bl` extension.
The frontend parses, type-checks, and desugars a program.
The lowered OCaml AST crosses a native FFI boundary into C++.
The backend converts that value into C++ data structures and emits LLVM IR.
The normal compiler output is `new_output.ll` in the current directory.
The `compile` helper then lowers and links that IR into `new_output.o`.
Despite its `.o` name, `new_output.o` is the final native executable.

The primary implementation languages and tools are:

- OCaml 4.14.2 for the frontend.
- Dune for OCaml builds and tests.
- ocamllex for lexing.
- Menhir for parsing.
- C++20 for the native bridge and backend.
- CMake for the backend build.
- LLVM 16 for IR construction, optimization, and native lowering.
- OUnit2 for tests.
- Docker Compose for a reproducible development toolchain.

## Compiler pipeline

The end-to-end flow is:

1. `frontend/src/blink.ml` parses command-line options and opens the input.
2. `frontend/src/compiler.ml` coordinates all frontend phases.
3. `frontend/src/parsing/` turns tokens into `Ast.program`.
4. `frontend/src/typing/` validates and annotates the source AST.
5. `frontend/src/desugaring/` lowers high-level constructs.
6. `Desugared_ast.convert_caml_ast` calls the C++ OCaml FFI entry point.
7. `backend/src/bridge/` converts OCaml runtime values into C++ values.
8. `backend/src/codegen/` creates and optionally optimizes an LLVM module.
9. `backend/main.cpp` writes the module to `new_output.ll`.
10. `compile` uses `llc` and `clang` to create a native executable.

The frontend supports `-O0`, `-O1`, `-O2`, and `-O3`.
The selected level is stored in the desugared program and consumed in C++.
`-O0` is the default.
AST debugging flags are `-print-ast`, `-print-typed-ast`, and
`-print-desugared-ast`.

## Repository map

### Root

- `README.md` contains user-facing setup, build, test, and usage notes.
- `makefile` is the main local build and test entry point.
- `compile` wraps `blink`, `llc`, and `clang` for native compilation.
- `Dockerfile` defines the Debian, OCaml, Clang, and LLVM 16 toolchain.
- `compose.yaml` mounts the checkout at `/workspace` in the dev container.
- `examples/` contains sample Blink programs and feature demonstrations.
- `tests/` contains additional `.bl` fixtures not wired into Dune directly.
- `LICENSE` contains the project license.

Generated root artifacts include `blink`, `new_output.ll`, `new_output.s`,
and `new_output.o`.
They are ignored and should not be committed.

### Frontend entry points

- `frontend/src/blink.ml` defines the CLI and optimization flags.
- `frontend/src/compiler.ml` sequences parse, type, desugar, and FFI phases.
- `frontend/src/dune` builds the CLI executable and public compiler library.
- `frontend/dune-project` holds Dune project and package metadata.
- `frontend/blink.opam` describes OCaml dependencies.
- `frontend/.ocamlformat` contains frontend formatting configuration.

### Source AST

- `frontend/src/ast/ast.ml` defines the parser-facing AST.
- AST nodes carry `Util.Range.t` source locations.
- This tree preserves source-level constructs and optional annotations.
- `frontend/src/ast/dune` exposes the AST library.

### Parsing

- `frontend/src/parsing/lexer.mll` defines keywords, literals, and tokens.
- `frontend/src/parsing/parser.mly` defines syntax and precedence.
- `frontend/src/parsing/parse.ml` wraps lexer and parser error handling.
- `frontend/src/parsing/dune` configures ocamllex and Menhir.

When adding syntax, update the lexer, parser, AST, and parser tests together.
Preserve source ranges on newly constructed parser nodes.
Check operator precedence explicitly for new unary or binary operators.

### Typing

- `frontend/src/typing/typed_ast.ml` defines the typed representation.
- `frontend/src/typing/type.ml` builds contexts and types declarations.
- `frontend/src/typing/type_stmt.ml` types statements and expressions.
- `frontend/src/typing/type_util.ml` provides type rules and helpers.
- `frontend/src/typing/tctxt.ml` stores locals, globals, classes, and prototypes.
- `frontend/src/typing/conversions.ml` converts source AST types.
- `frontend/src/typing/pprint_typed_ast.ml` prints typed trees for diagnostics.
- `frontend/src/typing/dune` exposes the typing library.

Type failures are reported with source locations through `type_error` helpers.
Keep constness and class context intact when extending context operations.
Function prototypes and definitions are collected before bodies are checked.
Classes track fields and method headers in a separate class context.

### Desugaring

- `frontend/src/desugaring/desugared_ast.ml` defines the backend-facing AST.
- `frontend/src/desugaring/desugar.ml` coordinates the lowering pass.
- `frontend/src/desugaring/desugar_stmt.ml` lowers statements and expressions.
- `frontend/src/desugaring/desugar_class.ml` lowers classes and methods.
- `frontend/src/desugaring/desugar_lambdas.ml` lifts lambdas and environments.
- `frontend/src/desugaring/desugar_util.ml` contains shared lowering helpers.
- `frontend/src/desugaring/conversions.ml` converts typed AST types.
- `frontend/src/desugaring/pprint_desugared_ast.ml` prints lowered trees.
- `frontend/src/desugaring/dune` links the native backend archive.

Desugaring removes constructs the backend does not understand directly.
For example, methods are extracted into functions and lambdas are lifted.
The final program includes the selected optimization level.
The external `convert_caml_ast` declaration is the OCaml side of the FFI.

### Utilities

- `frontend/src/util/range.ml` and `.mli` implement source ranges.
- `frontend/src/util/constants.ml` and `.mli` hold shared constants.
- `frontend/src/util/optimization_level.ml` and `.mli` define optimization levels.
- `frontend/src/util/dune` builds the utility library.

### Backend bridge

- `backend/main.cpp` defines `extern "C" convert_caml_ast`.
- `backend/include/bridge/` declares C++ AST types and converters.
- `backend/src/bridge/` implements conversions from OCaml runtime values.
- Bridge files are divided into `types`, `exp`, `stmt`, `decl`, and `prog`.

The bridge depends on the concrete OCaml runtime representation of variants,
records, tuples, lists, and booleans.
Constructor order and field order are therefore part of the FFI contract.
Changing `desugared_ast.ml` usually requires a matching bridge change.
Do not assume the C++ compiler will detect a mismatched OCaml layout.
Exercise every changed constructor through a backend or end-to-end test.

### LLVM code generation

- `backend/include/codegen/` declares the generator and visitor helpers.
- `backend/src/codegen/` implements type, expression, statement, declaration,
  and lvalue lowering.
- `backend/include/codegen/generator.h` owns the LLVM context and module.
- `backend/src/codegen/generator.cpp` sets the target and pass pipeline.
- `backend/include/util/` and `backend/src/util/` provide debug and print tools.
- `backend/CMakeLists.txt` discovers OCaml and LLVM and builds `libbackend.a`.

`Generator` owns mutually cooperating visitors.
It also tracks local allocas, class/struct metadata, and loop branch targets.
Program codegen declares classes first, then built-ins and function prototypes,
then emits function bodies and runs the requested LLVM optimization pipeline.
The generated module targets the build host and uses position-independent code.

### Tests

- `frontend/test/frontend_tests.ml` gathers parser, typer, and desugar suites.
- `frontend/test/test_parsing.ml` contains parser unit tests.
- `frontend/test/test_typing.ml` contains typing unit tests.
- `frontend/test/test_desugaring.ml` contains lowering unit tests.
- `frontend/test/e2e.ml` compiles source text through the complete pipeline.
- `frontend/test/backend_tests.ml` tests bridge/codegen with constructed ASTs.
- `frontend/test/backend_fixture_compiler.ml` builds those backend fixtures.
- `frontend/test/native_test_support.ml` links and runs generated native code.
- `frontend/test/dune` defines all test and helper executables.

Native tests run in OUnit-managed temporary directories.
They verify generated files and expected process exit codes.
They require `llc`, `clang`, and a previously built native backend.

## Build and development

The most reproducible setup is the development container:

```sh
docker compose build
docker compose run --rm dev
```

Inside the container, or on a correctly provisioned host, build everything:

```sh
make
```

The build order matters.
The backend target creates `frontend/lib/libbackend.a`.
Dune then links that archive into the native OCaml executable.
The root `make` target already enforces this order.

Build only the backend with:

```sh
make backend
```

Build only the frontend after the backend archive exists with:

```sh
make frontend
```

For a direct frontend development loop:

```sh
cd frontend
dune build ./src/blink.exe
```

Clean generated CMake, Dune, LLVM, assembly, and native artifacts with:

```sh
make clean
```

The clean target is intentionally broad over generated build artifacts.
Do not place hand-written `.ll`, `.s`, or `.o` files at the repository root.

## Running programs

Compile a source file to LLVM IR:

```sh
./blink -O0 examples/simple.bl
```

Compile and link a runnable native program:

```sh
./compile -O2 examples/simple.bl
./new_output.o
```

The optimization flag may appear before or after the filename.
Only one input file and one optimization flag are accepted.
The wrapper validates `.bl` filenames and rejects unknown options.
All generated output is written relative to the current working directory.

## Test commands

Run the complete Dune test alias from the repository root:

```sh
make test
```

Run only fast frontend unit tests:

```sh
make test-unit
```

Run source-to-native integration tests:

```sh
make test-e2e
```

Run bridge and backend-focused native tests:

```sh
make test-backend
```

From `frontend/`, `dune runtest` is equivalent to the full test alias.
After a clean checkout, run `make` before native test suites.
When changing one phase, run its focused tests first, then `make test`.

## Change guidelines

Prefer small changes that preserve phase boundaries.
Do not bypass typing by teaching codegen to recover missing type information.
Do not send source-only constructs through the desugared AST without a plan.
Keep diagnostics attached to the most specific available source range.
Use existing AST conversion helpers instead of duplicating type mappings.
Keep OCaml and C++ representations synchronized at the FFI boundary.
Avoid committing generated compiler output or build directories.
Prefer full, deep fixes rather than band-aid fixes to specific problems.

Follow KISS and DRY principles throughout the project.
Before adding logic, search for an existing implementation that can be reused.
If the same new behavior would be implemented in multiple files, extract it
into an appropriately scoped shared function, module, visitor helper, or test
utility and call that implementation from each site.
Keep abstractions focused on actual reuse; do not add indirection for logic that
is simple, isolated, and unlikely to be repeated.
There should be one authoritative implementation of each shared rule or
conversion so later fixes do not need to be synchronized across copies.

OCaml code should follow `frontend/.ocamlformat`.
Run `dune fmt` from `frontend/` when formatting frontend changes.
C++ code should follow `backend/.clang-format`.
Format touched C++ files with `clang-format` when available.
Match existing naming and visitor patterns in nearby backend code.

### Adding a language feature

Typical touch points are:

1. Add tokens in `lexer.mll` if the syntax introduces new lexical forms.
2. Add grammar and precedence rules in `parser.mly`.
3. Extend `ast.ml` and parser tests.
4. Extend `typed_ast.ml`, type rules, contexts, and typing tests.
5. Extend `desugared_ast.ml` and the appropriate lowering modules.
6. Update every affected C++ bridge converter.
7. Add LLVM lowering in the matching codegen visitor.
8. Add desugaring, backend, and end-to-end coverage as appropriate.
9. Run formatting, `make`, and `make test`.

### Changing the desugared AST

Treat this as a cross-language interface change.
Compare each modified OCaml constructor with its converter in
`backend/src/bridge/`.
Verify constructor tags, record fields, tuple positions, and optional values.
Update C++ types in `backend/include/bridge/` before changing codegen users.
Add a native test that forces the changed value across the boundary.
A frontend-only unit test is not sufficient for this class of change.

### Changing LLVM codegen

Check generated IR validity as well as runtime behavior.
Use a backend fixture when parsing and typing are irrelevant to the case.
Use an end-to-end test when source syntax or frontend lowering is involved.
Remember to maintain `breakTargets` and `continueTargets` for nested loops.
Keep allocas and environment state scoped consistently with existing visitors.
Test at `-O0` first; optimized builds can hide malformed or unstable IR.

### Changing dependencies or build configuration

OCaml dependencies originate in `frontend/dune-project`.
`frontend/blink.opam` is generated by Dune; edit the project metadata first.
If dependencies change, update the Docker image setup as needed.
LLVM component linkage is declared in `backend/CMakeLists.txt`.
Keep LLVM assumptions compatible with version 16 unless upgrading deliberately.
Rebuild the Docker image after Dockerfile or dependency changes.

## Validation checklist

Before handing off a code change:

- Inspect `git diff` for accidental generated files.
- Format all touched OCaml and C++ sources.
- Build the backend before linking a fresh frontend.
- Run the narrowest relevant unit test while iterating.
- Run `make test` for changes that can affect compiler behavior.
- Compile and run a representative file from `examples/` when appropriate.
- Confirm failures include useful source locations when diagnostics changed.
- Confirm new desugared variants have matching bridge coverage.
- Note any required toolchain or test limitation in the handoff.

## Known constraints and sharp edges

The frontend and backend are linked into one native executable.
This makes the backend archive a prerequisite even for some frontend builds.
The OCaml/C++ bridge is positional and lacks a versioned schema.
The compiler writes a fixed `new_output.ll` filename.
Parallel compiler invocations in the same directory will collide.
The `compile` script also uses fixed assembly and executable filenames.
Run concurrent compilations in separate working directories.
Native tests depend on host execution and expected process exit codes.
The project currently targets the native host rather than cross-compiling.
Generics and several prospective language features remain unsupported.
Use implemented examples and tests as the source of truth for syntax.

## Where to start

For CLI behavior, start at `frontend/src/blink.ml`.
For pipeline behavior, start at `frontend/src/compiler.ml`.
For syntax, start at `lexer.mll`, `parser.mly`, and `ast.ml`.
For type errors, start at `type.ml`, `type_stmt.ml`, and `type_util.ml`.
For lowering, start at `desugar.ml` and follow the specialized modules.
For FFI bugs, compare `desugared_ast.ml` with `backend/src/bridge/`.
For bad LLVM IR, start at the relevant visitor in `backend/src/codegen/`.
For build or link failures, inspect `makefile`, Dune files, and CMake together.
For expected behavior, search `frontend/test/` before relying on examples alone.
