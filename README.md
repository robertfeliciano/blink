# Blink
and you'll miss it...

## Imports and modules

Each `.bl` file is a module. Imports introduce a local alias; only functions,
function prototypes, classes and global variables marked `export` are accessible
through it.
Private declarations remain usable inside their defining file.

```blink
// geometry.bl
export class Box { let value: i32 = 42; }

// main.bl
import geometry as shapes;
import std.io;

fun main() => i32 {
    let box: shapes.Box = new shapes.Box {};
    io.println("Hello modules!");
    let result = box.value;
    free box;
    return result;
}
```

```sh
make
./compile -O0 -stdlib-root runtime/stdlib examples/modules/main.bl
./new_output.o
```

The project import root defaults to the entry file's directory. Pass
`-module-root DIR` for a larger source tree: `import app.geometry;` maps to
`DIR/app/geometry.bl`. `std.io` resolves only under `-stdlib-root DIR`, to
`DIR/io.bl`. No standard-library path is hardcoded; specify it explicitly.
Both options work with `blink` and `compile`, including from another directory.
`std.io.println` uses libc `puts`, adds a newline, and returns its status.
`std.math` provides `f64` math functions backed by libm: `sin`, `cos`, `tan`,
`asin` (arcsine), `acos` (arccosine), `atan`, `atan2(y, x)`, `sinh`, `cosh`,
`tanh`, `asinh`, `acosh`, `atanh`, `sqrt`, `cbrt`, and `hypot(x, y)`.
Trigonometric angles are in radians. `ln(x)` is the natural logarithm, and
`log(x, base)` computes `ln(x) / ln(base)`. After `import std.math;`, call
functions such as `math.sqrt(9.0)` or `math.log(8.0, 2.0)`.
The module also exports immutable `f64` constants: `pi`, `tau` (2π), `half_pi`,
`quarter_pi`, `e`, `sqrt2`, `sqrt3`, `ln2`, and `ln10`. Use them through the
import alias, for example `math.sin(math.half_pi)`.
The `compile` helper links libm automatically.

Imports must precede declarations. Aliases cannot be reused by declarations or
local bindings. Import cycles, private-member access and imported `main`
declarations are errors. Imports are not re-exported, and there are no
wildcard imports, packages or runtime module initialization.

The [module implementation notes](frontend/src/modules/README.md) describe
compiler phase boundaries, tests and the future C++ runtime milestone.

## Global variables

Declare globals with `let` or `const` outside functions. Globals are private
unless marked `export`; access exported globals through the imported module's
alias, just like functions:

```blink
// state.bl
export let count: i32 = 40;
export const limit: i32 = 100;
export fun increment() => void { count += 1; }

// main.bl
import state;
fun main() => i32 {
    state.increment();
    state.count += 1;
    return state.count; // 42
}
```

Every import refers to the same variable, including when several modules import
the same dependency. `let` globals can be reassigned through an import; `const`
globals cannot. Locals and parameters can shadow globals. A lambda can access
globals directly; explicitly capturing a global copies its current value using
the existing capture rules.

Globals support integer, floating-point, boolean and string types. Types may be
inferred from initializers. Initializers must be literals (including negative
numeric literals), integer constant expressions, or references to other constant
globals. Forward references to constants are supported, including exported
constants accessed through an import. Their declared or inferred types still
apply; referring to a constant does not make it an untyped literal. Constant
dependency cycles are errors.

A typed `let` without an initializer uses the usual primitive default: zero,
`false`, or the empty string. `const` requires an initializer. Runtime calls,
reads of mutable globals during initialization, and array, class and function
globals are unsupported. All globals are initialized before `main` and live for
the duration of the program. String literals retain their existing static
storage lifetime and must not be freed.

## Develop with Docker

The development image contains the complete Blink toolchain: OCaml 4.14.2,
opam and the frontend packages, LLVM/Clang 16, CMake, and the native build
dependencies. The repository is mounted into the container, so edits made on
the host are immediately available inside it.

Build the image and open a shell:

```sh
docker compose build
docker compose run --rm dev
```

Then build and test Blink from the container shell:

```sh
make
make test
./compile -O2 examples/simple.bl
./new_output.o
```

After changing source code, run `make` again. Rebuild the image only when the
Dockerfile or `frontend/blink.opam` dependencies change:

```sh
docker compose build
```

The container user defaults to UID/GID 1000 so generated files remain editable
on most Linux hosts. If your IDs differ, pass them when building:

```sh
USER_ID=$(id -u) GROUP_ID=$(id -g) docker compose build
```

## How to build this project

### Dependencies

To build the frontend of the language (parser, typechecker, desugarer) we need a few OCaml libraries. 

Before we install any of them, first 
I recommend creating a custom opam switch to compile our OCaml code using Clang rather than GCC. I have a short Github gist [here](https://gist.github.com/robertfeliciano/5f650d0c9d73707b22e2ec2a1003433b) explaining how to do this. 

Next, run `opam install . --deps-only` in `frontend/` which will install all the necessary dependencies listed in `blink.opam`. I've seen issues where `ounit2` won't be installed with this, but installing it separately should work. 

To build the backend of the langauge (LLVM codegen) we need to install LLVM. This project currently uses LLVM 16.0.0; I have plans to update to a newer version soon. I specifically used LLVM 16.0.0 installed to my machine from the Github repo. 

Here are some steps to install LLVM: 

```
sudo apt install -y cmake ninja-build build-essential python3-dev libz-dev libxml2-dev

git clone --depth 1 --branch llvmorg-16.0.0 https://github.com/llvm/llvm-project.git

cd llvm-project

mkdir build && cd build

cmake -G Ninja -S ../llvm -B . \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS="clang;lld;clang-tools-extra" \
  -DLLVM_TARGETS_TO_BUILD="AArch64;AMDGPU;ARM;NVPTX;X86;XCore;RISCV" \
  -DCMAKE_CXX_STANDARD=17 \
  -DLLVM_ENABLE_ASSERTIONS=OFF \
  -DCMAKE_INSTALL_PREFIX=/usr/local/llvm16

ninja  # might need to specify parallel jobs with -j <cores>

sudo ninja install
```

### Build

Running `make` in the root directory of this project builds the entire compiler. This produces the `blink` executable. Running this on a `.bl` file will produce an LLVM-IR file called `new_output.ll`. The compiler accepts `-O0`, `-O1`, `-O2`, and `-O3`; when omitted, `-O0` is used.

To build the frontend, you can simply run `dune build` in `frontend/`. 

### Tests

Every push and pull request runs the complete test suite in GitHub Actions.
The workflow builds the repository's development container and caches its
BuildKit layers in GitHub's cache service. LLVM 16, OCaml 4.14.2, and OPAM
dependencies are therefore rebuilt only when the Dockerfile or
`frontend/blink.opam` changes.

The CI image uses the same environment as local Docker development, so the
commands exercised in CI are simply:

```sh
make
make test
```

Run all frontend unit tests and compiler end-to-end tests from the repository
root:

```sh
make test
```

For a quicker development loop, the suites can be run separately:

```sh
make test-unit
make test-e2e
make test-backend
```

The end-to-end suite checks parsing, type checking, and desugaring before it
invokes the Blink compiler, lowers the generated LLVM IR with `llc`, links it
with `clang`, and asserts the native program's exit status. Each case uses an
OUnit-managed temporary directory, so generated `.ll` files, object files, and
executables are removed automatically and never written into the repository.
The end-to-end suite therefore requires the complete backend/LLVM toolchain;
run `make` first after a clean checkout.

The backend-only suite skips parsing, type checking, and desugaring. Its helper
constructs `Desugared_ast.program` values directly in OCaml and passes them to
the C++ bridge/code generator, then links and runs the result. This isolates
bridge and LLVM codegen behavior while retaining the same temporary-directory
cleanup and exit-status assertions as the full end-to-end suite.

### How to Use Blink
Take a look at the examples in `examples/`. You can compile a program to an
executable using `./compile -O2 program.bl`, which will generate
`new_output.o`. The optimization flag is optional and may appear before or
after the filename; it defaults to `-O0`. Take a look at the compile script if
you want to customize the final executable.

Functions and methods can request LLVM inlining with the `inline` modifier:

```blink
inline fun add_one(value: i32) => i32 {
  return value + 1;
}
```

The modifier uses LLVM's always-inliner and is honored even when compiling with
`-O0`.

Conditional expressions use `condition ? when_true : when_false`. The
condition must be `bool`, and only the selected branch is evaluated. They are
right-associative and bind less tightly than binary operators, so nested
expressions can be written without extra parentheses:

```blink
fun main() => i32 {
  let score = 87;
  let result = score >= 90 ? 1 : score >= 80 ? 2 : 3;
  return result;
}
```

Both branches must have compatible types. Blink applies its normal numeric
promotion rules, and an assignment or explicit declaration type can provide
the expected type for literals and other context-sensitive expressions.

Function and method calls must supply exactly the declared number of arguments.
Use a lambda with explicit captures to bind arguments for a later call:

```blink
fun add(left: i32, middle: i32, right: i32) => i32 {
  return left + middle + right;
}

fun main() => i32 {
  let ten = 10;
  let add_ten: (i32, i32) -> i32 = fn[ten](middle, right) {
    return add(ten, middle, right);
  };
  let result = add_ten(20, 12);
  free add_ten;
  return result;
}
```

Free lambda closures when they are no longer needed.
