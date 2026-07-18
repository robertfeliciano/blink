# Blink
and you'll miss it...

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
