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
cd frontend && dune runtest && cd ..
./compile examples/simple.bl
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

Running `make` in the root directory of this project builds the entire compiler. This produces the `blink` executable. Running this on a `.bl` file will produce an LLVM-IR file called `new_output.ll`.

To build the frontend, you can simply run `dune build` in `frontend/`. 

### How to Use Blink
Take a look at the examples in `examples/`. You can compile a program to an executable using `./compile` which will generate `new_output.o`. Take a look at the compile script if you want to customize the final executable.