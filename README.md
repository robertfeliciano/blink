# Blink
and you'll miss it...

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