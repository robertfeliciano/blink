# Blink

#### ...and you'll miss it

Blink is a small, statically typed language that compiles to native code through
LLVM. It combines type inference, generic functions, explicit lambda captures,
and classes with interfaces. Each source file is a module, and `@C` declarations
let you call C functions directly. Memory management is explicit with `free`.

```blink
import std.io;

@C fun abs(value: i32) => i32;

interface Reading {
    fun read() => i32;
}

class Sensor impl Reading {
    let value: i32 = 0;

    fun read() => i32 { return this.value; }
}

fun measure(source: Reading, transform: (i32) -> i32) => i32 {
    return transform(abs(source.read()));
}

fun main() => i32 {
    let offset = 2;
    let transform = fn[offset](value: i32) -> i32 {
        return value * 10 + offset;
    };
    let sensor = new Sensor { value = -4 };
    let result = measure(sensor, transform);
    io.println("Blink!");
    free sensor, transform;
    return result;
}
```

Save this as `main.bl`, then run from the repository root:

```sh
./compile -O2 -stdlib-root runtime/stdlib main.bl
./new_output.o
echo $?
```

It prints `Blink!` and exits with `42`. The `compile` helper compiles and links
an executable named `new_output.o` in the current directory.

Browse the [examples](examples/README.md) for more:
[modules](examples/modules/README.md), [lambdas](examples/lambdas/README.md),
[interfaces](examples/interfaces/README.md), and
[generic functions](examples/generic_functions.bl).

For a container-based toolchain, run `docker compose build` followed by
`docker compose run --rm dev`. Run `make` and `make test` inside the container.

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
