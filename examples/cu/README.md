# CUDA array addition

This example adds two four-element `f64` arrays on an NVIDIA GPU and checks
that the result is `[11, 22, 33, 44]`. It requires the CUDA toolkit (`nvcc`), a
CUDA-capable NVIDIA GPU with a working driver, and Blink's usual LLVM toolchain.

From the repository root:

```sh
make
make -C examples/cu run
# Also check optimized Blink code:
make -C examples/cu run OPT=-O2
```

Override `NVCC` or `LLC` if those tools are not on PATH.
Generated files stay in `examples/cu/build/`.

`runtime/stdlib/cu/vector_add.cu` exports `blink_cu_add_f64_4` with C linkage.
`runtime/stdlib/cu/vector.bl` exports the matching `@C` declaration, imported as
`std.cu.vector`. Blink's fixed-size arrays pass pointers to contiguous elements;
this API accepts exactly four doubles per array. The CUDA wrapper copies host
inputs to device memory, launches the addition kernel, copies the result back,
and releases device memory. It returns zero on success or a CUDA error code.
The example reports errors and exits nonzero if CUDA fails or any sum is wrong.

The example Makefile links with `nvcc` to include the CUDA runtime. The ordinary
`compile` helper only links standard host libraries, so use this target for the
CUDA example. This is an optional module; ordinary Blink builds do not need CUDA.
