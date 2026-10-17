# CUDA examples

The CUDA work in this repo is a proof-of-concept.
Not that we need much of a proof-of-concept for this method since we are simply exposing some CUDA entry-point with `extern "C"` and wrapping it in a Blink function, just like we do with standard C code. 

But the end goal of GPU functionality will take much longer to achieve. So I'm leaving this here for the sweet 0.0001% CUDA code on the language breakdown.

The vector example adds two four-element `f64` arrays on an NVIDIA GPU and checks
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

## cuBLAS GEMM with heap arrays

`gemm.bl` passes two flat heap arrays and a heap output array to cuBLAS. Import
`std.cu.blas` for `blink_cu_sgemm_2x3_3x4` (`f32`) or
`blink_cu_dgemm_2x3_3x4` (`f64`). Both compute `C = alpha*A*B + beta*C`:

- A: row-major 2-by-3 matrix, `[f32; 6]` or `[f64; 6]`.
- B: row-major 3-by-4 matrix, 12 elements.
- C: row-major 2-by-4 matrix, 8 elements, supplied by the caller.

These shapes are fixed in the Blink API. Use `new [...]` to allocate the buffers
on the heap and `free a, b, c;` after use. The wrapper retains no host pointers
and finishes before returning. Inputs may share storage with the output.
Return codes are zero on success, positive CUDA runtime errors, or negative
cuBLAS errors. A failing operation's output should be discarded.

The native implementation calls NVIDIA's `cublasSgemm` / `cublasDgemm` directly.
It swaps the operands and dimensions to support row-major data without transpose
kernels. There is no custom multiplication kernel. `beta = 0` skips uploading C.
Each call creates a cuBLAS handle, allocates device memory, uploads inputs, runs
GEMM, downloads C, and cleans up. Those costs dominate tiny matrices; this example
verifies interop and does not promise a speedup for this small shape. Repeated
large multiplies would benefit from a future API retaining device buffers and a
handle across calls. `f32` is usually the appropriate throughput-oriented choice
on Jetson hardware; the wrapper does not enable reduced-precision math modes.

The build requires cuBLAS headers and libraries and links `-lcublas`:

```sh
make -C examples/cu run            # vector addition and heap-array SGEMM/DGEMM
make -C examples/cu run OPT=-O2
make -C examples/cu test           # SGEMM/DGEMM, scaling, aliasing, null errors
```

GPU execution needs access to NVIDIA device nodes. If compilation succeeds but
execution fails inside a sandbox, run the generated binaries with GPU access.
