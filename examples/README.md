# Blink examples

| File | Topic | Exit code |
| --- | --- | --- |
| [basics.bl](basics.bl) | Types, inference, casts, operators, conditionals, and ternaries | 42 |
| [functions.bl](functions.bl) | Parameters, nested calls, and recursion | 42 |
| [loops.bl](loops.bl) | While, ranges, steps, foreach, nesting, break, and continue | 42 |
| [arrays.bl](arrays.bl) | Empty arrays, indexing, mutation, matrices, and nested arrays | 19 |
| [annotations.bl](annotations.bl) | C interop and inline functions and methods | 20 |
| [collections.bl](collections.bl) | Named imports, standard-library generic lists, circular linked storage, and interface inference | 42 |
| [classes/main.bl](classes/main.bl) | Fields, defaults, mutation, and method calls | 52 |
| [classes/arrays.bl](classes/arrays.bl) | Array fields and arrays of objects | 7 |
| [classes/nested.bl](classes/nested.bl) | Nested objects and default field initializers | 44 |

- [cu/](cu/README.md): CUDA array addition and cuBLAS GEMM with heap arrays;
  requires the CUDA toolkit, cuBLAS, and an NVIDIA GPU.
- [generics/](generics/README.md): generic functions, classes, interfaces, and
  interface constraints.
- [lambdas/](lambdas/README.md): basic calls, unnamed arguments, captures,
  returned closures, function values, and left folds.
- [modules/](modules/README.md): named and qualified imports, exports, shared globals, and `std.io`.
- [interfaces/](interfaces/README.md): interface contracts and dynamic dispatch;
  [fields.bl](interfaces/fields.bl) demonstrates interface field access and
  [generic.bl](interfaces/generic.bl) demonstrates generic contracts and inference (exit `42`).

Run from the repository root. Programs return their result as the process exit code:

```sh
./compile -O0 examples/loops.bl
./new_output.o
echo $?
```

The module example also needs the standard library root:

```sh
./compile -O0 -stdlib-root runtime/stdlib examples/modules/main.bl
```

The collections example uses the standard-library root as well:

```sh
./compile -O2 -stdlib-root runtime/stdlib examples/collections.bl
./new_output.o
echo $?
```

See the [collections API](../runtime/stdlib/collections/README.md) for indexed
operations and ownership rules.
