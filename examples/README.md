# Blink examples

| File | Topic | Exit code |
| --- | --- | --- |
| [basics.bl](basics.bl) | Types, inference, casts, operators, conditionals, and ternaries | 42 |
| [functions.bl](functions.bl) | Parameters, nested calls, and recursion | 42 |
| [type_aliases.bl](type_aliases.bl) | Transparent aliases for scalars, callbacks, arrays, and generic classes | 42 |
| [tuples.bl](tuples.bl) | Pairs, triples, tuple types, nested destructuring, copying, and iteration | 42 |
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

Tuple annotations follow the complete pattern: `let (value, position):
(string, i32) = parseToken(text);`. Omit the annotation to infer both types.
Each tuple contains two or three elements; patterns can nest and `_` discards
an element. `const` makes every named binding constant. Tuple values copy their
elements, with references retaining their normal ownership rules. Read elements
with constant indices such as `pair[0]`; reassign a mutable tuple as a whole.
Function types keep their existing parameter-list syntax: `(i32, i32) -> i32`
takes two arguments, while `((i32, i32)) -> i32` takes one tuple argument.
