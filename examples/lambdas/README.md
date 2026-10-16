# Lambdas and function values

Each file is a standalone program focused on one concept.

| Example | Demonstrates | Exit code |
| --- | --- | --- |
| [basic.bl](basic.bl) | Store and call a lambda with an explicit signature | 42 |
| [passing_unnamed.bl](passing_unnamed.bl) | Pass an unnamed lambda directly; infer its signature | 42 |
| [capturing_locals.bl](capturing_locals.bl) | Capture local variables explicitly with `fn[scale, offset]` | 42 |
| [returning_lambdas.bl](returning_lambdas.bl) | Return a closure that retains a captured parameter | 42 |
| [function_values.bl](function_values.bl) | Pass a named function through the same callback type | 42 |
| [fold_left.bl](fold_left.bl) | Reduce an array with accumulator-first callbacks | 10 |

From the repository root:

```sh
./compile -O0 examples/lambdas/passing_unnamed.bl
./new_output.o
echo $?
```

Function types use `(i32, i32) -> i32`. Lambdas can declare their parameter and
return types explicitly, or infer them from an expected function type. Captures
are listed in brackets after `fn`.

These examples free their closures after use. In `passing_unnamed.bl`,
`apply_once` owns and frees the callback. The other callback helpers borrow
their callbacks, leaving cleanup to the caller.
