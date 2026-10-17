# Interfaces

| Entry point | Demonstrates | Exit code |
| --- | --- | --- |
| [main.bl](main.bl) | Imported contracts, class-to-interface conversions, and dynamic dispatch | 0 |
| [fields.bl](fields.bl) | Mutable and read-only field requirements, different class layouts, and shared mutations | 0 |
| [generic.bl](generic.bl) | Generic contracts, class implementation inference, and shared field mutations | 42 |
| [temp.bl](temp.bl) | A minimal interface method call and freeing an object through its interface reference | 42 |

[contracts.bl](contracts.bl) exports the `Value` interface and the `read`
function used by `main.bl`; it is a dependency, not an entry point.

Run from the repository root:

```sh
./compile -O0 examples/interfaces/main.bl
./new_output.o
echo $?

./compile -O0 examples/interfaces/fields.bl
./new_output.o
echo $?
```

Both programs exit with `0` when their checks pass and `1` when they fail.
Neither needs the standard library root.

In `main.bl`, `First` and `Second` explicitly implement `contracts.Value` with
`impl`. `choose` returns either object as that interface, and `contracts.read`
calls the selected object's implementation. The two calls produce `10` and `20`.

In `fields.bl`, `Counter` requires a mutable `count`, a read-only `label`, and a
`value` method. `increment` changes `count` through the interface. The concrete
objects observe those same changes, ending at `11` and `21`. A `const`
requirement accepts either a mutable or constant class field, but prevents
assignment through the interface.

Required fields and method signatures must match exactly. Classes can add other
members and arrange fields differently. Converting to an interface keeps a
reference to the original object; free that object once after all aliases are
finished using it.

## Generic interfaces

Declare type parameters with `interface Value<T>` and use explicit arguments in
interface types and implementations: `Value<i32>` and
`class Box<T> impl Value<T>`. Fields and method signatures substitute the
interface arguments before checking conformance. Type arguments are invariant.
Generic methods and interface inheritance are not supported.

[generic.bl](generic.bl) demonstrates these rules. `let box = new Box<i32> { ... }`
infers `Box<i32>`. Calling `read(box)` infers the function's parameter from
`Box<i32> impl Value<i32>` and converts the argument to the required interface.
An annotation such as `let view: Value<i32> = box` gives a variable an explicit
interface type. Both references point to the same object.

Inference follows the implementation's declared arguments, including reordered
parameters and nested types. Conflicting deductions are errors. A class may
implement several different interfaces, but only one specialization of each
interface template; `impl Value<i32>, Value<bool>` is rejected. Existing exact
function-signature rules also apply to generic function values and callbacks.

Run `./compile -O2 examples/interfaces/generic.bl`, then `./new_output.o`.
The expected exit code is `42`.
