# Interfaces

| Entry point | Demonstrates | Exit code |
| --- | --- | --- |
| [main.bl](main.bl) | Named contract imports, class-to-interface conversions, and dynamic dispatch | 0 |
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

In `main.bl`, `import Value, read from contracts;` selects the exported interface
and function. `First` and `Second` explicitly implement `Value` with
`impl`. `choose` returns either object as that interface, and `read`
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

[generic.bl](generic.bl) demonstrates generic contracts, class implementation
inference, and shared field mutations. See the [generics guide](../generics/README.md)
for syntax, inference, specialization, and interface constraints.

Run `./compile -O2 examples/interfaces/generic.bl`, then `./new_output.o`.
The expected exit code is `42`.
