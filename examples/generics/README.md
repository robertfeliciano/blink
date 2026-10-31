# Generics

| Program | Demonstrates | Exit code |
| --- | --- | --- |
| [generic_functions.bl](generic_functions.bl) | Inferred function arguments, arrays, and callbacks | 42 |
| [generic_classes.bl](generic_classes.bl) | Explicit class arguments, nesting, and function inference | 42 |
| [constraints.bl](constraints.bl) | A generic function that requires a `Readable` implementation | 42 |
| [../interfaces/generic.bl](../interfaces/generic.bl) | Generic interface contracts, conversions, and shared field mutations | 42 |

Run from the repository root:

```sh
./compile -O2 examples/generics/constraints.bl
./new_output.o
echo $?
```

## Functions

Declare parameters with `fun identity<T>(value: T) => T`. Calls infer type
arguments from their arguments and expected result types; there is no explicit
call-site syntax such as `identity<i32>(42)`. Expected function signatures also
specialize function values, for example `let f: (i32) -> i32 = identity`.
Parameters can occur in array, function, class, and interface types. Repeated
occurrences must infer the same type. Every function parameter must appear in
its argument or return types.

The compiler generates and caches a concrete function for each distinct set of
type arguments. Recursive calls reuse that specialization. Dependent bodies are
checked when instantiated. Generic prototypes need matching definitions; their
parameter names may differ. Generic methods, generic `main`, and generic `@C`
declarations are unsupported.

## Classes and interfaces

Use explicit arguments in nominal types and object construction:

```blink
interface Value<T> { fun get() => T; }
class Box<T> impl Value<T> {
    let item: T;
    fun get() => T { return item; }
}

fun read<T>(value: Value<T>) => T { return value.get(); }
```

`new Box<i32> { item = 42 }` creates a `Box<i32>`. Calling `read` with that object
infers `T = i32` from its declared `Value<i32>` implementation and converts the
argument to the interface. An interface value keeps the original object's
identity and uses a shared method table; conversion does not allocate a wrapper.

Arguments are invariant: `Value<i32>` and `Value<i64>` are different types.
A class may implement at most one specialization of a given interface template.
Inference follows the declared implementation arguments, including reordered
parameters and nested types. Function values and callbacks require exact
function signatures. See [interface examples](../interfaces/README.md) for the
field, method, mutation, and ownership rules.

## Interface constraints

Start with [constraints.bl](constraints.bl): `Number` implements `Readable`,
so it can be passed to `readValue<T impl Readable>`. Passing an `i32` instead
would fail the constraint check. The program reads `42` and returns it.

Add one optional interface bound to each type parameter:

```blink
fun identity<T impl Value<i32>>(value: T) => T {
    return value;
}

fun read<T impl Value<U>, U>(value: T, fallback: U) => U {
    return value.get();
}

class Holder<T impl Value<i32>> { let value: T; }
interface HolderView<T impl Value<i32>> { const value: T; }
```

A bound must name an interface, optionally with type arguments. Imported bounds
use normal qualified names such as `contracts.Value<i32>` and obey export rules.
Bounds may refer to other parameters in the same declaration, including later
parameters, or to the bounded parameter itself.

A class argument must explicitly `impl` the required interface with exactly the
required type arguments. Merely having matching members does not qualify.
An interface argument satisfies a bound on that exact interface; interface
inheritance is unsupported. Primitives, arrays, strings, and function types do
not satisfy interface bounds. Invalid bounds are rejected even on unused
declarations.

Function inference runs normally, then validates the bound before specialization.
Bounds do not infer additional arguments: `U` in the example above is inferred
from the signature, not from `T`'s implementation. Class and interface arguments
are checked when their concrete types are used. Prototypes must have the same
bounds as their definitions, allowing renamed parameters.

A constraint preserves the actual type of `T`. Passing a `Box<i32>` to the
bounded `identity` returns a `Box<i32>`; passing a `Value<i32>` returns that
interface type. Concrete specializations use ordinary class member access;
interface specializations use existing field offsets and method tables. Bounds
do not add runtime checks or restrict the specialized body to interface members.
As with unconstrained generics, dependent bodies are checked per specialization.

Compilation limits are 4096 specializations and a requesting chain of 128 for
generic functions and nominal types.
