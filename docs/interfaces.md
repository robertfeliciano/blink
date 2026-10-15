# Interfaces

An interface names a contract consisting of field requirements and method
prototypes:

```blink
export interface Reading {
    fun read() => i32;
    fun update(value: i32);
}
```

Classes opt into a contract using `impl`:

```blink
class Sensor impl Reading {
    let value: i32 = 10;
    fun read() => i32 { return this.value; }
    fun update(next: i32) { this.value = next; }
}
```

Every interface prototype must have a matching method definition in the class.
Names, parameter counts, ordered parameter types, and return types must match
exactly. Parameter names do not participate in matching. Extra class methods are
allowed. Implementing several interfaces uses `impl First, Second`.

Interface declarations and implementation names may appear before or after the
classes that use them. Interfaces cannot be instantiated, and their bodies
contain field requirements and method prototypes, without initializers or method
implementations. Interface inheritance and default method implementations are
not part of this feature.

## Fields

Declare required fields before method prototypes, with explicit types:

```blink
class Owner { let id: i32 = 0; }

interface Metric {
    const name: string;
    let count: i32;
    let scale: f64;
    let owner: Owner;
}

class Sample impl Metric {
    const name: string = "sample";
    let count: i32 = 0;
    let scale: f64 = 1.0;
    let owner: Owner = new Owner {};
}

fun bump(metric: Metric) => void {
    metric.count += 1;
    metric.scale = 2.0;
    metric.owner.id = 42;
}
```

Each required field must exist on the implementing class with exactly the same
type. The class can infer its field's type from an initializer. Integers, floats,
booleans, strings, class references, interface references, and arrays use their
existing class-field representations. Interface field declarations have no
initializers and do not add storage to the implementing class.
Function-valued requirements are unsupported; declare an interface method
instead.

A `let` requirement needs a mutable class field. A `const` requirement permits
either a mutable or constant class field, but prevents assignment through the
interface view. Constness is shallow, as with class fields: a read-only reference
can still refer to a mutable object or array. Reading or writing through an
interface accesses the original concrete object's field; aliases observe the
same changes.

Classes can place required fields in any order and can have additional fields.
Field names and method names must be distinct within an interface. Field and
method slots are numbered independently, each in its own declaration order.

The [field example](../examples/interfaces/fields.bl) demonstrates two concrete
layouts, a read-only field, and shared mutations through interface aliases:

```sh
./compile -O2 examples/interfaces/fields.bl
./new_output.o
```

## Modules

`export interface Reading { ... }` makes the interface accessible to importing
modules. With `import contracts;`, use `contracts.Reading` in type annotations
and `class Sensor impl contracts.Reading` in implementation declarations.
An interface without `export` remains private to its defining module.

Module resolution assigns stable identities to interface names, just as it does
for class names. Interface method names remain member names, and interface
prototypes do not declare top-level linker symbols.

## Runtime behavior

A class explicitly implementing an interface can be converted to that interface
in a typed initializer, assignment, argument, or return value. Interface method
calls select the concrete class implementation at runtime. The interface exposes
its declared fields and methods; other concrete class members remain accessible
through the concrete type.

Interface elements can be initialized individually in an interface-typed array.
Casting an existing array of concrete references to an array of interface values
is rejected: their element representations differ. Interface-to-class downcasts
are also rejected.

Function values retain their exact parameter and return types. A function
returning a concrete class cannot be cast to one returning an interface;
write a wrapper with an interface return type to perform that conversion.

Interface calls require the full declared argument list, like other Blink calls.
Use an explicit lambda when a callback should invoke an interface method.

An interface value contains two pointers: the existing class object and a shared
method table for that class/interface pair. Conversions do not copy the object
or allocate a wrapper. Copying an interface value keeps references to the same
object and table. The shared table contains method pointers in interface
declaration order and a separate array of field byte offsets. Offsets are
computed from the concrete class's LLVM layout, so field access works even when
different implementations place the same required field at different positions.

Interfaces follow Blink's explicit object ownership rules. An interface view
does not extend an object's lifetime. Free the object once, through its concrete
reference or an interface reference, after all uses. Freeing both aliases would
free the same object twice. The shared method table is not freed with the object.

Default interface values and explicit `null` represent a null object. Calling a
method or accessing a field requires a live, non-null object. Equality compares
the underlying object references.

The desugared AST carries interface field and method signatures,
class-to-interface conversions, interface field projections, and interface method
calls across the OCaml/C++ bridge. Each class declaration
owns an `impls` list containing its interface names and ordered method symbols;
classes without interfaces have an empty list. The backend keeps interface values
as LLVM aggregates and emits shared constant tables plus indirect calls with the
concrete receiver as the first argument. Field projections calculate an address
using the shared offset table and use ordinary LLVM loads and stores.
