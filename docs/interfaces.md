# Interfaces

An interface names a contract consisting of method prototypes:

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
contain prototypes rather than fields or method implementations. Interface
inheritance and default method implementations are not part of this feature.

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
its declared methods, rather than the concrete class's fields or extra methods.

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
declaration order.

Interfaces follow Blink's explicit object ownership rules. An interface view
does not extend an object's lifetime. Free the object once, through its concrete
reference or an interface reference, after all uses. Freeing both aliases would
free the same object twice. The shared method table is not freed with the object.

Default interface values and explicit `null` represent a null object. Calling a
method requires a live, non-null object. Equality compares the underlying object
references.

The desugared AST carries interface signatures, class implementation tables,
class-to-interface conversions, and interface method calls across the OCaml/C++
bridge. The backend keeps interface values as LLVM aggregates and emits shared
constant tables plus indirect calls with the concrete receiver as the first
argument.
