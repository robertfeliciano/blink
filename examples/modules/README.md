# Modules

This folder contains one multi-file program. Compile [main.bl](main.bl) as the
entry point; Blink loads its imports automatically.

| File | Demonstrates |
| --- | --- |
| [main.bl](main.bl) | Named imports, qualified imports, an import alias, and standard library calls |
| [helper.bl](helper.bl) | An exported function that calls a private helper |
| [geometry.bl](geometry.bl) | An exported class with a field and a method |
| [state.bl](state.bl) | Exported mutable and constant globals, plus shared state |

Run from the repository root:

```sh
./compile -O0 -stdlib-root runtime/stdlib examples/modules/main.bl
./new_output.o
echo $?
```

The program prints `Hello from Blink modules!` and exits with `42`.
`answer()` supplies the box's initial value. `advance()` changes `count` from
`40` to `42`, and the entry point checks that it matches the box's value and
the qualified `state.count` reference.

Named imports bring selected exported declarations into the current module:

```blink
import answer from helper;
import count, advance, from state;
import println from std.io;
```

The trailing comma is optional. Use `answer()`, `count`, and `advance()` directly.
Named imports also support classes, interfaces, and generic declarations; see
[collections.bl](../collections.bl) for `List<T>` and `LinkedList<T>` imports.
They do not bind a module alias or re-export the imported declarations.
Missing or private members are errors even if the import is unused.
Duplicate imported names and conflicts with top-level declarations or module
aliases are errors. Local bindings may shadow named imports.

`import helper;` exposes exported declarations as `helper.answer`.
`import geometry as shapes;` uses `shapes.Box` instead of `geometry.Box`.
Declarations without `export`, such as `helper.bl`'s `add`, stay private to
their defining module. Imported globals refer to the defining module's storage,
so changes made by `advance()` are visible through both `count` and `state.count`.
`from` is a reserved keyword.

The project import root defaults to the entry file's directory. Use
`-module-root DIR` to override it. `import println from std.io;` resolves `io.bl`
under the explicit `-stdlib-root` directory; this example uses the bundled
[standard library](../../runtime/stdlib/io.bl).
