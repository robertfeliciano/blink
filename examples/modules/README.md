# Modules

This folder contains one multi-file program. Compile [main.bl](main.bl) as the
entry point; Blink loads its imports automatically.

| File | Demonstrates |
| --- | --- |
| [main.bl](main.bl) | Qualified imports, an import alias, and standard library calls |
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
`helper.answer()` supplies the box's initial value. `state.advance()` changes
`state.count` from `40` to `42`, and the entry point checks that it matches the
box's value.

`import helper;` exposes exported declarations as `helper.answer`.
`import geometry as shapes;` uses `shapes.Box` instead of `geometry.Box`.
Declarations without `export`, such as `helper.bl`'s `add`, stay private to
their defining module. Imported globals refer to the defining module's storage,
so changes made by `state.advance()` are visible through `state.count`.

The project import root defaults to the entry file's directory. Use
`-module-root DIR` to override it. `import std.io;` resolves `io.bl` under the
explicit `-stdlib-root` directory; this example uses the bundled
[standard library](../../runtime/stdlib/io.bl).
