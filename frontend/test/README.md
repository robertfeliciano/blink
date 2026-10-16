# Test coverage

Keep tests that assert different contracts, even when they exercise the same
language feature. Parser and lowering tests inspect AST structure, typing tests
check acceptance and diagnostics, backend fixtures isolate the native bridge,
and native tests check the complete pipeline and runtime behavior. O0 and O2
runs cover different optimization paths.

Avoid repeating work when the assertions are already covered:

| Coverage | Where it belongs |
| --- | --- |
| Full 72-case underapplication matrix | `test_typing.ml`, using `call_arity_fixtures.ml` |
| CLI arity diagnostics, exit status, and no IR on failure | Two representative expression/statement cases in `e2e.ml` |
| Imported calls and source filenames in errors | `module_e2e.ml` |
| Inline modifier parsing | `test_parsing.ml` checks the modifier in the AST |
| Branch/loop, for-loop, and foreach pipeline smoke coverage | Native fixtures in `e2e.ml`; lowering shape assertions remain in `test_desugaring.ml` |

Native fixtures invoke the compiler once per optimization level. The compiler
already parses, resolves, types, and desugars the program; running those phases
again in the test process adds no assertion. Compiler failures include captured
stdout and stderr.

`Native_test_support.compile_and_run` uses `llc` to verify the input LLVM module
and generate an object before linking and executing it. A preceding
`llc --filetype=null` repeats verification and lowering. Tests with custom native
link/run steps likewise rely on their object-generation invocation.
