# Frontend Typechecker Improvement Review

## Scope

This review covers improvements to the compiler frontend typechecker without
adding language features. It focuses on correctness fixes, clearer control
flow, safer error handling, maintainability, test coverage, and performance.

The existing frontend test suite passes, but several malformed programs are
either accepted by the typechecker or escape as uncaught OCaml exceptions.

## Highest-priority fixes

### 1. Enforce assignment compatibility explicitly

> **Status:** Implemented on `cdx/typechecker-enhancements`. Assignment RHS
> expressions are now contextually converted, the resolved RHS and LHS types
> are compared explicitly, and incompatible assignments produce `TypeError`.

In [`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L85),
the RHS type is bound as `_rhsty` and discarded. The code assumes that
`type_exp ~expected:lhsty` always enforces the expected type, but several
expression branches do not compare their resulting type with `expected`.

As a result, the following invalid assignment is accepted and produces an
inconsistent typed AST:

```blink
let x: i32 = 0;
x = 1 < 2;
```

After typing both sides, assignment checking should explicitly compare the RHS
type with the LHS type through a centralized assignability function. This
should also be the single place that handles any permitted literal coercions.

### 2. Do not treat ordinary loop bodies as guaranteed returns

> **Status:** Implemented on `cdx/typechecker-enhancements`. `while`, range
> `for`, and `foreach` statements now conservatively fall through even when
> every path in their bodies returns.

The `While`, `For`, and `ForEach` cases in
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L147)
propagate the body's `does_ret` value as if the body is guaranteed to execute.
For example, the checker accepts this as a complete non-void function body:

```blink
while (false) {
  return 1;
}
```

Ordinary loops should conservatively report that execution may fall through.
Only a loop proven to be non-terminating and to have no reachable `break` could
be treated as guaranteeing a return.

### 3. Check non-void lambdas for missing returns

> **Status:** Implemented on `cdx/typechecker-enhancements`. Lambda body flow is
> now checked through the same shared return-completeness helper as named
> functions, while void bodies remain exempt.

[`create_typed_lambda`](frontend/src/typing/type_stmt.ml#L668) discards the
`does_ret` result returned by `type_block`. A lambda declared as returning a
value can therefore reach the end of its body without returning anything,
which later produces malformed backend output.

Functions and lambdas should use the same return-completeness validation.

### 4. Convert expected type failures into structured type errors

> **Status:** Implemented on `cdx/typechecker-enhancements`. Function and
> lambda argument lists now use shared, total exact-pairing helpers; invalid
> constant division, modulo, exponent, and shift operations produce
> `TypeError`; optional context lookups replace exception-based lookups; and
> the public typechecking boundary converts any remaining exception into a
> structured compiler error instead of allowing an OCaml exception to escape.

Some invalid programs escape the frontend through standard-library exceptions:

- [`type_fn`](frontend/src/typing/type.ml#L44) uses `List.nth` to report a
  missing return. An empty non-void body therefore raises
  `Invalid_argument "List.nth"` instead of `TypeError`.
- The void-returning branch of
  [`type_func_app`](frontend/src/typing/type_stmt.ml#L530) does not catch the
  `List.map2` exception raised by a wrong argument count, although the
  value-returning branch does.

Argument counts should be checked before traversal, and missing-return errors
should use the function node when no final statement exists. Expected user
errors should never depend on catching `List.nth` or `List.map2` exceptions.

### 5. Preserve the actual type of a default loop step

> **Status:** Implemented on `cdx/typechecker-enhancements`. The synthesized
> step expression and the type recorded in the typed `For` node now both use
> the loop-bound type.

When a `for` loop omits its step,
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L175)
creates the correct step expression using `default_step start_ty`, but records
the step type as `i32` unconditionally.

For a floating-point loop, the typed AST consequently contains an `f64` step
expression marked as `i32`. This reaches the backend as mismatched operands and
can trigger an LLVM assertion. The recorded step type should be `start_ty`.

### 6. Validate every declared source type before conversion

> **Status:** Implemented on `cdx/typechecker-enhancements`. Locals, fields,
> function/prototype/method signatures, casts, and typed lambdas now use shared
> validate-and-convert helpers. Class names are predeclared so forward and
> self-references remain valid, while unsupported generics and unrepresentable
> array lengths produce normal type errors.

Local declarations convert annotations in
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L38)
without first calling `typecheck_ty`. This allows an undefined class type to
pass through the frontend:

```blink
let x: Missing = null;
```

The backend later fails because `Missing` is unknown. Cast targets and some
class-field paths have similarly inconsistent validation.

Every source type should pass through validation before conversion. A helper
such as `validate_and_convert_ty` would make it harder for individual call
sites to omit validation.

### 7. Correct prototype-definition reconciliation

> **Status:** Implemented on `cdx/typechecker-enhancements`. Prototype entries
> are unique by name; matching definitions update the existing entry, signature
> mismatches are rejected, and undefined checking no longer deduplicates
> shadowing entries after the fact. Resolved source prototypes are consumed by
> the frontend rather than also being emitted as external backend declarations.

[`check_undefined_protos`](frontend/src/typing/type.ml#L216) deduplicates
prototype entries in an order that retains the earlier `defined = false`
entry instead of the later function definition. A matching prototype followed
by its implementation is therefore still reported as undefined.

The function symbol table should have one entry per name. Adding a definition
should update that entry after verifying that its signature matches the
prototype.

## Additional correctness improvements

### Compare void function types correctly

> **Status:** Implemented as part of prototype reconciliation. Function-type
> equality now shares one parameter-and-return comparison path for both void
> and value-returning signatures.

[`equal_ref_ty`](frontend/src/typing/type_util.ml#L195) returns `false` for all
pairs of `RFun (_, RetVoid)`, even when their parameter lists are identical.
Void function types should compare their parameter lists just like
value-returning function types.

### Use expected parameter types while typing arguments

[`type_func_app`](frontend/src/typing/type_stmt.ml#L505) types each argument
without passing `~expected:aty`. A literal such as `1` therefore defaults to
`i32` and is rejected when passed to a `u8` parameter, despite fitting in
`u8`.

Passing the expected parameter type into `type_exp` would make call behavior
consistent with declarations and returns while retaining range checks.

### Do not discard typed-lambda argument comparison results

The typed-lambda path in
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L440)
calls `List.for_all2`, but binds and discards the resulting boolean. It reports
an error for different list lengths, but not directly for unequal argument
types.

The boolean result should be checked, with an error attached to the mismatched
argument when possible.

### Return `u128` when inferring an unsigned 128-bit literal

[`infer_integer_ty`](frontend/src/typing/type_util.ml#L247) recognizes values
in the unsigned 128-bit range but returns `TUnsigned Tu64`. It should return
`TUnsigned Tu128`.

### Validate compound-assignment operators

> **Status:** Implemented on `cdx/typechecker-enhancements`. Compound
> assignments now validate numeric, integer-only, and floating-point-only
> operator requirements before constructing the typed assignment.

The assignment case in
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L74)
does not apply the corresponding binary operator's constraints. For example,
`bool += bool` is accepted and later desugared into boolean addition.

Arithmetic, bitwise, and shift assignments should reuse the validation rules
for their corresponding binary operators.

### Check mutability for projected lvalues

Constness is checked only when the assignment LHS is an `Id`. A `Proj` or
`Index` is accepted unconditionally at
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L83), so
explicit projection can bypass const-field protection.

Lvalue typing should return both the value type and whether the target is
writable.

### Make constant folding total and type-aware

> **Status:** Implemented on `cdx/typechecker-enhancements`. Invalid constant
> arithmetic produces `TypeError`, and a folded integer expression now adopts
> its contextual integer type when the final value fits that type.

[`eval_const_exp`](frontend/src/typing/type_util.ml#L298) may expose arithmetic
exceptions for division by zero, oversized exponent or shift conversions, and
related invalid constant expressions. Constant-folded binary expressions also
do not consistently honor their expected result type.

Constant evaluation should return a result type such as
`(Z.t option, diagnostic) result`, explicitly guard invalid operations, and
retag the resulting literal with the resolved expected type.

### Preserve source order in `free`

> **Status:** Implemented on `cdx/typechecker-enhancements`. Free expressions
> are typed with an order-preserving `List.map` traversal.

The `Free` case in
[`frontend/src/typing/type_stmt.ml`](frontend/src/typing/type_stmt.ml#L214) uses
`List.fold_left` with `te :: acc` and does not reverse the result. This reverses
the order of expressions in the typed AST and may change evaluation or freeing
order.

Use `List.map`, `List.rev_map`, or reverse the accumulator before constructing
the typed statement.

## Readability and maintainability

### Use a richer control-flow result

The current `bool` return from `type_stmt` and `type_block` cannot clearly
represent fallthrough, return, break, and continue. Replacing it with a small
variant would make return analysis clearer:

```ocaml
type flow =
  | Falls_through
  | Returns
  | Breaks
  | Continues
```

Branch and loop composition could then be expressed explicitly instead of
overloading a boolean whose meaning changes by context.

### Centralize compatibility and coercion rules

Declarations, returns, calls, arrays, assignments, and casts currently perform
overlapping but different combinations of:

- expected-type propagation;
- exact equality checking;
- numeric literal range checking;
- integer-to-float handling;
- subtype checking.

A single compatibility function that returns the converted typed expression
or a diagnostic would reduce duplication and prevent inconsistent behavior.

### Replace boolean tuple fields with named records

The context uses tuples such as `(ty * bool)` for both variable constness and
prototype definition state. These booleans have unrelated meanings and are
easy to confuse.

Named records would make intent clearer, for example:

```ocaml
type binding = {
  ty : ty;
  is_const : bool;
}

type prototype = {
  ty : ty;
  is_defined : bool;
}
```

### Replace vague failures and unreachable-state messages

Messages such as `"bleh"`, `"Somehow reached unreachable state"`, and
`"impossible state"` appear in type conversion and checking paths. These can
turn source-level errors into internal exceptions with little context.

Unsupported source types should produce a normal `TypeError` at the original
node. Truly impossible internal states should carry enough information to
identify the violated invariant.

## Performance improvements

[`Tctxt`](frontend/src/typing/tctxt.ml#L17) stores locals, globals, classes,
prototypes, fields, and methods in association lists. Most lookups are linear,
and some operations perform nested scans, such as matching object initializer
fields against class fields.

Using `Map.Make(String)` for globals, classes, prototypes, class fields, and
methods would:

- make duplicate detection explicit;
- make updates independent of list ordering;
- simplify prototype state reconciliation;
- remove most `try ... with Not_found` lookup wrappers;
- improve repeated lookup from linear to logarithmic time.

Locals may remain a stack of scopes or use a scope-aware map if shadowing and
block lifetime need to be represented explicitly.

## Recommended regression tests

Add tests covering at least:

1. assignment of a boolean expression to an integer variable;
2. missing return after `while`, `for`, and `foreach` bodies;
3. missing return in a non-void lambda;
4. an empty non-void function producing `TypeError`, not `Invalid_argument`;
5. wrong arity for both void and value-returning calls;
6. default steps for integer and floating-point loops;
7. undefined class names in declarations, fields, arrays, casts, and function
   types;
8. a prototype followed by a matching definition;
9. a prototype followed by a mismatched definition;
10. equality of matching and non-matching void function types;
11. integer literals passed to narrow integer parameters;
12. unsigned 128-bit literal inference;
13. invalid compound assignments;
14. assignment to explicit const-field projections;
15. invalid constant arithmetic such as division by zero;
16. preservation of expression order in `free`.

## Suggested implementation order

1. Fix assignment compatibility and compound-assignment validation.
2. Correct loop and lambda return analysis.
3. Remove uncaught exceptions from empty functions and call arity checks.
4. Fix default loop-step typing.
5. Centralize source-type validation and conversion.
6. Correct prototype reconciliation and void function equality.
7. Harden numeric inference and constant folding.
8. Refactor the context representation and add the broader regression suite.
