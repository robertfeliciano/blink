---
name: squint
description: Perform suspicious, runtime-oriented code reviews of Blink branch or commit-to-current-state diffs. Use when reviewing a Blink branch, patch, or commit range for bugs, regressions, duplicated solutions, or diverging implementation approaches.
---

# Squint

Review Blink changes as a bug hunt. Inspect the complete changed-file context,
trace behavior through the compiler and runtime, and report anything that may be
wrong even when the evidence is not yet conclusive.

This is a review workflow. Do not edit source files unless the user separately
asks for fixes.

## Establish the review boundary

Determine and state the exact comparison before reviewing:

- If the user supplies a commit, compare that commit with the current branch
  state.
- Otherwise, review the branch relative to its merge base with the appropriate
  upstream or base branch. Prefer an explicitly named base, then configured
  upstream, then the repository's evident default branch. Do not silently guess
  when competing bases would materially change the review.
- Treat the current state as `HEAD` plus staged changes, unstaged changes, and
  relevant untracked files. Include each of these in the inventory.
- Record renames, copies, additions, deletions, submodule changes, and binary or
  generated files explicitly. Do not let an unsupported file type disappear
  from the review without explanation.

Use Git commands compatible with the installed Git version. Preserve the user's
worktree and do not check out commits, reset files, clean files, or otherwise
mutate repository state to construct the comparison.

## Read complete files

Start with the diff to learn the intended change, but never review from the diff
alone.

- Read every changed text file in full in its current form.
- For deleted files, read the complete file from the comparison base.
- For renames or wholesale replacements, read the complete before and after
  versions when both exist.
- For generated or binary changes, inspect the authoritative source and the
  available metadata or tooling needed to assess the change. State any content
  that could not be inspected.
- Follow references into unchanged files whenever needed to understand callers,
  callees, shared types, invariants, ownership, phase contracts, tests, and
  runtime effects.

Reading the full changed files is mandatory even when a hunk appears isolated or
the patch is large.

## Hunt for defects

Trace each meaningful change along the paths that execute it. Reason about real
inputs and state transitions rather than only checking local syntax.

For Blink compiler work, follow the affected behavior through every applicable
stage: lexing and parsing, source AST, typing and contexts, desugaring, the
positional OCaml/C++ bridge, C++ AST structures, LLVM lowering, optimization,
native execution, diagnostics, build wiring, and tests. Check cross-phase
constructor tags, record fields, tuple positions, option encodings, types,
evaluation order, control flow, ownership, and source ranges wherever relevant.

Actively look for:

- incorrect control flow, state transitions, evaluation order, or edge cases;
- mismatched assumptions across frontend, FFI, backend, build, and test code;
- missing callers, variants, cleanup, validation, diagnostics, or coverage;
- behavior that works only at one optimization level or for one syntactic path;
- malformed or unstable LLVM IR and differences between compile-time and native
  runtime behavior;
- regressions outside the changed hunk caused by changed invariants;
- error paths, empty inputs, nesting, boundary values, and invalid programs;
- accidental generated artifacts, compatibility failures, or build-order issues.

If something smells wrong, report it. Distinguish a demonstrated defect from a
plausible risk or an unresolved suspicion, and explain what evidence would settle
the latter. Do not suppress a concern merely because a minimal reproduction has
not yet been proven.

## Check for reuse and divergence

Search the entire repository for existing implementations of the same rule,
conversion, traversal, validation, or helper before accepting new logic.

- Identify the authoritative existing solution when one exists.
- Flag duplicated logic and approaches that encode the same invariant
  differently.
- Compare semantics, error handling, naming, and edge cases rather than relying
  on superficial textual similarity.
- When shared behavior is genuinely repeated, recommend the concrete existing
  helper or the narrow module/function that should own an extracted solution.
- Flag diverging approaches even if both currently work, because future fixes
  would need to remain synchronized.
- Do not demand abstraction for isolated logic without actual reuse.

## Enforce OCaml and C++20 best practices

Ensure changed OCaml and C++ code uses the language's established best
practices as well as Blink's nearby conventions. Treat departures as findings
when they create correctness, safety, clarity, performance, or maintenance
risk; avoid reporting purely subjective style preferences.

For OCaml, check in particular for exhaustive and intentional pattern matches,
precise variant and record modeling, correct structural versus physical
equality, safe option/result and exception handling, bounded stack use for
potentially large recursion, sensible immutability, preserved source ranges,
and reuse of standard or existing project helpers. Keep module interfaces and
phase-specific types narrow enough that invalid compiler states are difficult
to construct.

For C++20, check in particular for RAII, explicit ownership and lifetime rules,
const-correctness, safe value and reference semantics, correct move/copy
behavior, initialized state, iterator and bounds safety, signedness and
conversion hazards, exhaustive enum/variant handling, exception and error
behavior, and the absence of undefined behavior. Prefer standard C++20
facilities and existing project abstractions over manual resource management,
unchecked casts, raw owning pointers, or duplicated utilities. Apply the same
scrutiny to LLVM object lifetimes and API preconditions.

Confirm touched OCaml follows `frontend/.ocamlformat` and touched C++ follows
`backend/.clang-format`. Formatting compliance does not replace the semantic
review.

## Validate conclusions

Use focused builds, tests, static checks, generated IR inspection, or small
reproductions when they materially strengthen or disprove a finding. Start with
the narrowest relevant check and prefer `-O0` for LLVM/codegen investigations
before checking optimized behavior. Do not treat existing tests as proof that an
unexercised path is correct.

If validation cannot run, report the exact limitation and retain any concern
supported by code tracing.

## Report

Lead with findings ordered by severity. For each finding:

1. Give a concise defect or risk statement.
2. Point to the tightest useful file and line range in the current file, or the
   base version for deleted code.
3. Describe the concrete input or execution path that reaches it.
4. Explain the resulting incorrect behavior or maintenance hazard.
5. State whether it is confirmed, strongly inferred, or suspicious and needs
   validation.
6. Suggest the smallest sound direction for correction, including reuse or
   modularization where applicable.

Keep summaries secondary to findings. After the findings, note material open
questions, validation performed, and coverage gaps. If no findings remain, say
so explicitly, describe what was reviewed and tested, and identify residual
risk rather than implying certainty.
