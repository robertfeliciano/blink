---
name: language-feature-design
description: Design new Blink language syntax, semantics, typing rules, lowering, backend behavior, and coverage before implementation. Use for proposed language features or semantic changes, especially when the user provides links as design references.
---

# Language Feature Design

Design the feature across the complete Blink compiler before changing code.
This is a planning workflow, not implementation authorization.

## Design boundary

During this workflow:

- Inspect the repository and existing behavior as needed.
- Research user-provided references.
- Resolve design choices and produce an implementation plan.
- Do not edit project files or begin implementation.
- End by presenting the final plan and waiting for user input.

An initial request to design or add a feature is not approval to implement the
resulting plan.

## Linked-reference research

When the user supplies links, read the relevant content directly before
designing the feature. Distinguish source facts from Blink-specific
implementation inferences, and disclose any reference that cannot be accessed.
Do not invent content from an unreadable link.

Do not automatically create a multi-agent planning panel because references or
multiple compiler concerns are involved. The main agent owns the research,
repository analysis, design decisions, and final plan. Delegate planning only
when the user explicitly requests delegation or parallel agent work.

## Design synthesis

Review reference findings against the current repository. Resolve design
tradeoffs using the user's goal, existing Blink conventions, KISS, DRY,
implementation risk, and testability.

The final design should cover all applicable areas:

1. User-facing syntax with representative valid and invalid examples.
2. Runtime semantics and evaluation behavior.
3. Static typing rules, conversions, inference, and error conditions.
4. Parser-facing AST changes.
5. Typed AST changes.
6. Desugared AST representation and lowering behavior.
7. OCaml/C++ bridge contract changes.
8. C++ data structure and LLVM codegen changes.
9. Diagnostics and source-range expectations.
10. Mandatory test-file changes covering every implementation change,
    regardless of scope.
11. Example program or documentation changes.
12. Compatibility, migration, and optimization considerations.

Reuse existing compiler mechanisms when they already express the required
behavior. If logic would otherwise be repeated across phases or files, identify
the shared helper or module that should own it. Avoid abstractions that add
complexity without actual reuse.

Classify the implementation scope precisely. It is end-to-end only if the plan
requires modifications to both the frontend and backend. Frontend-only and
backend-only changes must use a narrower implementation workflow. In every
case, each implementation change must be paired with a test change; existing
tests, examples, documentation, or running the test suite are not substitutes.

For each planned change, identify the likely files or components, intended
behavior, dependencies on other steps, and validation. Clearly label unresolved
choices that require a user decision.

## Approval handoff

Present one coherent final plan to the user. Briefly include:

- the recommended design;
- important conclusions drawn from linked references;
- meaningful alternatives and tradeoffs;
- the ordered implementation plan; and
- any questions whose answers would materially change the design.

Then stop and wait for explicit user input. Do not create implementation
sub-agents or modify files until the user approves the final plan. After
approval, use the repository's end-to-end change workflow only when both
frontend and backend modifications are required; otherwise use a narrower
workflow. Always include the planned test changes during implementation.
