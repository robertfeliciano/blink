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

## Linked-reference design panel

Whenever the user supplies one or more links as references for the requested
change, create a design panel of two or three sub-agents before writing the
main agent's final plan.

Use two agents for a focused feature with a small or closely related reference
set. Use three agents when the feature, reference set, or implementation tradeoffs
span multiple substantial concerns. Do not create more than three panel agents.

Give every panel member:

- the user's requested behavior;
- the relevant link or links;
- enough Blink repository context to evaluate integration;
- an instruction to read the relevant linked content directly;
- an instruction to distinguish source facts from implementation inferences;
- an instruction to propose a concrete Blink design and implementation plan;
- an instruction not to edit files; and
- the identities of the other panel members once all have been created.

Assign complementary perspectives rather than asking for duplicate summaries.
Choose from these perspectives according to the feature:

1. Reference semantics and language-facing syntax.
2. Blink frontend, AST, typing, and desugaring integration.
3. Bridge, LLVM backend, compatibility, diagnostics, and test strategy.

Require the panel members to discuss the proposal with each other. After all
members are created, send them the panel roster and ask them to exchange their
findings, challenge incompatible assumptions, and converge on a recommendation.
They may retain explicitly documented disagreements when a real tradeoff remains.

Each panel member must return to the main agent:

- the reference behavior relevant to Blink;
- proposed syntax and semantics;
- recommended implementation approach;
- rejected alternatives and their tradeoffs;
- edge cases, compatibility risks, and open questions; and
- its final decision and proposed plan after panel discussion.

Wait for all panel members. If one fails to access a reference, have the other
members cover it when possible and disclose the gap. Do not invent content from
an unreadable link.

## Main-agent synthesis

The main agent owns the final decision and plan. Review the panel's conclusions
against the current repository instead of forwarding any one response verbatim.
Resolve disagreements using the user's goal, existing Blink conventions, KISS,
DRY, implementation risk, and testability.

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
10. Unit, backend, and end-to-end test coverage.
11. Example program or documentation changes.
12. Compatibility, migration, and optimization considerations.

Reuse existing compiler mechanisms when they already express the required
behavior. If logic would otherwise be repeated across phases or files, identify
the shared helper or module that should own it. Avoid abstractions that add
complexity without actual reuse.

For each planned change, identify the likely files or components, intended
behavior, dependencies on other steps, and validation. Clearly label unresolved
choices that require a user decision.

## Approval handoff

Present one coherent final plan to the user. Briefly include:

- the recommended design;
- important conclusions drawn from linked references;
- meaningful alternatives or panel disagreements;
- the ordered implementation plan; and
- any questions whose answers would materially change the design.

Then stop and wait for explicit user input. Do not create implementation
sub-agents or modify files until the user approves the final plan. After
approval, use the repository's end-to-end change workflow for implementation.
