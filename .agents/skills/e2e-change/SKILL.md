---
name: e2e-change
description: Plan and implement Blink compiler changes that require modifications to both the frontend and backend, with mandatory test changes. Do not use for frontend-only or backend-only changes.
---

# End-to-End Change

An end-to-end change is a change that requires implementation modifications in
both Blink's frontend and backend. Use this workflow only when both sides must
change. A frontend-only or backend-only implementation is not end-to-end, even
if it needs broad validation.

Every implementation change must also modify tests, regardless of its size or
which compiler phase it affects. Existing coverage, examples, documentation,
and validation runs do not substitute for a test change.

## Approval gate

Before editing files or delegating implementation:

1. Inspect the relevant code and determine the full end-to-end scope.
2. Construct a concrete implementation plan.
3. The plan must separately identify:
   - frontend changes;
   - backend changes; and
   - mandatory test changes; and
   - example changes, when useful.
4. Present the plan to the user or responsible human.
5. Explicitly ask for approval to execute it.
6. Wait for explicit approval before making changes or creating implementation
   sub-agents.

Do not treat the original request to plan an end-to-end change as approval to
execute the plan. If the human requests revisions, update the plan and wait for
approval of the revised version.

## Execution after approval

After the plan is explicitly approved, create exactly three sub-agents, one for
each workstream below:

1. **Frontend changes**
   - Own parsing, AST, typing, desugaring, and other OCaml frontend work.
   - Include the approved plan and precise file ownership in the assignment.

2. **Backend changes**
   - Own the OCaml/C++ bridge, backend data structures, LLVM code generation,
     and other C++ backend work.
   - Include the approved plan and precise file ownership in the assignment.

3. **Test library and example changes**
   - Always modify tests to cover the implementation change.
   - Own the appropriate unit, backend, and end-to-end test changes.
   - Own test helpers, fixture libraries, and example Blink programs.
   - Include expected behavior and validation requirements in the assignment.

Because this workflow applies only when both frontend and backend modifications
are required, the frontend and backend workstreams must each contain an
implementation change. The test workstream must contain a test change. If any
of those would be a verification-only or no-op workstream, this is not an
end-to-end change and must be routed to a narrower workflow instead.

The three assignments should be mutually exclusive wherever practical because
all agents share the same worktree. Tell each agent about the other workstreams
and instruct it not to edit files owned by another agent unless coordination is
required.

## Integration

While the sub-agents work, handle coordination and inspect relevant shared
interfaces. When they finish:

1. Review each result against the approved plan.
2. Reconcile cross-workstream contracts, especially changes to the desugared
   OCaml AST and its C++ bridge representation.
3. Resolve integration issues without silently expanding the approved scope.
4. Run focused checks for each workstream.
5. Run the complete relevant end-to-end test suite.
6. Confirm that the final diff contains test changes exercising every
   implementation change.
7. Report the implemented behavior, validation results, and any remaining
   limitations to the user.

If execution reveals that the approved plan needs a material scope change,
stop, explain the change, revise the plan, and obtain approval again before
continuing with the expanded work.
