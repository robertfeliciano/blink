---
name: e2e-change
description: Plan and implement end-to-end Blink compiler changes that span the frontend, backend, and tests or examples. Use when a requested language or compiler change must be carried through the complete pipeline.
---

# End-to-End Change

Use this workflow for changes that cross Blink's frontend/backend boundary or
otherwise require coordinated frontend, backend, and test coverage.

## Approval gate

Before editing files or delegating implementation:

1. Inspect the relevant code and determine the full end-to-end scope.
2. Construct a concrete implementation plan.
3. The plan must separately identify:
   - frontend changes;
   - backend changes; and
   - test library and example changes.
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
   - Own unit, backend, and end-to-end tests as applicable.
   - Own test helpers, fixture libraries, and example Blink programs.
   - Include expected behavior and validation requirements in the assignment.

Create all three sub-agents even when one workstream is expected to require
only verification or a no-op conclusion. Ask that agent to inspect its area,
make any required changes, and report why no change was needed if applicable.

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
6. Report the implemented behavior, validation results, and any remaining
   limitations to the user.

If execution reveals that the approved plan needs a material scope change,
stop, explain the change, revise the plan, and obtain approval again before
continuing with the expanded work.
