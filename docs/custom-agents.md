# Custom Agents

A `code-reviewer` agent is configured in `.claude/agents/code-reviewer.md`.
It performs quality, security, and best-practices review of changed
files, grouping findings by severity (critical / major / minor). It is
read-only (`Read`, `Grep`, `Glob`).

A `git-ops` agent is configured in `.claude/agents/git-ops.md`. It
handles basic Git operations — commit, push, and pull — writing
meaningful, logically-split commit messages and always using the
machine's configured Git identity (never hardcoded). It never
force-pushes or discards work without explicit confirmation, and it adds
no `Co-Authored-By` trailer.

A `planner` agent is configured in `.claude/agents/planner.md`. It
analyzes requirements, explores the codebase, and produces a detailed,
step-by-step implementation plan. It never edits files and is
read-only (`Read`, `Grep`, `Glob`). It runs on the `opus` model.

A `coder` agent is configured in `.claude/agents/coder.md`. Acting as a
senior Python engineer, it implements an approved plan cleanly and
verifies the changes with tests. It can write code and run commands
(`Read`, `Write`, `Edit`, `Bash`, `Grep`, `Glob`). It runs on the
`sonnet` model.

## Code-Writing Workflow

Whenever code needs to be written or changed, follow this sequence:

1. **Plan** — invoke the `planner` agent first to produce an
   implementation plan.
2. **Approve** — present the plan to the user and wait for explicit
   approval (revising it with the `planner` if requested). No code is
   written before the plan is approved.
3. **Implement** — only after approval, invoke the `coder` agent,
   passing it the approved plan to implement.

Optionally, the `code-reviewer` agent can then review the result, and
the `git-ops` agent can commit it when the user asks.
