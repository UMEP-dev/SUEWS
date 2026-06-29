# SUEWS Issue Label Hierarchy

Use this reference when auditing labels on SUEWS GitHub issues. The hierarchy is
orthogonal to issue-body readiness: an issue can be well framed but badly
labelled, or correctly labelled but still need a summary, scope decision, or
split.

## Required Layers

Every open issue should normally carry:

- one `1-*` type label;
- one or more `2-*` area labels;
- one `3-*` priority label;
- one `4-*` status label.

`0-*` labels are automation, CI, physics, or source metadata. Do not count them
as part of the four-layer hierarchy.

## Layer Meanings

### `1-*` Type

Use exactly one unless the user explicitly approves an exception.

- `1-bug`: observed incorrect behaviour, wrong docs, crash, validation failure,
  regression, or mismatch against intended model/API behaviour.
- `1-feature`: new capability, new validation rule, new workflow support, or
  intentionally expanded behaviour.
- `1-maintenance`: cleanup, refactor, docs maintenance, process/tooling cleanup,
  or wording improvements that do not change behaviour.
- `1-question`: support question, uncertainty, or design question that is not yet
  framed as an implementation task.

### `2-*` Area

Use one or more labels for affected code, documentation, process, or physics
areas. Multiple `2-*` labels are expected for cross-cutting issues, for example a
validator issue that also needs user docs.

Prefer the most specific module label (`2-module:*`) when the issue concerns a
named physics module. Use infrastructure labels (`2-infra:*`) for validation,
data model, CI, build, packaging, input/output, logging, type safety, schema,
site, tests, and bridge work. Use `2-doc:*` for documentation surfaces and
`2-meta:*` for governance or release process.

### `3-*` Priority

Use exactly one.

- `3-P0`: critical breakage or release-stopping issue.
- `3-P1`: high-priority bug, correctness problem, active release blocker, or
  important validator/data-model/user-facing problem.
- `3-P2`: valid medium-priority work, nice-to-have feature, cleanup, docs
  improvement, or backlog task.

### `4-*` Status

Use exactly one status label by default. If an issue carries multiple `4-*`
labels, flag it as a status conflict and suggest the primary status.

- `4-ready`: near-term work that is clear enough to implement or review.
- `4-in-progress`: actively being worked on or already covered by an open PR.
- `4-needs-deps`: waiting on another issue, PR, release, or design prerequisite.
- `4-needs-discussion`: needs a maintainer/catch-up decision before execution.
- `4-needs-science`: needs scientific input, evidence, benchmark decision, or
  model-design judgement.
- `4-deferred`: valid issue, but not planned for near-term work. Use this for
  old feature requests, broad master issues, unowned nice-to-haves, and work that
  is technically clear but not a current support priority.

## Audit Procedure

1. Fetch live repo labels first; do not invent label names.
2. For each issue, list current `1-*`, `2-*`, `3-*`, and `4-*` labels.
3. Flag missing layers, duplicate single-choice layers (`1-*`, `3-*`, `4-*`),
   and `4-ready` labels that are not near-term priorities.
4. Suggest exact additions and removals. Keep reasoning short and evidence-based:
   title/body/comments, assignee, linked PRs, active umbrella issues, or current
   team priority.
5. In interactive mode, stop at suggestions until the user approves the exact
   label edits.
6. In autonomous scheduled sweeps, report suggestions only unless the automation
   contract explicitly grants label edits for the exact tier being changed.

## Batch Output

For each issue needing label action, use:

```text
#<number> - <title>
Current: <current relevant labels>
Suggest: add <labels>; remove <labels>
Reasoning: <one or two concise reasons>
Approval: required before editing
```

Group the final report by action type:

- missing required layer;
- conflicting layer;
- `4-ready` but should be `4-deferred`;
- already complete / no action.
