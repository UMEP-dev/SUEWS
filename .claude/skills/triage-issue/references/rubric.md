# Issue Triage Rubric

Use this rubric to evaluate whether a SUEWS GitHub issue is ready for
implementation, review, or follow-up.

## Issue Types

### Bug

Look for:

- observed symptom
- minimal reproduction, preferably with bundled sample data where possible
- expected behaviour
- actual behaviour
- affected API, CLI command, config path, file format, or workflow
- environment/version information when relevant
- suspected root cause, if known
- validation condition or regression test expectation

### Feature

Look for:

- user need, scientific motivation, or workflow motivation
- proposed behaviour
- non-goals
- affected API, config, CLI, documentation, or workflow surface
- compatibility and migration considerations
- acceptance criteria

### Maintenance / Refactor

Look for:

- current pain point
- behaviour that must be preserved
- intended scope boundary
- risks
- validation plan

### Documentation

Look for:

- target reader
- current gap or confusion
- affected documentation location
- expected outcome

### Meta / Process

Look for:

- process problem being solved
- affected maintainers/contributors/workflow
- proposed policy, tool, or skill
- rollout plan
- acceptance criteria
- non-goals

## Readiness Statuses

- `ready`: clear enough to implement or review.
- `needs-repro`: bug report needs a minimal reproduction.
- `needs-scope`: feature/refactor scope needs clarification.
- `needs-acceptance`: expected outcome is not yet testable.
- `needs-summary`: useful context exists in comments but needs a maintainer summary.
- `misframed`: title/body describe a downstream symptom after triage has
  identified a clearer root cause.
- `superseded`: should be replaced by a clearer issue.
- `duplicate`: overlaps with an existing issue.

## Framing Checks

Ask:

- Does the title describe the current understanding of the issue?
- Does the body distinguish symptom from root cause where known?
- Would a contributor know what files, API, CLI command, or docs area to inspect?
- Can a PR author derive tests or acceptance criteria from the issue?
- Are important discoveries currently buried only in comments?
- If the framing changed, is the original issue author's contribution preserved?
