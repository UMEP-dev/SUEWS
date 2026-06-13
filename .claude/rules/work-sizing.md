# Right-Sizing Issues and Pull Requests

Criteria for scoping a GitHub issue or pull request to a single, reviewable,
revertible unit of work. Used by the issue and PR skills (`triage-issue`,
`fix-issue`, `audit-pr`, `split-pr`) and applicable to any session that opens an
issue or PR.

This is guidance, not a mechanical gate. The goal is work that one person can
understand, review, and revert in isolation. Prefer small and coherent over large
and bundled.

---

## A right-sized issue

A good issue is one actionable concern with a clear "done":

- Describes a single problem or change, not a list of separable deliverables.
- Has an acceptance criterion (or a small, coherent set) a PR author can test
  against.
- Points a contributor at the files, API, CLI, or docs area to inspect.
- Can land as one focused PR, or as a short ordered series of independently
  mergeable PRs.
- Is self-contained: it does not require resolving unrelated decisions first.

### Split it (readiness `needs-split`) when

- It conflates multiple independent bugs, or a bug plus a feature.
- Its acceptance criteria read as a checklist of deliverables that could ship
  separately.
- The framing is "and also / while we are here / epic / umbrella".
- It spans several subsystems that could be fixed independently.
- The resulting PR could not be reviewed in one sitting (see PR sizing below).
- It is blocked on several unrelated decisions.

On split: keep the original issue as the parent/umbrella, decompose into focused
child issues, link them with GitHub native sub-issues (not body mentions), and
preserve the original report and raiser attribution on the parent. Creating the
children is a human-gated decision (see `triage-issue`).

### Do not over-split

- A trivial typo or one-line fix does not need its own tracking issue; just fix
  it (batch related trivia).
- Children must each be independently meaningful; do not slice one atomic change
  into fragments that cannot stand alone.

---

## A right-sized PR

A good PR is one logical change addressing one issue (or one coherent slice):

- Explainable in a sentence or two; reviewable in a single pass.
- Code, tests, and docs for that one change travel together.
- No unrelated drive-by edits, reformatting, or opportunistic refactors.
- Independently correct and revertible: it builds and passes CI on its own.
- Minimal cross-subsystem blast radius.

Soft heuristic: aim for a focused diff a reviewer can hold in their head (roughly
a few hundred lines of meaningful change). Mechanical or generated diffs
(formatting sweeps, regenerated artefacts, large fixture updates) do not count
against this, but isolate them in their own commits or PRs so the semantic change
stays small and reviewable.

### Split it when

- It mixes a refactor, a behaviour change, and formatting in one diff.
- It bundles multiple unrelated fixes on one branch.
- The description needs headed sections to explain unrelated parts.
- A schema/data-model change is entangled with an unrelated feature (schema bumps
  already warrant a dedicated change; see `python/schema-versioning.md` and the
  "one PR per logical group" rule in `naming-convention.md`).
- Review stalls because the reviewer cannot assess it as a whole.

Prefer a stacked series of small PRs over one large branch: each PR independently
mergeable, all tracked under the parent issue.

### Do not over-split

- Do not break one atomic change into PRs that fail to build or test on their
  own. Each PR must leave the tree green.

### Code granularity (language-specific)

PR size is downstream of how the code is factored. A change lands small and
reviewable when functions, files, and modules are themselves right-sized, and
what "right-sized" means is language-specific. Apply the per-language size
guidance, which loads with each language's conventions:

- Fortran: `fortran/conventions.md` (one module per file, focused procedures,
  derived types over long argument lists).
- Python: `python/conventions.md` (single-responsibility functions, cohesive
  modules).
- Rust bridge: `rust/conventions.md` (one concern per module, small functions;
  `cargo clippy`, run locally, warns on `too_many_arguments` by default -- clippy
  is not wired into the Makefile or CI, so it is not an automated gate).

If a fix only stays small by editing an oversized function or god-module, prefer a
small preparatory refactor PR first, then the fix on top.

---

## Umbrella decomposition for large splits

When a `needs-split` issue decomposes into several related children, structure it
as a parent umbrella plus sub-issues so a human (or an automated runner that
consumes this umbrella format) can sweep them in order. The SUEWS pipeline itself
stops at "umbrella proposed"; creating the umbrella and running the waves is
outside its five stages. Structure:

- The parent issue carries the maintainer summary, the overall "done", and the
  preserved original report and raiser attribution.
- Each child is a focused, independently mergeable sub-issue, linked to the parent
  with GitHub native sub-issues (not body mentions).
- Record the children as a checklist on the parent (`- [ ] #N <task-id>: <title>`)
  and group them into dependency-ordered waves: each wave is a set of children
  with no unmet dependency on a later wave.
- Label each child by work nature (for example a mechanical vs design-sensitive
  distinction) and mark any child that must pause the sweep before its wave runs.
- A status diagram (for example a mermaid graph with todo/wip/done/blocked
  classes) on the parent is optional but helps track wave progress.

Creating the umbrella and children is human-gated (see `triage-issue`); propose
the structure and apply only after approval.

---

## Default relationships

- One issue -> one PR is the default.
- One large issue -> an ordered series of small PRs (stacked), each independently
  mergeable, tracked under the parent.
- Avoid many issues -> one PR; bundle only trivially related changes.

## Where this is checked

- `triage-issue`: assigns `needs-split` and proposes the decomposition at triage
  time.
- `fix-issue`: if a fix balloons mid-implementation, stop and propose a split
  rather than growing the PR.
- `audit-pr`: assesses PR size up front (Size Gate) and recommends a split before
  reviewing an oversized or bundled PR.
- `split-pr`: executes an approved PR split, carving a fat branch into a verified
  stack of independently-green PRs (completeness-checked), then hands off to
  `queue-pr`.
