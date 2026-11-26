# Pull Request Descriptions

Pull request descriptions exist so a human reviewer or future maintainer can understand, review, and trace a change without reverse-engineering the diff. [decision: software maintainer, 2026-11-19]
Every pull request in UMEP-dev/SUEWS -- whether opened by a person or drafted with a coding agent -- follows this rule. [decision: software maintainer, 2026-11-19]

---

## Writing principles

- **Lead with the human problem, not the patch mechanics.**
  Open by explaining what was broken, missing, or risked, who or what hits it, and why it matters now, before describing code changes. [decision: software maintainer, 2026-11-19]
- **Explain intent, invariants, and trade-offs rather than recapping the diff.**
  GitHub already shows changed lines; prose explains why this design was chosen, what constraints it preserves, and what alternatives were ruled out. [decision: software maintainer, 2026-11-19]
- **Write clear British English with human-scale detail.**
  Use concrete numbers only where they clarify the point; do not dump uncontextualised machine counts or add agent/model branding to PR titles. [decision: software maintainer, 2026-11-19]
- **Use progressive disclosure.**
  Keep the main PR body scannable in one to two minutes; put long tables, derivations, multi-site breakdowns, or verbose logs inside `<details>` blocks or linked PR comments. [decision: software maintainer, 2026-11-19]
- **Format GitHub Markdown as one line per paragraph.**
  In posted PR bodies and PR comments, do not hard-wrap prose at a column width or break sentences across single newlines inside a paragraph, because GitHub renders single newlines as visible line breaks. [decision: software maintainer, 2026-11-19]
- **Prove this PR, not an unrelated batch.**
  Lead `## Verification` with checks specific to this PR's change; if a combined integration-branch run is also cited, label it explicitly as combined-branch context alongside PR-specific proof rather than pasting identical batch boilerplate across sibling PRs. [procedure: Principle 1]
- **Keep outward text public-safe.**
  Never include internal tracker IDs, absolute local paths, or private host names (`.claude/rules/autonomous-workflow.md`, "Gating model"). [procedure: Principle 2]

---

## Structure by PR size

### Trivial or mechanical PR

For a genuinely small change -- a typo fix, a one-line doc or CI tweak, or a mechanical dependency pin -- write one short paragraph covering **why**, **what changed**, and **how it was verified**, plus any issue link (`Fixes #N` or `Refs #N`). [decision: software maintainer, 2026-11-19]
Do not pad a trivial PR with empty headings. [decision: software maintainer, 2026-11-19]

### Substantive PR

For any bug fix, feature, refactor, physics or numerical change, bridge or schema change, or governance/rule change, use the canonical sections below and omit conditional sections that do not apply. [decision: software maintainer, 2026-11-19]

1. **Issue link line** (top or bottom): `Fixes #N` only when merging closes the issue; otherwise `Refs #N`. [decision: software maintainer, 2026-11-19]
2. **`## Why`**: the problem, failure mode, or motivation in plain language, including how it arises and why it should land now. [decision: software maintainer, 2026-11-19]
   If a linked issue already carries the full background and reproduction, give a brief recap and point to that issue rather than repeating it. [decision: software maintainer, 2026-11-20]
   A PR that adds or amends a standing rule states its origin here (the pull requests, issues, incident, or maintainer decision it comes from, per `.claude/rules/autonomous-workflow.md`, "Statements in rules"). [procedure: Principle 2]
3. **`## What changed`**: high-level design and behavioural effect grouped by concept rather than by file list, calling out any user-facing, breaking, or `[experimental]` change, followed by **`Out of scope / known limits:`** stating what the PR deliberately leaves untouched, trade-offs accepted, or follow-up issues. [procedure: Principle 1, 3]
4. **Conditional domain sections** (include only those triggered by the diff; each owning rule holds the detailed contract): [procedure: Principle 1, 2]
   - `## Scientific evidence` when `0-physics:change` applies (`.claude/rules/physics-change-evidence.md`).
   - `## Numerical change` when `0-numerics:change` applies (`.claude/rules/physics-change-evidence.md`).
   - `## Bridge contract` when `src/suews_bridge/**` or either side of its boundary changes (`.claude/rules/rust/bridge-contract.md`).
   - `## Schema migration` when `CURRENT_SCHEMA_VERSION` bumps (`.claude/rules/python/schema-versioning.md`).
   - `## Stack` when the PR is part of a stacked series (`.agents/skills/split-pr/SKILL.md`).
5. **`## How to review`**: a short reviewer map listing: [decision: software maintainer, 2026-11-19]
   - **Start here:** the one to three files or symbols that hold the real logic, in reading order, with one clause on what to check in each.
   - **Skip / skim:** regenerated fixtures, generated docs, formatting commits, or mechanical moves that need no line-by-line reading (omit if none).
   - **Decision needed:** the named role (`MAINTAINERS.md`) and exact question awaiting sign-off, or `"None -- standard code review only."` (`.claude/rules/autonomous-workflow.md`, "Gates"). [procedure: Principle 2]
6. **`## Verification`**: PR-specific evidence distinguishing **reported** (run by the author/agent at a stated commit) from **confirmed** (CI run link or independent reproducer), per `AGENTS.md` Principle 1: [procedure: Principle 1]
   - The targeted test or command that fails before the fix and passes after, with commit SHA and observed outcome.
   - Broader suites run (`make test-smoke`, `make test`, targeted pytest/cargo/linter commands, CI links).
   - Anything skipped, deferred to CI or another platform, or only observed on a combined integration branch.
   - Reference-fixture status (unchanged, or refreshed per `.claude/rules/physics-change-evidence.md`).

### Skeleton

```markdown
Refs #N  <!-- use Fixes #N only when merging closes the issue -->

## Why

<Plain-language problem or motivation: what breaks or is missing, when it happens, and why fix it now. If a linked issue already has the full context, give a brief recap and point to #N.>

## What changed

<High-level behavioural and design summary grouped by concept. Note any user-facing, breaking, or experimental impact.>

**Out of scope / known limits:** <What this PR deliberately does not address, trade-offs, or follow-up issues.>

<!-- Conditional sections -- include only when triggered by the diff:
     ## Scientific evidence  (0-physics:change)
     ## Numerical change     (0-numerics:change)
     ## Bridge contract      (src/suews_bridge/**)
     ## Schema migration     (CURRENT_SCHEMA_VERSION bump)
     ## Stack                (stacked PR series)
-->

## How to review

- **Start here:** <1-3 key files or symbols in reading order, and what to check in each.>
- **Skip / skim:** <Regenerated fixtures, generated docs, or mechanical commits; omit if none.>
- **Decision needed:** <Named role from MAINTAINERS.md and exact question, or "None -- standard code review only.">

## Verification

- **Targeted check (reported at `<sha>`):** <Exact test or command proving this PR's change and its result.>
- **Broader checks:** <Additional suites or CI links (confirmed once green).>
- **Not run / unconfirmed:** <Deferred checks or platform limits, or "None.">
- **Reference fixtures:** <Unchanged / refreshed in this PR.>
```

---

## Review gate

PR description quality is enforced by human review and `/audit-pr`, not by a mechanical CI regex on prose. [decision: software maintainer, 2026-11-19]
During `/audit-pr`: [procedure: Principle 1, 2]

- Missing `## Why`, `## How to review`, or PR-specific `## Verification` on a substantive PR -- or copy-pasted batch-integration boilerplate standing in for PR-specific proof -- is `[major]`. [decision: software maintainer, 2026-11-19]
- Missing required `## Scientific evidence` or `## Numerical change` is `[blocking]` (`.claude/rules/physics-change-evidence.md`). [decision: software maintainer, 2026-11-19]
- Any privacy leak (internal tracker ID, local path, private host name) is `[blocking]` (`.claude/rules/autonomous-workflow.md`). [procedure: Principle 2]
- Phrasing polish or minor structure improvements are `[minor]`. [decision: software maintainer, 2026-11-19]

Rewriting a PR description after review or merge is permitted when it makes the public record clearer; GitHub retains the full edit history. [decision: software maintainer, 2026-11-19]

---

## Related

- `.github/PULL_REQUEST_TEMPLATE.md` -- GitHub web-UI prompt mirroring this structure. [fact: test -f .github/PULL_REQUEST_TEMPLATE.md]
- `.claude/rules/physics-change-evidence.md` -- `## Scientific evidence` presentation protocol (tiered depth, figures, provenance) and `## Numerical change`.
- `.claude/rules/autonomous-workflow.md` -- rule-origin requirement, privacy invariant, and gating model.
- `.claude/rules/work-sizing.md` -- PR scoping criteria.
- `.claude/rules/rust/bridge-contract.md` -- `## Bridge contract` audit record.
- `.claude/rules/python/schema-versioning.md` -- schema migration bookkeeping.
