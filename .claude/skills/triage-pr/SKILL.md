---
name: triage-pr
description: Use whenever auditing, prioritising, or batch-triaging SUEWS draft and stalled pull requests so each gets a clear disposition. Trigger for "triage PRs", "which draft PRs need attention", "what's the disposition of these PRs", "groom the PR backlog", "defer this PR with a reason", "which PRs are ready to advance to merge", or a scheduled PR-backlog sweep. Routes live PRs to prep-pr/split-pr/queue-pr; defers or escalates the rest; never merges or closes.
---

# Triage PR

The PR-axis sibling of `triage-issue`. Where `triage-issue` grooms issue descriptions
into maintainer-ready engineering work items, `triage-pr` grooms the draft and stalled
PR backlog so every open PR has a clear, reasoned disposition.

This skill is a **router**, not a reviewer. It decides *what happens next* for each PR;
it does not re-review code, order the merge queue, or carve diffs. Those responsibilities
stay with their owners:

- Code review -> `audit-pr`
- Merge queue ordering -> `queue-pr`
- Oversized diff carving -> `split-pr`

Verdict routes:

- `advance` -> route to `prep-pr`, `split-pr`, or `queue-pr`
- `continue` -> route to `fix-issue` or author
- `defer` -> parked with a named revisit trigger
- `close` -> **human-gated**: propose, never act
- `escalate` -> **human-gated**: surface conflicting signals, never guess

`triage-pr` never merges and never closes a PR.

---

## Triggers

Use this skill when asked to:

- Audit, prioritise, or groom the SUEWS draft/stalled PR backlog.
- Assign a disposition to one or many draft or stale PRs.
- Find which PRs are ready to advance, which should be deferred, and which need a
  maintainer decision.
- Run a scheduled PR-backlog sweep (autonomous batch mode).

---

## Capabilities

- Scan the open PR backlog using the bundled read-only scanner.
- Compute one of five verdicts (`advance`, `continue`, `defer`, `close`, `escalate`)
  per PR using the signal-to-verdict rules in `references/rubric.md`.
- Apply a `0-pr:*` disposition label (if it exists in the repo) and an idempotency
  label (`0-auto:pr-audited`) to each processed PR.
- Post (or upsert) a machine-readable marker comment containing the verdict block.
- Record `defer` verdicts with a named revisit trigger (`revisit-by:` or `blocked-on:`).
- Route `advance` PRs to the correct downstream skill (`prep-pr`, `split-pr`, or
  `queue-pr`) based on CI, merge state, and size signals.
- Escalate `close` and genuinely ambiguous `escalate` verdicts to a maintainer without
  guessing or acting.

---

## Quick Start

Run the read-only scanner to collect candidates:

```
bash .claude/skills/triage-pr/scripts/scan-prs.sh --stale-days 14
```

The scanner prints a `Scope:` line followed by one TSV row per candidate PR:

```
PR  STATE  CI  MERGE  REVIEW  AGE_D  CHURN  FILES  AUTHOR  TITLE
```

Then compute a verdict for each row using `references/rubric.md` and apply the
procedure in `references/methodology.md`.

For a different staleness threshold or a non-default repository:

```
bash .claude/skills/triage-pr/scripts/scan-prs.sh --repo UMEP-dev/SUEWS --stale-days 7
```

For JSON output (for scripting):

```
bash .claude/skills/triage-pr/scripts/scan-prs.sh --json 2>/dev/null | jq .
```

---

## Safety Rules

- Drafting or scanning is not permission to post, label, or flip PR state. In
  interactive mode, present the verdict block for each PR and wait for explicit
  approval before any write.
- Reading a PR's signals is not permission to act on the PR.
- Privacy is a hard invariant on every outward write. Apply the Privacy scrub gate
  (below) before any comment or label rationale reaches GitHub.
- `triage-pr` NEVER closes a PR, NEVER merges, and NEVER enqueues. These are
  always human-gated.
- Do not create labels. Apply labels only if they already exist in the repository.

---

## Label Model

### Disposition labels (mutually exclusive, one per PR per run)

| Verdict | Label |
|---------|-------|
| `advance` | `0-pr:advance` |
| `continue` | `0-pr:continue` |
| `defer` | `0-pr:deferred` |
| `close` | `0-pr:close-proposed` |
| `escalate` | `0-pr:escalate` |

These labels are mutually exclusive. Before applying any new `0-pr:*` label, remove
every other `0-pr:*` label already on the PR (there should be at most one, but remove
all if more than one is present). Never leave two `0-pr:*` labels on a PR simultaneously.

### Bookkeeping label

`0-auto:pr-audited` -- marks that an autonomous pass has processed this PR.
It is additive: apply it once and leave it. Do not remove it on a re-run.
It enables idempotency: batch re-runs skip PRs carrying this label whose
last substantive activity predates the previous audit timestamp.

### Classification label: `0-physics:change`

Independent of the disposition label, apply `0-physics:change` (if it exists in
the repo) when the PR diff touches physics source (`suews_phys_*.f95`, the Rust
physics backend), moves a reference fixture
(`test/fixtures/data_test/sample_output.csv.gz`,
`.../stebbs_test/sample_output_stebbs.csv`, `test/fixtures/benchmark1/*.pkl`), or
changes a physics-affecting data-model default. It is additive (not part of the
mutually-exclusive `0-pr:*` family) and activates the evidence requirements in
`.claude/rules/physics-change-evidence.md`: a `## Scientific evidence` PR-body
section, domain-owner sign-off, and the full `-m physics` CI tier. When you apply
it, note in the marker comment that the PR needs the evidence section and owner
sign-off (STEBBS -> `@yiqing1021`) before it can advance.

### Priority

Priority (P0/P1/P2) is **not** a label. It lives in the verdict block only.

### Maintainer one-off setup

Create these six labels once in the repository:

```
0-auto:pr-audited
0-pr:advance
0-pr:continue
0-pr:deferred
0-pr:close-proposed
0-pr:escalate
```

`triage-pr` applies labels that already exist and NEVER creates them. If a label is
absent, the verdict is recorded in the marker comment and handoff block only, with a
note that the label is missing.

---

## Privacy Scrub Gate

Before any outward write (marker comment, label rationale, or any text posted to
GitHub), scan the model-authored distilled text for:

- Internal tracker IDs (`PER-\d+` and similar private references).
- Absolute local or home-directory paths (`/Users/...`, worktree paths).
- Private host or infrastructure names (internal compute servers and the like).

On a hit in distilled text, redact the offending token before posting.

If a private token appears inside a verbatim quote (for example from the PR description
or a commit message) that must be preserved to make the finding actionable, replace it
with neutral phrasing or escalate to a maintainer rather than post as-is.

This operationalises "Privacy is a hard invariant on every outward write" from
`.claude/rules/autonomous-workflow.md`.

---

## Gating Model -- Three Tiers

### Auto-applicable (additive, reversible, or self-scoped)

May run without human approval:

- Run the read-only scanner (`scan-prs.sh`).
- Compute verdicts per `references/rubric.md`.
- Post or upsert the marker comment on each PR (idempotent: replace an existing
  `<!-- triage-pr:verdict ts=... -->` comment rather than duplicating it).
- Apply the matching `0-pr:*` label using the mutual-exclusion swap described above,
  if that label exists in the repo.
- Apply the `0-auto:pr-audited` label if it exists in the repo.

### Confidence-gated

Applied without human approval only when all stated gates hold; escalated otherwise:

- Flipping a draft PR to "ready for review" -- only when ALL advance gates in
  `references/rubric.md` (Part A) hold AND the PR is one the agent created this run
  (not a third-party PR).

### Human-gated (always escalate; never auto-apply)

- Closing or superseding a PR.
- Any merge or queue-entry (`queue-pr --enqueue` or `gh pr merge`).
- Posting to, marking ready, or force-pushing a PR the agent does not own.
- Editing the body or metadata of a PR the agent does not own.

### The verdict-vs-action split

**A verdict is a disposition; enacting it is a separate tiered action.**

An `advance` verdict on a third-party PR still gets the `0-pr:advance` label (auto)
and the recommendation in the marker comment, but the ready-flip and downstream
handoff are human-gated. The `Next:` field in the verdict block says "recommend
marking ready / hand to `<route>`". The verdict is NOT downgraded to `escalate`
because the enacting action is human-gated -- only the action waits; the verdict
is recorded immediately.

---

## Workflow

Read `references/methodology.md` for the full step-by-step procedure. The summary is:

1. Run `scan-prs.sh` to collect the candidate set and the `Scope:` line.
2. For each candidate PR, check idempotency (already audited with no new activity
   and no fired revisit trigger -> record as `noop`, stop).
3. Compute the verdict using `references/rubric.md`.
4. Apply the auto-tier: upsert the marker comment; swap the `0-pr:*` label; apply
   `0-auto:pr-audited`.
5. State the route and next step in the verdict block:
   - `advance` -> route to `prep-pr`, `split-pr`, or `queue-pr`; record priority.
   - `continue` -> note live WIP; route is `fix-issue` or author notification.
   - `defer` -> record `Revisit-by:` or `Blocked-on:` in the verdict block.
   - `close` -> present `0-pr:close-proposed` for maintainer decision; no further
     action.
   - `escalate` -> state conflicting signals explicitly; present for maintainer
     resolution.

---

## Autonomous Mode

By default `triage-pr` is interactive: it drafts and waits for explicit approval
before any write. Autonomous mode is an opt-in contract for unattended orchestration
(a scheduled backlog sweep or an upstream orchestrator). It never asks a human
mid-run.

Opt-in standing approval covers ONLY the auto-applicable tier. The human-gated tier
(close, merge/enqueue, third-party PR edits) always escalates in autonomous mode;
there is no standing approval for those actions.

In autonomous batch mode:

- Bound the input set explicitly with `--limit`. Emit the scanner's `Scope:` line
  verbatim so a silent cap is never read as full coverage.
- Apply the idempotency check per PR before computing a verdict. A `noop` PR (already
  audited, no new activity) is counted as `noop` in coverage, never as `skipped`.
- Accumulate verdict counts across the batch and emit a `Summary:` line at the end.
- A PR that cannot be fetched or processed is recorded as `skipped` with a reason in
  the `Next:` field. Never silently drop a PR.
- Coverage equation (must hold exactly):
  `advance + continue + defer + close + escalate + noop + skipped = K`
  where K is the fetched set size. Full coverage of the open backlog is assertable
  only when `truncated: no`.

Any review -> revise loop inside a batch run follows `.claude/rules/review-convergence.md`
(round cap + oscillation guard). A single PR must not consume the batch; escalate and
move on if it does not converge within the cap.

---

## Output Format

Use the machine-readable handoff verdict block defined in `references/methodology.md`.
The block is reproduced here for convenience; keep it identical to the canonical form
in that file:

```
[triage-pr] verdict
PR: #<n>
Verdict: advance | continue | defer | close | escalate
Priority: P0 | P1 | P2 | -        (advance only)
Route: prep-pr | split-pr | queue-pr | fix-issue | -
Reason: <one line>
Revisit-by: YYYY-MM-DD | -        (defer only)
Blocked-on: #<n> | -             (defer only)
Applied: comment | label:<0-pr:...> | ready | none
Next: <downstream action / the human question>
```

Downstream skills (`prep-pr`, `split-pr`, `queue-pr`, `fix-issue`) branch on `Verdict`
and `Route`. They do NOT need to parse prose.

---

## References

- `references/rubric.md` -- Signal-to-verdict decision rule (five verdicts, priority,
  near-green CI definition).
- `references/methodology.md` -- Step-by-step procedure, marker and idempotency
  mechanics, machine-readable verdict block, batch coverage accounting.
- `scripts/scan-prs.sh` -- Read-only PR signal scanner; emits TSV candidates for the
  router.
- `.claude/rules/autonomous-workflow.md` -- Pipeline contract: gating tiers, the
  always-human-gated list, and the merge gate.
- `.claude/rules/work-sizing.md` -- Right-sized-PR criteria; referenced by the `advance`
  route decision (size gate -> `split-pr` vs `queue-pr` vs `prep-pr`).
- `.claude/rules/review-convergence.md` -- Bounded loop discipline for any
  review -> revise cycle inside a batch run.
