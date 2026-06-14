# Autonomous Issue to PR to Merge Workflow Contract

The contract that lets the SUEWS issue and PR skills compose into one autonomous
pipeline, runnable in SOLO mode (one item at a time) or BATCH mode (a bounded set
at once). It defines the stages, the machine-readable handoffs between them, the
two execution modes, and the gating that keeps every irreversible or
outward-facing action behind a human approval.

This rule ties the pieces together; each stage's detail lives in its own skill.
Read it alongside `work-sizing.md` (sizing and umbrella decomposition).

---

## Stages

1. `triage-issue` -- issue governance. Emits a `[triage-issue] verdict` block.
2. `fix-issue` -- one triaged issue to a PR-ready branch. Consumes the verdict
   block; emits a `[fix-issue] PR-ready report`.
3. `audit-pr` -- reviews the PR; runs a Size Gate first. Emits `Verdict`,
   `Size gate`, and per-finding `[severity]` tags.
4. `split-pr` -- carves an oversized PR into a stacked series. Emits a
   `[split-pr] stack plan`. Invoked when audit-pr's Size Gate recommends a split.
5. `queue-pr` -- orders ready PRs and (human-gated) enters the merge queue.

`work-sizing.md` is the shared sizing authority for the four issue/PR skills
(stages 1-4: triage-issue, fix-issue, audit-pr, split-pr). queue-pr orders ready
PRs and does not itself apply the sizing criteria.

---

## Handoff vocabulary (machine-readable)

Each stage ends with a parseable block so the next stage or orchestrator branches
without reading prose:

- triage-issue -> fix-issue: `Verdict: triaged|needs-discussion|skipped`,
  `Readiness:`, `Applied:`, `Blocking question:`, `Next:`. fix-issue proceeds on
  any `Verdict: triaged` (which triage-issue derives from `ready`, `needs-summary`,
  or a `misframed` issue resolved to triaged via a gated rewrite); every
  `needs-discussion` (which absorbs needs-repro / needs-scope / needs-acceptance /
  superseded / duplicate) does NOT implement. `skipped` is a batch-coverage
  accounting verdict, never handed to fix-issue as an implementation input.
  `Applied:` drives the batch coverage split; `Blocking question:` / `Next:` carry
  the needs-discussion reason fix-issue echoes.
- needs-split -> umbrella decomposition (`work-sizing.md`), not implementation.
  The five-stage pipeline terminates here at "umbrella proposed"; a human creates
  the umbrella and sub-issues, and sweeping the resulting waves is outside this
  contract (each child re-enters the pipeline as its own issue).
- fix-issue -> audit-pr / merge: `[fix-issue] PR-ready report` with `Audit loop:`
  and `Remaining:`. fix-issue terminates at PR-ready; it never merges.
- audit-pr -> fix-issue / split-pr: `Verdict: clean|needs-attention`,
  `Size gate: pass|split-recommended`, per-finding `[severity]`. fix-issue's audit
  loop branches on these; `split-recommended` routes to split-pr.
- split-pr -> queue-pr: `[split-pr] stack plan` with `Completeness:` and
  `Concerns -> Children`.

---

## Execution modes

### Solo

One issue or PR at a time, a human typically present. A stage may pause for
approval and resume. This is the default and the safe baseline.

### Batch

A bounded set processed unattended. Additional requirements over solo:

- Bound the input explicitly and surface it. Every batch stage emits a `Scope:`
  line (filter, returned/total, truncation flag) so a silent cap cannot read as
  full coverage. Never rely on a tool's default limit.
- Idempotency. triage-issue marks processed issues with `0-auto:audited`; batch
  re-runs skip the redundant work for issues that carry it and have not been
  updated since. An idempotency-skipped issue is still counted as triaged-noop in
  the aggregate, never as `skipped`.
- Coverage accounting. The batch aggregate partitions the returned set K and must
  sum to it: triaged-applied + triaged-noop + needs-discussion + skipped = K. Full
  backlog coverage holds only when `truncated: no`; otherwise the (total - K)
  issues beyond the limit remain unswept.
- No silent drop. An item that cannot be processed is recorded `skipped` with a
  reason.
- Bounded loops. Any review -> revise -> re-review loop follows
  `.claude/rules/review-convergence.md`: a round cap, a signature-based
  oscillation guard, and an explicit termination criterion so one item cannot
  consume the batch. fix-issue's audit loop (cap 3) is the per-PR instance.

---

## Gating model

Every stage partitions its actions into three tiers:

- Auto-applicable: additive, reversible, read-only, or self-scoped. May run
  unattended (e.g. triage-issue's maintainer-summary comment plus label; audit-pr's
  read-only review and verdict computation -- drafting only, posting any comment is
  human-gated per the always-human-gated list below; queue-pr's scan and ordering).
- Confidence-gated: applied unattended only when stated gates hold, else escalate
  (e.g. triage-issue's body-rewrite and retitle, which must preserve the original
  report and raiser).
- Human-gated: never auto-applied; the autonomous terminal is escalate-to-human.

The following are ALWAYS human-gated, in every stage and both modes:

- Merging or enqueuing a PR (`queue-pr --enqueue` / any `gh pr merge`).
- Closing, superseding, or marking an issue duplicate.
- Creating issues or parent/child links (umbrella decomposition).
- Pushing to a branch the agent did not create this run (including any force-push
  or rewrite), or editing the body/metadata of a PR you do not own.
- Posting to a reporter or onto a PR you do not own.
- Any structural schema/data-model migration.

Privacy is a hard invariant on every outward write: no internal tracker IDs,
absolute local paths, or private host names reach a public issue / PR / comment /
commit. Scrub model-authored distilled text; force escalation on a hit inside
preserved verbatim content.

---

## The merge gate

The pipeline's terminal action -- merge -- is always a separate, explicitly
approved step. fix-issue stops at PR-ready; split-pr hands a stack to queue-pr;
queue-pr enters the merge queue only on explicit per-run approval. Handing work
downstream never transfers merge authority. An autonomous batch run therefore
converges to "PRs ready plus verdicts emitted", and a human authorises the merge.

---

## The 0-auto:audited label

A repo label in the `0-` automation namespace marking that an autonomous pass has
processed an issue. triage-issue applies it (if it exists) to every issue reaching
a `triaged` or `needs-discussion` verdict; batch mode queries it to skip
already-processed issues unless updated since. It is bookkeeping, so it is not
counted as substantive work in the `Applied:` field. A maintainer creates the
label once; the autonomous tier never creates labels.
