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

`triage-pr` is the PR-axis sibling of triage-issue. Where triage-issue grooms the
issue backlog for readiness, triage-pr grooms the draft and stalled PR backlog for
disposition. It is a router, not a reviewer: it assigns each PR one of five verdicts
(`advance`, `continue`, `defer`, `close`, `escalate`) and routes live PRs into the
existing stages -- `advance` -> audit-pr / split-pr / queue-pr (and the user-level
prep-pr where applicable); `continue` -> fix-issue / author. `defer` parks the PR
with a named revisit trigger; `close` and `escalate` are always human-gated. It
never merges and never closes (terminates at "disposition assigned plus live PRs
handed off"). The always-human-gated list in the Gating model section already
covers its close and escalate terminals.

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
- triage-pr -> downstream skills: `[triage-pr] verdict` block per PR. Downstream
  skills branch on `Verdict` and `Route`; they do not parse prose. The canonical
  block (reproduce exactly):

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

  `Verdict: close` and `Verdict: escalate` are human terminals: labels
  `0-pr:close-proposed` / `0-pr:escalate` are applied; no further automated
  action. Downstream skills pick up `Verdict: advance` (via `Route:
  prep-pr|split-pr|queue-pr`) and `Verdict: continue` (via `Route: fix-issue`)
  without reading prose.

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

## The 0-auto:pr-audited label and 0-pr:* disposition family

`0-auto:pr-audited` is the PR-axis equivalent of `0-auto:audited`. triage-pr
applies it (if it exists) to every PR that completes a triage run; batch mode
queries it to skip PRs with no substantive activity since the previous audit. It
is additive: applied once and left in place on re-runs. Not counted as substantive
work in the `Applied:` field. A maintainer creates the label once; the autonomous
tier never creates labels.

The `0-pr:*` disposition family carries the current triage-pr verdict for a PR:

| Verdict | Label |
|---------|-------|
| `advance` | `0-pr:advance` |
| `continue` | `0-pr:continue` |
| `defer` | `0-pr:deferred` |
| `close` | `0-pr:close-proposed` |
| `escalate` | `0-pr:escalate` |

These labels are mutually exclusive: before applying any `0-pr:*` label, remove
every other `0-pr:*` label already on the PR (state machine -- one label at a
time). A maintainer creates all six labels once; triage-pr applies those that
already exist and never creates labels. If a label is absent, the verdict is
recorded in the marker comment and verdict block only, with a note that the label
is missing.

## The 0-physics:change classification label

`0-physics:change` is a classification label in the `0-` automation namespace,
independent of the `0-pr:*` disposition family (a PR can carry both). It marks
that a PR changes model physics or moves a reference output, and it activates the
evidence gate in `.claude/rules/physics-change-evidence.md`: a `## Scientific
evidence` PR-body section, domain-owner sign-off for any owned subsystem (STEBBS
-> `@yiqing1021`), the reference-fixture refresh travelling in the same PR, and
the full `-m physics` CI tier (including `slow`) running as a required check
before merge. `triage-pr` and `audit-pr` apply it when the diff matches the
physics-change triggers; a maintainer creates the label once and the autonomous
tier never creates labels. It is additive (applied once, not swapped) and is not
counted as substantive work in the `Applied:` field. Unlike
`0-ci:schema-audit-ok`, there is no cosmetic bypass: the gate is satisfied by
providing the evidence, not by waiving it. Domain-owner sign-off on a
physics-change PR is part of the always-human-gated merge gate.
