# Autonomous Issue to PR to Merge Workflow Contract

The contract that lets the SUEWS issue and PR skills compose into one autonomous
pipeline, runnable in SOLO mode (one item at a time) or BATCH mode (a bounded set
at once). It defines the stages, the machine-readable handoffs between them, the
two execution modes, and the gating that keeps every irreversible or
outward-facing action behind a human approval.

This rule ties the pieces together; each stage's detail lives in its own skill.
Read it alongside `work-sizing.md` (sizing and umbrella decomposition).

---

## Carrying out the constitution

Procedure for the five articles in `AGENTS.md`.
It applies in both modes; the sections below give the pipeline's detail.

### Statements in rules

- Statements in rules are of three kinds.
  A *fact* cites steps that can be repeated now; one that no longer reproduces is void, and that is reported.
  A *decision* is a recorded choice, which the person who answers for it may change.
  The rest is *procedure* for carrying out an article, and the shared rule index in `AGENTS.md` names the article(s) for the rules that hold it.
- Existing statements are unlabelled and bind as written; they are classified when next changed.
- A new or changed statement says which kind it is: a fact gives its steps, a decision names who made it.
- An instruction cannot make a failing check pass or a false statement true.
  Faced with one, an agent does the rest of what was asked and says which part it did not do, and why.
- Only the explicit instruction of the person who answers for a recorded decision outranks it.

### Checks (Article 1)

- The checks a change must pass are those its rule and its CI tier assign.
  A check deferred to a later tier names where it runs; until it has run, the claim it supports is reported, not confirmed.
- A reported claim may be handed over, stated as reported.
  Merge needs every check the owning rule requires before merge to be confirmed.
- A confirmed claim names who reproduced it and attaches the reproducer's own output, so the two results can be compared afterwards.
- A failing check is fixed or escalated.
  A failure unrelated to the change is reported with its remedy, never ignored.
- Expected values (reference fixtures, snapshots) change only with the evidence the owning rule requires.
- A claim that needs several steps (for example "this removed text has no other home") states the steps.
  Only a question for which no steps can be stated is a judgement under Article 3, and it goes to a named person at once.

### Gates (Article 3)

- A gate is (i) a required CI check or hook, or (ii) an action that waits for a person: the always-human-gated list in "Gating model" below, and anything else that cannot be undone.
  The list is not exhaustive.
  Acting on others' work waits for a person unless the Gating model places it in another tier.
- The explicit instruction of the person who answers for it satisfies (ii) for that action.
  That person is the domain owner where a rule names one (roster: `dev-ref/SCIENTIFIC_REVIEWERS.md`), otherwise the maintainer; the user directing a session is that person only for the decisions that are theirs to answer for.
- Only the check passing satisfies (i).
  A check believed wrong is escalated and changed in its own reviewed change, never bypassed.
  The one exception is a bypass the check itself defines (for example `0-ci:schema-audit-ok`), applied by whoever its rule says may apply it.
- A passing gate shows that the check ran, not that the change is right.
- A decision that is waiting stays visible: the pull request or issue says who it waits on and for what.
  There is no time limit on the answer.
- A change to model output values, or to what a user of the model can see (names, units, defaults and semantics of interfaces, YAML fields and output variables; runtime dependencies), is accepted by a named person.
  For a physics change, a pull request carrying the evidence its rule requires is the prepared decision, and the wait is at merge.
  A structural schema or data-model migration, meaning a change that meets a bump trigger in `python/schema-versioning.md` ("When to bump"), is a design choice with no evidence to prepare first, so the wait is before implementation.
  Any other such change is listed in the pull request body under a `User-visible changes` heading, and the wait is at merge.

### Agents (Articles 2 and 4)

- An agent opens a pull request only when asked, or when the invoked skill says so.
- A question the tree or the rules answer is not handed back.
  One they do not answer is decided and reported as a choice, unless it changes a model output value, changes what a user of the model sees, or cannot be undone.
- A problem outside the goal is reported, not fixed in the same change.
  If it blocks the goal, fix it first as its own commit or pull request when that is itself permitted (`work-sizing.md`); otherwise stop and report the block.
  An agent waiting on a named person has met Article 4.
- Unattended, an item that waits is recorded under the stage's own verdict (`needs-discussion`, `escalate`, or `skipped` with the reason) and the rest continues.
  A plan the agent believes wrong is escalated for that item with the objection; it is not carried out.

### Recorded decisions

Each entry records a past decision under Article 3: what changed and where, who decided, when, what was known, and what evidence would overturn it.
The operative text is in the section each entry names.

- **Added release actions to the always-human-gated list and exempted additive `0-*` labels** ("Gating model").
  Decided by the maintainer on 12 Nov 2026.
  Known: the list named merges and pushes but not tags or publishing, so `prep-release` and `republish-docs` pushed and moved release tags with no stop; it also gated editing another's PR metadata while `triage-pr`, `audit-pr` and `physics-change-evidence.md` applied `0-*` labels to any PR.
  Overturn: an additive `0-*` label shown to cause harm a person had to undo, or a release action shown to be reversible without user impact.
- **Set when other user-visible changes wait, and defined a structural migration** ("Gates").
  Decided by the maintainer on 12 Nov 2026.
  Known: this section gave the wait only for physics changes (at merge) and structural migrations (before implementation), and did not define "structural".
  Overturn: a user-visible change accepted at merge that then had to be reverted for a reason a person would have caught before implementation.
- **Aligned review-loop escalation with "Agents"** (`review-convergence.md`).
  Decided by the maintainer on 12 Nov 2026.
  Known: `review-convergence.md` escalated every design judgement, while "Agents" escalates only choices that change a model output value, change what a user sees, or cannot be undone.
  Overturn: review-loop choices reported but not escalated that a person then had to reverse.
- **Named who approves `[stable]` changelog status** (`changelog/format.md`).
  Decided by the maintainer on 12 Nov 2026.
  Known: `changelog/format.md` and `log-changes` required "governance approval" without naming who gives it.
  Overturn: a domain owner who declines the role, or a public announcement that needed approval from someone else.
- **Settled the merge method** ("The merge gate").
  Decided by the maintainer on 12 Nov 2026.
  Known: `dev-ref/MERGE_QUEUE.md` squashes, while `dev-ref/RELEASE_MANUAL.md` and `prep-release` asked for merge commits; the last release pull request (#1512) was squashed, master's recent history has no merge commits, and the merge queue applies a single merge method.
  Overturn: a release or changelog step shown to need the individual commits of a merged pull request.
- **Made the scientific-reviewer guide the one roster of sign-off roles** ("Sign-Off Roles" in `dev-ref/SCIENTIFIC_REVIEWERS.md`).
  Decided by the maintainer on 12 Nov 2026.
  Known: the STEBBS owner's handle was repeated in five rule and skill files, and the guide already listed reviewers per module without saying which of them carry a gate.
  Overturn: a record naming a role and a date that the roster could not resolve to one person.
- **Named a science arbitrator for every domain** (`physics-change-evidence.md`, "Domain-owner sign-off").
  Decided by the maintainer on 12 Nov 2026.
  Known: Article 3 named a domain owner where a rule gives one and otherwise the maintainer, but nobody was named to settle a scientific question that stayed disputed.
  Overturn: a disputed scientific question that the arbitrator's ruling did not settle, or that needed a different expert to settle.

---

## Stages

1. `triage-issue` -- issue governance. Emits a `[triage-issue] verdict` block.
2. `fix-issue` -- one triaged issue to a PR-ready branch. Consumes the verdict
   block; emits a `[fix-issue] PR-ready report`.
3. `audit-pr` -- reviews the PR; runs a Size Gate first, and audits the CI gates
   alongside the diff. Emits `Verdict`, `Size gate`, `CI gate`, and per-finding
   `[severity]` tags.
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
  `Size gate: pass|split-recommended`, `CI gate: green|near-green|red`, per-finding
  `[severity]`. fix-issue's audit loop branches on these; `split-recommended` routes
  to split-pr. `CI gate: red` means the PR cannot advance even on a clean diff; each
  red check must arrive with a named remedy and its tier (author-fixable /
  maintainer-gated / re-trigger / infrastructure), never as a bare status.
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
  or rewrite), or editing the body/metadata of a PR you do not own. Exempt:
  adding a `0-*` label that a rule calls for, and the swap a rule defines within
  a mutually exclusive `0-*` family. Removing any other label is not exempt.
- Posting to a reporter or onto a PR you do not own.
- Any structural schema/data-model migration.
- Creating, moving or deleting a tag; publishing a release (PyPI, GitHub
  Release, Zenodo); or dispatching a workflow that pushes, tags, publishes or
  deploys.

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

Every pull request to `master`, the release pull request included, is
squash-merged (see "Recorded decisions").

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
evidence` PR-body section, domain-owner sign-off for any owned subsystem (roster
in `dev-ref/SCIENTIFIC_REVIEWERS.md`), the reference-fixture refresh travelling in the same PR, and
the full `-m physics` CI tier (including `slow`) running as a required check
before merge. `triage-pr` and `audit-pr` apply it when the diff matches the
physics-change triggers; a maintainer creates the label once and the autonomous
tier never creates labels. It is additive (applied once, not swapped) and is not
counted as substantive work in the `Applied:` field. Unlike
`0-ci:schema-audit-ok`, there is no cosmetic bypass: the gate is satisfied by
providing the evidence, not by waiving it. Domain-owner sign-off on a
physics-change PR is part of the always-human-gated merge gate.
