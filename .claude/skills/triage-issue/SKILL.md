---
name: triage-issue
description: Use whenever auditing, clarifying, rewriting, or batch-reviewing SUEWS GitHub issue descriptions so they become maintainer-ready. Trigger for requests like "audit this issue", "make this issue agent-ready", "rewrite issue #123", "clean up open issues", "summarise comments into the issue body", "preserve the original report", or "triage SUEWS backlog". This skill preserves issue-author context, summarises discussion, drafts a clarified issue body, and requires explicit approval before editing GitHub.
---

# Triage Issue

Turn workflow-originated SUEWS issues into maintainer-ready engineering work
items without erasing the original issue author's contribution. Many good
issues start from a real modelling/API workflow and only become technically
clear after comments, reproductions, and maintainer discussion. Your job is to
capture that transition.

## Triggers

Use this skill when the user asks to audit, clarify, rewrite, summarise,
supersede, or batch-triage SUEWS GitHub issues. It is especially relevant when
an issue body no longer reflects discoveries made in comments.

## Capabilities

- Audit an issue body against a type-specific rubric.
- Summarise comments into maintainer-ready context.
- Preserve the original report while drafting a clarified issue body.
- Propose issue title, label, readiness, and follow-up changes.
- Recommend splitting an oversized or multi-concern issue into focused child
  issues under a parent, and propose that decomposition
  (see `.claude/rules/work-sizing.md`).
- Apply approved GitHub edits using `gh`.

## Scripts

No bundled script is required. Use the GitHub CLI commands below.

## Quick Start

Inspect one issue:

```bash
gh issue view <number> --repo UMEP-dev/SUEWS \
  --json number,title,body,labels,assignees,state,author,comments,url,createdAt,updatedAt
```

Audit a list (set an explicit `--limit`; gh defaults to 30 and silently
truncates a larger backlog):

```bash
gh issue list --repo UMEP-dev/SUEWS --state open --limit 200 \
  --json number,title,labels,assignees,author,updatedAt,url
# independent open total for the coverage check (do NOT derive it from the capped list):
gh api -X GET search/issues -f q="repo:UMEP-dev/SUEWS is:issue is:open" --jq .total_count
```

Apply an approved rewrite:

```bash
gh issue edit <number> --repo UMEP-dev/SUEWS --body-file <approved-body.md>
```

## Safety Rules

- In interactive mode, do not edit an issue, labels, assignment, title, or
  comments unless the user explicitly approves that exact action. In autonomous
  mode, the opt-in contract in "Autonomous Mode" is the standing approval for the
  auto-applicable and confidence-gated tiers only; the human-gated tier always
  escalates to `needs-discussion`.
- Preserve the original issue body in the proposed rewrite unless the user asks
  for a different archival strategy.
- Treat comments as evidence, not as permission to overwrite the issue author's
  framing silently.
- Acknowledge issue-author contributions when triage changes the framing.
- Do not introduce internal tracker IDs (for example Linear references), private
  infrastructure paths, or other non-public references into the rewritten body or
  any posted comment. The issue surface is public; keep it public-safe.
- Do not close unclear issues only because they are unclear. Either ask for the
  missing information, add a maintainer summary, or propose a clearer
  superseding issue.
- Reading or drafting is not permission to post.

## Workflow

1. Fetch the issue body, labels, assignees, state, comments, and linked PR
   context needed to understand scope.
2. Classify the issue type and readiness using `references/rubric.md`.
3. Audit what is missing from the title/body and what has only been clarified
   in comments.
4. Preserve the original body, summarise the discussion, and draft a clearer
   maintainer-ready body using `references/rewrite-template.md`.
5. Present the audit, proposed title/body, label/status suggestions, and open
   questions for approval.
6. Apply approved edits with `gh issue edit --body-file`; optionally post an
   approved explanatory comment.

Read `references/methodology.md` for the full step-by-step procedure and
output format before performing a rewrite or batch audit.

## Output Format

Use the single-issue and batch formats in `references/methodology.md`. In
autonomous mode, end with the machine-readable verdict block defined there.

## Batch Audit Guidance

- Start with recently updated open issues or a bounded label set unless the user
  explicitly asks for the whole backlog.
- Do not attempt to rewrite many issues in one pass. First produce an audit
  list and group follow-up actions.
- Prioritise issues that are in progress, high priority, linked to active PRs,
  or have long comment threads where the current body is stale.
- Suggest follow-up categories: add reproduction, add maintainer summary,
  clarify scope, split into focused child issues, supersede, close as duplicate,
  or leave as ready.
- Whenever the input set is bounded (label filter, recency window, `--limit`, or
  gh's default cap), emit a `Scope:` line stating the filter and the returned
  count against an independently-obtained total open count (see Quick Start), and
  flag truncation when the returned count K is less than that total (`K == --limit`
  is a secondary corroborating signal only).
- Skip the redundant GitHub writes and re-analysis for issues already carrying the
  `0-auto:audited` label whose `updatedAt` has not advanced since; re-audit only
  those updated since. Such an idempotency-skipped issue still emits a verdict
  block (`Verdict: triaged`, `Applied: none`), counted as triaged-noop -- "skip"
  means skip the redundant work, not drop the issue from output.

## Autonomous Mode

By default this skill is interactive: it drafts and waits for explicit approval
before any GitHub edit. Autonomous mode is an opt-in contract for unattended
orchestration (a scheduled backlog sweep, or an upstream skill that calls
triage). It never asks a human mid-run. Every issue ends in exactly one of two
terminal verdicts:

- `triaged`: the issue is now maintainer-ready and only safe, additive,
  reversible changes were applied.
- `needs-discussion`: the next action needs human judgement or external input, so
  nothing destructive is applied and the skill hands back the specific question.

### Action tiers

Auto-applicable (additive, reversible):

- Before composing any text to post, run the Privacy scrub gate (below) over the
  distilled content.
- Post one maintainer-summary comment carrying the current framing distilled from
  the discussion, tagged with the stable marker `<!-- triage-issue:summary -->`
  for idempotency.
- Apply a readiness label only if that exact label already exists in the repo.
  Never create labels.
- Mark the issue as processed: apply the `0-auto:audited` label if it exists in
  the repo. It records that an autonomous pass handled the issue so batch re-runs
  can skip it unless it is later updated. It is bookkeeping, not substantive work,
  so it is not listed in the verdict `Applied:` field. Do not create the label.

### Privacy scrub gate

Before posting any comment or writing any rewritten body, scan the
model-authored distilled text (Maintainer summary, Discussion summary) for:
internal tracker IDs (`PER-\d+` and similar private references), absolute local
paths (`/Users/...`, home-directory paths), and private host/infrastructure names
(internal compute servers, worktree managers, and the like). On a hit in distilled
text, redact the
offending token. The verbatim "Original report" block must NOT be redacted (that
would break the preservation gate): if the scan hits a private token inside the
verbatim original body, force `needs-discussion` rather than post or rewrite.

Confidence-gated (body rewrite and matching retitle, auto-applied only when every
gate holds):

- Replace the issue body with the maintainer-ready rewrite ONLY when all of:
  - the new framing is already established in the thread (for example stated by a
    maintainer), not a novel hypothesis the skill is introducing;
  - the rewrite preserves every substantive claim from the original report; and
  - the rewrite embeds the preserved "Original report" section below.

  If any gate fails, do not rewrite -> `needs-discussion`.
- A confidence-gated body rewrite MUST preserve the original body and its raiser.
  Open the rewritten body with the marker `<!-- triage-issue:rewrite -->` and
  include, verbatim, the "Original report" section from
  `references/rewrite-template.md`, which carries the original body content, the
  raiser's `@handle`, the creation date, and the original title. Never drop the
  author attribution: GitHub keeps the issue's original author field, but the body
  must still make provenance visible to a reader.
- Retitle the issue when the confident reframe makes the current title
  misleading, under the same gates: the new title must reflect framing already
  established in the thread, and a retitle is applied only together with the body
  rewrite (never standalone). The original title is preserved in the "Original
  report" section and remains in GitHub's edit timeline.

Human-gated (always force `needs-discussion`, never auto-applied):

- Assignment.
- Close, supersede, or mark duplicate.
- Any reporter-facing request (for example "please add a reproduction").

### Verdict decision rule (deterministic)

Pre-condition (before the readiness mapping): if the issue cannot be fetched or
processed, or is excluded by the scope bound, the verdict is `skipped` with the
reason in `Blocking question`/`Next` and nothing applied; such issues never enter
the readiness mapping (they have no rubric readiness). Otherwise map the audited
rubric readiness status to a verdict:

- `ready` -> `triaged` (apply label-if-exists, otherwise nothing).
- `needs-summary` -> `triaged` (post the maintainer-summary comment; label-if-exists).
- `needs-repro`, `needs-scope`, `needs-acceptance` -> `needs-discussion`
  (state the precise missing item; do not ping the reporter).
- `needs-split` -> `needs-discussion` (propose the decomposition into focused
  child issues per `.claude/rules/work-sizing.md`; never auto-create issues or
  parent/child links).
- `misframed` -> `triaged` via a confidence-gated body rewrite (plus a matching
  retitle when the current title is misleading) when every gate above holds;
  otherwise `needs-discussion`.
- `superseded`, `duplicate` -> `needs-discussion` (close/link needs approval).

### Idempotency and re-runs

- Before posting a summary comment, scan existing comments for
  `<!-- triage-issue:summary -->`. If present and the thread has not materially
  changed, verdict is `triaged` with nothing applied. If present but the framing
  is stale, update that comment in place rather than posting a second one.
- Before a body rewrite, check whether the body already starts with
  `<!-- triage-issue:rewrite -->`. If present, the issue has already been
  rewritten: update the maintainer-ready sections in place and never nest a second
  "Original report" block. The preserved original body and `@handle` from the
  first rewrite are authoritative and must survive every later rewrite untouched.

### Batch in autonomous mode

- Process the bounded input set, apply only auto-tier actions, and accumulate one
  verdict per issue. Body rewrites, closes, and supersedes are recorded as
  `needs-discussion`, never applied in bulk. Stop when the set is exhausted and
  emit the scope line and partitioned aggregate defined in `methodology.md`.
- Idempotency across runs uses the `0-auto:audited` label as the queryable marker
  (complementing the in-body `<!-- triage-issue:* -->` markers): skip the
  redundant work for issues that carry it and have not been updated since. Such an
  idempotency-skipped issue still reports as triaged-noop (`Applied: none`), never
  as the `skipped` verdict.
- An issue that cannot be fetched/processed, or is excluded by the bound, gets the
  `skipped` verdict (distinct from the idempotency skip above) with the reason,
  never silently dropped.

The Privacy scrub gate above applies in full to every autonomous post: never let
internal tracker IDs or private paths reach a posted comment or body.

## References

- `references/rubric.md` - Issue type rubric and readiness statuses.
- `references/rewrite-template.md` - Maintainer-ready body template.
- `references/methodology.md` - Detailed audit workflow and output formats.
