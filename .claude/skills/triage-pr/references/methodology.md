# PR Triage Methodology

Step-by-step procedure for the `triage-pr` skill. This file covers the
execution workflow, marker and idempotency mechanics, the machine-readable
handoff verdict block, and batch coverage accounting. Verdict decision rules
live in `rubric.md`; this file covers the procedure for applying them.

---

## Step-by-step procedure

### Solo mode (one PR at a time)

1. **Run the scanner** for the target PR:

   ```
   bash .claude/skills/triage-pr/scripts/scan-prs.sh --repo UMEP-dev/SUEWS
   ```

   Or to scan a single PR directly by inspecting its open-PR entry in the
   output. The scanner emits per-PR columns:

   ```
   PR  STATE  CI  MERGE  REVIEW  AGE_D  CHURN  FILES  AUTHOR  TITLE
   ```

   and a `Scope:` line (see Batch coverage accounting below).

2. **Check idempotency** before computing a verdict:
   - Does the PR carry the `0-auto:pr-audited` label?
   - Is the scanner's `last_activity_at` (the `--json` field carrying the UTC
     instant of last substantive activity) `<= marker_ts` (no new commits or
     human comments since the previous audit)?
   - If deferred, has the revisit trigger fired (i.e. `revisit-by:` date <=
     today, or `blocked-on:#N` is now closed)?
   - If ALL three conditions hold (and the revisit trigger has NOT fired for a
     deferred PR), record this PR as a no-op and stop. Do not re-apply labels
     or re-post a comment.

3. **Compute the verdict** using `rubric.md`. Map the scanner signals to
   exactly one of the five verdicts: `advance`, `continue`, `defer`,
   `close`, or `escalate`.

4. **Check ownership and repository label inventory before any GitHub write**:
   - Query the active operator/automation login once per run
     (`gh api user --jq .login`) and the repository's existing labels
     (`gh label list --repo UMEP-dev/SUEWS --limit 200 --json name --jq '.[].name'`).
   - Per `.claude/rules/autonomous-workflow.md`, posting comments or editing
     labels/metadata on a PR **not owned by the current operator/automation
     account** is **ALWAYS human-gated**.
   - In unattended mode:
     - **Own PR** (`author == operator_login`): upsert the
       `<!-- triage-pr:verdict ts=... -->` marker comment; swap the `0-pr:*`
       label (`advance` -> `0-pr:advance`, `continue` -> `0-pr:continue`,
       `defer` -> `0-pr:deferred`, `close` -> `0-pr:close-proposed`,
       `escalate` -> `0-pr:escalate`) and apply `0-auto:pr-audited` (plus
       `0-physics:change` when triggered), skipping any label missing from
       `gh label list` so missing labels degrade gracefully without failing.
     - **Third-party PR** (`author != operator_login`): perform **zero GitHub
       writes** and emit the local `[triage-pr] verdict` block with
       `Applied: none` and the human-gated maintainer action recorded in
       `Reason:` / `Next:`.
   - In interactive solo mode, present the draft comment/label changes and wait
     for explicit human approval before writing to GitHub.

5. **State the route and next step**:
   - `advance` -> state the route (`prep-pr`, `split-pr`, or `queue-pr`; note
     that `prep-pr` is a user-level skill whose portable repo-native fallback is
     `audit-pr` + `queue-pr`) and priority (P0/P1/P2) in the verdict block.
   - `continue` -> note in the verdict block that the PR is live WIP; route
     is `fix-issue` or direct author notification.
   - `defer` -> record `Revisit-by:` or `Blocked-on:` in the verdict block;
     no further action until the trigger fires.
   - `close` -> **HUMAN-GATED**: propose closure (and apply `0-pr:close-proposed`
     only on an own PR or with explicit human approval). The skill never closes a PR.
   - `escalate` -> **HUMAN-GATED**: state the conflicting signals in the verdict
     block (and apply `0-pr:escalate` only on an own PR or with explicit human
     approval). The skill never resolves ambiguity by guessing.

Solo mode may pause for human approval before any human-gated action and
resume after confirmation.

### Batch mode (bounded set, unattended)

Batch mode runs the steps above for each PR in the candidate set without
pausing mid-run.

1. Run the scanner with an explicit `--limit` to bound the set. The scanner
   prints the `Scope:` line before the rows. Never rely on the tool's
   default limit as if it were full coverage.
2. For each PR in the returned set:
   - Apply the idempotency check (step 2 of solo mode). Idempotency-skipped
     PRs are counted as `noop`; they are NOT counted as `skipped`.
   - Compute verdict and apply auto-tier.
   - Accumulate the verdict into the coverage buckets.
3. Emit one verdict block per PR, then the `Scope:` line and a partitioned
   `Summary:` (see Batch coverage accounting below).

Any review -> revise loop inside the batch run follows
`.claude/rules/review-convergence.md` (round cap + oscillation guard).
A single item must not consume the batch; escalate and move on if it
does not converge within the cap.

### Deferred-PR re-surface check

A PR carrying `0-pr:deferred` re-enters active triage when EITHER:

- Its `revisit-by:YYYY-MM-DD` date is <= today, OR
- Its `blocked-on:#N` issue or PR has been closed/merged.

When a trigger has fired, treat the PR as a fresh candidate: recompute the
verdict from current scanner signals, swap the `0-pr:deferred` label off, apply
the new label, and upsert the marker comment with the new verdict.

If neither trigger has fired, the PR is a no-op for this run: count it as
`noop` in the coverage accounting and do not re-apply labels or repost.

---

## Marker and idempotency mechanics

### The marker comment

Every PR that completes triage carries a marker comment with a machine-parseable
header on the first line:

```
<!-- triage-pr:verdict ts=YYYY-MM-DDTHH:MM:SSZ -->
```

The rest of the comment is the verdict block (see below), including for
`defer` verdicts the reason and revisit trigger(s).

The timestamp `ts` is the UTC instant at which this run applied the verdict.
On a re-run, the existing comment is **replaced in place** (upsert semantics),
not duplicated. If no marker comment exists yet, a new comment is posted.

### Idempotency skip condition

A PR is a no-op for this run when ALL of the following hold:

1. The PR carries the `0-auto:pr-audited` label.
2. `last_activity_at <= marker_ts` -- no new commits or human comments have
   arrived since the previous audit. Both sides are absolute UTC instants:
   `last_activity_at` is the scanner `--json` field; `marker_ts` is the `ts=`
   value parsed from the marker comment header.
3. If the PR is deferred: neither revisit trigger has fired.

**Critical: `last_activity_at` definition.** The scanner emits `last_activity_at`
as the UTC instant of last substantive activity:

```
last_activity_at = max(last_commit_at, last_human_comment_at)
```

where `last_human_comment_at` (`LAST_HUMAN` in `scan-prs.sh`) excludes both bot
accounts and automation marker comments (`<!-- triage-pr:verdict` and
`<!-- suews-merge-queue-coordinator:`), even when posted under an operator's
human GitHub login. This is NEVER the raw `updatedAt` field from the GitHub API.
The reason is a correctness trap: the triage skill's own label writes and
comment upserts bump `updatedAt` (and would match a naive non-bot comment check).
If `updatedAt` or marker comments were included, every triage run would reset
the clock, making every PR appear "recently active" on the next run and breaking
`last_activity_at <= marker_ts` idempotency. Use the scanner's
`last_activity_at` field for the absolute `<= marker_ts` comparison (it is null
only when no commit, comment, or creation timestamp is available); the `AGE_D`
column is the human-readable staleness signal in whole days and is NOT
comparable to an absolute marker timestamp.

### Label transition and graceful degradation

Check `gh label list --repo <OWNER/REPO> --limit 200 --json name --jq '.[].name'`
once per run before applying any label. On PRs owned by the operator/automation
account (or with explicit human approval in interactive mode):

1. Inspect the PR's current labels (`labels` in `scan-prs.sh --json`).
2. Remove any label whose name begins with `0-pr:`.
3. If the target `0-pr:<verdict>` label exists in `gh label list`, apply it;
   otherwise skip the label write gracefully and note the missing label.
4. If `0-auto:pr-audited` exists in `gh label list`, apply it once and leave it
   in place across re-runs.

---

## Machine-readable handoff verdict block

Every triage run ends with one verdict block per PR. This is the contract other
skills and orchestrators branch on; reproduce it exactly.

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

Field rules:

- `Verdict` is always one of the five values above; no other values are
  permitted.
- `Priority` is P0/P1/P2 only for `Verdict: advance`; use `-` for all
  others.
- `Route` names the downstream skill for `advance` (`prep-pr`, `split-pr`,
  or `queue-pr` -- when user-level `prep-pr` is unavailable, consumers fall
  back to the portable repo-native `audit-pr` + `queue-pr` pair) and `fix-issue`
  for `continue`; use `-` for all other verdicts.
- `Revisit-by` and `Blocked-on` are populated only for `Verdict: defer`;
  both are `-` for all other verdicts. A defer verdict must carry at least
  one of them as a non-`-` value.
- `Applied` is a comma-separated subset of the actions taken in this run:
  `comment` (marker comment posted or upserted), `label:<0-pr:...>` (the
  specific label applied), or `ready` (draft PR marked ready -- only if
  applicable). Use `none` for no-op runs or when unattended mode skips GitHub
  writes on a third-party PR (recording the human-gated action in `Reason` /
  `Next`).
- `Next` states the actionable next step, or for human-gated verdicts the
  specific question the maintainer must answer.

### Human terminals

`Verdict: close` and `Verdict: escalate` are human terminals:

- `close` -> label `0-pr:close-proposed`; no further automated action.
  A maintainer reads the `Reason` and `Next` fields and decides whether
  to close or reassign.
- `escalate` -> label `0-pr:escalate`; `Reason` must name the conflicting
  signals explicitly. A maintainer reads the block and resolves the
  ambiguity.

Downstream skills branch on `Verdict` (to pick a workflow) and on `Route`
(to pick which skill handles the PR next). They do NOT need to parse prose.

---

## Batch coverage accounting

### Scope line

The scanner emits a `Scope:` line before the candidate rows. Emit it
verbatim in any batch output:

```
Scope: repo=<OWNER/REPO>, drafts+stale-ready (stale>Nd); fetched=K, candidates=M, fresh_ready_skipped=K-M, open=T; truncated: yes|no
```

where `K` is the number of open PRs fetched (`--limit`), `M` is the number of
candidate PRs matching the draft / stale-ready / changes-requested filter,
`K - M` (`fresh_ready_skipped`) is the count of fresh ready PRs excluded by the
scanner filter, and `T` is the independently-obtained total open PR count. When
`truncated: yes` (`K < T`), the backlog is NOT fully covered; the `(T - K)` PRs
beyond `--limit` remain unswept and a re-run with a higher limit is required for
full coverage.

### Coverage equations

The candidate set `M` partitions exactly into seven buckets, and together with
`fresh_ready_skipped` accounts for all `K` fetched PRs:

```
advance + continue + defer + close + escalate + noop + skipped = M
M + fresh_ready_skipped = K
```

where:

- `advance` -- candidate PRs assigned the `advance` verdict.
- `continue` -- candidate PRs assigned the `continue` verdict.
- `defer` -- candidate PRs assigned the `defer` verdict.
- `close` -- candidate PRs assigned the `close` verdict (`0-pr:close-proposed`).
- `escalate` -- candidate PRs assigned the `escalate` verdict.
- `noop` -- candidate PRs that were idempotency-skipped (already audited, no new
  substantive activity, deferred trigger not fired). A `noop` PR is NOT `skipped`.
- `skipped` -- candidate PRs that could not be processed (fetch failure or parse
  error). A reason must be recorded; never silently drop a candidate PR.

### Summary line

After the per-PR verdict blocks, emit:

```
Summary: M candidates processed (fetched=K, fresh_ready_skipped=K-M) = <advance> advance, <continue> continue, <defer> defer, <close> close-proposed, <escalate> escalate, <noop> noop, <skipped> skipped
```

The seven candidate counts must sum to `M` (and `M + fresh_ready_skipped = K`).
Full coverage of the open backlog is assertable only when `truncated: no`.

### No silent drop

A PR that cannot be processed is always recorded as `skipped` with a reason
in the `Next` field of its verdict block. It is never omitted from the count.
