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

4. **Apply the auto-tier** (operations that run without human approval):
   - **Upsert the marker comment** in place: find any existing
     `<!-- triage-pr:verdict ts=... -->` comment on the PR and replace it;
     if none exists, post a new one. The comment body contains the verdict
     block (see below) with the updated timestamp.
   - **Swap the `0-pr:*` label**: remove any existing `0-pr:*` label first
     (these are mutually exclusive; never two at once), then apply the label
     corresponding to the verdict:
     - `advance` -> `0-pr:advance`
     - `continue` -> `0-pr:continue`
     - `defer` -> `0-pr:deferred`
     - `close` -> `0-pr:close-proposed`
     - `escalate` -> `0-pr:escalate`
   - Apply the `0-auto:pr-audited` label if it does not already exist.

5. **State the route and next step**:
   - `advance` -> state the route (`prep-pr`, `split-pr`, or `queue-pr`) and
     priority (P0/P1/P2) in the verdict block; the downstream skill can act.
   - `continue` -> note in the verdict block that the PR is live WIP; route
     is `fix-issue` or direct author notification.
   - `defer` -> record `Revisit-by:` or `Blocked-on:` in the verdict block;
     no further action until the trigger fires.
   - `close` -> **HUMAN-GATED**: post `0-pr:close-proposed` label and present
     the verdict block for maintainer review. The skill never closes a PR.
   - `escalate` -> **HUMAN-GATED**: post `0-pr:escalate` label and state the
     conflicting signals in the verdict block. The skill never resolves
     ambiguity by guessing.

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

This is NEVER the raw `updatedAt` field from the GitHub API. The reason is a
correctness trap: the triage skill's own label writes and comment upserts bump
`updatedAt`. If `updatedAt` were used for staleness and idempotency, every
triage run would reset the clock, making every PR appear "recently active" on
the next run and defeating idempotency. The scanner already derives the correct
substantive timestamp from commit data and filtered (non-bot) comment
timestamps. Use the scanner's `last_activity_at` field for the absolute
`<= marker_ts` comparison (it is null only when no commit, comment, or creation
timestamp is available); the `AGE_D` column is the human-readable staleness
signal in whole days and is NOT comparable to an absolute marker timestamp.

### Label transition

The `0-pr:*` labels are mutually exclusive. Before applying any new
`0-pr:<verdict>` label:

1. Query the PR's current labels.
2. Remove any label whose name begins with `0-pr:` (there should be at most
   one, but remove all if more than one is present).
3. Apply the new `0-pr:<verdict>` label.

This prevents a PR from ever carrying two `0-pr:*` labels simultaneously.

The `0-auto:pr-audited` label is additive: apply it once and leave it; do not
remove it on a re-run.

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
  or `queue-pr`) and `fix-issue` for `continue`; use `-` for all other
  verdicts.
- `Revisit-by` and `Blocked-on` are populated only for `Verdict: defer`;
  both are `-` for all other verdicts. A defer verdict must carry at least
  one of them as a non-`-` value.
- `Applied` is a comma-separated subset of the actions taken in this run:
  `comment` (marker comment posted or upserted), `label:<0-pr:...>` (the
  specific label applied), `ready` (draft PR marked ready -- only if
  applicable). Use `none` for no-op runs or when nothing was written.
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
Scope: repo=<OWNER/REPO>, drafts+stale-ready (stale>Nd); K fetched / T open; truncated: yes|no
```

where `K` is the number of PRs fetched and `T` is the independently-obtained
total open PR count. When `truncated: yes`, the backlog is NOT fully covered;
the `(T - K)` PRs beyond the `--limit` remain unswept and a re-run with a
higher limit (or pagination) is required for full coverage.

### Coverage equation

The returned set K partitions exactly into seven buckets. The equation below
must hold; any other sum is a bug:

```
advance + continue + defer + close + escalate + noop + skipped = K
```

where:

- `advance` -- PRs assigned the `advance` verdict.
- `continue` -- PRs assigned the `continue` verdict.
- `defer` -- PRs assigned the `defer` verdict.
- `close` -- PRs assigned the `close` verdict (label `0-pr:close-proposed`).
- `escalate` -- PRs assigned the `escalate` verdict.
- `noop` -- PRs that were idempotency-skipped (already audited, no new
  activity, deferred trigger not fired). A `noop` PR is NOT a `skipped` PR.
- `skipped` -- PRs that could not be processed (fetch failure, parse error,
  or explicitly excluded by the bound). A reason must be recorded; never
  silently drop a PR.

### Summary line

After the per-PR verdict blocks, emit:

```
Summary: K processed = <advance> advance, <continue> continue, <defer> defer,
         <close> close-proposed, <escalate> escalate, <noop> noop, <skipped> skipped
```

The seven counts must sum to K, not to T. Full coverage of the open backlog
is assertable only when `truncated: no`.

### No silent drop

A PR that cannot be processed is always recorded as `skipped` with a reason
in the `Next` field of its verdict block. It is never omitted from the count.
