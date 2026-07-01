# PR Triage Rubric

Signal-to-verdict decision rule for the `triage-pr` skill. The skill reads
per-PR signals from the scanner (`scan-prs.sh`) and maps each PR to exactly
one of five verdicts using the rules below.

The scanner emits these columns per PR. All conditions in this rubric are
expressed in these column names:

    PR  STATE(draft|ready)  CI(passing|failing|pending|none)
    MERGE(CLEAN|DIRTY|UNSTABLE|BLOCKED|BEHIND|UNKNOWN)
    REVIEW(NONE|CHANGES_REQUESTED|APPROVED|REVIEW_REQUIRED)
    AGE_D(int days)  CHURN(additions+deletions)  FILES(count)
    AUTHOR(login)  TITLE

Scope: this rubric assigns the verdict. Size assessment defers to
`audit-pr`'s Size Gate (`.claude/rules/work-sizing.md`); merge ordering
defers to `queue-pr`. Do not duplicate those rules here.

---

## TIE-BREAK RULE

**Where signals genuinely conflict, the verdict is `escalate` -- never a
guess.** If you can construct a reasonable argument for two different
verdicts from the same signal set, choose `escalate` and state the
conflicting signals in the reason field.

---

## Part A -- the five verdicts

### advance

A PR that is substantively complete and can move forward now.

**All of the following must hold:**

1. The diff is substantively complete: no TODO markers, no obviously missing
   tests, no commented-out placeholder code in the body of the change.
2. CI is near-green (see Part C). Failing required checks -> not `advance`.
3. MERGE is CLEAN, or BEHIND with no conflicting changes (the only action
   needed is a rebase, not a resolution). MERGE=DIRTY or MERGE=BLOCKED ->
   not `advance`.
4. The PR is tied to a live goal: there is a linked open issue, the
   milestone is active, or the TITLE clearly maps to a current roadmap item.
5. Size is within the right-sized-PR threshold. If audit-pr's Size Gate would
   recommend a split, route to `split-pr` instead (this does not prevent an
   `advance` verdict, but the route changes -- see below).

**Route for `advance`:**

- Size Gate would split -> route: `split-pr`.
- CI passing, MERGE=CLEAN, REVIEW=APPROVED or REVIEW=NONE with low churn
  -> route: `queue-pr`.
- Otherwise (needs polish, review not yet requested, minor fixes outstanding)
  -> route: `prep-pr`.

**Priority** (Part B) is recorded in the verdict block for every `advance`.

---

### continue

A PR with genuine live work-in-progress that is not yet ready to advance.

**Signals that point to `continue`:**

- STATE=draft, AND there is recent activity: AGE_D is low (roughly under
  the stale threshold, typically 14 days), or a linked issue has an open
  assignee actively progressing it.
- CI=failing due to incomplete work: the failure is in a test or lint that
  the author clearly has not addressed yet (e.g. a new test file is missing,
  or the author's own commit message says "WIP").
- REVIEW=CHANGES_REQUESTED and AGE_D is low: the author is expected to
  iterate.
- The diff contains TODO markers or commented-out code that is clearly
  intentional scaffolding, not an oversight.

**Route:** return to `fix-issue` workflow, or notify the author directly.

**Boundary: continue vs defer.** `continue` means "keep going -- it is
alive and someone is on it." `defer` means "valid work, intentionally
paused, with a named trigger." If the PR has no recent activity and no open
linked issue with an assignee, prefer `defer` over `continue`.

Note: the scanner does not emit the linked-issue / assignee signal. When the
`AGE_D` and CI/MERGE signals alone leave the continue/defer choice genuinely
balanced, look it up out-of-band (for example
`gh pr view <n> --json closingIssuesReferences` and the linked issue's
assignees) before deciding. If that lookup is not available or still
ambiguous, apply the tie-break and `escalate` rather than guessing.

---

### defer

Valid work that should not move forward right now, with a reason and a
revisit trigger.

**Signals that point to `defer`:**

- MERGE=BLOCKED: the PR is explicitly blocked on another PR or upstream
  change (and that blocker is not itself an `advance` candidate).
- The PR targets a feature or fix that was deprioritised relative to the
  current release wave (linked issue is backlogged, milestone is future).
- REVIEW=CHANGES_REQUESTED and AGE_D is high: the author has not responded
  and the work is otherwise sound -- valid but stalled without intent to
  resume now.
- The PR is waiting on a design or scope decision that is unresolved but
  clearly recorded.

**Every `defer` verdict must carry:**

- `reason:` -- one sentence stating why it is not moving forward now.
- At least one of:
  - `revisit-by:YYYY-MM-DD` -- a calendar trigger.
  - `blocked-on:#N` -- a specific PR or issue whose merge/close unblocks
    this one.

**Boundary: defer vs close.** `defer` is revivable via a named trigger;
`close` is dead. If no plausible trigger exists, prefer `close` (with human
gate).

---

### close

A PR that is dead: superseded, obsolete, or abandoned with no prospect of
revival.

**Signals that point to `close`:**

- The same change was merged in another PR (superseded).
- The target feature, file, or subsystem was removed or substantially
  refactored since the PR was opened, making the diff inapplicable.
- STATE=ready or STATE=draft, AGE_D is very high (roughly 60+ days), no
  linked open issue, no assignee, no recent comments, and REVIEW=NONE or
  REVIEW=CHANGES_REQUESTED with no author response for weeks.
- The PR is an experiment that was abandoned in favour of a different
  approach, and the description or comments confirm this.

**HUMAN-GATED.** `triage-pr` NEVER closes a PR. It proposes closure and
states the reason. A maintainer takes the action.

---

### escalate

Disposition genuinely unclear after applying the rules above.

**Use `escalate` when:**

- Signals conflict and no single verdict is clearly correct (tie-break rule
  above).
- The CI failure is in a required check but the cause is not apparent from
  the diff or the check log (cannot tell whether it is a flaky check, a
  real regression, or an infrastructure hiccup).
- The PR is large and complex and the "substantively complete" question
  cannot be answered without scientific or domain judgement.
- The PR appears to overlap with another open PR or merged change, but the
  relationship is not explicit.
- MERGE=UNKNOWN and the reason is not obvious.

**`escalate` is a human terminal.** The verdict block must state the
conflicting or unclear signals so a maintainer can act without re-reading
the entire PR.

---

## Part B -- advance priority rubric

Priority applies only to `advance` verdicts. Record it in the verdict block.
Do NOT apply it as a label.

- **P0** -- the PR fixes a release blocker or a CI blocker, or its merge
  directly unblocks one or more other `advance`-candidate PRs or open issues.
- **P1** -- the PR is ready and right-sized; the author has merely stalled
  (REVIEW=NONE or REVIEW=APPROVED, CI passing, AGE_D above stale threshold
  but no blocker).
- **P2** -- the PR is ready but lower impact: small doc fix, minor nit,
  or an enhancement without an active milestone dependency.

When priority is ambiguous between P0 and P1, prefer P0 only if there is a
concrete blocking dependency (another PR, a failing release gate, an open
issue explicitly citing this PR as a blocker). Otherwise default to P1.

---

## Part C -- "near-green" CI definition

This is the single canonical definition for CI readiness used by `triage-pr`.

### Green (CI=passing)

CI=passing: all required checks pass. `advance` is permitted.

### Near-green (advance permitted with note)

CI=failing or CI=pending, but ONLY if ALL of the following hold:

1. The failing checks are all NON-REQUIRED (optional checks, informational
   jobs, or jobs flagged as `continue-on-error` in the workflow definition).
2. OR the failing check is on the documented known-flaky list for this
   repository (currently: none defined; add here when a flaky check is
   formally identified and acknowledged by a maintainer).
3. All REQUIRED checks either pass or have not yet run (CI=pending for
   required checks -> route is `prep-pr`, not `queue-pr`, until they
   resolve).

When advancing a near-green PR, the verdict block must name the failing
check(s) and state that they are non-required or known-flaky.

### Not green (advance blocked)

**A genuinely red REQUIRED check blocks `advance` unconditionally.**

- If the red required check is due to incomplete or incorrect work in the
  diff -> verdict is `continue`.
- If the red required check is unexplained (no obvious cause in the diff or
  the check log) -> verdict is `escalate`.

NEVER advance a PR with a red required check. This rule has no exceptions.

### CI=none

CI=none means no checks have been registered. Treat as near-green for the
purposes of the `advance` verdict, but note the absence in the verdict block.
A CI=none PR should be routed to `prep-pr` so checks can be confirmed before
`queue-pr`.
