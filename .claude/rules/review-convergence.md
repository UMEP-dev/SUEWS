# Review-Revise Convergence Loop

A reusable discipline for any iterative review -> revise -> re-review loop: PR
audits, skill/rule authoring, documentation review, or revision against a feedback
stream. It exists so the loop converges on a written criterion instead of running
forever or stopping by feel. An iterative loop without a defined termination
condition is a defect, not a process.

This generalises `fix-issue`'s "Audit, Address, Re-Audit" loop; that step is one
instance of the procedure below.

---

## The loop

1. **Review** -- produce findings. The reviewer may be a single pass (a human, or
   `audit-pr`) or an adversarial fan-out (several reviewers across dimensions, each
   finding independently verified by a skeptic before it counts). Every confirmed
   finding carries a severity (`blocking | major | minor | nit`) and a stable
   signature (file + location + problem).
2. **Triage and apply** -- apply confirmed blocking/major findings and cheap
   minors. For any finding needing design, scientific, or human judgement, do NOT
   guess: escalate and stop (see Escalation).
3. **Re-review** -- run the next round over the revised artefact. Mid-loop rounds (`R2..Rn-1`) may focus on prior findings plus changed hunks; final readiness requires the cold-start final pass below.
4. **Test for convergence** (below). If converged, stop and report; otherwise go
   to step 2.

---

## Convergence criterion (stop when ANY holds)

- **Clean round**: a round yields zero confirmed blocking/major findings and
  introduces no new finding class (only nits, or minors already judged
  acceptable, remain).
- **Diminishing returns**: confirmed findings strictly decrease round over round
  and the remainder are all minors/nits explicitly judged acceptable.
- **Round cap**: a hard cap (default 3-4 rounds). At the cap, stop and log every
  remaining finding as accepted or deferred -- never silently.
- **Escalation**: any round surfaces a finding needing human/design/scientific
  judgement -> stop and hand it back.

## Oscillation guard

Track finding signatures across rounds. If a finding with the same signature
reappears after a fix (the fix did not take, or each fix spawns the same class),
stop and escalate rather than loop -- the same signature-based early exit
`fix-issue` uses.

## Outer-boundary fix verification

[decision: software maintainer, 2026-11-22] [procedure: Principle 1]
Recognised from the diff or agent action whenever a change or review fix alters user-visible output, CLI or JSON formatting, exception wrapping, or error propagation across layers (origin: recorded incident gh#2158).

- Verify the fix through the outermost public entry point a caller invokes (installed CLI stdout/JSON/exit code, public Python API, or standalone Rust CLI), not solely through an internal helper unit test.
- Trace wrapped or re-raised exceptions across every wrapper boundary (for example `str(exc)` discarding `exc.__notes__` or structured attributes) so diagnostic detail reaches the user boundary intact. See `.claude/rules/tests/verification-methodology.md`, item 3 ("Trace every public execution path").

## Cold-start final pass

[decision: software maintainer, 2026-11-22] [procedure: Principle 1]
Recognised from the agent's actions before pushing a final revision for review or enabling auto-merge / queue entry whenever a review-fix loop ran more than one round or after rebasing onto a moved base (origin: recorded incident gh#2158).

- Mid-loop rounds (`R2..Rn-1`) may inspect targeted deltas for prior findings, but the final pre-push or pre-enqueue gate must be one blind, cold-start, unanchored full-diff review against `origin/master...HEAD`, the PR description, and base-branch rules.
- Do not pass prior-round verdicts, passing assertions, or narrow verification checklists into that final pass: the reviewer starts without expected results, matching `AGENTS.md` Principle 1.

## Pre-enqueue CI coverage and advisory audit

[decision: software maintainer, 2026-11-22] [procedure: Principle 1, 2]
Recognised from the agent's actions when reading PR workflow runs and bot comments prior to `gh pr merge --auto` or `queue-pr --enqueue` (origin: recorded incident gh#2158).

- A green `statusCheckRollup` alone is insufficient before enqueueing:
  1. Inspect workflow runs and the `<!-- ci-build-plan -->` PR comment to confirm every platform or execution path the PR claims to fix actually ran rather than being skipped by PR matrix or path filters; if a claimed target platform was skipped, run the needed workflow check or escalate with that unconfirmed gap rather than enqueueing silently.
  2. Read every CI bot comment on the PR (including non-gating sections such as `<!-- cost-markers:start -->`), resolve author-fixable advisories in scope, or record an explicit disposition in the PR body or verification record.

## No silent termination

At convergence, report: rounds run, the confirmed-per-round trend, what was
applied, the blind cold-start final pass outcome, the pre-enqueue CI platform
coverage and bot-advisory disposition, and what remains
(accepted / deferred / escalated) with reasons. A loop that ends without stating
the remainder reads as "all clean" when it may not be.

---

## Relation to other rules and skills

- `fix-issue` step 5 is the per-PR instance: `audit-pr` is the reviewer, 3
  iterations is the cap, signature recurrence is the early exit, and the
  outer-boundary, cold-start final pass, and pre-enqueue CI coverage / advisory
  checks apply before declaring ready or enqueueing.
- `audit-pr` (`references/ci-gates.md`) surfaces skipped target platforms claimed
  by the PR and non-gating CI bot advisories alongside status checks.
- `queue-pr` verifies that each queued PR cleared the cold-start final pass and
  pre-enqueue CI coverage / advisory audit before running `--enqueue`.
- `dev-ref/REGRESSION_CHECKLIST.md` indexes the outer-boundary fix check against
  diff triggers.
- For multi-artefact or skill/rule authoring, the reviewer is typically an
  adversarial fan-out (review -> independent verify -> confirmed) and the same
  criterion applies.
- In autonomous or batch mode the loop MUST be bounded by the cap and escalate
  rather than spin, matching the "bounded loops" requirement in
  `autonomous-workflow.md`.
- Smaller units converge faster: keep the artefact under review right-sized
  (`work-sizing.md`).
- This rule (`blocking | major | minor | nit`) governs iterative diff and
  artefact review loops, whereas pinned-SHA whole-repository or subsystem audits
  follow `dev-ref/CODEBASE_AUDIT_PROTOCOL.md`. [decision: software maintainer, 2026-11-17]
