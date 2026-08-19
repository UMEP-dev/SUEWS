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
3. **Re-review** -- run the next round over the revised artefact.
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

## No silent termination

At convergence, report: rounds run, the confirmed-per-round trend, what was
applied, and what remains (accepted / deferred / escalated) with reasons. A loop
that ends without stating the remainder reads as "all clean" when it may not be.

---

## Relation to other rules and skills

- `fix-issue` step 5 is the per-PR instance: `audit-pr` is the reviewer, 3
  iterations is the cap, signature recurrence is the early exit.
- For multi-artefact or skill/rule authoring, the reviewer is typically an
  adversarial fan-out (review -> independent verify -> confirmed) and the same
  criterion applies.
- In autonomous or batch mode the loop MUST be bounded by the cap and escalate
  rather than spin, matching the "bounded loops" requirement in
  `autonomous-workflow.md`.
- Smaller units converge faster: keep the artefact under review right-sized
  (`work-sizing.md`).
