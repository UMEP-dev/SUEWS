# Code Design: Fix Placement and Encapsulation

Where a change lives matters as much as whether its output is correct. A guard,
short-circuit, special-case, or optimisation can produce byte-identical output and
still sit in the wrong place. Such a change passes a correctness check and fails a
design check, so it needs an explicit design pass, not just a "does the output match"
pass.

Complements `work-sizing.md` (how big a change should be) and
`python/conventions.md` (single-responsibility functions). This rule is about *where* a
behaviour belongs once you know what it should do.

---

## Put the behaviour in the function that owns the operation

When you add "skip the work when X", "fast-path when Y", or "cache Z", ask whether the
concern belongs to the operation being called rather than to this one caller.

"Do not resample when the target frequency already equals the source" is a property of
resampling. It belongs inside the resample function, which already receives the target
frequency, not bolted onto one call site that happens to invoke it.

Bolting it onto a call site is wrong for three reasons:

- **It does not cover the other callers.** A shared function usually has several call
  sites, and the recommended/public API path is often one of them. A guard at one call
  site leaves every other caller paying the full cost. Grep the callers before deciding
  the fix is complete: `grep -rn "func_name(" src/`.
- **It leaks internals outward.** The caller now has to know how the operation behaves
  at the boundary (what "no-op" means, what shape to return). That knowledge belongs
  inside the operation.
- **It creates a divergent path.** A call-site bypass that returns the raw input skips
  whatever cleanup the real function does (dropna, reindex, column selection). It may be
  identical today and drift tomorrow. An internal early-return returns the function's
  own contract, so there is nothing to drift from.

Fix: add the early return inside the owning function; leave the call sites calling it
plainly. Tested once, correct everywhere.

## Do not re-derive a value the system already holds

Before computing a property from the data, check whether the system already carries it.

- A native timestep is in the config/state (`tstep`) and on a pandas index (`.freq`).
- A regular cadence is recoverable with `pd.infer_freq()`, which returns `None` for an
  irregular index and so declines to answer when the answer would be wrong.

Hand-rolling the value (for example `index.diff().median()` to re-derive a frequency)
is redundant and usually less safe: a median fabricates a single number even for a
ragged index and can false-trigger a fast-path, whereas the canonical source declines.

## Review lens

When auditing a diff that adds a guard, special-case, or optimisation:

- Does this belong in the called function rather than at the call site?
- If the function has N callers and the diff patches one, are the other N-1 flagged?
- Does the new code re-derive something already carried by data/config/state?
- Is "the output is identical" being used to conclude "the design is fine"? It does not
  follow.

This lens is also wired into the `audit-pr` checklist
(`.claude/skills/audit-pr/references/review-checklist.md`, "Design and Placement").

## Origin

Distilled from the review of gh#1599, where a "skip resample when the output frequency
matches the model timestep" optimisation was added at one save-time call site. The
output was verified byte-identical to the previous path, but the guard belonged inside
`resample_output` (five callers, one patched) and re-derived the native frequency by
hand instead of reading the timestep the data already carried.
