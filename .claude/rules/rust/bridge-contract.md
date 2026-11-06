---
paths:
  - src/suews_bridge/**
  - src/supy/_run_rust.py
  - dev-ref/RUST_BRIDGE*.md
---

# Rust bridge contract and audit rules

The bridge transports resolved parameters, forcing, state and outputs between
Python and Fortran. Keep that responsibility small. Apply these rules to changes
on either side of the boundary, including the standalone engine. They extend
[code-design.md](../code-design.md), not a separate design policy for Rust.

The [design](../../../dev-ref/RUST_BRIDGE_DESIGN.md) explains ownership and data
flow. The [baseline audit](../../../dev-ref/RUST_BRIDGE_AUDIT.md) records existing
deviations: these rules are requirements for changes, not a claim that the whole
current implementation already meets them.

## Rules

| ID | Requirement | Evidence to collect for an affected path |
|---|---|---|
| BR-01 | Keep scientific equations and option semantics in their owning model layer; keep configuration defaults, migrations and validation in the Python data model. Bridge code may marshal, validate representation, invoke the kernel and report its result. | Identify the owning operation and all callers. Explain any new calculation or dependency; move it to its owner if it is not transport. |
| BR-02 | Consume resolved input. A required missing, null, malformed or incorrectly sized value must produce an actionable error with its path and, where applicable, index or expected length. Do not add numerical fallbacks, silent key drops, truncation or padding that supplies model values. | Exercise valid non-default input and each relevant failure case through Python and engine entry points. Trace the value to the kernel member, not just the YAML reader. |
| BR-03 | Represent conditional absence explicitly. A field may be absent only when the selected options make it unused; its contract must establish that the kernel will not read it. | Record the enabling condition and representation, and check both option branches. Allocation zeros and scratch buffers are permissible only when written before consumption; they must not become missing-input defaults. |
| BR-04 | Preserve names, field order, dimensions, units and schema versions across every mapping, including nested parents and the site-member table of contents. | Trace Python/YAML -> Rust -> flat buffer -> facade unpack -> kernel member. Use distinct non-default values and a kernel-facing wiring check. Length checks and round trips alone do not prove the mapping. |
| BR-05 | Prove FFI buffer validity at the safe boundary: checked sizes and integer conversions, layout/stride, writable output, non-overlap, lifetime and exclusive mutation for the duration of the call. | Locate validation before each affected unsafe call and document its assumptions. Check malformed shapes, overflow, aliasing and error propagation where relevant. Do not widen `unsafe` or release the GIL without re-establishing those assumptions. |
| BR-06 | Keep cold-start resolution separate from restart. Restart must preserve the complete state and elapsed timer, reject incompatible envelopes, and never silently initialise missing members. | Check version/member/dimension validation and split-run equivalence against an uninterrupted run. Check timestep compatibility. Distinguish documented inactive sentinels from invalid active values. |
| BR-07 | Preserve the forcing and output contracts. Use owning registries and generated widths; keep conversion, interpolation and file-format policy in the relevant I/O layer. | Check column order, units, timestamps, cumulative/average/instantaneous semantics and memory layout. Compare Python and engine routes where they implement the same contract. Do not infer correctness from equal array sizes. |
| BR-08 | Make failure visible. Reject unsupported input before the kernel when possible; return kernel errors and warnings with useful context, without substituting success, clipping values or retrying with different science. | Inspect error/warning transfer and at least one relevant negative path. Record any diagnostic truncation or unsupported case rather than concealing it. |
| BR-09 | State the feature/build mode used for evidence. Check native source selection and order against the Fortran `USE` dependency graph: disabling physics does not remove the facades' type-module prerequisites. Lint-only checks cannot prove native linkage. | Run the affected supported modes on the designated compute host. Record commands, source revision, compiler and results. Build required `.mod` files with the matching compiler; do not repair a missing prerequisite by copying stale modules from another build. A physics build does not prove the non-physics mode works. |
| BR-10 | Keep additions issue-led and bounded. Specify responsibility, input/output contract, invariants and acceptance checks before implementation. Reuse the same rules for all agents. | Link the follow-up issue to #1989 and the relevant #1991 work item. Include an audit record for the affected boundaries and use the existing review convergence rule. |

Application defaults and representation initialisers are different. A shadow
type or `Default` implementation can initialise storage, but it is not authority
to choose a value for a missing model input. Likewise a codec check may enforce
an established range; it must not invent a new scientific constraint.

## Audit record

For each applicable rule, record:

```text
revision / boundary or module family / rule ID
status: checked | violation | not verified | not applicable
evidence: source symbols; command and result, or explicitly static inspection
finding: severity + stable signature (file, symbol, problem), if any
disposition: fix in this change | linked follow-up | decision required
```

`checked` means the named boundary and evidence were checked, not that every
field in the crate has been proved correct. Use `not verified` for missing
evidence; do not turn a suspicion into a confirmed bug. Severity follows
[review-convergence.md](../review-convergence.md). Do not average findings into
a score: a wrong mapping or unsafe boundary cannot be offset by good comments.

For a small change, a compact table in the PR description is enough. For a broad
audit, inventory module families and state the sampled paths and exclusions.
Update the baseline only when repeating a baseline audit; routine changes carry
their own revision-specific evidence.

Agents implement and check the contract. Maintainers decide requirements,
scope and genuine scientific/design exceptions; routine human line-by-line
Rust inspection is not an additional gate. Existing
[physics evidence](../physics-change-evidence.md),
[schema migration](../python/schema-versioning.md) and release requirements
still apply. A proposal needing a new scientific default, unit policy or state
meaning must return to that owner rather than being decided inside the bridge.
