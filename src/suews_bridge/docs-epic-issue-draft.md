# Epic draft: Derived-type bridge from SUEWS to Python via Rust adapter

## Summary

Build a maintainable bridge that exposes selected SUEWS derived types to Python
through a Rust middle layer, while preserving SUEWS core physics integrity and
avoiding slow wrapper-regeneration loops.

## Problem statement

Current derived-type transfer between Fortran and Python requires manual,
error-prone mapping steps that are hard to maintain and slow to iterate on.
Direct f90wrap on evolving derived-type interfaces has high rebuild cost in this
workflow, which slows development and debugging.

## Why this approach

1. Preserve core physics source ownership: keep adapter code outside
   `src/suews/src/`.
2. Use a stable, narrow C ABI boundary from Fortran to Rust.
3. Move high-frequency interface iteration to Rust/Python where tooling is
   faster.
4. Enforce explicit schema contracts (field order, payload length, schema
   version) to reduce silent drift.

## Scope in this epic

1. Keep OHM bridge as pilot and harden it.
2. Establish reusable pattern for next derived type:
   - schema/version API,
   - default-state export,
   - one-step update API,
   - Rust typed state + payload conversion,
   - Python class-like accessors and payload methods.
3. Document rationale, risks and rollout plan.

## Out of scope

1. Rewriting SUEWS physics kernels.
2. Full immediate exposure of all SUEWS derived types.
3. Large refactor inside existing SUEWS physics modules.

## Work packages

1. WP1: Contract hardening
   - runtime schema-version checks,
   - strict payload-length validation,
   - stable ordered field map.
2. WP2: Tooling and UX
   - CLI JSON contract commands (schema/default/step),
   - clear Python-side error messages for payload mismatch.
3. WP3: Test and regression guard
   - Rust unit tests (roundtrip, mismatch rejection, command paths),
   - Python-feature tests,
   - repository smoke tier.
4. WP4: Documentation and maintainability
   - architecture rationale,
   - phase roadmap,
   - objection/response section for reviewers.
5. WP5: Next derived-type onboarding template
   - checklist and acceptance criteria reused from OHM pilot.

## Acceptance criteria

1. End-to-end schema-version checks active (Fortran runtime -> Rust -> CLI/Python).
2. Payload conversion is strict and deterministic (no silent extra-field ignore).
3. Default derived-type state can be exported/imported in map and ordered-values
   forms.
4. Python API can manipulate state in class-like style with explicit field access.
5. Tests green for:
   - `cargo test`,
   - `cargo test --features python`,
   - repository smoke tier.

## Risks and mitigation

1. Risk: schema drift across language boundaries.
   Mitigation: schema metadata API + strict version/length checks + tests.
2. Risk: confusion between adapter and core science code.
   Mitigation: adapter isolation in `src/suews_bridge/fortran/` + explicit docs.
3. Risk: extra maintenance cost.
   Mitigation: small phased delivery and push-on-green incremental commits.

## Reviewer FAQ (defence points)

1. "Why not stay with f90wrap only?"
   - For this workflow, iteration speed and debug cycle are materially slower for
     frequently changing derived-type interfaces.
2. "Is this duplicating logic?"
   - The bridge centralises interface logic; science formulas remain in Fortran
     kernels. It reduces duplicate ad-hoc conversion logic spread across call
     sites.
3. "Does this compromise scientific integrity?"
   - No new physics is introduced in adapter layers; changes are contract and
     transport focused.

## Rollout strategy

1. Land small commits with immediate push after each green milestone.
2. Validate each milestone on Rust tests and smoke tier before extending scope.
3. Add next derived type only after OHM pattern remains stable.
