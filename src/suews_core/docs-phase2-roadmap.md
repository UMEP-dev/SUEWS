# Phase 2 Roadmap: Extending Beyond OHM_STATE

This roadmap defines how to extend the Rust bridge from the OHM pilot to an
additional derived type while preserving build speed and codebase clarity.

## Goals

1. Reuse the schema/version contract validated in OHM.
2. Keep SUEWS core physics modules untouched.
3. Avoid long wrapper regeneration loops.
4. Keep Python usage class-like with explicit field-level mapping.

## Phase 2A: Candidate selection and boundary definition

1. Select one derived type with high practical value and moderate size.
2. Freeze MVP boundary:
   - required fields,
   - required update kernels,
   - required diagnostics.
3. Define schema invariants (field order, surface order if relevant, version).

## Phase 2B: Fortran C API adapter

1. Add a separate adapter file in `src/suews_core/fortran/`.
2. Expose:
   - schema length,
   - schema version,
   - default-state export,
   - one-step update kernel.
3. Keep formulas delegated to existing SUEWS physics modules.

## Phase 2C: Rust safety layer

1. Add FFI declarations.
2. Add typed Rust state struct.
3. Add conversions:
   - flat values,
   - field map,
   - versioned values payload.
4. Add tests for:
   - schema consistency,
   - roundtrip,
   - version mismatch rejection.

## Phase 2D: Python bridge API

1. Expose class-like PyO3 bindings.
2. Provide:
   - `from_dict` / `to_dict`,
   - `from_values_payload` / `to_values_payload`,
   - schema metadata functions.
3. Add a minimal demo script mirroring production usage.

## Phase 2E: Validation and rollout

1. Run Rust tests + Python feature tests.
2. Run repository `make test-smoke`.
3. Land in small commits with immediate push.
4. Repeat for next derived type only after Phase 2E passes.

## Completion criteria

1. Runtime schema version check active end-to-end.
2. One new derived type reachable from Python without f90wrap.
3. Mapping logic documented and tested.
4. No changes to core physics behaviour.

## Likely objections and responses

1. Objection: "This adds an extra layer and therefore extra complexity."
   Response: Yes, the bridge adds one layer, but it removes repeated ad-hoc
   conversion logic spread across Python and Fortran call sites. Complexity is
   centralised and testable instead of duplicated and hidden.
2. Objection: "Why not just use f90wrap for derived types directly?"
   Response: For this workflow, f90wrap iteration cost is high when interfaces
   change frequently. The Rust layer keeps development loops shorter while still
   preserving explicit contracts at the Fortran boundary.
3. Objection: "Does this pollute or fragment SUEWS core source?"
   Response: The adapter is kept in `src/suews_core/fortran/`, not inside
   `src/suews/src/`, and delegates all science logic to existing modules. This
   keeps core ownership and scientific integrity intact.
4. Objection: "How do we prevent schema drift between languages?"
   Response: Schema version and runtime schema checks are enforced in bridge
   APIs, and payload length/field-order contracts are validated by tests.
5. Objection: "Could this become a maintenance burden?"
   Response: Scope is intentionally phased by derived type, with push-on-green
   incremental commits. Each phase must pass Rust tests, Python bridge tests and
   repository smoke tests before extension.
