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
