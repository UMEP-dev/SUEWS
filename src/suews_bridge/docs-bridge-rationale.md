# Rust Bridge Adapter Rationale (OHM MVP)

## Why keep the adapter outside `src/suews/src/`

We deliberately place adapter code under `src/suews_bridge/` rather than inside
SUEWS physics source directories to protect core integrity and reduce confusion:

1. Core physics ownership remains with existing SUEWS modules.
2. Adapter evolution can move quickly without destabilising core Fortran.
3. Review scope is clearer: adapter change vs physics change.
4. Build and dependency boundaries stay explicit.

This means we do not alter existing science logic in `module_phys_ohm`; we only
add a bridge layer with stable ABI wrappers.

## Why not rely on direct f90wrap for derived types

For this workflow, direct f90wrap around large derived-type hierarchies causes a
high iteration cost (long regenerate/rebuild cycles) and hard-to-track mapping
steps across Fortran and Python.

The Rust middle layer aims to reduce this cost by:

1. Keeping a compact stable C ABI at the Fortran boundary.
2. Moving rapid interface iteration to Rust and Python.
3. Exposing explicit schema and field mapping for derived-type payloads.

## Current boundary contract

The bridge currently focuses on `OHM_STATE` as a pilot:

1. Fortran side: `bind(c)` wrappers in `src/suews_bridge/fortran/suews_c_api_ohm.f95`.
2. Rust side: safe wrappers and state model in `src/suews_bridge/src/`.
3. Python side: class-like API via PyO3 with field-level access.

## Risks and mitigation

1. Risk: schema drift between Fortran and Rust/Python.
   Mitigation: expose schema length, surface names, ordered field list, and add
   tests that validate dimensions and roundtrip.
2. Risk: adapter code is mistaken for new physics.
   Mitigation: keep adapter in separate directory and document "no new physics"
   rule in file headers and docs.
3. Risk: maintenance overhead of an extra layer.
   Mitigation: keep scope narrow per phase and require incremental commits with
   push after each stable milestone.

## Decision statement

For the OHM pilot, keeping the adapter isolated in `src/suews_bridge/` is the
best balance between preserving SUEWS core cleanliness and enabling fast,
practical iteration on derived-type bridging to Python.
