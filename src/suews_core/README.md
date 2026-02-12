# SUEWS Rust Bridge MVP

This crate is a minimal end-to-end bridge for one SUEWS OHM timestep:

- Fortran `bind(c)` facade in `src/suews_core/fortran/suews_c_api_ohm.f95`
- Rust FFI + safe wrappers in `src/suews_core/src/`
- CLI binary (`suews`)
- Optional Python module via PyO3 (`maturin develop`)

It now includes a first derived-type bridge for `OHM_STATE` using explicit
flatten/unflatten mapping at the Fortran boundary.

To support Python-side class mirroring with less manual glue code, the bridge
also exposes a stable ordered field-name schema (`ohm_state_fields`) matching
the flat payload layout.

## Build and test (Rust only)

```bash
cd src/suews_core
cargo test
cargo run -- qs --qn1 250 --dqndt 12.5 --a1 0.3 --a2 0.1 --a3 5
cargo run -- state-step --dt 300 --dt-since-start 0 --qn1 200 --a1 0.3 --a2 0.1 --a3 5
cargo run -- state-schema
```

## Build Python extension

```bash
cd src/suews_core
maturin develop --features python-extension
python -c "import suews_core; print(suews_core.ohm_step(300,0,0.0,0.0,200.0,0.3,0.1,5.0))"
python -c "import suews_core as sc; s=sc.OhmState.default(); print(sc.ohm_state_schema(), s.step(300,0,200.0,0.3,0.1,5.0), s.qn_av)"
python -c "import suews_core as sc; print(sc.ohm_state_fields()[:10])"
python -c "import suews_core as sc; s=sc.OhmState.default(); s.set_field_value('qn_surfs.paved', 120.0); print(s.field_value('qn_surfs.paved'))"
```

## Current scope

This MVP intentionally targets the OHM path to validate:

1. stable C ABI contract,
2. Rust safety layer,
3. dual CLI/Python consumption,
4. explicit class-like state transfer for one SUEWS derived type (`OHM_STATE`).

It does not yet expose the full `SUEWS_STATE` hierarchy.
