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
cargo run -- state-schema-json
cargo run -- state-default-json
cargo run -- state-default-values-json
cargo run -- state-step-json --dt 300 --dt-since-start 0 --qn1 200 --a1 0.3 --a2 0.1 --a3 5
# state-step-json also accepts wrapped payload with schema_version + state object
cargo run -- state-step-values-json --dt 300 --dt-since-start 0 --qn1 200 --a1 0.3 --a2 0.1 --a3 5
# state-step-values-json accepts either [values...] or {schema_version, values}
```

## Build Python extension

```bash
cd src/suews_core
maturin develop --features python-extension
python -c "import suews_core; print(suews_core.ohm_step(300,0,0.0,0.0,200.0,0.3,0.1,5.0))"
python -c "import suews_core as sc; s=sc.OhmState.default(); print(sc.ohm_state_schema(), s.step(300,0,200.0,0.3,0.1,5.0), s.qn_av)"
python -c "import suews_core as sc; print(sc.ohm_state_schema_version())"
python -c "import suews_core as sc; print(sc.ohm_state_schema_version_runtime())"
python -c "import suews_core as sc; print(sc.ohm_state_schema_meta())"
python -c "import suews_core as sc; print(sc.ohm_state_fields()[:10])"
python -c "import suews_core as sc; print(sc.ohm_surface_names())"
python -c "import suews_core as sc; s=sc.OhmState.default(); s.set_field_value('qn_surfs.paved', 120.0); print(s.field_value('qn_surfs.paved'))"
python -c "import suews_core as sc; s=sc.OhmState.from_dict({'qn_surfs.paved': 42.0}); print(s.to_dict()['qn_surfs.paved'])"
python -c "import suews_core as sc; s=sc.OhmState.default(); ver, vals=s.to_values_payload(); s2=sc.OhmState.from_values_payload(ver, vals); print(s2.qn_av)"
# after installing the extension in your environment:
python src/suews_core/examples/ohm_state_python_demo.py
```

## Design notes

- Adapter rationale and architecture notes:
  `src/suews_core/docs-bridge-rationale.md`
- Phase 2 extension roadmap:
  `src/suews_core/docs-phase2-roadmap.md`

## Current scope

This MVP intentionally targets the OHM path to validate:

1. stable C ABI contract,
2. Rust safety layer,
3. dual CLI/Python consumption,
4. explicit class-like state transfer for one SUEWS derived type (`OHM_STATE`).

It does not yet expose the full `SUEWS_STATE` hierarchy.
