# Legacy SUEWS input-table fixtures

Canonical King's College London (KCL) **parameter** table sets for two
historical SUEWS versions, used by
`test/data_model/test_legacy_table_roundtrip.py` to exercise the
`legacy tables <-> modern YAML` round-trip (forward `convert_table`, the
`df_state -> 2025a tables` writer, and the reverse table converter).

- `2016a/` — oldest, root-layout era; the widest conversion delta (173 column
  adds on the path to the current schema).
- `2018b/` — `SUEWS-SourceCode`-layout era; the narrowest delta (one captured
  delete) and the version whose source data exercises the legacy-bounds (C3)
  path.

## Provenance and scope

Each set is the `Release/InputTables/<ver>/` directory from the public
`2020a` git tag of this repository, restricted to **input parameter tables**:
`RunControl.nml`, `SUEWS_*.txt`, `InitialConditions*.nml`, and (2018b)
`ESTMinput.nml`.

**Meteorological forcing is deliberately excluded** — the round-trip test
converts site/surface parameters only and never runs the model, so no `Kc_*`
forcing file is vendored. These are model parameters already public in the
repository's tagged history; vendoring them here makes the test self-contained
under the standard (shallow, tag-less) CI checkout.
