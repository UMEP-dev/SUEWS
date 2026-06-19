# Legacy SUEWS input-table fixtures

Historical SUEWS **parameter** table sets for every version on the conversion
chain from `2016a` to `2020a`, used by
`test/data_model/test_legacy_table_roundtrip.py` to exercise the
`legacy tables <-> modern YAML` round-trip in both directions (forward
`convert_table`, the `df_state -> 2025a tables` writer, and the reverse table
converter).

Vendored versions: `2016a`, `2017a`, `2017b`, `2018a`, `2018b`, `2018c`,
`2019a`, `2019b`, `2020a`.

- `2016a/` — oldest, root-layout era; the widest conversion delta (173 column
  adds on the path to the current schema). Canonical KCL site.
- `2017a/` — Ls-site fixture (different grid numbering, multiple
  InitialConditions years); exercises the writer's grid-adoption path. Its
  `SUEWS_SiteSelect.txt` is trimmed to a 3-grid representative subset (from the
  original 100 grids of the source set) — the round-trip asserts faithfulness,
  which a handful of grids prove as well as a hundred, and the full set made the
  `df_state` round-trip scale quadratically in grid count and dominate the
  `slow`-tier runtime (gh#1522). All other tables are kept intact: every grid in
  the set references the same surface codes, so trimming grids drops no
  referenced code.
- `2017b/` — table format identical to `2017a` (code-only release); enters the
  conversion graph through the `2017b -> 2017a` equivalence edge.
- `2018a/`, `2019a/`, `2019b/`, `2020a/` — synthetic `test`-site fixtures from
  the tagged release tree.
- `2018b/` — canonical KCL; the narrowest delta (one captured delete) and the
  version whose source data exercises the legacy-bounds (C3) path.
- `2018c/` — canonical KCL; table-identical sibling of `2018b` in the release
  tree, kept so the `2018c -> 2019a` rules edge is exercised from its own tag.

The two anchor versions (`2016a`, `2018b`) run in the normal CI tier; the rest
are marked `slow` and run in the `all` tier and locally.

## Provenance and scope

Each set is the `Release/InputTables/<ver>/` directory from the public
`2020a` git tag of this repository, restricted to **input parameter tables**:
`RunControl.nml`, `SUEWS_*.txt`, `InitialConditions*.nml`, and (where shipped)
`ESTMinput.nml`.

The one deviation from a verbatim copy is `2017a/SUEWS_SiteSelect.txt`, whose
grid rows are trimmed to a 3-grid subset (see the `2017a` note above); all other
tables across all versions are unmodified.

**Meteorological forcing is deliberately excluded** — the round-trip tests
convert site/surface parameters only and never run the model, so no forcing
file is vendored. These are model parameters already public in the
repository's tagged history; vendoring them here makes the tests
self-contained under the standard (shallow, tag-less) CI checkout.
