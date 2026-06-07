# Legacy benchmark inputs as modern YAML (Phase B)

Each `<ver>.yml` here is the **modern-YAML view of a historical canonical-KCL
benchmark input**: the legacy `Release/InputTables/<ver>/` table set converted
forward to the current schema (`schema_version: '2026.5'`) via the existing
`suews-convert` path. These are *not yet* table-regenerative (regenerating the
original tables from YAML is Phase C); they prove the legacy input is faithfully
representable in the current schema and that its `df_state` round-trips stably.

## Scope: canonical KCL only

A survey of `Release/InputTables/<ver>/RunControl.nml` (vendored in the `2020a`
git tag) showed only **four** legacy versions ship the canonical King's College
London input (`FileCode='Kc'`):

| Version | FileCode | Stored here? | Notes |
|---------|----------|--------------|-------|
| 2016a   | `Kc`     | yes          | Converted `from_ver=2016a`. |
| 2017a   | `Ls`     | no           | Different site (London/`Ls`), not the KCL benchmark. |
| 2017b   | `Kc`     | yes          | 2017a-format tables (column-identical to 2017a); converted `from_ver=2017a`. |
| 2018a   | `test`   | no           | Synthetic dev fixture. |
| 2018b   | `Kc`     | **Phase-C candidate** | Source data rejected by current validator (see below). |
| 2018c   | `Kc`     | **Phase-C candidate** | Byte-identical InputTables to 2018b (same git tree). |
| 2019a   | `test`   | no           | Synthetic dev fixture. |
| 2020a   | `test`   | no           | Synthetic dev fixture (Gothenburg). |
| 2021a   | —        | no           | No `Release/InputTables/2021a` in the `2020a` tag. |

The `test`/`Ls` versions are synthetic or a different site and are out of scope
for the KCL benchmark lineage.

## Acceptance gate (both halves must hold)

For each stored YAML, `tests/test_legacy_yaml_roundtrip.py` asserts:

1. **Loads** under the current validator
   (`SUEWSConfig(**yaml.safe_load(...))`); and
2. **`df_state` round-trips stably**:
   `df = cfg.to_df_state(); cfg2 = from_df_state(df); df2 = cfg2.to_df_state()`
   with `df.equals(df2)`.

| Version | Loads | Round-trip stable | df_state shape |
|---------|-------|-------------------|----------------|
| 2016a   | yes   | yes (`df.equals` True) | (1, 2866) |
| 2017b   | yes   | yes (`df.equals` True) | (1, 2866) |
| 2018b   | **no** | n/a (fails at load) | — |
| 2018c   | **no** | n/a (fails at load) | — |

## Cosmetic cleanups applied (no physics hand-edited)

The forward converter leaves two machine-specific artefacts; both are cleaned to
neutral placeholders. No physics value is hand-edited.

- `model.control.output.dir` — converter writes an absolute
  `/private/var/folders/.../Output` temp path → set to `./output`.
- `model.control.forcing.file` — a `forcing.txt` placeholder (or an absolute
  temp path where the converter located a year-stamped forcing). The legacy
  forcing is not year-stamped in a way the YAML path needs, and forcing wiring
  is a separate concern (handled by the legacy run staging in
  `benchmark/legacy_input.py`) → set to a neutral `forcing.txt` placeholder.

`name`/`description` were also made version-specific (metadata only).

## 2018b / 2018c — Phase-C candidates (load failure, not round-trip failure)

2018b and 2018c ship **byte-identical** InputTables (same git tree
`b99627f9…`), so they produce identical YAML and the same failure. The current
validator **rejects the converted config at load time**:

```
test site evergreen trees: alb_id (0.1) must be in range
[alb_min, alb_max] ([0.11, 0.12] provided)
```

This is a **genuine inconsistency in the source KCL tables**, carried through
faithfully by the converter — not a converter or round-trip bug:

- The KCL `SUEWS_SiteSelect.txt` sets `Code_EveTr=662`, but `SUEWS_Veg.txt`
  row 662 is labelled "DecTr London" with `AlbedoMin=0.12, AlbedoMax=0.18`; the
  effective evergreen bounds the validator sees are `[0.11, 0.12]`.
- `InitialConditionsKc_2012.nml` sets `albEveTr0=0.10`, which is **below** the
  referenced row's `AlbedoMin`.

Only the **evetr** surface violates (`alb_id=0.10 < alb_min=0.11`); `dectr` and
`grass` sit exactly at their lower bound (accepted). The legacy Fortran did not
enforce `alb_min <= alb_id <= alb_max`; the modern validator does. This is
exactly the "structurally valid but physically out-of-bounds" legacy data the
modern schema now rejects.

These two versions are therefore **Phase-C candidates**: storing them as
validated modern YAML needs either a legacy-bounds relaxation path or a
documented source-data correction (out of scope for Phase B, which must not
hand-edit physics values). They are intentionally **not** committed as fixtures
so the round-trip test stays green; convert them on demand with
`suews-convert -f 2018b -i <InputTables/2018b>/RunControl.nml -o 2018b.yml`.

## Converter forward-defaults (Phase-C "legacy extras" input)

When chaining a legacy table set forward, `suews-convert` injects modern columns
the old tables lacked (filled with schema defaults) and renames/drops some
RunControl keys. These are the fields a Phase-C reverse bridge must restore from
a version-tagged `legacy_extras` block. Per-version counts (from the converter
log):

- **2016a** (chained 2016a→2025a): 173 columns added, 18 RunControl renames,
  19 deletes. The widest gap — adds the full ESTM-class fractions/codes, AnOHM
  coefficients (`AnOHM_Cp/Kk/Ch`), OHM thresholds (`OHMThresh_SW/WD`),
  RSL/FAI method flags (`rslmethod`, `rsllevel`, `faimethod`), anthropogenic-CO2
  and BEU traffic fields, internal/wall thermal layers, etc.
- **2017b** (via 2017a→2025a): 57 columns added, 8 renames, 13 deletes — mostly
  the anthropogenic-emission / CO2 / traffic-profile and RSL/FAI additions.
- **2018b/2018c** (2018b→2025a): 18 columns added, 1 rename, 3 deletes — the
  narrowest gap (`rslmethod`, `rsllevel`, `faimethod`, `BaseTMethod`,
  `CO2PointSource`, irrigation-fraction split, `H_maintain`, `n_buildings`,
  `rcmethod`, `FcEF_v_kgkmWD/WE`, `FrPDDwe`, metabolism min/max).
