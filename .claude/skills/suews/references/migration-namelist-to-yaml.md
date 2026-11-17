# Migration: namelist to YAML

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-inspect`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

The legacy `RunControl.nml` + tables format is deprecated. Use YAML for
all new work; convert old cases with `suews convert`.

## One-shot conversion

```bash
suews convert --format json -i RunControl.nml -o config.yml
suews validate --dry-run --format json config.yml
```

The first command produces a current-schema YAML. The second confirms it
parses and passes schema/science validation. Read both envelopes -- `warnings`
from `convert` often flag fields that received a default because the namelist
did not provide them.

## Cross-release YAML upgrade

If you have an older YAML (e.g. from `2025.10.15`):

```bash
suews convert --format json -i old.yml -o new.yml
```

The migration registry at `src/supy/util/converter/yaml_upgrade.py`
applies the chain of handlers between the source and current schema.
Always validate after migration; the registry never deletes user fields
silently -- any drop is in the `warnings` channel.

## Hand-mapping common namelist fields

If the auto-converter lacks coverage for an obscure namelist variant,
the principal mappings are:

- `RunControl.nml` physics switches -> YAML `model.physics.*`, e.g.
  `NetRadiationMethod` -> `model.physics.net_radiation`, `EmissionsMethod` ->
  `model.physics.emissions.heat` and `model.physics.emissions.co2`,
  `StorageHeatMethod` -> `model.physics.storage_heat` (legacy `FileCode` was an
  output-file prefix, not a physics method).
- `SUEWS_AnthropogenicEmissions.txt` (and associated diurnal profiles in
  `SUEWS_Profiles.txt`) -> YAML `model.physics.emissions.heat` +
  `sites[*].properties.anthropogenic_emissions` (`heat` and `co2` blocks).
- `SUEWS_BiogenCO2.txt` -> YAML `model.physics.emissions.co2.biogenic` +
  per-vegetation parameters under `sites[*].properties.land_cover.{evetr,dectr,grass}`
  (`alpha_bio_co2`, `beta_bio_co2`, `theta_bio_co2`, `alpha_enh_bioco2`,
  `beta_enh_bioco2`, `min_res_bioco2`, `resp_a`, `resp_b`; legacy fused aliases
  `alpha_bioco2`, `beta_bioco2`, `theta_bioco2` fold to the `snake_case` keys).
- `SUEWS_NonVeg.txt` / `SUEWS_Veg.txt` / `SUEWS_Water.txt` ->
  `sites[*].properties.land_cover.<surface>` per surface.

Refer to `docs/source/inputs/yaml/` for the canonical mapping.

## Checklist after migration

1. `schema_version` in the new YAML matches `CURRENT_SCHEMA_VERSION`.
2. All seven `sfr` values present and sum to 1.0.
3. Forcing path (`model.control.forcing.file`) is portable (relative to the
   YAML location, not an absolute local path).
4. Site `sites[*].properties.timezone` is populated (some old namelists omit
   UTC offset; there is no `Country` field in YAML).
5. `suews validate --dry-run --format json config.yml` returns `status: success`
   (warnings are OK if you accept them).
6. A fresh `suews run config.yml` reproduces qualitatively the old run's main
   fluxes.
