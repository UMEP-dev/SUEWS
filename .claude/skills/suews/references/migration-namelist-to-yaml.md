# Migration: namelist to YAML

The legacy `RunControl.nml` + tables format is deprecated. Use YAML for
all new work; convert old cases with `suews convert`.

## One-shot conversion

```bash
suews convert -i RunControl.nml -o config.yml --format json
suews validate config.yml --format json
```

The first command produces a current-schema YAML. The second confirms it
parses. Read both envelopes — `warnings` from convert often flag fields
that received a default because the namelist did not provide them.

## Cross-release YAML upgrade

If you have an older YAML (e.g. from `2025.10.15`):

```bash
suews convert -i old.yml -o new.yml --format json
```

The migration registry at `src/supy/util/converter/yaml_upgrade.py`
applies the chain of handlers between the source and current schema.
Always validate after migration; the registry never deletes user fields
silently — any drop is in the `warnings` channel.

## Hand-mapping common namelist fields

If the auto-converter lacks coverage for an obscure namelist variant,
the principal mappings are:

- `RunControl.nml` `FileCode` -> YAML `model.physics.netradiation_method`
  (and friends).
- `SUEWS_AnthropogenicEmissions.txt` -> YAML
  `sites[*].properties.anthropogenic` block.
- `SUEWS_BiogenCO2.txt` -> `sites[*].properties.biogen_co2`.
- `SUEWS_NonVeg.txt` / `SUEWS_Veg.txt` / `SUEWS_Water.txt` ->
  `sites[*].properties.land_cover.<surface>` per surface.

Refer to `docs/source/inputs/yaml/` for the canonical mapping.

## Checklist after migration

1. `schema_version` in the new YAML matches `CURRENT_SCHEMA_VERSION`.
2. All seven `sfr` values present and sum to 1.0.
3. Forcing path is portable (relative to the YAML location, not absolute).
4. Site `Country` / `timezone` is populated (some old namelists omit it).
5. `suews validate` returns `status: success` (warnings are OK if you
   accept them).
6. A fresh `suews run` reproduces qualitatively the old run's main fluxes.
