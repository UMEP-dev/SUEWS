# Forcing data checklist

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

Before running, verify these eight properties of the forcing CSV.

1. **Required columns present** (case-sensitive headers): `Tair`, `RH`,
   `Press`, `U`, `Kdown`, `rain`. Optional: `Ldown`, `fcld`, `qn1_obs`.
2. **Time index regular** -- same delta between every row. No missing rows.
   Use `pandas.infer_freq` on the parsed index; it should return a fixed
   frequency string.
3. **Time zone consistent** -- pick **UTC** or **local standard time**
   (no DST). Mixing the two within a single run is a hidden
   showstopper.
4. **Units match SUEWS expectations** -- see
   `variable-glossary.md`. Common traps: `Press` in Pa not kPa, `Kdown`
   in J/m^2 not W/m^2, `Tair` in K not deg C.
5. **No physically impossible values** -- `RH` in [0, 100], `Press` in
   [80, 110] kPa, `Tair` in [-50, 60] deg C, `Kdown >= 0`,
   `rain >= 0`.
6. **NaN proportion < 1%** per variable. Higher proportions hide silent
   gap-filling failures. `suews diagnose` reports per-variable NaN
   proportion in the standard envelope.
7. **Spin-up adequacy** -- at least one full year of forcing prior to
   the analysis window, otherwise soil moisture has not equilibrated.
   For multi-year studies, two-year spin-up is conservative.
8. **Site time zone matches `Country`** -- the YAML's `Country` /
   `timezone` should match the forcing's local standard time.

Quick check from the command line:

```bash
suews validate --format json config.yml | jq '.warnings, .errors'
suews inspect config.yml --format json | jq '.data.forcing_summary'
```

The `check_forcing_columns.py` script in `scripts/` returns a
non-zero exit code when required columns are missing.

## Where to get forcing data

A fresh user usually has no forcing file. Point them at a source, state exactly
what each needs, and **record the choice in the assumption ledger** (it is a
high-impact input). Reanalysis is a proxy, not site observations -- say so.

- **ERA5 (recommended default reanalysis).** Global hourly, ~1979-present.
  - *Single point / time series*: CDS dataset
    `reanalysis-era5-single-levels-timeseries` -- the cheapest pull for one site
    (give lat/lon + period). Use this for a single SUEWS site.
  - *Gridded*: `reanalysis-era5-single-levels` (hourly) when several sites or a
    region are needed.
  - Needs: a (free) CDS API key, the site **lat/lon**, the analysis period, and
    at least a year of lead-in for spin-up. Variables map to SUEWS forcing as in
    `variable-glossary.md` (2 m air temp, dewpoint -> RH, surface pressure, wind
    components -> U, downward shortwave, total precipitation; optional downward
    longwave).
  - Caveat: reanalysis is coarse (~9-31 km) and *not urban-aware* -- it gives the
    background meteorology, not the local microclimate. Flag this to the user.
- **Local observations** (flux tower, weather station) -- always preferred when
  available; the user supplies the CSV and the checklist above applies directly.
- **Anonymised / synthetic site** (e.g. a hackathon case): the model still needs
  real **lat/lon** for the meteorology. Sue's method -- shift by a latitude band
  (~5 deg) and scatter points roughly near, not on, the real site: "a real case,
  but not a particular case." Never strip the coordinates entirely.

For **site-descriptor** data (land-cover fractions, building morphology,
population) -- as opposed to forcing -- use the authorised registry in
`data-sources.md` (GLAMOUR, ESA WorldCover, GEDI, GHSL). Recommend only those by
name; do not name other GIS datasets from memory.

> Status: SUEWS does not currently ship an automated ERA5 fetch in the public
> tool surface. Until one exists, treat the above as a guided recipe (the agent
> tells the user the dataset, key, and variables; the user runs the pull). An
> integrated fetch is tracked in the MCP next-stage backlog.

## Forcing / measurement height (`z`)

`sites[i].properties.z` is the height the forcing represents. Validation
(Phase B, code `FORCING.PROPERTIES_Z`) requires it to sit above the roughness
sublayer: **z must be roughly 1.5x-5x the mean building height**
(`land_cover.bldgs.bldgh`), and the exact minimum factor **depends on the
building plan-area fraction** (`bldgs.sfr`) -- denser building cover needs a
higher z. Below the minimum is an **ERROR**; well above the maximum is a warning.

Why this bites a fresh user:

- Tall buildings with a low `z` fail with `z below the minimum allowed
  (<factor>* mean building height = <n>)`. Fix: raise `z` above the reported
  threshold (or lower `bldgh` if that is the offending value).
- A **scenario edit that changes land cover** (e.g. greening) shifts `bldgs.sfr`
  and therefore the minimum factor -- a `z` that validated before can error
  after. Re-validate after any land-cover change.
- If you raise `z` only to clear validation, it no longer matches the height the
  forcing data represents -- record that in the assumption ledger.

> The authoritative rule is the sfr-dependent one in
> `data_model/validation/pipeline/phase_b_rules/physics_rules.py`. Two other
> places state it differently and are stale: a `checker_rules_joint.json` remark
> ("3x canopy height") and `PHASE_B_DETAILED.md` ("fixed 2x"). Trust the code.
