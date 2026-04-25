# Forcing data checklist

Before running, verify these eight properties of the forcing CSV.

1. **Required columns present** (case-sensitive headers): `Tair`, `RH`,
   `Press`, `U`, `Kdown`, `rain`. Optional: `Ldown`, `fcld`, `qn1_obs`.
2. **Time index regular** — same delta between every row. No missing rows.
   Use `pandas.infer_freq` on the parsed index; it should return a fixed
   frequency string.
3. **Time zone consistent** — pick **UTC** or **local standard time**
   (no DST). Mixing the two within a single run is a hidden
   showstopper.
4. **Units match SUEWS expectations** — see
   `variable-glossary.md`. Common traps: `Press` in Pa not kPa, `Kdown`
   in J/m^2 not W/m^2, `Tair` in K not deg C.
5. **No physically impossible values** — `RH` in [0, 100], `Press` in
   [80, 110] kPa, `Tair` in [-50, 60] deg C, `Kdown >= 0`,
   `rain >= 0`.
6. **NaN proportion < 1%** per variable. Higher proportions hide silent
   gap-filling failures. `suews diagnose` reports per-variable NaN
   proportion in the standard envelope.
7. **Spin-up adequacy** — at least one full year of forcing prior to
   the analysis window, otherwise soil moisture has not equilibrated.
   For multi-year studies, two-year spin-up is conservative.
8. **Site time zone matches `Country`** — the YAML's `Country` /
   `timezone` should match the forcing's local standard time.

Quick check from the command line:

```bash
suews validate config.yml --format json | jq '.warnings, .errors'
suews inspect config.yml --format json | jq '.data.forcing_summary'
```

The `check_forcing_columns.py` script in `scripts/` returns a
non-zero exit code when required columns are missing.
