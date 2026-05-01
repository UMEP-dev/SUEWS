# Publication review checklist

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

Run through this before submitting a paper that uses SUEWS results.

## Reproducibility

- The exact `config.yml` used is in the supplementary material or a
  public repository.
- The schema version (`schema_version` field) is stated in the methods
  text.
- The supy / SUEWS version (`suews --version` or
  `python -c "import supy; print(supy.__version__)"`) is stated.
- The git commit (from `provenance.json::git_commit`) is recorded if
  the run used a development version.
- Forcing data are either public or available on reasonable request,
  with the same time axis as the run.
- A `provenance.json` is preserved next to the run output.

## Validation

- `suews validate config.yml --format json` returns `status` of
  `success` or `warning` (not `error`).
- All warnings have been read and judged acceptable; reasons are
  documented in the methods or supplement.
- `suews diagnose <run_dir> --format json` shows zero `severity: fail`
  checks.

## Physical plausibility

- Energy balance closure within ~10% on multi-day means.
- Bowen ratio falls in the expected range for the surface mix and
  season.
- Seasonal QE is positive on summer afternoons; negative QE values are
  rare (< 5% of timesteps) and explained.
- No NaN propagation in any reported variable.
- Spin-up adequate (at least one full year before the analysis window).

## Methods text checklist

- Forcing source named with version (e.g. "ERA5 single-levels, hourly,
  retrieved DD-MM-YYYY").
- Time zone explicit and matched between forcing and site.
- Land cover fractions reported per surface; totals sum to 1.0.
- Anthropogenic heat scheme named (`QF_method` value or LUCY-style
  description).
- Storage heat scheme named (OHM coefficients or AnOHM if used).
- Comparison metrics (rmse, bias, r) reported with units and sample
  size; computed via `suews compare` for reproducibility.

## Figure / table checklist

- Axes use SI units consistent with the variable glossary.
- Time axes labelled with time zone.
- Multi-panel figures share comparable y-ranges where the comparison is
  the point.
- Captions identify the SUEWS variable name plus the plain-language
  description.

## Avoid

- Quoting metrics from a single day.
- Calling a single short run a "validation"; that is a sanity check,
  not a validation.
- Reporting closure better than 5% as a model achievement; that level is
  inside measurement uncertainty.
