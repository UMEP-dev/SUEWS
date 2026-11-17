# Publication review checklist

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-inspect`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

Run through this before submitting a paper that uses SUEWS results.

## Reproducibility

- The exact `config.yml` used is in the supplementary material or a
  public repository.
- The schema version (`schema_version` field) is stated in the methods
  text.
- The supy / SUEWS version (`python -c "import supy; print(supy.__version__)"`,
  also recorded in `provenance.json`; note `suews` has no `--version` flag) is
  stated.
- The git commit (from `provenance.json`) is recorded if the run used a
  development version.
- Forcing data are either public or available on reasonable request,
  with the same time axis as the run.
- A `provenance.json` is preserved next to the run output.

## Validation

- `suews validate --dry-run --format json config.yml` (or `suews validate
  --format json config.yml`) returns `status` of `success` or `warning` (not
  `error`).
- All warnings have been read and judged acceptable; reasons are
  documented in the methods or supplement.
- `suews diagnose --format json <run_dir>` shows zero `severity: fail`
  checks.

## Physical plausibility

- When evaluating against eddy-covariance flux observations, the
  **observational** energy-balance closure residual (`QN + QF` vs observed
  `QH + QE + QS`) is quantified and discussed alongside flux evaluation metrics
  (modelled fluxes always close identically by construction because `QH` is
  solved as the residual).
- Bowen ratio falls in the expected range for the surface mix and
  season.
- Seasonal `QE` is positive on summer afternoons; negative `QE` values are
  rare (< 5% of timesteps) and explained.
- No NaN propagation in any reported variable.
- Spin-up adequate (at least one full year before the analysis window).

## Methods text checklist

- Forcing source named with version (e.g. "ERA5 single-levels, hourly,
  retrieved DD-MM-YYYY").
- Time zone (`sites[*].properties.timezone` and
  `model.control.forcing.timestamp_reference`) explicit and matched between
  forcing and site.
- Land cover fractions reported per surface; totals sum to 1.0.
- Anthropogenic heat scheme named (`model.physics.emissions.heat` selector and
  `sites[*].properties.anthropogenic_emissions` configuration).
- Storage heat scheme named (`model.physics.storage_heat` -- OHM coefficients,
  AnOHM, or EHC).
- Comparison metrics (`rmse`, `bias`, `r`) reported with units and sample
  size (`n`); computed via `suews compare --format json <run_a> <run_b_or_obs>`
  for reproducibility.

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
- Reporting **model** energy-balance closure as a model skill achievement --
  SUEWS solves `QH` as the energy-balance residual every timestep, so model
  closure holds identically by construction; closure checks apply only to
  independent observational flux-tower measurements.
