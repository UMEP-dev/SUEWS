# Common SUEWS errors

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-inspect`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

The errors most users hit and how to diagnose them. Each entry: the
symptom, the root cause class (schema / input data / unit / physics), and
the fix.

## Schema errors

- **`schema_version` mismatch** -- the YAML's `schema_version` is older
  than `CURRENT_SCHEMA_VERSION`. Fix: `suews convert --format json -i old.yml
  -o new.yml`. The migration registry in
  `src/supy/util/converter/yaml_upgrade.py` runs the appropriate handlers.
- **Unknown field** -- a key the validator does not recognise. Fix: inspect
  the schema via `suews schema --format json` (canonical `{status, data}`
  envelope) or `suews schema -q export` (raw JSON Schema on stdout; `-q`
  before `export` suppresses Rich panel borders; `-o schema.json` writes to a
  file) and look up the canonical name. Do not rename fields silently.
- **Type error** -- value is not the expected type (e.g. a string where a
  number is expected). Fix: read the validator message; the standard
  envelope's `errors[].field` shows the YAML path.

## Input data errors

- **No forcing data found** -- `model.control.forcing.file` is missing, the
  path is wrong, or the file is empty. Fix: confirm
  `model.control.forcing.file` resolves relative to the YAML file's directory.
- **Missing required forcing column** -- the forcing header is missing one of
  the case-sensitive baseline columns: time coordinates (`iy`, `id`, `it`,
  `imin`) or meteorological drivers (`U`, `RH`, `Tair`, `pres`, `rain`,
  `kdown`; note lowercase `pres` and `kdown`, not `Press` or `Kdown`). Fix:
  rename or supply the missing column, then run
  `python .claude/skills/suews/scripts/check_forcing_columns.py <forcing>`
  (or `plugins/suews/skills/suews/scripts/check_forcing_columns.py` when
  installed as a plugin).
- **Forcing time axis irregular** -- gaps or duplicate rows in the time
  index. Fix: resample to the model timestep before running.
- **Land cover does not sum to 1** -- the seven `sfr` values must sum to
  1.0 (within 1e-6). Fix: rebalance, usually by absorbing the residual
  into the largest fraction.

## Unit errors

- **`pres` in Pa not kPa** -- order-of-magnitude pressure errors crash
  the boundary-layer code. Fix: divide by 1000 if you ingested ERA5 raw (or
  by 10 if in hPa).
- **`Tair` in K not deg C** -- values around 280 give an unphysical heat
  signal. Fix: subtract 273.15.
- **`kdown` in J/m^2 instead of W/m^2** -- common when ingesting hourly
  ERA5 accumulations. Fix: divide by 3600 (or by the forcing interval in
  seconds).

## Physics errors / suspicious outputs

- **Misreading energy-balance closure or missing `QF`** -- SUEWS computes
  `QH` as the residual of `QN + QF = QS + QE + QH` (plus snow terms) every
  timestep (`src/suews/src/suews_ctrl_driver.f95::SUEWS_cal_QH`), so **model**
  energy balance closes identically by construction. A non-zero closure
  residual flagged by `suews diagnose` means saved output files are truncated,
  mixed across runs, or contain improperly aggregated missing-value rows -- not
  missing `QF`. Conversely, 10-20% non-closure is normal in **observational**
  eddy-covariance tower fluxes; when evaluating against observations, ensure
  anthropogenic heat (`model.physics.emissions.heat` and
  `sites[*].properties.anthropogenic_emissions`) is configured realistically
  for the site.
- **Negative latent heat** -- possible at night (dewfall / condensation on a
  cool surface). Becomes physical when `Tsurf < Tair` and surface vapour
  pressure is below air vapour pressure. Confirm via
  `suews diagnose --format json <run_dir>`.
- **NaN propagation** -- usually starts in `state_*` from divide-by-zero
  in surface water store updates or forcing gaps. Re-check rainfall units,
  forcing NaNs, and timestep.

## Run-time errors

- **Output directory not writable** -- usually a permission issue or an
  invalid target directory. `suews run` does **not** accept an `--output` CLI
  flag; fix `model.control.output.dir` in the YAML configuration so it points
  to a writable directory.
- **Spin-up too short** -- soil moisture has not equilibrated. Fix:
  prepend at least a year of forcing or supply equilibrated `initial_states`.
