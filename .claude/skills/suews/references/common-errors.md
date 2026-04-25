# Common SUEWS errors

The errors most users hit and how to diagnose them. Each entry: the
symptom, the root cause class (schema / input data / unit / physics), and
the fix.

## Schema errors

- **`schema_version` mismatch** — the YAML's `schema_version` is older
  than `CURRENT_SCHEMA_VERSION`. Fix: `suews convert -i old.yml -o new.yml
  --format json`. The migration registry in
  `src/supy/util/converter/yaml_upgrade.py` will run the appropriate
  handlers.
- **Unknown field** — a key the validator does not recognise. Fix: check
  the schema export with `suews schema export --format json` and look for
  the canonical name. Do not rename fields silently.
- **Type error** — value is not the expected type (e.g. a string where a
  number is expected). Fix: read the validator message; the standard
  envelope's `errors[].field` shows the YAML path.

## Input data errors

- **No forcing data found** — `forcing_file` is missing, the path is
  wrong, or the file is empty. Fix: confirm `forcing_file` resolves
  relative to the config file's directory.
- **Missing required forcing column** — the CSV header is missing
  `Tair` / `RH` / `Press` / `U` / `Kdown` / `rain`. Fix: rename the
  column or add it. Run `check_forcing_columns.py` from `scripts/`.
- **Forcing time axis irregular** — gaps or duplicate rows in the time
  index. Fix: resample to the model timestep before running.
- **Land cover does not sum to 1** — the seven `sfr` values must sum to
  1.0 (within 1e-6). Fix: rebalance, usually by absorbing the residual
  into the largest fraction.

## Unit errors

- **`Press` in Pa not kPa** — order-of-magnitude pressure errors crash
  the boundary-layer code. Fix: divide by 1000 if you ingested ERA5 raw.
- **`Tair` in K not deg C** — values around 280 give a hot-summer signal.
  Fix: subtract 273.15.
- **`Kdown` in J/m^2 instead of W/m^2** — common when ingesting hourly
  ERA5. Fix: divide by 3600 (or by the timestep in seconds).

## Physics errors / suspicious outputs

- **Energy balance closure off by >20%** — usually `QF` (anthropogenic
  heat) is missing or wrong. Fix: configure `Anthropogenic` realistically
  for the site or set the simple `QF_method` to a known regional default.
- **Negative latent heat** — possible at night with a warm surface.
  Becomes physical only when `Tsurf < Tair` and `state` is dry. Confirm
  via `suews diagnose`.
- **NaN propagation** — usually starts in `state_*` from divide-by-zero
  in surface water store updates. Re-check rainfall units and timestep.

## Run-time errors

- **Output directory not writable** — usually a permission issue or
  the path resolved outside the project root. Fix: provide
  `--output <abs_dir>`.
- **Spin-up too short** — soil moisture has not equilibrated. Fix:
  prepend a year of forcing or supply a warm-start checkpoint.
