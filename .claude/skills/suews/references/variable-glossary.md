# SUEWS variable glossary

Quick lookup for the variables you will see most often in SUEWS configs,
forcing files, and outputs. Forcing column names are **case-sensitive** and
defined in `src/supy/data_model/forcing/variables.py`; YAML fields follow
`snake_case`. Units are SI (with temperature in deg C and pressure in kPa).

## Forcing variables (baseline required)

Time-coordinate columns (interval-end timestamps):

- `iy` -- calendar year (integer).
- `id` -- day of year, 1-366 (integer).
- `it` -- hour component, 0-23 (integer).
- `imin` -- minute component, 0-59 (integer).

Baseline meteorological drivers:

- `Tair` -- air temperature, deg C (legacy file alias: `temp_c`).
- `RH` -- relative humidity, %.
- `pres` -- surface air pressure, kPa (lowercase `pres`, **not** `Press`).
- `U` -- wind speed at measurement height `sites[*].properties.z`, m/s.
- `kdown` -- incoming shortwave radiation, W/m^2 (lowercase `kdown`, **not** `Kdown`).
- `rain` -- precipitation accumulated over the forcing interval, mm per timestep.

Conditional / optional forcing columns (required only when selected physics
options consume them; missing values use `-999`):

- `ldown` -- incoming longwave radiation, W/m^2 (lowercase `ldown`, **not** `Ldown`).
- `fcld` -- cloud fraction, 0-1.
- `qn` -- observed net all-wave radiation, W/m^2 (legacy file alias: `qn1_obs`).
- `qs` -- observed storage heat flux, W/m^2 (legacy file alias: `qs_obs`).
- `qf` -- observed anthropogenic heat flux, W/m^2 (legacy file alias: `qf_obs`).
- `snow` -- observed snow-cover fraction, 0-1 (legacy file alias: `snowfrac`).
- `Wuh` -- bulk external water use over the interval, mm (legacy file aliases:
  `wu_mm`, `wuh`; per-surface columns `wuh_paved`..`wuh_water` also accepted).
- `xsmd` -- observed soil-moisture deficit (`m^3/m^3` or `kg/kg` depending on
  `model.physics.soil_moisture_deficit`).
- `lai` -- bulk observed leaf area index, m^2/m^2 (or per-vegetation
  `lai_evetr`, `lai_dectr`, `lai_grass`).
- `kdiff`, `kdir` -- diffuse and direct incoming shortwave components, W/m^2.
- `wdir` -- wind direction, deg (accepted column; not currently consumed).

## Surface cover (`sites[*].properties.land_cover.<surface>.sfr`)

Seven fractions that must sum to 1.0:

- `paved` -- sealed surfaces (roads, pavements).
- `bldgs` -- buildings (roof + walls).
- `evetr` -- evergreen trees.
- `dectr` -- deciduous trees.
- `grass` -- irrigated and unirrigated grass.
- `bsoil` -- bare soil.
- `water` -- open water.

## Output flux variables (W/m^2 unless noted)

- `QN` (or `Q*`) -- net all-wave radiation.
- `QH` -- sensible heat flux.
- `QE` -- latent heat flux.
- `QS` (or `dQS`) -- net storage heat flux.
- `QF` -- anthropogenic heat flux.

Energy balance: SUEWS solves `QH` as the residual of `QN + QF = QS + QE + QH`
(plus snow melt/freezing terms when active) every timestep, so model fluxes
close identically by construction; 10-20% non-closure is a property of
observational eddy-covariance tower data, not model outputs.

## Output state variables

- `Tsurf` -- surface temperature, deg C.
- `T2` -- diagnostic 2 m air temperature, deg C.
- `q2` -- diagnostic 2 m specific humidity, g/kg.
- `U10` -- diagnostic 10 m wind speed, m/s.
- `state_*` -- surface water store, mm.
- `soilstore_*` -- sub-surface soil moisture store, mm.

## Time and grid

- `Year`, `DOY`, `Hour`, `Min` -- output time keys.
- `Grid` -- site identifier (multi-grid runs).

## Common gotchas

- Forcing time axis must be **regular** (no missing rows) and in **UTC** or
  **local standard time** matching `sites[*].properties.timezone` and
  `model.control.forcing.timestamp_reference`. DST jumps break the model; use
  fixed standard time.
- Wind speed `U` is at the **measurement height** `sites[*].properties.z`
  declared in the config, not 10 m by default.
- `rain` is **per timestep**, not per hour, unless your forcing interval is 1 h.
