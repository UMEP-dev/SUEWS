# SUEWS variable glossary

Quick lookup for the variables you will see most often in SUEWS configs and
outputs. Names follow the YAML/snake_case convention. Units are SI.

## Forcing variables (required)

- `Tair` -- air temperature, deg C.
- `RH` -- relative humidity, %.
- `Press` -- air pressure, kPa.
- `U` -- wind speed at measurement height, m/s.
- `Kdown` -- incoming shortwave radiation, W/m^2.
- `rain` -- precipitation, mm per timestep.

Optional forcing (improves model skill if available): `Ldown` (longwave),
`fcld` (cloud fraction), `qn1_obs` (observed Q*).

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

Energy balance: `QN + QF ~= QH + QE + QS` (closure within ~10% on hourly
data is reasonable).

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
  **local standard time consistent with the site time zone**. DST changes
  break the model; use a non-DST time zone string.
- Wind speed `U` is at the **measurement height** declared in the config,
  not 10 m by default.
- `rain` is **per timestep**, not per hour, unless your timestep is 1 h.
