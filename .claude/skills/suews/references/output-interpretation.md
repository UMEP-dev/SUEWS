# Output interpretation

> CLI: this reference uses unified `suews <subcommand>` syntax. If your install only has the legacy hyphenated entry points (`suews-validate`, `suews-inspect`, `suews-convert`, `suews-schema`, `suews-run`), substitute accordingly. See `../SKILL.md` for the rule.

How to read SUEWS results without overreaching.

## Energy balance

SUEWS enforces `QN + QF = QS + QE + QH` (plus snow melt/freezing terms when snow
is active) **identically at every timestep by construction**, solving `QH` as
the energy-balance residual (`src/suews/src/suews_ctrl_driver.f95::SUEWS_cal_QH`).
Consequently:

- **Model closure is NOT a validation or skill check.** Do not evaluate a SUEWS
  run by whether `QN + QF` matches `QS + QE + QH` within 10-20%: in intact model
  output the residual is zero to numerical precision (a non-zero residual in
  `suews diagnose` indicates truncated/mixed files or improper aggregation over
  missing-value rows).
- **Observational closure applies to flux-tower measurements only.** Eddy-covariance
  observations commonly fail to close the surface energy balance by 10-20%.
  Account for that observational residual when comparing modelled `QH` and `QE`
  against tower observations, and verify physical drivers directly:
  - `QF` (`model.physics.emissions.heat` and
    `sites[*].properties.anthropogenic_emissions`) realistic for the site.
  - spin-up long enough for soil moisture and thermal state to equilibrate.
  - forcing units (`kdown` in W/m^2, `pres` in kPa, `Tair` in deg C).

## Storage heat (`QS`)

Diurnal hysteresis with `QN` is expected -- `QS` leads in the morning, lags
in the afternoon. Large nighttime `QS` (still very negative after sunset)
suggests the OHM coefficients are wrong for this surface mix. Check
`derive_ohm_coef` outputs in `supy.util._ohm`.

## Anthropogenic heat (`QF`)

Two regimes:

- Daily mean `QF` typically 5-50 W/m^2 in mid-latitude cities,
  highest in dense centres in winter (heating) or summer (cooling).
- Diurnal cycle for traffic-dominated sites peaks at rush hour;
  building-dominated sites peak at meal times or in early morning
  heating/cooling cycles.

If `QF` is implausibly flat, check `model.physics.emissions.heat` and the
diurnal/day-of-week profiles under
`sites[*].properties.anthropogenic_emissions.heat` so the scheme uses
site-appropriate diurnal weighting.

## Latent heat (`QE`)

Negative `QE` is rare but physical -- dewfall on a cool surface. Usually a
data quality flag, not a model bug. Drying surface store after a
rainfall pulse should produce a clear `QE` peak followed by exponential
decay.

## Bowen ratio (`B = QH / QE`)

Useful summary by surface type and season:

- B >> 1: dry, dominated by sensible heating (paved, bare soil in summer).
- B ~ 1: mixed surfaces, transition seasons.
- B << 1: wet, vegetated, irrigated.

## Temperature diagnostics (`T2`, `Tsurf`)

`Tsurf - T2` gives the surface-air temperature difference. A persistent
negative value at noon suggests evaporative cooling is overactive (often
an irrigation parameter issue).

## Comparing scenarios

When using `suews compare --format json <baseline> <scenario>`, read in this
order:

1. `time_axis_overlap` -- confirm both runs cover the same period.
2. Per-variable `r` (Pearson correlation) -- should be high (>0.9) for
   the same site under different parameters; low values mean a phase
   shift, often a time zone issue.
3. `bias` -- sign and magnitude tell you direction of the change.
4. `rmse` -- overall magnitude.

Do not over-interpret single-day differences. SUEWS is a flux model; the
useful comparison is multi-day to multi-week aggregates.
