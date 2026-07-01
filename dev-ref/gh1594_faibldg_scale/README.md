# gh#1594 — resolution-dependent internal FAIBldg

Investigation and evidence bundle for the building frontal area index (FAIBldg)
scale problem reported on the community forum and tracked in gh#1594.

## Problem

With the legacy `FAImethod = 1` (MODELLED) building branch, the internally
calculated building frontal area index is tiny on coarse grids (~0.005 on a
1 km cell), which drives z0m to near-zero and produces counter-intuitive,
resolution-sensitive wind responses.

## Root cause

The legacy `FAImethod = 1` building branch was

    FAIBldg = sqrt(sfr_bldg / surfaceArea) * bldgH        # = lambda_p * H / sqrt(lambda_p * A)

i.e. it sets the *effective building width* to `sqrt(lambda_p * A)` — the side of
the whole built fraction merged into a single block. That length scales with the
grid size, so:

- `FAIBldg` is **not** an intensive (grid-independent) property: it scales as
  `1 / sqrt(cell area)`.
- It underestimates the true frontal area index by a factor `~sqrt(N)`, where
  `N` is the number of buildings in the cell. It is only correct when a cell
  holds ~one building (very fine resolution).

Frontal area is `N * b * H` (b = building width); merging the footprints into one
block keeps the plan area but collapses the frontage by `sqrt(N)`. That is why a
field of many small buildings is far rougher than one big block of the same
footprint, and why `lambda_p` and `A` alone cannot determine FAIBldg — a length
scale (building width, or count) is required.

The tree branches (`1.07*lambda_p`, `1.66*(1-P)*lambda_p`) are already
scale-free because they tie the crown radius to height; only the building branch
used the grid area.

## Fix (revise FAImethod = 1 and keep it experimental)

Use a fixed characteristic building width `b0` instead of the grid size:

    FAIBldg = lambda_p * H / b0        # b0 = FAI_BLDG_WIDTH, default 15 m

This is resolution-independent, reduces to the textbook cubic-array result
(`FAIBldg = lambda_p` when `b0 = H`), and is consistent with the tree branches.
The change updates the existing modelled branch (`FAImethod = 1`); it does not
add a new public FAI method.

Because the modelled FAI closure is still experimental, public-mode validation
rejects `frontal_area_index: modelled`. Normal users should use
`frontal_area_index: observed` / `FAImethod = 0` and provide site-specific
building and tree FAI values (`FAIBldg`, `FAIEveTree`, `FAIDecTree`) from
morphology data.

`b0` is a single universal constant (like the `1.07`/`1.66` tree constants), not
a per-site input. Literature building footprints (~100-250 m^2, i.e. widths
~10-16 m; up to ~20-25 m for commercial/compact fabric) and observed FAIBldg
(Grimmond & Oke 1999: 0.30 Vancouver centre, 0.33 Arcadia suburb) bracket
`b0 ~ 15-20 m`. Calibrating `b0` against building-footprint data (e.g. Microsoft
Global ML Building Footprints + GLAMOUR heights) is the recommended follow-up.

## Impact (see the scripts)

`fai_impact_analytical.py` — closed-form FAIBldg / z0m (MacDonald) / RSL Lc, beta.
`fai_impact_coupled.py` — coupled supy sweep over building density, comparing
the revised modelled branch against the legacy #192 formula, MOST and RSL.

Coupled results (H = 22 m, MacDonald roughness, b0 = 15 m, 2-week mean):

- z0m: legacy #192 gives 0.009-0.05 m (near-smooth); revised modelled FAI gives
  0.55-3.2 m (realistic), rising monotonically with density.
- MOST U10 vs density: legacy #192 is ~flat/non-monotonic (~3.5 m/s, density
  signal lost); revised modelled FAI decreases with density (2.95 -> 2.5 m/s),
  as expected.
- RSL U10 / U3 vs density: legacy #192 barely changes (~3 m/s); revised FAI drops
  strongly with density (U10 1.72 -> 0.02; U3 1.38 -> 0.00), i.e. a physical,
  density-responsive canopy wind field.

The reported anomalies trace to the legacy #192 building branch leaving the
surface aerodynamically near-smooth and density-insensitive; the revised
modelled branch restores a physical response. (The exact sign of the reporter's
U10/U3-vs-density curves depends on their full configuration; the scripts
reproduce the mechanism, not that specific config.)
