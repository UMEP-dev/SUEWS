# SPARTACUS ground shortwave allocation: issue #1857

The previous allocation multiplied already-absorbed ground shortwave by each surface's absorptivity.
The building residual received the missing ground energy.
The correction is confined to `SPARTACUS` in `src/suews/src/suews_phys_spartacus.f95`.

For ground surface `i`, provisional absorption is
`A_i = D_dir * (1 - alpha_dir_i) + D_diff * (1 - alpha_i)`.
The incident direct and diffuse fluxes use the solver's ground-area normalisation.
The direct albedo uses `ground_albedo_dir_mult_fact` when enabled.
The final allocation is `A_i * KNet_Grnd / sum(f_i * A_i)`.
This preserves the relative absorption and conserves the solver's grid-area ground budget even though its effective ground albedo excludes tree fractions and the surface allocation includes them.
When the provisional absorbed budget vanishes, a uniform allocation of `KNet_Grnd / sum(f_i)` avoids division by a vanishing flux; darkness and perfectly reflecting ground retain zero absorption.
The erroneous, unused inverse-albedo downwelling estimate is removed.

## Scientific evidence

Baseline: `origin/master` at `2ffde7377024d58f5c3e46e61beea7c8164fdc7a`.
Both versions were built and run on Linux x86_64 with GNU Fortran 11.5.0 and Python 3.12.12.

The one-layer probe calls the full production radiation routine at fixed temperatures.
For mixed ground under 500 W m-2 incident radiation, with direct fraction 0.6, the baseline ground allocation was 129.283272 W m-2 against a solver budget of 160.933534 W m-2.
The corrected allocation conserves that budget to the 1e-8 W m-2 test tolerance.
Building QN falls from 1070.166279 to 943.565233 W m-2 of building plan area: the removed 126.601046 W m-2 is the missing ground absorption divided by the 0.25 building fraction.
The probe also checks relative surface absorption with the direct-albedo multiplier, pure direct/diffuse illumination, darkness, black ground and reflecting ground.

The coupled comparison uses `run_surface_case()` in `test/physics/test_spartacus_surface_run.py`: six five-minute sample-forcing timesteps, 2012-01-02 09:35--10:00, SPARTACUS radiation (1003) and OHM storage (1).
The ground-area geometry is matched to the land-cover fractions and the top layer above the tree crowns has zero vegetation fraction.
There is no spin-up; this is a controlled short regression case, not a climatological assessment.
The table contains six-step means in W m-2 of each surface.

| Surface | Fraction | QN before | QN after | QH before | QH after |
|---|---:|---:|---:|---:|---:|
| Paved | 0.43 | 4.789356 | 10.268319 | 152.586269 | 158.387858 |
| Buildings | 0.38 | 26.403649 | 16.076578 | 174.200562 | 164.196117 |
| Evergreen trees | 0 | -4.101442 | 1.360680 | 141.861896 | 147.599631 |
| Deciduous trees | 0.02 | -3.800129 | 1.530276 | 133.682317 | 139.344735 |
| Grass | 0.03 | 8.333912 | 13.333215 | 145.719480 | 151.053471 |
| Bare soil | 0 | 3.952562 | 8.953086 | 149.851804 | 155.131681 |
| Water | 0.14 | 23.723464 | 29.695665 | 50.227114 | 53.555767 |

OHM assigns the bulk QS to each surface in this case.
Every surface's mean QS changes from -21.671657 to -21.994283 W m-2.
The zero-fraction surfaces in the table are diagnostics and contribute nothing to the grid budget.

| Grid-area quantity | Before | After | Change |
|---|---:|---:|---:|
| QN | 15.588110 | 15.112472 | -0.475638 |
| QS | -21.671657 | -21.994283 | -0.322626 |
| QH | 145.885336 | 145.317610 | -0.567726 |
| QE | 17.499686 | 17.914401 | +0.414715 |
| Upward shortwave | 33.035196 | 33.035196 | 0 |
| Upward longwave | 360.140380 | 360.616017 | +0.475638 |
| Ground net shortwave | 38.606210 | 38.606210 | 0 |
| Top net shortwave | 86.958138 | 86.958138 | 0 |

**Mechanism and expected direction:** correcting ground absorption raises ground QN and removes the spurious building contribution.
The solver's ground and top shortwave budgets remain unchanged.
In the coupled case, changed surface heating and evaporation feed back through temperature and longwave emission, OHM storage and sensible heat.
The observed bulk changes are below 0.7 W m-2 at every timestep, while the surface redistribution is several W m-2.
The ground shortwave budget error changes from -3.398610 to -4.678832 W m-2 across the baseline window to at most 5.7e-14 W m-2 after correction.

**Flat-tile limitation:** the uniform grass test is retained, but the current baseline segfaults at the canopy-depth assignment before radiation when both building and tree fractions are zero.
That specific native failure is expected under the separate PR #1854.
The test executes its 400 W m-2 shortwave and surface-QN-equals-bulk assertions as soon as the flat path becomes reachable.
No code from #1854 is included here.

## Reference audit and validation

The full physics tier passes its existing tolerances, including the sample and STEBBS reference tests.
An additional before/after comparison of every STEBBS output column nevertheless finds changes in 64 of 82 columns across the 288 validation timesteps on 2017-08-27, after the existing one-day spin-up.
This uses the configuration and forcing window from `TestSTEBBSOutput.test_stebbs_building_energy_outputs`.
Examples of maximum absolute changes are:

| STEBBS quantity | Maximum absolute change |
|---|---:|
| `QN_bldg_FA` | 5.226263 W m-2 |
| `QS_total_FA` | 4.021043 W m-2 |
| `QH_bldg_FA` | 2.592112 W m-2 |
| `QS_wall_FA` | 3.524005 W m-2 |
| `QHload_cooling_FA` | 0.142800 W m-2 |
| `Tair_ind` | 0.014164 K |

**Reference fixture requiring a decision:** `test/fixtures/data_test/stebbs_test/sample_output_stebbs.csv`.
No reference fixture was regenerated or edited.
The job's explicit instruction was to stop at diagnosis if reference outputs move.
This branch therefore records the correction and evidence for review, but is not merge-ready under the physics-change evidence rule until the reference update is authorised and resolved.
The main sample and benchmark configurations use NARP, which does not enter the changed allocation.

Validation commands, run on the compute host:

```text
make dev
make test-smoke
python -m pytest test/physics/test_spartacus_ground_budget.py test/physics/test_spartacus_surface_run.py -q --tb=short
python -m pytest test -m physics -q --tb=long
make test
```

The smoke suite passed with 23 passed and 1 skipped.
The new tests passed with 12 passed and the one flat-tile expected failure described above.
The full physics tier passed with 328 passed, 1 skipped and 3 expected failures.
The everyday suite passed with 2164 passed, 6 skipped and 3 expected failures.
The other two expected physics failures are the existing snow-enabled and Hoegstrom stability option-sweep cases.

**Domain-owner sign-off:** pending `@sunt05` for SPARTACUS and `@yiqing1021` for the changed STEBBS reference behaviour.
No approval is implied by these calculations.
