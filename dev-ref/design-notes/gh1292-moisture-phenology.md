# Moisture-aware phenology for rainfall-driven sites

**Upstream issue:** [GH-1292](https://github.com/UMEP-dev/SUEWS/issues/1292)
**Related:** [GH-1291](https://github.com/UMEP-dev/SUEWS/issues/1291) (observed-LAI forcing silently ignored)
**Status:** Design, pre-implementation. Needs scientific review before any code lands.
**Labels:** `1-feature`, `2-module:dailystate`, `4-needs-science`.
**Scope note:** This is an internal design record for core developers. The public issue on GitHub is sufficient for community visibility -- we do not paraphrase community forum threads here.

---

## 1. Problem statement

SUEWS phenology currently evolves LAI through growing-degree-day (GDD) and senescence-degree-day (SDD) thresholds computed from daily mean air temperature. There is no precipitation or soil-moisture term anywhere in the governing equations.

Thermal-only accumulation is implemented in `src/suews/src/suews_phys_dailystate.f95`:

- `update_GDDLAI` signature at lines 538-560.
- GDD/SDD accumulation loop at lines 582-617.
- LAI power-law update at lines 627-672.

For sites where the annual LAI cycle is controlled by rainfall rather than temperature (monsoon grasslands, arid/semi-arid systems, pulsed wet-season savannas, double-cropping agricultural systems) the scheme cannot reproduce observed seasonality at any tuning of `BaseT`, `GDDFull`, `SDDFull`, or `LAIPower`. The limitation is acknowledged in Omidvar et al. (2022), *Geosci. Model Dev.* 15, 3041-3078, Appendix D (Figs D1/D2; US-MMS deciduous broadleaf works, US-SRG monsoon grassland fails). The paper explicitly flags hydrological feedback as future work.

---

## 2. Empirical evidence: the thermal scheme's failure mode

Ting's prior FLUXNET2015 evaluation (the `SUEWS-FLUXNET2015` repo that produced Omidvar et al. 2022) already derived per-site thermal-only LAI parameters for ~100 sites via the `LAIModel` Mathematica package. The pattern at moisture-limited sites is diagnostic: the fit collapses to near-zero seasonality rather than signalling failure.

Selected rows from the derived parameters (`data/csv-prm/prm-LAI-Albedo.csv` in that repo):

- **AU-ASM** (Alice Springs, mulga, semi-arid, MAP ~300 mm): LAImin = 0.20, LAImax = 0.30, GDDFull = 93. The fitted seasonal amplitude is essentially zero and GDDFull is tiny -- the thermal driver saturates year-round, so LAI is effectively constant.
- **AU-DaS** (Daly River savanna, tropical monsoon): LAImin = 0.97, LAImax = 1.86, GDDFull = 351. Amplitude under-estimated compared with MODIS.
- **AU-Gin** (Gingin, Mediterranean woody savanna): LAImin = 0.60, LAImax = 1.09, GDDFull = 239. Again compressed.

Contrast with thermally-controlled sites fitted by the same pipeline:

- **US-MMS** (Morgan-Monroe, temperate deciduous broadleaf): LAImin = 1.01, LAImax = 5.00, GDDFull = 69. Strong, physically sensible cycle.
- **US-Syv** (Sylvania, temperate mixed forest): LAImin = 0.66, LAImax = 4.81.

Taken together, the thermal-only fits at dryland and monsoon sites do not merely mistime leaf-on -- they erase the seasonality altogether. This is the empirical evidence that motivated Omidvar Appendix D and is the basis for introducing a moisture control.

**Data available locally.** The `SUEWS-FLUXNET2015` repo under `/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS-FLUXNET2015/` contains per-site pre-processed forcings and SUEWS simulation outputs in HDF5, MODIS LAI, SoilGrids soil properties, and the derived-parameters CSVs. The Mathematica parameter-fitting package (`LAIModel.m` from `/Users/tingsun/Dropbox (Personal)/6.Repos/LAI-model/`) is the pipeline to extend for a moisture-aware refit. The `AmeriFluxEvaluation` repo has companion Python notebooks (`GDD_SDD_LAI_OBS.ipynb`, `LAI.ipynb`) that explore GDD/SDD vs observed LAI.

---

## 3. Verified facts about the current code

Confirmed in the repo at time of writing (branch `sunt05/gh1292-lai-moisture`):

- **Driver call order.** In `src/suews/src/suews_ctrl_driver.f95`:
  - Step A1 `SUEWS_cal_DailyState` at line 320.
  - Step A10 `SUEWS_cal_SoilState` at line 3085.
  - Consequence: when end-of-day phenology fires inside DailyState, current-timestep SMD is not yet computed. DailyState therefore sees the previous day's end-of-day SMD, which is exactly what a moisture-aware phenology scheme wants. No driver-level reordering is needed.
- **`LAIType` switch.** Declared as `FlexibleRefValue(int)` in `src/supy/data_model/core/site.py` around line 401. Existing values: `0` = original, `1` = new high-latitude variant. Adding values 2+ is the natural extension.
- **`LAIParams`.** Defined in `src/supy/data_model/core/site.py` lines 348-496, nested inside `EVETRParams` / `DECTRParams` / `GRASSParams`. `to_df_state` / `from_df_state` already tolerate missing columns with defaults (pattern at 441-450).
- **Per-surface vegetation SMD.** Computed by `cal_smd_veg()` in `src/suews/src/suews_phys_waterdist.f95` and carried as `modState%hydroState%smd_nsurf`. Capacity information accessible via the soil-store parameters on the same state object.
- **`LAI_obs` forcing path.** `LAI_obs` is read into the `forcing` struct in the dailystate module and used only when `LAICalcYes == 0`. That branch is currently broken -- tracked in GH-1291. The interim workaround recommended to users (prescribe observed LAI) therefore does not work today.

---

## 4. Literature review: how other models couple water to phenology

Our design decision should not invent a new formulation if a published scheme already fits. The literature divides into three schools.

### 4.1 Empirical bioclimatic indices

- **Jolly, Nemani & Running (2005).** *Glob. Change Biol.* 11, 619-632. doi:10.1111/j.1365-2486.2005.00930.x. The **Growing Season Index (GSI)** is a multiplicative product of piecewise-linear indicators on minimum temperature, vapour-pressure deficit and photoperiod. No soil-moisture term; water stress enters only indirectly via VPD. This is close to what SUEWS does now, minus the photoperiod term, and is known to be inadequate for monsoon systems because leaf-on there follows rainfall pulses regardless of VPD.
- **Stockli et al. (2008, 2011).** *JGR Biogeosci.* doi:10.1029/2008JG000781 and doi:10.1029/2010JG001545. Prognostic extension of GSI with a fourth multiplicative factor on soil moisture; MODIS FPAR/LAI assimilated for parameter estimation. The SM factor is Jarvis-style.
- **Caldararu, Purves & Palmer (2014).** *Biogeosciences* 11, 763-778. doi:10.5194/bg-11-763-2014. Prognostic LAI optimising net canopy carbon return under light, temperature and soil-moisture constraints. Grasslands emerge explicitly as water-limited in the Bayesian fit.
- **Knorr et al. (2010).** *JGR Biogeosci.* doi:10.1029/2009JG001119. Generic 3-PFT-class BETHY phenology where leaf dynamics respond prognostically to temperature, water and light. Documents a known oscillation pathology under sustained drought -- a design warning for any NPP-coupled leaf balance.
- **Forkel et al. (2014).** *Glob. Change Biol.* 21, 3414-3435. doi:10.1111/gcb.12950. LPJmL with explicit water-availability factor; finds water is a co-dominant control on start-of-season and peak greenness globally, dominant in drylands.

### 4.2 Major LSM precedents

- **CLM5 (Lawrence et al. 2019).** *J. Adv. Model. Earth Syst.* 11, 4245-4287. doi:10.1029/2018MS001583. Defines three phenology types: evergreen, seasonally deciduous (GDD-triggered), and **stress-deciduous**. Stress-deciduous triggers leaf-on when both a root-zone soil-water threshold **and** a soil-temperature threshold persist for ~15 days; leaf-off triggered when either falls below the off-threshold. This is the closest published precedent for what SUEWS needs.
- **Noah-MP (Niu et al. 2011).** *JGR Atmos.* 116, D12109. doi:10.1029/2010JD015139. Dynamic-vegetation option carries prognostic leaf/stem/root/wood C pools; LAI = f(leaf C, SLA). SM enters indirectly through GPP stomatal stress. Known to underperform in arid/semi-arid unless retuned.
- **JULES/TRIFFID (Best et al. 2011, Clark et al. 2011).** *GMD* 4, 677-699 and 701-722. GDD-controlled leaf turnover; no direct SM trigger. Water acts on GPP through the beta factor.
- **ORCHIDEE (Krinner et al. 2005).** *Glob. Biogeochem. Cycles* 19, GB1015. doi:10.1029/2003GB002199. PFT-specific schemes: GDD for temperate trees, **GDD plus moisture trigger for tropical raingreen**, and a **moisture-onset trigger** for C4 grass; water stress accelerates senescence. A direct precedent for per-surface differentiation of the moisture response.
- **LPJ-DGVM / LPJmL (Sitch et al. 2003; Smith et al. 2014).** *Glob. Change Biol.* 9, 161-185; *Biogeosciences* 11, 2027-2054. The **raingreen PFT** holds LAI at maximum while water-stress scalar W >= 0.35 and drops leaves when W < 0.35 -- the simplest published threshold form.
- **ISBA-A-gs / SURFEX (Gibelin et al. 2006).** *JGR Atmos.* 111, D18102. doi:10.1029/2005JD006691. Implicit phenology: LAI = leaf biomass / constant ratio; biomass driven by A-gs photosynthesis so SM enters via stomatal conductance. Reproduces precipitation-controlled interannual LAI variability reasonably without a dedicated SM trigger.

### 4.3 Cropping / rangeland schemes with explicit water stress

- **WOFOST / DSSAT / APSIM.** Jarvis-style water-stress factor Ta/Tp (actual over potential transpiration, or TURFAC/SWFAC in DSSAT) multiplies daily leaf area expansion and accelerates senescence. Jones et al. (2003) *Eur. J. Agron.* 18, 235-265. Holzworth et al. (2014) *Environ. Model. Softw.* 62, 327-350.
- **Biome-BGC (White et al. 2000; Thornton et al. 2002).** Daily bucket soil water; GPP-driven leaf allocation. Phenology timing uses climatologic triggers, LAI accumulation scales with GPP.
- **Grassland-specific.** LINGRA (Schapendonk et al. 1998), PaSim (Riedo et al. 1998): leaf expansion * water-stress factor. DayCent/CENTURY (Parton et al. 1998) was historically criticised for thermal-only phenology and has been patched; Liu et al. (2022), *Agric. For. Meteorol.* 327, 109230, doi:10.1016/j.agrformet.2022.109230, propose a nonlinear soil-water senescence function specifically for grassland brown-down.

### 4.4 Dryland and monsoon studies

- **Dahlin, Fisher & Lawrence (2015).** *Biogeosciences* 12, 5061-5074. doi:10.5194/bg-12-5061-2015. Tests CLM's drought-deciduous trigger against FLUXNET and MODIS; the double-threshold design works but is sensitive to rooting depth and the 15-day persistence window.
- **Broich et al. (2014).** *Biogeosciences* 11, 5181-5198. doi:10.5194/bg-11-5181-2014. Australian semi-arid LSP; precipitation alone explains ~80 % of interannual variance in greening-season length on the North Australian Tropical Transect.
- **Zhang et al. (2005).** *Int. J. Remote Sens.* 26, 4517-4533. doi:10.1080/01431160500113971. 1-3 month precipitation-to-NDVI lags across Africa.
- **Jolly & Running (2004).** *Glob. Change Biol.* 10, 303-308. doi:10.1111/j.1365-2486.2003.00701.x. Kalahari field evidence that **soil-water potential** outperforms cumulative rainfall as a leaf-out predictor.
- **Hickler et al. (2005).** *Geophys. Res. Lett.* 32, L21415. doi:10.1029/2005GL024370. Precipitation controls on Sahel greening trend.

### 4.5 Common mathematical forms

- **Multiplicative Jarvis-style stress factor** f(w) in [0,1]: used by WOFOST, DSSAT, APSIM, Biome-BGC, ORCHIDEE (beta), Stockli 2008, ISBA implicitly. Two parameters (wilting point / field capacity or equivalent thresholds), optional curvature exponent.
- **Threshold / persistence trigger.** Leaf-on when running-mean SM > W_on for tau days; leaf-off when SM < W_off. Used by CLM5 stress-deciduous, LPJ raingreen (single 0.35 threshold). Three parameters.
- **Logistic** f = 1 / (1 + exp(-k(w - w50))). Smooth alternative; Caldararu et al., Liu et al. 2022. Two parameters.
- **Additive senescence stress** dSDD_eff = dSDD + gamma*(1 - f(w)). Used by ORCHIDEE, WOFOST family. One extra parameter.
- **Bucket trigger** growth allowed only when theta/theta_FC > theta_crit. Stockli 2008 fourth factor; raingreen variants. One parameter.

### 4.6 Observational consensus

Across the remote-sensing phenology literature (Zhang et al. 2005; Broich et al. 2014, 2015; Wang, Rich & Price 2003 *IJRS* 24, 2345-2364; Jolly & Running 2004) the consistent findings are:

- Antecedent precipitation over a 1-3-month window is a competitive predictor of onset timing in drylands and monsoon systems.
- Root-zone soil moisture is a better predictor than rainfall itself once a simple bucket model is available.
- In monsoon grass/savanna leaf-on is triggered by rainfall onset regardless of GDD state, so a multiplicative stress factor alone is necessary but not sufficient -- a trigger is needed.

---

## 5. Design space for SUEWS

Given the literature, four candidate designs remain on the table:

- **Design A: Multiplicative SM factor only.** Keep the GDD accumulator; multiply dGDD by f(w). Two new parameters. Closest to WOFOST-family schemes. Will **reduce** GDD accumulation during dry periods but cannot **prevent** leaf-on if GDD has already saturated -- a real risk in dryland sites where the thermal driver saturates year-round (cf AU-ASM in section 2 above).
- **Design B: Persistence trigger only.** Gate leaf-on on a 15-day running mean of SM exceeding W_on. Three parameters (W_on, W_off, tau). Equivalent to CLM5 stress-deciduous. Solves the AU-ASM pathology but discards the temperature signal when it matters (e.g. high-latitude continental where cold snaps should still delay leaf-out).
- **Design C: Hybrid (multiplicative + trigger).** Apply both: f(w) multiplies dGDD during the build-up phase, and leaf-on is additionally gated on the persistence trigger. Four parameters but no redundancy -- each addresses a distinct failure mode. This is effectively CLM5's stress-deciduous logic wrapped around the SUEWS GDD accumulator.
- **Design D: GPP-driven prognostic LAI.** Replace the GDD/SDD scheme entirely with a Noah-MP or ISBA-A-gs style leaf-biomass pool that accumulates from net GPP. Cleanest scientifically but a full architectural refactor; requires a leaf-C state and a Q10 or similar respiration term. Out of scope for an incremental PR roadmap.

Sub-choices orthogonal to A-D:

- **Water state variable**: cumulative antecedent precipitation (API) vs soil-moisture deficit (SMD) vs volumetric soil water. SUEWS already carries per-surface SMD via `cal_smd_veg`; adopting API would introduce a new diagnostic with no clear advantage (Jolly & Running 2004: SM potential outperforms cumulative rainfall).
- **Applied to growth only, or growth and senescence**: symmetric gating (ORCHIDEE-style) captures drought-driven brown-down. Option to layer the senescence term in a later PR without changing PR1/PR2 plumbing.
- **Per-surface versus shared parameters**: EVETR, DECTR, GRASS differ in rooting depth and drought strategy; per-surface parameters are the ORCHIDEE precedent.
- **Backward compatibility**: must be opt-in via a new `LAIType` value; existing YAMLs produce bit-identical output.

---

## 6. Recommendation

**Design C (Hybrid)**, introduced as `LAIType = 2`. Rationale:

- **Addresses both empirical failure modes.** AU-ASM thermal saturation (needs trigger) and monsoon-grass rainfall pulse (needs multiplicative response during build-up) are both captured. A single-mechanism scheme would fix one and expose the other.
- **Directly portable from CLM5 stress-deciduous.** Lawrence et al. 2019 provides the governing equations and default thresholds; we adopt rather than invent.
- **Reuses SUEWS hydrology.** `modState%hydroState%smd_nsurf` from `cal_smd_veg` is the natural input. Previous-day SMD is already available thanks to driver ordering (section 3).
- **Backward-compatible.** `LAIType` default stays `0`; all existing YAMLs and existing fitted parameters in `SUEWS-FLUXNET2015/data/csv-prm/prm-LAI-Albedo.csv` are untouched.
- **Incremental.** Senescence gating (Design C plus option) can land as a later PR without altering the PR1/PR2 scaffolding.

### 6.1 Governing equations (LAIType = 2)

Operating on previous-day vegetation SMD per surface iv:

```
w(iv)      = clamp(1 - xsmd_prev(iv) / smdcap(iv), 0, 1)                 # relative root-zone water
wbar(iv)   = tau-day running mean of w(iv)                               # persistence window

# Multiplicative stress on GDD build-up (Jarvis)
f_SMD(iv)  = clamp((w(iv) - w_wilt(iv)) / (w_opt(iv) - w_wilt(iv)),
                   0, 1) ** shape(iv)
dGDD_eff   = dGDD * f_SMD(iv)

# CLM5-style persistence trigger
if not leaf_on_permitted(iv):
    leaf_on_permitted(iv) = (wbar(iv) >= w_on(iv)) AND (Tbar >= Tbar_on)
if leaf_on_permitted(iv) AND (wbar(iv) < w_off(iv)):
    leaf_off_trigger(iv)  = TRUE   # force SDD accumulation
```

LAI update then reuses the existing power-law on the gated dGDD_eff and on the (possibly triggered) SDD accumulator.

### 6.2 Parameter inventory (per vegetated surface)

- `w_wilt`: dimensionless, lower bound of Jarvis factor. Default 0.15 (wilting-point equivalent). Range [0.0, 0.5].
- `w_opt`: dimensionless, upper bound. Default 0.4. Range [0.1, 0.9]. Validator: `w_opt > w_wilt`.
- `shape`: dimensionless exponent, default 1.0. Range [0.25, 4.0].
- `w_on`: dimensionless trigger threshold for leaf-on. Default 0.35 (Sitch et al. 2003 raingreen value). Range [0.05, 0.95].
- `w_off`: trigger threshold for leaf-off. Default 0.20. Range [0.05, 0.95]. Validator: `w_off < w_on`.
- `tau`: persistence window in days. Default 15 (CLM5). Range [1, 60].

Total: six per-surface parameters. Four can be held at sensible defaults; two (`w_wilt`, `w_opt`) may need site calibration against MODIS LAI -- this is what the existing FLUXNET2015 fitting pipeline will estimate for us.

### 6.3 Defaults when not calibrated

Defaults are defensible without site calibration: `w_on=0.35` (Sitch 2003), `w_off=0.20`, `tau=15` (CLM5), `w_wilt=0.15`, `w_opt=0.4`, `shape=1.0`. Under these defaults the scheme degrades gracefully: in well-watered regimes `f_SMD -> 1` and the trigger permits leaf-on so behaviour collapses toward the thermal scheme; under sustained drought the trigger prevents spurious leaf-on from GDD saturation.

---

## 7. Implementation sketch (no code in this branch)

### 7.1 Fortran

- Extend `update_GDDLAI` signature in `src/suews/src/suews_phys_dailystate.f95` (lines 538-560) with:
  - `smd_id_prev(nvegsurf)`, `smdcap_id_prev(nvegsurf)` -- previous-day SMD and capacity, mm.
  - `tbar_id_prev(nvegsurf)` already effectively available via `Tmin_id_prev` / `Tmax_id_prev` -- no new arg.
  - Persistent state for the `tau`-day running-mean `wbar` and the boolean `leaf_on_permitted`: add to the daily-state derived type in `src/suews/src/suews_ctrl_type.f95` parallel to `Tmin_id`, `Tmax_id`.
- In the accumulation loop (lines 582-617) branch on `LAItype(iv) == 2`: compute `f_SMD`, update `wbar`, evaluate the trigger, gate `dGDD` and `dSDD`.
- Caller `SUEWS_cal_DailyState` in the same file (~lines 100-300): thread `modState%hydroState%smd_nsurf` and soil-store capacity through to `update_GDDLAI`.

### 7.2 Python data model

- Extend `LAIParams` in `src/supy/data_model/core/site.py` (lines 348-496) with six new optional `FlexibleRefValue(float)` fields listed in section 6.2, each with default, range, unit and description in line with the existing `LAIParams` style.
- Update the `laitype` description at line 403: `0: original, 1: new high latitude, 2: moisture-aware (hybrid multiplicative + persistence trigger, CLM5-style)`.
- Extend `to_df_state` / `from_df_state` to (de)serialise the six new fields with defaults; legacy YAMLs must round-trip unchanged.
- Pydantic validators: `w_opt > w_wilt`, `w_off < w_on`, all fields within stated ranges; when `laitype == 2` and fields unset, emit an info-level log that defaults apply (do not raise).

### 7.3 Backward-compatibility contract

`LAIType` default stays `0`. Output must be bit-identical for existing YAMLs after the eventual implementation. A regression test built on `SUEWSSimulation.from_sample_data()` covering `LAIType in {0, 1}` is a required part of the first scaffolding PR, independent of the numerical changes.

---

## 8. Validation plan

The existing `SUEWS-FLUXNET2015` pipeline is the validation harness. No new data collection needed.

### 8.1 Site set

- **Thermal control sites** (scheme must not regress): US-MMS (DBF), US-Syv, DE-Hai, DK-Sor.
- **Semi-arid / dryland** (scheme should recover seasonality): AU-ASM (mulga), AU-Stp (Sturt Plains), AU-Dry, AU-Cpr, US-SRG (Santa Rita grassland -- needs to be added to the archive if not already), US-Wkg (Walnut Gulch), US-Ton (Mediterranean savanna).
- **Monsoon / tropical** (scheme should retime leaf-on to rainfall pulse): AU-DaS, AU-DaP (Daly River monsoon), AU-Ade (Adelaide River).
- **Mediterranean** (strong summer drought, winter growth): AU-Gin, FR-Pue, IT-Noe.

### 8.2 Fitting pipeline

Extend the Mathematica `LAIModel.m` package (at `/Users/tingsun/Dropbox (Personal)/6.Repos/LAI-model/LAIModel.m`, functions `fitTSLAI`, `calLAIGDDSDD`, `simLAI`) to expose the hybrid Design C model as a fittable form. Fit the six per-surface parameters per site against MODIS LAI; retain `BaseTGDD`, `GDDFull` etc. from the existing thermal fits unchanged.

### 8.3 Success metrics

- **Regression (thermal sites)**: new fit with `LAIType = 2` recovers the existing thermal fit within a small tolerance (i.e. moisture modifier is near-unity at these sites). Acceptance: RMSE of simulated vs MODIS LAI not worse than the thermal-only baseline.
- **Dryland sites**: LAImin-LAImax seasonal amplitude recovers to within some fraction of the MODIS-observed amplitude -- where thermal-only collapsed to near-flat (see section 2).
- **Monsoon sites**: green-up timing lag (modelled vs MODIS) reduced relative to the thermal-only baseline.
- **Energy-balance consistency**: evaluate albedo, Qh and Qe against FLUXNET eddy-covariance data to ensure the improved LAI does not degrade the surface energy balance.

### 8.4 Reporting

A comparison mirroring Omidvar et al. 2022 Appendix D: reproduce the thermal-only failure at US-SRG (and add AU-ASM and AU-Stp as additional dryland stress tests), then show the Design C fix. Package the result as a short note for the next SUEWS release and as supplementary material if the scheme reaches publication.

---

## 9. Incremental PR roadmap

Follow `dev-ref/FEATURE_DEVELOPMENT_WORKFLOW.md` (bottom-up):

- **PR1 -- scaffolding (no numerical change).**
  - `LAIParams` extended with the six new optional fields; `laitype` description updated.
  - `update_GDDLAI` signature extended; caller plumbs SMD, capacity and running-mean state through.
  - New daily-state slots (`wbar`, `leaf_on_permitted`) added to the derived type in `suews_ctrl_type.f95` but consumed only under `LAItype == 2`.
  - `LAItype == 2` branch is a no-op: `f_SMD = 1`, trigger always permits leaf-on. Thermal outputs bit-identical to `LAItype = 0`.
  - Regression test: `SUEWSSimulation.from_sample_data()` on `master` vs branch for `LAItype in {0, 1, 2}`.
- **PR2 -- implementation of Design C.**
  - Enable the Jarvis-style `f_SMD` and the CLM5-style trigger.
  - Synthetic dry-then-wet unit test: identical temperature, zero precipitation for 20 days then normal for 20 days; assert effective GDD reduction and delayed leaf-on.
  - Soft validator warnings when fields unset.
- **PR3 -- calibration and optional senescence gating.**
  - Extend `LAIModel.m` to fit Design C; refit FLUXNET2015 dryland and monsoon sites.
  - Publish per-IGBP default `w_wilt`, `w_opt`, `w_on`, `w_off` from the fitting results.
  - Optional: add additive senescence stress term (ORCHIDEE / WOFOST style) behind the same `LAIType = 2` switch.

Prerequisite: **GH-1291** (observed-LAI forcing silently ignored) should be resolved before PR3 so users have a viable fallback while calibrated parameters propagate into the sample configs.

---

## 10. Open scientific questions

- **Per-surface vs shared parameters.** Rooting depth and drought strategy differ between EVETR, DECTR and GRASS; per-surface is the ORCHIDEE precedent but increases parameter count. Can we collapse to IGBP-class defaults with per-site override?
- **Symmetric gating of SDD.** Drought-driven brown-down (ORCHIDEE, Liu et al. 2022) is well-supported observationally. PR2 vs PR3 split is a pragmatic choice; does the monsoon-site fit collapse without it?
- **Irrigated urban grass.** Omidvar et al. 2022 flagged irrigation as a confounder. Should the scheme bypass the moisture response when a site is flagged as irrigated, falling back to thermal-only? Or should urban irrigation be modelled explicitly through the water balance and implicitly influence SMD?
- **Interaction with observed-LAI forcing (post GH-1291).** When the user supplies observed LAI the moisture-aware calculation is bypassed. This is likely correct; worth documenting explicitly.
- **tau window choice.** CLM5 uses 15 days; Dahlin et al. 2015 note sensitivity to this choice. Should it be per-surface and fittable, or class-default?
- **Rooting depth parameterisation.** The SMD capacity SUEWS uses is not the same as CLM5's root-zone soil-water potential. Does the SUEWS bucket depth need re-examining for dryland PFTs, or is the per-surface `SurfaceStoreCap` sufficient?

---

## 11. Prerequisite and related work

- **GH-1291** (observed-LAI forcing silently ignored, OPEN): the documented interim workaround for rainfall-driven sites does not work until fixed. Not a hard blocker for PR1 but must be resolved before PR3 calibration is meaningful.
- **Omidvar et al. 2022** Appendix D provides the motivating contrast and is the target for PR3 validation.
- **Existing FLUXNET2015 archive** under `/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS-FLUXNET2015/` and `/Users/tingsun/Dropbox (Personal)/6.Repos/LAI-model/` is the calibration and validation substrate. Read-only; copy required inputs into the active workspace rather than operating in place.

---

## 12. References

- Omidvar, H., Sun, T., Grimmond, S. et al. (2022). Surface Urban Energy and Water balance scheme (v2020a) in non-urban areas: developments, evaluation and impacts. *Geosci. Model Dev.* 15, 3041-3078. https://doi.org/10.5194/gmd-15-3041-2022
- Lawrence, D. M. et al. (2019). The Community Land Model Version 5 (CLM5). *J. Adv. Model. Earth Syst.* 11, 4245-4287. https://doi.org/10.1029/2018MS001583
- Sitch, S. et al. (2003). Evaluation of ecosystem dynamics, plant geography and terrestrial carbon cycling in the LPJ dynamic global vegetation model. *Glob. Change Biol.* 9, 161-185. https://doi.org/10.1046/j.1365-2486.2003.00569.x
- Smith, B. et al. (2014). Implications of incorporating N cycling and N limitations on primary production in an individual-based dynamic vegetation model. *Biogeosciences* 11, 2027-2054. https://doi.org/10.5194/bg-11-2027-2014
- Krinner, G. et al. (2005). A dynamic global vegetation model for studies of the coupled atmosphere-biosphere system. *Glob. Biogeochem. Cycles* 19, GB1015. https://doi.org/10.1029/2003GB002199
- Niu, G.-Y. et al. (2011). The community Noah land surface model with multiparameterization options (Noah-MP). *JGR Atmos.* 116, D12109. https://doi.org/10.1029/2010JD015139
- Best, M. J. et al. (2011). The Joint UK Land Environment Simulator (JULES), model description - part 1. *GMD* 4, 677-699. https://doi.org/10.5194/gmd-4-677-2011
- Clark, D. B. et al. (2011). JULES - part 2. *GMD* 4, 701-722. https://doi.org/10.5194/gmd-4-701-2011
- Gibelin, A.-L. et al. (2006). ISBA-A-gs evaluation using interactive vegetation. *JGR Atmos.* 111, D18102. https://doi.org/10.1029/2005JD006691
- Jolly, W. M., Nemani, R. & Running, S. W. (2005). A generalized, bioclimatic index to predict foliar phenology in response to climate. *Glob. Change Biol.* 11, 619-632. https://doi.org/10.1111/j.1365-2486.2005.00930.x
- Jolly, W. M. & Running, S. W. (2004). Effects of precipitation and soil water potential on drought deciduous phenology in the Kalahari. *Glob. Change Biol.* 10, 303-308. https://doi.org/10.1111/j.1365-2486.2003.00701.x
- Stockli, R. et al. (2008). Use of FLUXNET in the Community Land Model development. *JGR Biogeosci.* https://doi.org/10.1029/2007JG000562. And Stockli et al. (2011). A global reanalysis of vegetation phenology. *JGR Biogeosci.* 116, G03020. https://doi.org/10.1029/2010JG001545
- Caldararu, S., Purves, D. W. & Palmer, P. I. (2014). Phenology as a strategy for carbon optimality: a global model. *Biogeosciences* 11, 763-778. https://doi.org/10.5194/bg-11-763-2014
- Knorr, W. et al. (2010). Carbon cycle data assimilation with a generic phenology model. *JGR Biogeosci.* https://doi.org/10.1029/2009JG001119
- Forkel, M. et al. (2014/2015). Codominant water control on global interannual variability and trends in land surface phenology and greenness. *Glob. Change Biol.* 21, 3414-3435. https://doi.org/10.1111/gcb.12950
- Dahlin, K. M., Fisher, R. A. & Lawrence, P. J. (2015). Environmental drivers of drought deciduous phenology in the Community Land Model. *Biogeosciences* 12, 5061-5074. https://doi.org/10.5194/bg-12-5061-2015
- Broich, M. et al. (2014). Land surface phenology performance across the semi-arid Australian continent. *Biogeosciences* 11, 5181-5198. https://doi.org/10.5194/bg-11-5181-2014
- Zhang, X. et al. (2005). Monitoring the response of vegetation phenology to precipitation in Africa. *Int. J. Remote Sens.* 26, 4517-4533. https://doi.org/10.1080/01431160500113971
- Liu, Y. et al. (2022). Green-up and brown-down: modelling grassland foliage phenology responses to soil moisture availability. *Agric. For. Meteorol.* 327, 109230. https://doi.org/10.1016/j.agrformet.2022.109230
- Hickler, T. et al. (2005). Precipitation controls Sahel greening trend. *Geophys. Res. Lett.* 32, L21415. https://doi.org/10.1029/2005GL024370
- Wang, J., Rich, P. M. & Price, K. P. (2003). Temporal responses of NDVI to precipitation and temperature in the central Great Plains, USA. *Int. J. Remote Sens.* 24, 2345-2364. https://doi.org/10.1080/01431160210154812

Internal references:

- Upstream issue: https://github.com/UMEP-dev/SUEWS/issues/1292
- Prerequisite: https://github.com/UMEP-dev/SUEWS/issues/1291
- Developer workflow: `dev-ref/FEATURE_DEVELOPMENT_WORKFLOW.md`
- Repository style: `.claude/rules/00-project-essentials.md`
- Ting's prior FLUXNET2015 work: `/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS-FLUXNET2015/`, `/Users/tingsun/Dropbox (Personal)/6.Repos/LAI-model/`, `/Users/tingsun/Dropbox (Personal)/6.Repos/AmeriFluxEvaluation/` (read-only; see `ref_dropbox-legacy-repos` memory for handling rules)

---

## Appendix A. Calibration methodology (PR3)

The moisture-aware scheme exposes six parameters per vegetation surface: `w_wilt`, `w_opt`, `f_shape`, `w_on`, `w_off`, `tau_w`. PR3 ships the calibration **scaffolding**: a parameter-sweep tool that scans any one of these over a list of candidate values, reruns the sample, and reports RMSE vs an `LAIType = 0` baseline plus seasonal amplitude and green-up DOY. PR3 does not ship calibrated per-IGBP defaults -- that requires dedicated FLUXNET site configurations (lat/lon, land cover fractions, soil-store capacity, thermal GDD fits from Omidvar et al. 2022 Table S1), which live outside this repo. See A.3 below.

### A.1 Sweep tool

`scripts/verify/moisture_phenology_sweep.py` exposes two invocations:

```
# Scan one parameter with user-supplied values:
uv run python scripts/verify/moisture_phenology_sweep.py \
    --param w_opt --values 0.25 0.40 0.55 0.70 0.90 --dry-start

# Scan all six with the bundled default ranges:
uv run python scripts/verify/moisture_phenology_sweep.py --all --dry-start
```

The `--dry-start` flag depletes `soilstore_surf` for the three vegetation surfaces to 10 mm on day 1 (the minimum the data model allows). Without it the bundled London sample is well-watered and the moisture gate never engages, so every sweep value collapses onto the thermal baseline. For a real dryland calibration the flag is unnecessary because the site is dry by construction.

Outputs land in `.context/gh1292/<site>/sweep_<param>.json` (tabular dose-response: mean LAI, RMSE vs baseline, seasonal amplitude, green-up DOY) and `.context/gh1292/<site>/sweep_<param>.png` (dual-axis sensitivity plot). A pairwise-validator co-adjustment in the tool ensures `w_opt > w_wilt` and `w_on > w_off` remain satisfied during extreme sweep values.

### A.2 London sensitivity results (PR3 demo, depleted soil initial state)

From the `--all --dry-start` scan on the bundled London sample with default `LAIType = 0` baseline mean LAI = 4.546 m2/m2 (note London is a UK suburban site, not a dryland; the depleted soil gives a short "synthetic drought" pulse for ~30 days before winter recharge):

- `w_wilt` strong effect: raising from 0.15 to 0.70 drops mean LAI from 4.545 to 4.435 (~2.4%) and delays green-up from DOY 104 to DOY 109. The Jarvis lower bound is the most influential parameter under transient drought.
- `w_opt` moderate: raising from 0.40 to 0.90 drops mean LAI from 4.545 to 4.470 (~1.6%) and delays green-up by five days. Tightening `w_opt` while keeping `w_wilt` unchanged narrows the Jarvis transition and can eclipse `w_wilt` for high values.
- `w_off` asymmetric: values below the default 0.20 are silent (the latch stays open); raising to 0.45 drops mean LAI 4.546 -> 4.483 (~1.4%) and lags green-up to DOY 106 by triggering the CLM5 off-latch early after the initial dry pulse.
- `w_on`, `tau_w`, `f_shape`: **no measurable effect** on the London synthetic-drought scenario with all other parameters at default. This is consistent with the latch mechanics: once the initial transient clears and the soil recharges, `wbar_id` sits comfortably above `w_off` and the leaf-on latch is never consulted again, so `w_on` / `tau_w` / `f_shape` stay off the integration.

The practical implication for calibration: **start with `w_wilt`, `w_opt`, and `w_off`** as the sensitive trio; treat `w_on`, `tau_w`, and `f_shape` as fine-tuning parameters that only matter when the run actually experiences sustained drought transitions. Real dryland sites (AU-ASM, US-SRG, AU-DaS) will almost certainly exercise all six.

Raw sweep outputs are checked into the `.context/` cache and can be regenerated with the command in A.1. PR3 does not check those outputs into the repo because they depend on the specific supy version and sample configuration.

### A.3 Roadmap for real FLUXNET2015 calibration (out of scope for PR3)

Full per-IGBP calibration requires:

- Per-site SUEWS YAML configurations matching the FLUXNET2015 tower locations -- latitude, land-cover fractions, soil-store capacity, roughness, and the thermal fits already derived in `SUEWS-FLUXNET2015/data/csv-prm/prm-LAI-Albedo.csv`. These do not ship with SUEWS; recreating them from the Omidvar et al. (2022) pipeline is a half-day of work per site.
- MODIS LAI time series aligned to the simulation output at daily aggregation; the `h5-lsm/df_LSM_<site>.h5` files in the Dropbox archive carry the MODIS fields already.
- An optimiser wrapper around the sweep tool. The natural choice is a bounded Nelder-Mead or differential-evolution run in the 6-dimensional parameter space minimising RMSE(MODIS, simulated) subject to the pairwise validators. Default IGBP priors from the literature (Sitch et al. 2003 raingreen `0.35`, CLM5 stress-deciduous `~15 d` persistence window) bracket the starting point.
- Primary tier (dryland + monsoon): AU-ASM, AU-DaS, AU-DaP, US-SRG, US-Wkg, US-Ton, AU-Stp.
- Control tier (non-regression at temperate sites): US-MMS, DE-Hai, US-Syv, DK-Sor.

Acceptance bars (from section 8.3): dryland amplitude recovery >= 60 % of MODIS, monsoon green-up DOY error <= 21 days, temperate-site RMSE degradation <= 0.15 m2/m2, monthly flux (Qh, Qe) deviation <= 10 W/m2 outside the growing season.

This calibration is genuinely scientific work, not engineering, and is recommended as a follow-up issue under the same GH-1292 umbrella (or a new issue that cites the fitted parameters).

### A.4 Optional moisture-stress senescence term (deferred)

Design C Section 5 option (d) proposes an **additive** SDD drought-stress term `delta_SDD_eff = delta_SDD - mu * max(0, w_off - wbar_id)` that accelerates senescence during sustained drought. In the CLM5-style latch implementation shipped with PR2, drought already shuts off thermal accumulation entirely via the latch, so the additive SDD term is scientifically optional rather than strictly necessary. PR3 does not add it because:

- The PR2 latch captures the dominant effect (drought halts leaf-on) without new parameters.
- Adding the term would cascade through the bridge (LAI_PRM schema 2 -> 3, LC_*_PRM +1) and warrants its own review.
- Whether the additive term is needed is a calibration outcome: if the latch-only scheme under-predicts brown-down at monsoon sites (Liu et al. 2022 would suggest it might), add the term; if not, the cleaner single-mechanism scheme wins.

The term is tracked as future work and will be revisited after A.3 calibration delivers empirical evidence.
