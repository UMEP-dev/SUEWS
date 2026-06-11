# SCM validation evidence (research preview)

Archived metrics from the validation campaign of the coupled
single-column model (`module_phys_scm` / `SUEWS_cal_multitsteps_scm`),
kept as fixture-grade evidence for the pull request introducing the
feature and for `test_scm_native.py`.

## Files

- `gabls1_metrics.json` — GABLS1 stable boundary layer vs the LES
  ensemble of Beare et al. (2006): depth 168/198 m (stress/Ri) against
  the 150–250 m LES range, friction velocity 0.274 m s⁻¹, supergeostrophic
  low-level jet at 184 m; long-tail over-deepening of Cuxart et al. (2006)
  reproduced.
- `cbl_metrics.json` — convective growth vs the Tennekes (1973) analytic
  law: column within 4.0 %, slab within 0.3 %; effective entrainment
  ratio 0.06 (LES consensus 0.15–0.25) reported as a known first-order
  K-profile limitation.
- `coupled_kcl_metrics.json` — five coupled July days over central London
  (KCL 2012): 3.7 K air-temperature RMSE (3.0 day / 4.5 night), r = 0.73,
  against observations never shown to the model after initialisation.
- `native_vs_python.json` — cross-backend pinning: the native Fortran
  loop vs the Python reference on the same episode (0.23 K mean
  trajectory agreement; skill statistically indistinguishable) and the
  measured 555x speedup (0.34 s vs 186.7 s for five coupled days).
- `native_regression.json` — frozen six-hour coupled series used by
  `test/test_scm_native.py` as the regression baseline.
- `multiyear.json` — three coupled years (urban + anchored rural
  companion, 2012 forcing recycled): six column-years in 160 s measured
  vs 22.7 h projected for the per-step Python loop; emergent climatology
  (nocturnal UHI 3.2 K mean; urban daily-max boundary-layer depth
  1549 m JJA / 574 m DJF).
- `demo_urban_rural.json`, `demo_qf.json`, `demo_cool_roofs.json` —
  the three coupled experiments (emergent heat island; anthropogenic
  heat x0/x1/x2; cool roofs 0.12 -> 0.55) over 23–26 July 2012.

Figures rendered from these runs live in `docs/source/assets/img/scm/`
and on the site preview page (`site/preview/scm/`).

## Provenance

The numbers were produced by the now-retired pure-Python reference
implementation (`scm/` package) and the native Fortran port, both in
this repository's history:

- `96513b82f` — Python numerical core + GABLS1/CBL benchmarks
- `2ca25d90f` — Python coupled SCM, demos, original demo page
- `dc939564a` — native Fortran port + cross-backend equivalence
- `7bcf157f0` — multi-year native runs + interactive page features

The Python reference (including its 26-test suite: exact conservation,
Monin-Obukhov log-law recovery, slab analytic solutions, GABLS1 ranges)
can be recovered from those commits; the native implementation is the
single maintained one.
