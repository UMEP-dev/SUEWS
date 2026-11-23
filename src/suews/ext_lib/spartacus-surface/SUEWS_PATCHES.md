# Local SUEWS patches

This file records the vendored upstream base and every post-vendoring local SUEWS modification under `src/suews/ext_lib/spartacus-surface/`.
Existing ECMWF licence notices (`LICENSE`, `NOTICE`) and Robin Hogan's attribution remain intact.

## Upstream base

- **Vendored fork commit:** `Urban-Meteorology-Reading/spartacus-surface` commit `5c3a997b7abc8840434a709216409c91498125a1` (2021-08-03, "Merging origin/master@c21fcea5 into master@ebbe7471"), previously pinned as a Git submodule at `SUEWS-SourceCode/ext_lib/spartacus-surface` in SUEWS commit `bdbbfaef52`.
- **SUEWS vendoring commit:** `71f853044d07477fe875bce5ffeee0dbedc2614a` (`71f853044d`, 2023-02-19, "update source code folders"), which copied all 81 vendored files verbatim from `5c3a997b7abc8840434a709216409c91498125a1` (omitting only `lib/.gitignore`).
- **ECMWF upstream ancestor and pre-vendoring fork delta:** `ecmwf/spartacus-surface` commit `c5a6e78daaab632d4b3b7721e1d2069ec5a3402e` (2021-07-09, `ChangeLog` version `0.7.2`, merge-base with `ecmwf/spartacus-surface` `master`). Prior to vendoring into SUEWS, the Reading fork commit `5c3a997b7abc8840434a709216409c91498125a1` already diverged from `c5a6e78daaab632d4b3b7721e1d2069ec5a3402e` across 13 files (including fork-side edits under `doc/`, `radsurf/`, and `test/`, and the removal of `test/simple/test_overhang_in.nc`). Those pre-vendoring Reading-fork differences are part of the imported `71f853044d` snapshot rather than post-vendoring SUEWS commits.
- **Reproducing the post-vendoring SUEWS delta:** run `git diff --stat 71f853044d..HEAD -- src/suews/ext_lib/spartacus-surface/`. Excluding this file (`SUEWS_PATCHES.md`), 19 vendored files carry post-vendoring SUEWS modifications across five changes (`13e8c75ba4`, `31e4d8bb1b`, `a0bc551e61`, `f7c14cb19a`, and issue #2172).

## Modified files summary

| File | SUEWS PR(s) | Summary |
| --- | --- | --- |
| `Makefile_include.gfortran` | #1209 | Update `DEBUGFLAGS` for gfortran debug builds |
| `driver/spartacus_surface_config.F90` | #2075 | Return after `radiation_abort` on missing namelist |
| `driver/spartacus_surface_driver.F90` | #2075 | Unwind on `radiation_error()`, capture first OpenMP TLS error, exit with status 1 |
| `driver/spartacus_surface_read_input.F90` | #2075 | Propagate `radiation_error()` after NetCDF `file%get` calls |
| `radsurf/radsurf_canopy_flux.F90` | #2075 | Return after `radiation_abort` in flux scaling and summing |
| `radsurf/radsurf_canopy_properties.F90` | #2075 | Propagate `radiation_error()` after NetCDF `file%get` calls |
| `radsurf/radsurf_config.F90` | #2075 | Validate stream and vegetation-region counts; return after `radiation_abort` |
| `radsurf/radsurf_forest_sw.F90` | #1570 | Fix `spread` dimension order in shortwave absorption terms |
| `radsurf/radsurf_interface.F90` | #2075 | Propagate `radiation_error()` after urban SW/LW solver calls |
| `radsurf/radsurf_overlap.F90` | #2075 | Return after `radiation_abort` when building fraction increases with height |
| `radsurf/radsurf_save.F90` | #2075 | Propagate `radiation_error()` across NetCDF output routines |
| `radsurf/radsurf_simple_spectrum.F90` | #1570, #1854, #2075 | Guard canopy emission on urban/flat tiles; return after `radiation_abort` |
| `radsurf/radsurf_urban_lw.F90` | #1570, #2075, #2172 | Store optional layer-top/base LW diagnostics; propagate `radiation_error()`; reset `norm_perim_air_veg` per layer |
| `radsurf/radsurf_urban_sw.F90` | #1570, #2075, #2172 | Store optional layer-top/base SW diagnostics; propagate `radiation_error()`; reset `norm_perim_air_veg` per layer |
| `radtool/radtool_calc_matrices_lw_eig.F90` | #1854 | Handle fully transparent LW spectral bands analytically |
| `radtool/radtool_calc_matrices_sw_eig.F90` | #1854 | Handle fully transparent SW spectral bands without singular inverses |
| `utilities/Makefile` | #2075 | Compile `../../../src/suews_ctrl_error_tls.c` into `libutilities.a` |
| `utilities/easy_netcdf.F90` | #2075 | Propagate `radiation_error()`, return after `my_abort`, initialise `-1` sentinels |
| `utilities/radiation_io.F90` | #2075 | Replace `call abort` with SUEWS TLS error code `111`; expose `radiation_error()` |

## Changes by SUEWS PR

### PR #1209 (`13e8c75ba4`, 2026-02-20): gfortran debug flags

- `Makefile_include.gfortran`: changed `DEBUGFLAGS` from `-g -ffpe-trap=invalid,zero,overflow -fcheck=bounds -finit-real=snan` to `-g -fcheck=all -finit-real=zero`.

### PR #1570 (`31e4d8bb1b`, 2026-06-26): forest SW spread fix, urban guards, and layer-flux diagnostics

- `radsurf/radsurf_forest_sw.F90`: corrected `spread(1.0_jprb/lg%mu,nsw,1)` to `spread(1.0_jprb/lg%mu,1,nsw)` in six direct and diffuse clear-air and vegetation absorption expressions in `spartacus_forest_sw` (already fixed upstream in `ecmwf/spartacus-surface@b1f91b389cb73dc0da3f4eb2c42452a7694a906c`, present on upstream `master` at `63afcbb6ded0e2f9e1067bd3f279b590168fd222`, so this needs no upstream submission).
- `radsurf/radsurf_simple_spectrum.F90`: guarded `roof_emission` and `wall_emission` assignments with `if (config%do_urban)` and `allocated(...)` checks.
- `radsurf/radsurf_urban_lw.F90`: stored optional `flux_dn_layer_top`, `flux_up_layer_top`, `flux_dn_layer_base`, and `flux_up_layer_base` diagnostics in `lw_internal` and `lw_norm` when allocated.
- `radsurf/radsurf_urban_sw.F90`: stored optional `flux_dn_dir_layer_top`, `flux_dn_layer_top`, `flux_up_layer_top`, `flux_dn_dir_layer_base`, `flux_dn_layer_base`, and `flux_up_layer_base` diagnostics in `sw_norm_dir`, and diffuse layer-top/base diagnostics in `sw_norm_diff`, when allocated.

### PR #1854 / issue #1498 (`a0bc551e61`, 2026-10-21): flat tiles and transparent spectral bands

- `radsurf/radsurf_simple_spectrum.F90`: skip canopy emission calculations when neither urban nor vegetation canopy is enabled (`config%do_urban .or. config%do_vegetation`), avoiding unallocated canopy arrays on flat tiles.
- `radtool/radtool_calc_matrices_lw_eig.F90`: handle fully transparent spectral bands before matrix inversion, with identity transmission, zero reflection, and analytical integrated-flux and emission source terms (`recursive subroutine`, `implicit none`).
- `radtool/radtool_calc_matrices_sw_eig.F90`: handle fully transparent spectral bands before matrix inversion, with identity transmission and zero reflection/scattering (`recursive subroutine`, `implicit none`).
  The three shortwave integral coefficients (`int_dir`, `int_diff`, `int_dir_diff`) are zero as an absorption convention; they do not reconstruct the nonzero integrated flux of an illuminated transparent layer.

The matrix solvers process ordinary bands separately when a transparent band is present.
Band-isolation tests require bit identity with single-band execution under release and checked compiler flags; ordinary multi-band execution is compared within double-precision round-off, since release optimisation can change its last bits.
Regression coverage in `test/physics/test_spartacus_transparent.py` checks analytical cases, scattering, rectangular direct-to-diffuse matrices, band isolation, and independent longwave emission.
`test/physics/test_spartacus_non_urban.py` covers forest and flat-tile model runs and flat-tile radiation budgets.

These changes should be offered upstream with a minimal reproducer: zero exchange matrices alongside an absorbing/scattering band, and a flat tile with both canopy flags disabled.
The matrix test's Fortran wrapper in `test/physics/test_spartacus_transparent.py` compiles only `radtool/` plus `utilities/parkind1.F90` and `utilities/yomhook.F90` (without `radiation_io.F90` or the SUEWS kernel), providing a self-contained starting point for an upstream matrix reproducer.

### PR #2075 (`f7c14cb19a`, 2026-11-19): fatal error propagation via thread-local storage

Instead of terminating the host process via Fortran `call abort`, `radiation_abort` records registered SUEWS fatal error code `111` (`src/suews/src/suews_ctrl_error.f95`) through SUEWS's C thread-local storage (TLS) hooks (`src/suews/src/suews_ctrl_error_tls.c`), and callers unwind early via `radiation_error()` and `return`.
All 15 modified files under `src/suews/ext_lib/spartacus-surface/` are listed below:

- `utilities/radiation_io.F90`: bind `suews_tls_error_set` and `suews_tls_error_flag` via `iso_c_binding`, replace `call abort` in `radiation_abort` with `tls_error_set(111_c_int, ...)`, and expose `radiation_error()`.
- `utilities/Makefile`: compile `../../../src/suews_ctrl_error_tls.c` into `suews_ctrl_error_tls.o` and include it in `libutilities.a` so the offline `spartacus_surface` driver built inside the SUEWS source tree resolves SUEWS's TLS symbols.
- `utilities/easy_netcdf.F90`: import `radiation_error`, return immediately after `my_abort` calls, propagate `if (radiation_error()) return` across NetCDF read/write/define routines, and initialise `-1` return sentinels in `get_rank` and `get_outer_dimension` before early exit.
- `radsurf/radsurf_config.F90`: check positive stream counts and valid vegetation region counts (`1` or `2` when `do_vegetation` is enabled) in `consolidate_config` before calling `radiation_abort` and returning; add `return` after existing `radiation_abort` calls in `read_config_from_namelist`.
- `radsurf/radsurf_canopy_flux.F90`: add `return` after `radiation_abort` in `scale_canopy_flux` (spectral resolution mismatch) and `sum_canopy_flux` (unallocated target object).
- `radsurf/radsurf_canopy_properties.F90`: import `radiation_error` and add `if (radiation_error()) return` after NetCDF `file%get` calls in `read_from_netcdf`.
- `radsurf/radsurf_interface.F90`: import `radiation_error` and add `if (radiation_error()) return` after urban and vegetated-urban shortwave/longwave solver calls in `radsurf`.
- `radsurf/radsurf_overlap.F90`: add `return` after `radiation_abort('Building fraction cannot increase with height')` in `calc_overlap_matrices_urban`.
- `radsurf/radsurf_save.F90`: import `radiation_error` and propagate `if (radiation_error()) return` across `save_canopy_fluxes`, `define_canopy_flux_variables`, and `write_canopy_flux_variables`.
- `radsurf/radsurf_simple_spectrum.F90`: add `return` after `radiation_abort` when `lw_spectral_props%nspec > 1` in `calc_simple_spectrum_lw`.
- `radsurf/radsurf_urban_lw.F90`: import `radiation_error`, add `return` after `radiation_abort` when `do_vegetation .and. .not. config%do_vegetation`, and check `if (radiation_error()) return` after `calc_overlap_matrices_urban` in `spartacus_urban_lw`.
- `radsurf/radsurf_urban_sw.F90`: import `radiation_error`, add `return` after `radiation_abort` when `do_vegetation .and. .not. config%do_vegetation`, and check `if (radiation_error()) return` after `calc_overlap_matrices_urban` in `spartacus_urban_sw`.
- `driver/spartacus_surface_config.F90`: add `return` after `radiation_abort('Driver configuration error')` when the namelist file is not found in `read_config_from_namelist`.
- `driver/spartacus_surface_read_input.F90`: import `radiation_error` and add `if (radiation_error()) return` after NetCDF `file%get` calls in `read_input`.
- `driver/spartacus_surface_driver.F90`: bind `suews_tls_error_code` and `suews_tls_error_message`, propagate `radiation_error()` across configuration, input reading, serial/OpenMP solver loops, flux scaling/summing, and output saving, capture the first failing worker's TLS code and message in `capture_radiation_error` (`!$OMP CRITICAL(spartacus_first_error)`), write `SPARTACUS error <code>: <message>` to `error_unit`, and exit with `stop 1`.

Regression coverage lives in `test/core/test_spartacus_errors.py`, `test/physics/test_fatal_error_propagation.py`, and `test/physics/test_standalone_spartacus_errors.py` (which exercises the `spartacus_surface` executable built inside the SUEWS source tree with `src/suews/src/suews_ctrl_error_tls.c`).
Because `utilities/radiation_io.F90`, `utilities/Makefile`, and `driver/spartacus_surface_driver.F90` bind directly to SUEWS's C TLS implementation (`src/suews/src/suews_ctrl_error_tls.c`, `suews_tls_error_*`) and error code `111`, building `spartacus-surface` outside SUEWS would require replacing or removing those TLS bindings; the `radiation_error()` early-return unwinding pattern itself can inform an upstream error-status API.

### Issue #2172 (PR pending, 2026-11-22): reset `norm_perim_air_veg` per layer in urban solvers

Inside the upward layer loop (`do jlay = 1, nlay`) of both urban solvers, `norm_perim` was reset to `0.0_jprb` at the start of every layer, whereas `norm_perim_air_veg` was assigned only inside `if (veg_fraction(jlay) > config%min_vegetation_fraction)`.
When buildings extend above the vegetation canopy (`building_fraction(jlay) > config%min_building_fraction` and `veg_fraction(jlay) <= config%min_vegetation_fraction`) and `veg_contact_fraction(jlay) > 0.0_jprb` (including SUEWS's default `veg_contact_fraction_const = 0.5`), `norm_perim_air_veg` silently carried over the lower vegetated layer's air-vegetation perimeter (or read an uninitialised stack value when `jlay == 1` had no vegetation without `-finit-real=zero`).
The solver then computed a spurious positive `norm_perim_wall_veg = min(norm_perim_air_veg * veg_contact_fraction(jlay), norm_perim_wall(1))` and transferred wall perimeter from clear air (`norm_perim_wall(1)`) into the absent vegetated region(s) (`norm_perim_wall(2:)`).
On vegetation-free layers, `f_wall(2:, jlay)` is zeroed and only region `1` (`1:ns`) is solved in `calc_matrices_{lw,sw}_eig`:

- In `radsurf/radsurf_urban_lw.F90`, clear-air wall interception (`f_wall(1, jlay)`) and wall emission into region `1` both drop, while `emiss_wall(:, jlay)` still sums `sum(norm_perim_wall)` across all regions; the unemitted wall-vegetation share disappears from the radiation field, violating canopy longwave energy conservation by several to tens of `W m-2`.
- In `radsurf/radsurf_urban_sw.F90`, the reduced `norm_perim_wall(1)` lowers `f_wall(1, jlay)`, making above-canopy walls artificially transparent to direct and diffuse shortwave radiation.

Local changes:

- `radsurf/radsurf_urban_lw.F90`: initialise `norm_perim_air_veg = 0.0_jprb` alongside `norm_perim = 0.0_jprb` at the start of each layer in `spartacus_urban_lw`.
- `radsurf/radsurf_urban_sw.F90`: initialise `norm_perim_air_veg = 0.0_jprb` alongside `norm_perim = 0.0_jprb` at the start of each layer in `spartacus_urban_sw`.

Minimal upstream reproducer: construct a two-layer urban canopy with `nreg = 2` (or `3`), `building_fraction = [0.25, 0.25]`, `veg_fraction = [0.10, 0.0]`, and `veg_contact_fraction = [0.5, 0.5]` and run `spartacus_urban_lw` and `spartacus_urban_sw`.
Without the reset, `spartacus_urban_lw` fails longwave budget closure (`LTopNet` vs ground + roofs + walls + clear air + vegetation) on layer 2, and `spartacus_urban_sw` reduces layer-2 wall interception even in a zero-albedo canopy where no upward flux leaves layer 1.
Regression coverage in SUEWS lives in `test/physics/test_spartacus_above_canopy_perimeter.py`.
This two-line fix has no SUEWS-specific dependencies and is ready for upstream submission to `ecmwf/spartacus-surface`.
