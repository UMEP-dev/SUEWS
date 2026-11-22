# Reactivating the BLUEWS convective boundary-layer module

The Boundary Layer Urban Energy and Water Balance Scheme (BLUEWS) convective boundary-layer (CBL) slab model is **temporarily disabled**, not deleted.
Following the ESTM precedent (gh#1802, `dev-ref/ESTM_REACTIVATION.md`), gh#2172 took `suews_phys_bluews.f95` out of the active SUEWS build because it had zero callers anywhere in SUEWS, SuPy, or the Rust bridge (`nm -uA` confirmed zero external references across all compiled archives), kept all of its run state in module-level mutable variables, and carried three uninitialised-variable diagnostics plus additional latent memory-safety defects.
The source is intact in git history, and this page records what was taken out, what has to be fixed before the module can run again, and the mechanical steps to put it back.

## Where the code is

- Last commit that holds the file: `dc9575f62593e9639bd97751a99ea47b5e089e6b` (`src/suews/src/suews_phys_bluews.f95`, 931 lines).
- To inspect the retired source from git history:

  ```bash
  git show dc9575f62593e9639bd97751a99ea47b5e089e6b:src/suews/src/suews_phys_bluews.f95
  ```

## What was taken out

- `src/suews/src/suews_phys_bluews.f95` (the whole file, 931 lines).
  It defined `module_phys_bluews_cbl` (module-level CBL parameters and state), the deprecated alias `cbl_MODULE`, `module_phys_bluews` (procedures `CBL`, `CBL_ReadInputData`, `CBL_initial`, `NBL`, `RKUTTA`, `diff`, `sonde`, and `gamma_sonde`), and the deprecated alias `BLUEWS_module`.
  None of these modules or procedures had any caller outside `suews_phys_bluews.f95`.
- `src/suews/Makefile`: the `suews_phys_bluews.o` entry in the `PHYS` object list.
- `src/suews/Makefile.deps`: the `src/suews_phys_bluews.o` dependency rule.
- `src/supy/meson.build`: the `'suews_phys_bluews.f95'` entry in `src_f95_all`.
- `scripts/lint/fortran_uninit_baseline.json`: the three BLUEWS entries (`diff:dhds`, `gamma_sonde:gamqq`, and `gamma_sonde:gamtt`), lowering the pinned GCC 14.2.0 baseline from 33 to 30 entries (`unit_count` 114 -> 113).
- `scripts/lint/fortran_literals_baseline.json`: the `src/suews/src/suews_phys_bluews.f95` entry (`kelvin_offset: 1`), lowering total `kelvin_offset` from 2 to 1.
- `test/known_defects.yml`: the `ledger: "U"` selector for `src/suews/src/suews_phys_bluews.f95` whose underlying baseline entries were removed.

## What was deliberately kept

- Legacy `RunControl.nml` `CBLUse` documentation (`docs/source/inputs/tables/CBL_input/CBL_input.rst`, `docs/source/inputs/tables/RunControl/scheme_options.rst`, and `docs/source/inputs/tables/RunControl/csv-table/CBLUse.csv`), the legacy table-converter rename rule (`src/supy/util/converter/table/rules.csv`), and legacy table test fixtures (`test/fixtures/`).
  The modern YAML/Pydantic data model (`src/supy/data_model/`) and Rust bridge (`src/suews_bridge/`) never exposed a `CBLUse` or `cbl_use` configuration option, so no YAML validation guard was required.
- Legacy declarations in `src/suews/src/suews_ctrl_const.f95` (`CBLuse`, `ncolumnsdataOutBL`, `dataOutBL`, `qhforCBL`, `qeforCBL`, and `BLout`), the `BL` output-line length constant in `src/suews_bridge/c_api/output_line.f95` (`SUEWS_CAPI_OUTPUT_LINE_BL_LEN`), and the CBL `ErrorHint` text entries (codes 23-25) in `src/suews/src/suews_ctrl_error.f95`, keeping the driver and C-API surface unchanged.
- The `2-module:bluews` GitHub label and the `BLUEWS (disabled)` row in `MAINTAINERS.md`.

## Blockers to fix before it can run

Restoring the file alone reinstates dead code with module-global mutable state, uninitialised reads, and out-of-bounds array accesses.
These defects (all verified against commit `dc9575f62593e9639bd97751a99ea47b5e089e6b`) must be addressed before reactivation:

1. **Missing `RETURN` and error unwinding after fatal `ErrorHint` calls (`diff` line 829, `CBL_ReadInputData` lines 362-363, and `sonde` lines 886 and 890-891), plus dropped non-fatal warnings (`ErrorHint(22)`).** In modern SuPy and Rust-bridge builds, fatal `ErrorHint` calls (such as codes 23, 24, and 25) record a thread-local fatal error via `set_supy_error` and return instead of executing `STOP` (see `.claude/rules/fortran/error-reporting.md`).
   In `diff` (line 829), an inadmissible `EntrainmentType` outside `{1, 2, 3, 4}` calls fatal `ErrorHint(24, 'BLUEWS_DIff- CBL- illegal alpha', ...)` without `RETURN` and falls through to lines 834-849 reading uninitialised `dhds`; `RKUTTA` and `CBL` never check `supy_error_flag()` to unwind.
   Separately, `CBL` (line 160), `CBL_initial` (line 394), and `NBL` (line 516) call `ErrorHint(22, ...)` on `qh_use < -900 .OR. qe_use < -900`; in `suews_ctrl_error.f95`, code 22 sets `flag_continue_on_error = .TRUE.` (a non-fatal warning that is silently dropped without `modState`, rather than setting the fatal error flag), so reactivating BLUEWS must wire warning propagation through `modState%errorstate` (and any proposal to make invalid `qh_use`/`qe_use` fatal requires an explicit behavioural decision).
2. **Uninitialised `gamtt`/`gamqq` in `gamma_sonde` (lines 901-919) and downstream division by zero in `diff` (lines 766, 776, 781, 796, and 819).** When `sondeflag == 1`, `zmax >= 2`, and `blh_m < gtheta(1, 1)` or `blh_m < ghum(1, 1)` (an ordinary finite case after `NBL` resets `blh_m = 200` m overnight at line 536), `gamtt` and/or `gamqq` are never assigned yet lines 911 and 918 unconditionally overwrite `gamt_Km = gamtt` and `gamq_kgkgm = gamqq/1000.` with uninitialised memory.
   Under `-finit-real=zero`, zeroing both sets `gamtv_Km = gamt_Km + 0.61*tm_K*gamq_kgkgm = 0.0` (`diff:766`), causing division by zero in `ftva_Kms/(h1*gamtv_Km)` (`diff:776, 781, 796, 819`); under `-finit-real=snan`, it injects `NaN`.
   Re-enabling radiosonde lapse-rate profiles requires an explicit scientific decision from the science maintainer for boundary-layer heights below the lowest sonde level (preserve prior lapse rates, clamp to level 1, or raise a fatal error).
3. **Off-by-one `zmax` loop in `gamma_sonde` (lines 907-919) and `IniCBLdata` indexing defects (`CBL_initial` lines 419-434 and 467; `NBL` lines 519-544 and 611).** In `gamma_sonde`, `DO j = 2, zmax` inspects `gtheta(j - 1, 1)` and `ghum(j - 1, 1)` (`1 .. zmax - 1`), silently ignoring the top sonde level `zmax`.
   In `CBL_initial` and `NBL`, `nLineDay` stays `0` if `IniCBLdata(1, 1) > id` (reading out-of-bounds index `0` of the 1-based allocatable array), and lines 467 and 611 index `IniCBLdata(id, 2)` by day-of-year `id` rather than `nLineDay` (and dereference unallocated `IniCBLdata` when `InitialData_use == 0`).
4. **Fixed-1000 buffer overflow in `sonde` (lines 880-887).** `DO i = 1, 1000` reads directly into `gtheta(0:500, 2)` and `ghum(0:500, 2)` (declared with upper bound `500` at line 52) before checking `IF (zmax > izm)` (`izm = 500`, line 868) after the loop terminates, writing past the end of both arrays whenever a sonde file contains more than 500 data rows.
5. **Module-level mutable state incompatible with `SUEWS_STATE` and multi-grid execution (`module_phys_bluews_cbl`, lines 3-55).** Over 40 CBL configuration and per-timestep state variables (including `blh_m`, `tm_K`, `tp_K`, `qm_kgkg`, `qp_kgkg`, `gamt_Km`, `gamq_kgkgm`, `cbldata(1:10)`, `cblday(366)`, `FileSonde(366)`, `gtheta(0:500, 2)`, `ghum(0:500, 2)`, `y(6)`, and allocatable `IniCBLdata(:,:)`) live in module variables rather than `SUEWS_STATE`, preventing thread-safe OpenMP and multi-grid runs.
   All mutable state must move into derived types threaded through `SUEWS_STATE` and `SUEWS_CONFIG`, the deprecated alias modules (`cbl_MODULE` and `BLUEWS_module`) must not be reinstated, and legacy Fortran `OPEN`/`READ` namelist I/O on `CBLInput.nml` must be replaced with typed YAML/bridge inputs.

## Mechanical restore

Once the blockers above are addressed (or as the starting point of the branch that fixes them):

```bash
git checkout dc9575f62593e9639bd97751a99ea47b5e089e6b -- src/suews/src/suews_phys_bluews.f95
```

Then ensure the reactivating branch:

- Restores `suews_phys_bluews.o` to `PHYS` in `src/suews/Makefile` and `'suews_phys_bluews.f95'` to `src_f95_all` in `src/supy/meson.build`, and regenerates `src/suews/Makefile.deps` via `python scripts/suews/gen_fortran_deps.py`.
- Keeps `scripts/lint/check_fortran_uninit.py` (pinned GCC 14.2.0), `scripts/lint/check_fortran_literals.py` (using `C2K` from `module_ctrl_const_physconst` rather than a bare `273.15` literal), and `scripts/lint/check_saved_fortran_locals.py` clean without re-adding baseline entries.
- Wires BLUEWS configuration, state, and outputs through `src/supy/data_model/` and `src/suews_bridge/` (with a data-interface version bump if new forcing or state fields are added).
- Updates `MAINTAINERS.md` and `dev-ref/CODING_GUIDELINES.md`, and deletes this page.
- Adds multi-grid regression tests, reference fixtures, and a `## Scientific evidence` section per `.claude/rules/physics-change-evidence.md`.

## Related

- gh#2172: triage of the remaining uninitialised-variable diagnostics in BLUEWS, the driver, and vendored SPARTACUS.
- gh#1802 / `dev-ref/ESTM_REACTIVATION.md`: precedent for retiring unmaintained module-state physics (`suews_phys_estm.f95`) while keeping reactivation notes.
- `.claude/rules/fortran/error-reporting.md`: error-propagation contract (`set_supy_error` and `supy_error_flag()`).
