# Reactivating the ESTM storage-heat scheme

The Element Surface Temperature Method (ESTM, `storage_heat = 4`) is
**temporarily disabled**, not deleted. gh#1802 took `suews_phys_estm.f95` out of
the build because the scheme could not run one grid (gh#1785, a segfault) and
could never have run two: its run state lived in module variables and in an
implicitly saved first-call counter. The source is intact in git history, and
this page records what was taken out, what has to be fixed before the scheme
can run again, and the mechanical steps to put it back.

## Where the code is

- Last commit that holds the file:
  `c664e024857dbd2388e6d9bef24749d3487f06a5`
  (`src/suews/src/suews_phys_estm.f95`, 2147 lines).
- The disabling change: merge commit
  `d88ea495846354654d9eebcc844b68bd886e894a` (PR #1809, closing gh#1802).
  Its diff is the authoritative record of every line that was taken out:

  ```bash
  git show d88ea495846354654d9eebcc844b68bd886e894a --stat
  git show d88ea495846354654d9eebcc844b68bd886e894a -- \
      src/suews/Makefile \
      src/suews/src/suews_ctrl_driver.f95 \
      src/suews/src/suews_ctrl_const.f95
  ```

## What was taken out

- `src/suews/src/suews_phys_estm.f95` (the whole file). It defines
  `module_phys_estm` (procedures `SUEWS_GetESTMData`, `ESTM_initials`,
  `load_GridLayout`, `ESTM_ehc_initialise`, `ESTM_ehc_finalise`,
  `ESTM_translate`, `ESTM`) plus the helper modules `module_phys_estm_data`,
  `module_phys_estm_interp`, `module_phys_estm_solver`,
  `module_phys_estm_solarcalc`, an ESTM copy of `heatflux`, and the legacy
  aliases `ESTM_data`, `mod_interp`, `mod_solver`, `modSolarCalc`,
  `ESTM_module`. Only `ESTM` had a caller.
- `src/suews/Makefile`: the `suews_phys_estm.o` entry in the `PHYS` object list.
- `src/suews/src/suews_ctrl_driver.f95`:
  - `USE module_phys_estm, ONLY: ESTM` at the module header;
  - the `ELSEIF (StorageHeatMethod == 4 .OR. StorageHeatMethod == 14)` branch
    in `SUEWS_cal_Qs` that called `ESTM(...)`, replaced by an `ELSE` that raises
    error code 106 through `set_supy_error` and returns;
  - the `ASSOCIATE` names only that call read (`Gridiv`, `Ts5mindata_ir`,
    `avkdn`, `avu1`, `temp_c`, `avrh`, `press_hpa`, `Tair_av`, `zenith_deg`,
    `ldown`, `bldgh`);
  - the end-of-timestep copy
    `IF (storageheatmethod == 4) dataOutESTM(ir, :, Gridiv) = set_nan(dataOutLineESTM)`.
- `src/suews/src/suews_ctrl_const.f95`: `ncolsESTMdata`, the thirteen `cTs_*`
  column constants of the legacy `_ESTM_Ts_data.txt` input, and the module
  arrays `Ts5mindata`, `ts5mindata_ir`, `Tair24HR`.
- `scripts/lint/check_saved_fortran_locals.py`: the two allowlist entries for
  the ESTM first-call counters (`ESTMStart`, `Tair2Set`). The allowlist is now
  empty and the lint covers every Fortran procedure local.

## What was deliberately kept

- The `ESTM` output group (`ncolumnsDataOutESTM`, `dataOutLineESTM`,
  `OutputGroup.ESTM`, `estm_vars.py`) stays in the output contract, filled with
  -999 as it already was for every other storage-heat method.
- `forcing%Ts5mindata_ir` and its pass-through in the driver's outer routines,
  the C API flat forcing layout and the Rust bridge. Dead at present, but it is
  the slot a surface-temperature input would use.
- The `module_ctrl_const_allocate` `*_grids` array family that
  `ESTM_ehc_initialise` allocated.
- The data-model refusal from gh#1785 (`_reject_estm` in
  `src/supy/data_model/core/model.py`), which stops the value before the kernel.
- The `2-module:estm` GitHub label and the ESTM row in
  `SCIENTIFIC_REVIEWERS.md`.

## Blockers to fix before it can run

Restoring the file alone reinstates a scheme that segfaults on one grid and
cannot run two. These have to be addressed first, in roughly this order:

1. **Per-grid run state.** Every module variable in `module_phys_estm_data`
   that changes during a run, and the two saved first-call counters
   (`ESTMStart`, `Tair2Set`), must move into `SUEWS_STATE` (see
   `.claude/rules/fortran/` and the gh#1741 audit). Do not put the counters
   back in the lint allowlist; the lint exists to stop exactly this pattern.
   Per-grid arrays that `ESTM_ehc_initialise` allocated must be allocated on
   the YAML path or replaced by state fields.
2. **A surface-temperature input on the YAML path** (gh#1785). ESTM reads
   `Ts5mindata_ir`, the legacy `_ESTM_Ts_data.txt`, which the YAML interface
   never carried, so the run read past a zero-length array. Either add the
   input to the forcing contract (with a data-interface version bump, see
   `docs/source/contributing/schema/data_interface_versioning.rst`) or change
   the scheme so it does not need it.
3. **Module-name clashes.** The file defines a module named `heatflux`; EHC's
   heat-conduction module is `module_phys_ehc_heatflux` and nothing else uses
   the bare name today, but check `git grep -in 'USE heatflux'` before
   restoring, and prefer dropping the legacy alias modules (`ESTM_data`,
   `mod_interp`, `mod_solver`, `modSolarCalc`, `ESTM_module`) rather than
   reinstating them.

## Mechanical restore

Once the blockers are dealt with (or as the starting point of the branch that
deals with them):

```bash
git checkout c664e024857dbd2388e6d9bef24749d3487f06a5 -- src/suews/src/suews_phys_estm.f95
git revert -m 1 --no-commit d88ea495846354654d9eebcc844b68bd886e894a   # merge commit: mainline 1
```

A straight revert will conflict where the wording of gh#1802 was later
adjusted (including this page and the comments that point to it); resolve
by hand against the list above. Whichever route you take,
the branch must end up with:

- `suews_phys_estm.o` back in the `PHYS` list in `src/suews/Makefile`. That
  list is the single source for every build: `src/supy/run_make.py` (called
  from `src/supy/meson.build`) runs `make -C src/suews`, and
  `src/suews_bridge/build.rs` links the library that produces.
- In `suews_ctrl_driver.f95`: the `USE module_phys_estm, ONLY: ESTM` line, the
  `ELSEIF (StorageHeatMethod == 4)` branch ahead of the code-106 `ELSE`
  (keep the `ELSE`: it still guards values with no scheme, and 14 was never a
  data-model value), the `ASSOCIATE` names it reads, and the `dataOutESTM`
  copy at the end of the timestep.
- In `suews_ctrl_const.f95`: `ncolsESTMdata` and the `cTs_*` constants if the
  legacy text input is kept; otherwise leave them out.
- `src/supy/data_model/core/model.py`: remove `_reject_estm` and reword the
  option-4 line of the `storage_heat` docstring.
- Tests that assert the refusal, to be flipped to assert a run:
  `test/core/test_storage_heat_kernel_guard.py` (method 4; keep 14 as the
  refused control), `TestEstmRejected` in
  `test/data_model/test_physics_options.py`, and the `("storage_heat", 4)`
  entry in `test/physics/test_option_sweep.py`.
- Docs that currently say the option is disabled or not available:
  `docs/source/inputs/tables/RunControl/csv-table/StorageHeatMethod.csv`,
  `docs/source/inputs/tables/ESTM_input/ESTM_input.rst`,
  `docs/source/inputs/yaml/layer-conventions.rst`,
  `docs/source/inputs/transition_guide.rst`,
  `docs/source/outputs/legacy_text_columns.rst`,
  `docs/source/outputs/text_format.rst`; the ESTM row in
  `.claude/skills/sync-docs/references/data-model-checks.md` (and the row
  #1809 dropped from `areas-to-check.md`, to reinstate); the code-106 comment
  in `suews_ctrl_error.f95`; and this page, which should then be deleted.
- A reference fixture for a multi-grid ESTM run, since no fixture ever
  exercised the scheme, and a `## Scientific evidence` section in the PR
  (`.claude/rules/physics-change-evidence.md`): reinstating a scheme that
  moves `QS` is a physics change.

## Related

- gh#1802: ESTM cannot run multiple grids (the disabling issue).
- gh#1785: `storage_heat = 4` segfault, refused at validation in PR #1807.
- gh#1741: audit of implicitly saved Fortran locals.
- `scripts/lint/check_saved_fortran_locals.py`: the lint that must stay clean.
