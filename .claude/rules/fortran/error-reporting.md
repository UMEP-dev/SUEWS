---
paths:
  - src/suews/**/*.f9*
  - src/suews/**/*.F90
  - src/suews_bridge/c_api/**/*.f9*
---

# Error Reporting in SUEWS Fortran

Rules for raising fatal errors and warnings from Fortran code inside SUEWS.

---

## Never use `WRITE(*,...)` + `STOP` to report errors

SUEWS runs embedded inside Python via SuPy and the Rust bridge. A plain `STOP`
terminates the entire host process, preventing SuPy from catching the failure
and presenting a clean exception. Likewise, `WRITE(*, ...)` to stdout bypasses
SuPy's logger -- users see garbled output on some platforms, and parallel runs
interleave messages illegibly.

`src/suews/src/suews_ctrl_error.f95::module_ctrl_error_state` was introduced in
GH#1035 precisely to avoid this. Its header states:

> Allows Python to detect Fortran errors without STOP terminating the
> process.

---

## Use `set_supy_error` for fatal validation errors

The canonical pattern when Fortran detects invalid input that must halt
the run:

```fortran
MODULE module_phys_foo
   USE module_ctrl_error_state, ONLY: set_supy_error

   IMPLICIT NONE

CONTAINS

   SUBROUTINE compute_bar(input, output)
      REAL(KIND(1D0)), INTENT(IN) :: input
      REAL(KIND(1D0)), INTENT(OUT) :: output

      ! Validate inputs at the top of the subroutine.
      IF (input <= 0.0D0) THEN
         CALL set_supy_error( &
            <code>, &
            'compute_bar: input must be positive')
         output = -999.0D0      ! safe default to avoid undefined OUT
         RETURN                 ! let the caller and driver surface the error
      END IF

      ! ... main body ...
   END SUBROUTINE

END MODULE
```

Key points:

- **`USE module_ctrl_error_state, ONLY: set_supy_error`** at the module
  header, not inside the subroutine.
- **Message prefix with the subroutine name** (`'compute_bar: ...'`) so
  the user can locate the offender from the Python traceback.
- **Assign `-999.0D0` (or a type-appropriate sentinel) to `INTENT(OUT)`
  arguments** before `RETURN` -- an unassigned intent-out is undefined
  behaviour under some compilers.
- **`RETURN`** -- do not continue. The driver checks `supy_error_flag()`
  between grids/timesteps and surfaces the error to Rust/Python.
- **Guard later work with `IF (supy_error_flag()) RETURN`** (a function call
  since GH#1736, not a variable). The fatal store is thread-local
  (`src/suews/src/suews_ctrl_error_tls.c`): each grid run on a Rayon worker sees
  only its own error, so never cache the flag in module-level or implicitly
  `SAVE`d variables.
- **Never** call `STOP`, `ERROR STOP`, `CALL abort`, or `WRITE(*,...)`.

Working examples:
- `src/suews/src/suews_phys_stebbs.f95::calculate_x1` (STEBBS thermal-parameter guard, code `102`)
- `src/suews/src/suews_phys_rslprof.f95::interp_z` (`interp_z` NaN/bounds guards, code `103`)
- `src/suews/src/suews_phys_lai.f95::update_gddlai` (observed-LAI guard, code `105`, GH#1296)

---

## Use `ErrorHint` for numbered messages with continuation semantics

When the error message already exists in the numbered registry (1-99) or
when the condition is non-fatal and the run should continue, use
`ErrorHint`:

```fortran
CALL ErrorHint(15, 'In compute_bar, input at lower bound', value, value2, notUsedI, modState)
```

- `modState` is OPTIONAL -- pass it where available (it enables the
  thread-safe per-grid warning channel introduced in GH#1042).
- Codes 1-99 map to hard-coded text in `src/suews/src/suews_ctrl_error.f95::ErrorHint`.
  Add a new code only with an accompanying change in that file.
- For non-fatal warnings without a numbered code, report through the
  per-grid state so the message survives the timestep and reaches the user
  (GH#1737):

  ```fortran
  CALL modState%errorstate%report( &
     message='compute_bar: input at lower bound, clamped', &
     location='compute_bar', is_fatal=.FALSE.)
  ```

  If the routine does not receive `modState`, add
  `TYPE(SUEWS_STATE), INTENT(INOUT), OPTIONAL :: modState` (or an OPTIONAL
  `TYPE(error_state)` argument for leaf helpers) and pass it from the caller,
  guarding the call with `IF (PRESENT(modState))`. The driver stamps each
  entry with the current timestep, and SuPy surfaces the log as
  `SUEWSSimulation.kernel_warnings`. Do not add a module-level warning
  buffer: it is shared between parallel grid workers.

---

## Register new error codes

`src/suews/src/suews_ctrl_error.f95` (`Error Codes (GH#1035):` header comment
block) is the single authoritative registry for fatal codes `100+` (currently
`100` through `109`; do not duplicate the code table elsewhere). When
introducing a new fatal error site:

1. Read the `Error Codes (GH#1035):` header in `src/suews/src/suews_ctrl_error.f95`
   and pick the next unallocated integer after the highest registered code.
2. Add a one-line entry to that header block describing the subsystem and scope.
3. Reference the GitHub issue number so the history stays traceable.

---

## Why this matters

- `STOP` inside an embedded Fortran call kills Python -- users see
  `Abort trap: 6` or similar, with no stack, no test report, and no way
  to continue batch runs.
- `WRITE(*,...)` is not captured by pytest under some configurations,
  and is interleaved unpredictably in parallel runs.
- Fatal errors and per-grid warnings cross the C-ABI boundary
  (`src/suews/src/suews_ctrl_error_tls.c`, `src/suews_bridge/c_api/driver.f95`,
  and `src/suews_bridge/c_api/error_state.f95`) into the Rust PyO3 bridge
  (`src/suews_bridge/src/error.rs`), becoming clean Python `RuntimeError` /
  `ValueError` exceptions and `kernel_warnings` entries in SuPy.

When in doubt: **validate at the top, set the error state, assign safe
defaults, and `RETURN`.** The driver handles the rest.
