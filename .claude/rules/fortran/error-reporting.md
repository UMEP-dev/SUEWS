# Error Reporting in SUEWS Fortran

Rules for raising fatal errors and warnings from Fortran code inside SUEWS.

---

## Never use `WRITE(*,...)` + `STOP` to report errors

SUEWS runs embedded inside Python via SuPy. A plain `STOP` terminates the
entire Python interpreter, preventing SuPy from catching the failure and
presenting a clean exception. Likewise, `WRITE(*, ...)` to stdout bypasses
SuPy's logger — users see garbled output on some platforms, and parallel
runs interleave messages illegibly.

The `module_ctrl_error_state` module (in `suews_ctrl_error.f95`) was
introduced in GH#1035 precisely to avoid this. Its header states:

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
  arguments** before `RETURN` — an unassigned intent-out is undefined
  behaviour under some compilers.
- **`RETURN`** — do not continue. The driver checks `supy_error_flag`
  between grids/timesteps and surfaces the error to Python.
- **Never** call `STOP`, `ERROR STOP`, `CALL abort`, or `WRITE(*,...)`.

Working examples: `suews_phys_stebbs.f95:469`, `suews_phys_rslprof.f95:658`,
`suews_phys_dailystate.f95` (observed-LAI guard, GH#1296).

---

## Use `ErrorHint` for numbered messages with continuation semantics

When the error message already exists in the numbered registry (1–99) or
when the condition is non-fatal and the run should continue, use
`ErrorHint`:

```fortran
CALL ErrorHint(15, 'In compute_bar, input at lower bound', value, value2, notUsedI, modState)
```

- `modState` is OPTIONAL — pass it where available (it enables the
  thread-safe per-grid warning channel introduced in GH#1042).
- Codes 1–99 map to hard-coded text in `ErrorHint` (`suews_ctrl_error.f95`).
  Add a new code only with an accompanying change in that file.
- For non-fatal warnings without a numbered code, use
  `add_supy_warning(message)` from the same module.

---

## Register new error codes

Fatal codes 100+ are maintained in the header of `suews_ctrl_error.f95`.
When introducing a new error site:

1. Pick the next free integer (last allocated was `105` for GH#1296 — DailyState observed-LAI non-negative guard).
2. Add a one-line entry to the `Error Codes (GH#1035):` block at the top
   of `suews_ctrl_error.f95` describing the scope.
3. Reference the issue number so the history is traceable.

---

## Why this matters

- `STOP` inside an embedded Fortran call kills Python — users see
  `Abort trap: 6` or similar, with no stack, no test report, and no way
  to continue batch runs.
- `WRITE(*,...)` is not captured by pytest under some configurations,
  and is interleaved unpredictably in parallel runs.
- The state module is f90wrap-exposed to SuPy; clean errors become
  clean Python `RuntimeError` / `ValueError` exceptions.

When in doubt: **validate at the top, set the error state, assign safe
defaults, and `RETURN`.** The driver handles the rest.
