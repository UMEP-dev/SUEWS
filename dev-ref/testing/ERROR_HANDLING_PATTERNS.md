> **Historical / Unimplemented Design Exploration -- Do Not Copy Into Production Code**
>
> The custom Python exception hierarchy and formatting helpers originally sketched in this file were **never implemented** in SUEWS. Do **not** import fictional exception classes (`SUEWSError`, `ConfigurationError`, `PhysicsError`, etc.), use emojis in messages, reference deleted `supy_driver` wrappers, or return silent fallback values (`0.0`) on physics failures.
>
> **Follow the active production error-handling rules instead:**
> - Fortran fatal errors and warnings: [`.claude/rules/fortran/error-reporting.md`](../../.claude/rules/fortran/error-reporting.md)
> - Rust bridge FFI & error contracts: [`.claude/rules/rust/conventions.md`](../../.claude/rules/rust/conventions.md) and [`.claude/rules/rust/bridge-contract.md`](../../.claude/rules/rust/bridge-contract.md) (BR-02, BR-08)
> - Test assertions for errors: [`.claude/rules/tests/patterns.md`](../../.claude/rules/tests/patterns.md)

# Error Handling Patterns for SUEWS

## Active Production Error Architecture (Canonical Summary)

Before reading the historical UX exploration below, understand how SUEWS actually reports and propagates errors in production:

1. **Fortran Kernel (`src/suews/src/`)**:
   - **Fatal validation or numerical errors**: Call `set_supy_error(code, 'routine_name: message')` from `module_ctrl_error_state` (`suews_ctrl_error.f95`), assign safe sentinel values (`-999.0D0`) to `INTENT(OUT)` arguments, and `RETURN`. Guard subsequent work with `IF (supy_error_flag()) RETURN`. Never call `STOP`, `ERROR STOP`, or `WRITE(*, ...)`.
   - **Thread-local isolation**: Fatal error state lives in C11 thread-local storage (`suews_ctrl_error_tls.c`, GH#1736) so parallel multi-grid Rayon workers never clobber one another's error state.
   - **Non-fatal kernel warnings**: Route numbered messages via `CALL ErrorHint(..., modState)` or unnumbered warnings via `CALL modState%errorstate%report(message=..., location=..., is_fatal=.FALSE.)` (GH#1737), surfaced on `SUEWSSimulation.kernel_warnings`.
2. **C-ABI and Rust Bridge (`src/suews_bridge/`)**:
   - `ISO_C_BINDING` facades (`src/suews_bridge/c_api/`) inspect `supy_error_flag()` / `get_supy_error` and return integer status codes (`SUEWS_CAPI_OK`, `SUEWS_CAPI_KERNEL_ERROR`, `SUEWS_CAPI_BAD_BUFFER`, etc.).
   - Rust converts non-zero C-ABI statuses and input-contract failures into `Result<T, BridgeError>`.
   - PyO3 maps `BridgeError` to standard Python exceptions (`RuntimeError`, `ValueError`), preserving the Fortran subroutine prefix and error code.
3. **Python Configuration and I/O (`src/supy/`)**:
   - Configuration schema and physics-option checks run through Pydantic v2 (`pydantic.ValidationError` / `ValueError`) and the config validation pipeline.
   - All messages and `logger_supy` calls use **plain ASCII (`0x00..0x7F`)** -- no emojis or Unicode symbols.
   - **No silent physics fallbacks**: Never catch a numerical or parameter failure and silently substitute `0.0` or retry with unrequested physics (forbidden by BR-02 and BR-08).

---

## Historical Error-UX Design Notes

The remainder of this document records an early design exploration around structuring user-facing diagnostic context alongside technical debugging details. Keep only the high-level principles (actionable messages, plain ASCII, explicit parameter paths); use standard Python exceptions (`ValueError`, `RuntimeError`, `FileNotFoundError`, `pydantic.ValidationError`) in real code.

### Dual-Layer Error Principles

```text
Technical Layer (for debugging)          User Layer (for experience)
|-- Exact routine / field path           |-- What went wrong
|-- Error code or exception type         |-- Why it matters
|-- Offending input values               |-- How to fix it
`-- Timestep / grid identifier           `-- Documentation / schema pointer
```

### Example 1: Configuration Validation (`ValueError` / Pydantic)

Validate physical bounds at the configuration boundary and name the offending YAML path and valid range in plain ASCII:

```python
def check_building_fraction(bldg_frac: float, all_fractions: dict[str, float]) -> None:
    """Validate building fraction and total surface-fraction closure."""
    if not (0.0 <= bldg_frac <= 1.0):
        raise ValueError(
            f"[X] surface_fractions.building must be between 0.0 and 1.0, got {bldg_frac}. "
            "Hint: check your YAML configuration so each surface fraction is in [0, 1]."
        )

    total = sum(all_fractions.values())
    if abs(total - 1.0) > 0.01:
        raise ValueError(
            f"[X] Surface fractions must sum to 1.0 (+/- 0.01), got {total:.3f}. "
            f"Hint: adjust surface fractions by {1.0 - total:+.3f} so they sum to 1.0."
        )
```

### Example 2: Forcing Data Input Checks

Raise standard `FileNotFoundError` / `ValueError` with explicit UTF-8 file reading and actionable column diagnostics:

```python
from pathlib import Path
import pandas as pd


def load_forcing_csv(path_forcing: Path) -> pd.DataFrame:
    """Load forcing CSV and verify required columns."""
    if not path_forcing.is_file():
        raise FileNotFoundError(
            f"Cannot find forcing data file: '{path_forcing}' "
            f"(resolved: '{path_forcing.resolve()}')."
        )

    df_forcing = pd.read_csv(path_forcing, encoding="utf-8")
    required = ["DateTime", "Tair", "RH", "Wind", "Pres", "Rain", "Kdown"]
    missing = sorted(set(required) - set(df_forcing.columns))
    if missing:
        raise ValueError(
            f"Forcing data '{path_forcing.name}' is missing required columns: {missing}."
        )
    return df_forcing
```

### Example 3: Kernel Error Propagation (No Silent Fallbacks)

When a physics calculation encounters invalid state, `NaN`, or non-convergence, **fail visibly** rather than substituting `0.0`:

```fortran
! Fortran kernel (src/suews/src/suews_phys_*.f95)
USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_NAN
USE module_ctrl_error_state, ONLY: set_supy_error

IF (IEEE_IS_NAN(avail_energy) .OR. aero_resist <= 0.0D0) THEN
   CALL set_supy_error(101, 'cal_evap: invalid avail_energy or non-positive aero_resist')
   qe_out = -999.0D0
   RETURN
END IF
```

In Python tests, assert the surfaced `RuntimeError` or `ValueError` directly:

```python
import pytest


def test_invalid_parameter_surfaces_clean_error():
    """Verify invalid kernel input raises a clean exception rather than crashing or returning 0."""
    with pytest.raises((ValueError, RuntimeError), match="must be positive"):
        run_invalid_configuration()
```

### Anti-Pattern: Silent Physics Degradation (Forbidden)

```python
# WRONG -- DO NOT DO THIS!
# Returning 0.0 when a physics calculation fails hides broken inputs and violates
# energy/water conservation and .claude/rules/rust/bridge-contract.md (BR-02, BR-08).
def bad_evaporation_wrapper(surface_temp: float, air_temp: float, wind_speed: float) -> float:
    try:
        return penman_monteith(surface_temp, air_temp, wind_speed)
    except RuntimeError:
        return 0.0  # FORBIDDEN silent fallback
```

## Error Message Checklist

- [ ] Message uses plain ASCII (`0x00..0x7F`) with no emojis or Unicode symbols.
- [ ] Message identifies the routine or YAML parameter path and the offending value.
- [ ] Fortran errors use `set_supy_error` + sentinel `INTENT(OUT)` assignment + `RETURN` (never `STOP`).
- [ ] Failures propagate cleanly to the caller without silent `0.0` physics substitutions.
- [ ] Negative test cases assert the exception type and a stable substring via `pytest.raises(..., match=...)`.
