> **Historical / Unimplemented Design Exploration -- Do Not Copy Into Production Code**
>
> The per-subroutine Python-to-Fortran `f90wrap` / `supy_driver` interface explored in this document has been **removed from SUEWS**. Do **not** import `supy_driver` or write tests targeting deleted `*_Module` wrappers.
>
> **Follow the active testing and bridge rules instead:**
> - Test design, markers, and placement: [`.claude/rules/tests/patterns.md`](../../.claude/rules/tests/patterns.md) and [`.claude/rules/tests/verification-methodology.md`](../../.claude/rules/tests/verification-methodology.md)
> - Fortran error reporting in tests: [`.claude/rules/fortran/error-reporting.md`](../../.claude/rules/fortran/error-reporting.md)
> - Rust bridge architecture and CLI tests: [`.claude/rules/rust/conventions.md`](../../.claude/rules/rust/conventions.md)

# Fortran Test Patterns for SUEWS

## How Fortran Physics Is Tested Today (Canonical Summary)

Fortran physics routines in `src/suews/src/` are compiled into the native physics library and invoked exclusively through the C-ABI + Rust PyO3 bridge (`src/suews_bridge/`). Individual Fortran modules are **not** wrapped as standalone Python modules.

Instead, Fortran physics and state handling are verified at two active layers:

1. **End-to-End and Subsystem Pytest Suites (`test/physics/`, `test/core/`)**:
   - Construct or load configurations via `SUEWSSimulation` (`SUEWSSimulation.from_sample_data()`, `SUEWSSimulation(path_config)`, or session fixtures such as `completed_sample_sim` in `test/conftest.py`).
   - Declare `pytestmark = pytest.mark.physics` at module level.
   - Assert physical invariants (energy and water balance closure, state continuity across restarts, option sweeps, and `set_supy_error` -> `RuntimeError` guards) using justified tolerances (`np.testing.assert_allclose`, `pytest.approx`).
   - Guard against unintentional drift via `test/core/test_sample_output.py` (cross-platform tolerance check) and `test/core/test_sample_output_exact.py` (byte-for-byte shard check on the reference CI lane; see `.claude/rules/physics-change-evidence.md`).
2. **Rust Bridge and C-ABI Tests (`src/suews_bridge/`)**:
   - Rust unit/integration tests and codec round-trips verify `StateCodec` / `CompositeCodec` packing and unpacking between typed structs, flat `f64` buffers, and `src/suews_bridge/c_api/*.f95` `ISO_C_BINDING` wrappers.
   - Standalone developer CLI subcommands (`cargo run -- ...` or `make bridge` -> `src/suews_bridge/target/release/suews-engine`) allow exercising bridged physics entry points with JSON or resolved YAML inputs without Python overhead.

---

## Active Physics Test Examples

### Pattern 1: Physics Conservation and Option Testing via `SUEWSSimulation`

```python
import numpy as np
import pytest
import supy as sp

pytestmark = pytest.mark.physics


@pytest.mark.core
def test_surface_energy_balance_closure(completed_sample_sim):
    """Verify surface energy balance closure: QN + QF = QH + QE + QS."""
    df_suews = completed_sample_sim.output.SUEWS

    residual = (df_suews["QN"] + df_suews["QF"]) - (
        df_suews["QH"] + df_suews["QE"] + df_suews["QS"]
    )

    np.testing.assert_allclose(
        residual,
        0.0,
        atol=1.0,  # [W m-2] within measurement/numerical tolerance
        err_msg="Surface energy balance failed to close",
    )
```

### Pattern 2: State Continuation / Restart Equivalence

Verify that Fortran `SUEWS_STATE` fields survive C-ABI pack/unpack without hidden `SAVE` state:

```python
import numpy as np
import pytest
from supy import SUEWSSimulation

pytestmark = pytest.mark.physics


def test_split_run_matches_continuous_run():
    """Verify splitting a run across two SUEWSSimulation steps matches an uninterrupted run."""
    sim_full = SUEWSSimulation.from_sample_data()
    df_forcing_short = sim_full.forcing.df.iloc[:576]  # 2 days at 5-min step
    sim_full.update_forcing(df_forcing_short)
    out_full = sim_full.run()

    sim_day1 = SUEWSSimulation.from_sample_data()
    sim_day1.update_forcing(df_forcing_short.iloc[:288])
    sim_day1.run()

    sim_day2 = SUEWSSimulation.from_state(sim_day1.state_final)
    sim_day2.update_forcing(df_forcing_short.iloc[288:])
    out_day2 = sim_day2.run()

    np.testing.assert_allclose(
        out_day2.SUEWS["QH"].to_numpy(),
        out_full.SUEWS["QH"].iloc[288:].to_numpy(),
        rtol=1e-6,
        atol=1e-6,
    )
```

### Pattern 3: Kernel Fatal-Error Guard Testing

Verify that invalid inputs reaching Fortran trigger `set_supy_error` and surface as a clean Python exception rather than calling `STOP` or silently returning `0.0`:

```python
import pytest
from supy import SUEWSSimulation

pytestmark = pytest.mark.physics


def test_disabled_or_invalid_kernel_option_raises_clean_error():
    """Verify kernel-level error reporting surfaces cleanly to Python."""
    sim = SUEWSSimulation.from_sample_data()
    # Mutate forcing or state to exercise a kernel validation guard
    df_forcing = sim.forcing.df.iloc[:12].copy()
    df_forcing["lai"] = -999.0
    sim.update_config({"model": {"physics": {"leaf_area_index": {"value": 0}}}})
    sim.update_forcing(df_forcing)

    with pytest.raises(RuntimeError, match="laimethod=0"):
        sim.run()
```

---

## Historical Context: Why Direct `f90wrap` Tests Were Retired

Prior to the Rust bridge migration, an experimental test design proposed importing individual Fortran modules (`Atmmoiststab_Module`, `Evap_Module`, `Resist_Module`, `Snow_Module`, `Suews_Def_Dts`, etc.) through `f90wrap` to unit-test subroutines in isolation. That approach was retired because:

- `f90wrap` / `f2py` tied compiled wheels to specific CPython and NumPy C-ABI versions, preventing a single `cp312-abi3` wheel from serving CPython 3.12+ and both NumPy 1.x (QGIS 3 LTR) and NumPy 2.x.
- Global `f90wrap` module state was not thread-safe for multi-grid execution.
- Testing physics contracts through `SUEWSSimulation` (`test/physics/`) and typed C-ABI codecs (`src/suews_bridge/`) exercises the exact code path users run while allowing internal Fortran refactoring without breaking brittle per-subroutine Python bindings.

## Common Pitfalls When Testing Fortran Physics

1. **Implicitly `SAVE`d locals**: Never initialise Fortran local variables in declarations (`REAL(KIND(1D0)) :: x = 0.0D0`). Run multi-grid or repeated-call tests to confirm state isolation; `scripts/lint/check_saved_fortran_locals.py` enforces this statically.
2. **Array indexing and layout**: Fortran uses 1-based column-major indexing; Python and Rust slices use 0-based indexing. Verify multi-layer and per-surface mappings with distinct non-default values per index (BR-04).
3. **Floating-point comparisons**: Never use `==` on floating-point outputs; use `np.testing.assert_allclose` or `pytest.approx` with physically justified tolerances (`.claude/rules/tests/patterns.md`).
4. **Checked Fortran builds**: Run `SUEWS_BUILD_PROFILE=checked make dev` locally when debugging suspected out-of-bounds array accesses or uninitialised reads.
