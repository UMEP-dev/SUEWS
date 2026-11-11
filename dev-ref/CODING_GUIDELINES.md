# SUEWS Coding Guidelines and Policy

> **Canonical Rules Notice**
>
> Active, path-scoped rules in `.claude/` and executable CI lints supersede this overview whenever an example drifts. Always read the governing rule files first:
> - [`AGENTS.md`](../AGENTS.md) and [`.claude/rules/00-project-essentials.md`](../.claude/rules/00-project-essentials.md)
> - Fortran: [`.claude/rules/fortran/conventions.md`](../.claude/rules/fortran/conventions.md) and [`.claude/rules/fortran/error-reporting.md`](../.claude/rules/fortran/error-reporting.md)
> - Python: [`.claude/rules/python/conventions.md`](../.claude/rules/python/conventions.md), [`.claude/rules/python/api-approach.md`](../.claude/rules/python/api-approach.md), and [`.claude/rules/python/config-patterns.md`](../.claude/rules/python/config-patterns.md)
> - Rust bridge: [`.claude/rules/rust/conventions.md`](../.claude/rules/rust/conventions.md) and [`.claude/rules/rust/bridge-contract.md`](../.claude/rules/rust/bridge-contract.md)
> - Identifiers and CHANGELOG: [`.claude/rules/naming-convention.md`](../.claude/rules/naming-convention.md) and [`.claude/rules/changelog/format.md`](../.claude/rules/changelog/format.md)

This document summarises the coding standards and architectural conventions for the SUEWS (Surface Urban Energy and Water Balance Scheme) project across Python (`src/supy/`), the Rust/C-ABI bridge (`src/suews_bridge/`), and Fortran (`src/suews/`).

## Table of Contents
1. [General Principles](#general-principles)
2. [Python Guidelines](#python-guidelines)
3. [Fortran Guidelines](#fortran-guidelines)
4. [Rust Bridge Overview](#rust-bridge-overview)
5. [Documentation Standards](#documentation-standards)
6. [Testing Requirements](#testing-requirements)
7. [Version Control and CHANGELOG Practices](#version-control-and-changelog-practices)
8. [Configuration and Data Model Guidelines](#configuration-and-data-model-guidelines)
9. [Automated Formatting and Linting](#automated-formatting-and-linting)

## General Principles

### 1.1 Language, Spelling, and Plain ASCII
- Use **British English** for all documentation, comments, and user-facing text (exception: NumPy/SciPy API names such as `analyze`).
- Avoid `-ise`/`-ize` spelling variants in new identifiers; prefer neutral verbs (`init`, `setup`, `prepare`, `complete`, `scale`, `compute`, `tune`).
- Enforce **plain ASCII (`0x00..0x7F`)** in logging, CLI output, error messages, validation reports, and developer documentation:
  - Use `->` instead of Unicode arrows.
  - Use `[OK]` and `[X]` instead of checkmarks or emojis.
  - Use ASCII unit notation such as `W m-2`, `kg m-3`, and `degC`.

### 1.2 File Organisation
- Keep related functionality together in cohesive modules.
- Separate concerns cleanly across layers:
  - `src/supy/`: Python user interface (`SUEWSSimulation`), Pydantic data model, validation, CLI, and I/O.
  - `src/suews_bridge/`: PyO3 + Rust + ISO_C_BINDING transport layer (`suews_bridge` extension and `suews-engine` developer CLI).
  - `src/suews/src/`: Fortran physics engine and kernel state types.
- Register new Python or Fortran source files in `meson.build`, and regenerate `src/suews/Makefile.deps` via `python scripts/suews/gen_fortran_deps.py` whenever Fortran files or `USE` edges change.

### 1.3 Code Quality
- Write self-documenting code with clear variable names following `.claude/rules/naming-convention.md` (`snake_case`, physical quantity first, then component/location, then sub-class).
- Add comments to explain **why** (physical assumptions, invariants, literature equations), not **what**.
- Prefer clarity and explicit contracts over implicit fallbacks.

## Python Guidelines

SUEWS requires **Python `>= 3.12`** (`pyproject.toml` and `.ruff.toml` `target-version = "py312"`).

### 2.1 Import Organisation

Order imports as standard library, third-party, then local relative imports (enforced by `ruff` rule `I`):
```python
# 1. Standard library imports
from pathlib import Path
from typing import Literal

# 2. Third-party imports
import numpy as np
import pandas as pd

# 3. Local imports
from ._env import logger_supy, trv_supy_module
from ._suews_simulation import SUEWSSimulation
```

### 2.2 Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Functions / methods | `snake_case` | `check_forcing`, `from_sample_data` |
| Variables / YAML fields | `snake_case` | `df_forcing`, `temperature_wall_internal` |
| Classes | `PascalCase` | `SUEWSSimulation`, `SUEWSConfig` |
| Constants | `UPPER_CASE` | `DEFAULT_TIMESTEP`, `NSURF` |
| Private items | Leading underscore | `_internal_helper` |
| Module names | `snake_case` | `yaml_upgrade.py` |

**Container Prefixes (where helpful in internal code):**
- DataFrames: `df_` (for example `df_forcing`, `df_state`)
- Dictionaries: `dict_` (for example `dict_state`)
- Lists: `list_` (for example `list_grids`)
- Series: `ser_` (for example `ser_output`)
- Paths: `path_` (for example `path_config`)

### 2.3 Type Hints and Public API

Target Python `>= 3.12` syntax (`X | None`, built-in generic collections `list[...]`, `dict[...]`, `tuple[...]`, and `Literal[...]`):
```python
from pathlib import Path
from typing import Literal
import pandas as pd

from supy import SUEWSSimulation


def run_site_window(
    path_config: Path,
    start_date: str | None = None,
    end_date: str | None = None,
    output_format: Literal["parquet", "txt"] = "parquet",
) -> pd.DataFrame:
    """Run a SUEWSSimulation window and return the output DataFrame."""
    sim = SUEWSSimulation(path_config)
    if start_date is not None or end_date is not None:
        df_forcing = sim.forcing.df.loc[start_date:end_date]
        sim.update_forcing(df_forcing)
    output = sim.run()
    return output.df
```

Use `SUEWSSimulation` for all new scripts, tests, and tutorials (see `.claude/rules/python/api-approach.md`). Note that the legacy `supy_driver` / `f2py` / `f90wrap` wrappers and internal `run_supy_ser` entry points have been replaced by `src/suews_bridge/` and `src/supy/_run_rust.py`.

### 2.4 Docstrings

Use **NumPy-style docstrings** for all public functions, methods, and classes (enforced by `ruff check --select D`):
```python
def calculate_flux(
    df_input: pd.DataFrame,
    site: str,
    freq_s: int = 3600,
) -> pd.DataFrame:
    """Calculate energy flux from input forcing data.

    Parameters
    ----------
    df_input : pd.DataFrame
        Input forcing data with meteorological variables.
    site : str
        Site identifier.
    freq_s : int, optional
        Output frequency in seconds, by default 3600.

    Returns
    -------
    pd.DataFrame
        Calculated flux values.
    """
```

### 2.5 Logging, File Encoding, and Error Handling

- **Plain-ASCII logging**: Use `logger_supy` (not `print()`, except inside CLI handlers under `src/supy/cmd/`) with plain ASCII messages only:
  ```python
  try:
      result = run_step(df_forcing)
  except ValueError as exc:
      logger_supy.error("Validation failed for site %s: %s", site_name, exc)
      raise
  ```
- **Explicit UTF-8 file operations**: Always pass `encoding="utf-8"` to `open()`, `Path.read_text()`, `Path.write_text()`, and text-mode `NamedTemporaryFile` (enforced in CI by `.github/workflows/encoding-audit.yml` and `scripts/lint/check_pathlib_encoding.py`).
- **Path handling**: Use `pathlib.Path` rather than `os.path`, and `importlib.resources` (`trv_supy_module` from `supy._env`) rather than `Path(supy.__file__).parent` for packaged data.

## Fortran Guidelines

See **[FORTRAN_NAMING_CONVENTIONS.md](FORTRAN_NAMING_CONVENTIONS.md)** and [`.claude/rules/fortran/conventions.md`](../.claude/rules/fortran/conventions.md) for complete rules.

### 3.1 Module and Derived-Type Structure

```fortran
! File: src/suews/src/suews_phys_snow.f95
MODULE module_phys_snow
   USE module_ctrl_error_state, ONLY: set_supy_error, supy_error_flag
   IMPLICIT NONE
   PRIVATE

   PUBLIC :: dts_snow_state, update_snow_state

   ! Constants use PARAMETER (never mutable module variables)
   REAL(KIND(1D0)), PARAMETER :: SNOW_DENSITY_MIN = 50.0D0  ! [kg m-3]

   ! Derived types use the dts_ prefix; component defaults apply per instance
   TYPE :: dts_snow_state
      REAL(KIND(1D0)) :: depth = 0.0D0          ! [m]
      REAL(KIND(1D0)) :: density = 100.0D0      ! [kg m-3]
   END TYPE dts_snow_state

CONTAINS

   SUBROUTINE update_snow_state(temp_air, state)
      REAL(KIND(1D0)), INTENT(IN) :: temp_air         ! [K]
      TYPE(dts_snow_state), INTENT(INOUT) :: state

      ! Local variables: declare bare, NEVER initialise in the declaration!
      REAL(KIND(1D0)) :: temp_val                     ! [K]

      ! Assign local variables in the executable section
      temp_val = 0.0D0

      IF (temp_air <= 0.0D0) THEN
         CALL set_supy_error(100, 'update_snow_state: temp_air [K] must be positive')
         RETURN
      END IF

      temp_val = temp_air - 273.15D0
      ! ... physics calculations ...
   END SUBROUTINE update_snow_state

END MODULE module_phys_snow
```

### 3.2 Naming Conventions (Summary)

| Element | Convention | Example |
|---------|------------|---------|
| **Files** | `suews_<category>_<name>.f95` (`ctrl`, `phys`, `type`, `util`) | `suews_phys_snow.f95`, `suews_type_snow.f95` |
| **Modules** | `module_<category>_<name>` | `module_phys_snow`, `module_type_snow` |
| **Derived types** | `dts_<name>` | `dts_snow_state` |
| **Subroutines / Functions** | `snake_case` | `update_snow_state`, `calc_snow_density` |
| **Constants (`PARAMETER`)** | `UPPERCASE_UNDERSCORES` | `STEFAN_BOLTZMANN`, `VON_KARMAN` |
| **Variables & components** | `lowercase_underscores` | `air_temperature`, `surface_fraction` |

### 3.3 Thread Safety: No Implicit `SAVE` or Mutable Module State

The Rust bridge (`src/suews_bridge/`) executes multi-grid simulations in parallel across Rayon worker threads calling the same compiled Fortran procedures. Any static variable is shared across threads and corrupts runs.

- **NEVER initialise a procedure-local variable in its declaration**:
  ```fortran
  ! WRONG -- implies SAVE in Fortran! Value persists across calls and races across threads.
  ! Blocked in CI by scripts/lint/check_saved_fortran_locals.py
  REAL(KIND(1D0)) :: temp_val = 0.0D0
  LOGICAL :: first_call = .TRUE.

  ! CORRECT -- declare bare, then assign in the executable part
  REAL(KIND(1D0)) :: temp_val
  LOGICAL :: first_call

  temp_val = 0.0D0
  first_call = .TRUE.
  ```
- **Constants**: Mark true compile-time constants with `PARAMETER` (after verifying no callee mutates a dummy argument lacking `INTENT(IN)`).
- **Persistent state**: Any quantity that must persist across timesteps belongs on `SUEWS_STATE` (`suews_type_*.f95` / `suews_ctrl_type.f95`), **never** in a `SAVE` local or mutable module variable.
- **Error reporting**: Never use `STOP`, `ERROR STOP`, or `WRITE(*,...)`. Report fatal errors via `set_supy_error(code, 'routine: message')`, assign safe sentinels (such as `-999.0D0`) to `INTENT(OUT)` arguments, and `RETURN`. Guard downstream calls with `IF (supy_error_flag()) RETURN`. Report non-fatal warnings via `ErrorHint(..., modState)` or `modState%errorstate%report(...)` (see `.claude/rules/fortran/error-reporting.md`).

### 3.4 Precision, Numerical Guarding, and Floating-Point Comparisons

- Always use explicit double precision: `REAL(KIND(1D0))` or `REAL(REAL64)` from `ISO_FORTRAN_ENV`, never bare `REAL`.
- Never test floating-point numbers with exact `==`:
  ```fortran
  ! Wrong
  IF (h_flux == 0.0D0) THEN

  ! Correct
  REAL(KIND(1D0)), PARAMETER :: EPS_FP = 1.0D-12
  IF (ABS(h_flux) <= EPS_FP) THEN
  ```
- Comparisons such as `x < 0.0D0` evaluate `.FALSE.` when `x` is `NaN`. Check `IEEE_IS_NAN(x)` (from `IEEE_ARITHMETIC`) explicitly when guarding physical inputs.

## Rust Bridge Overview

`src/suews_bridge/` is the sole Python-Fortran interface (replacing `f90wrap` / `f2py`).

- **Layers**: PyO3 `#[pyclass]` bindings -> safe Rust codecs (`StateCodec` / `CompositeCodec`) and simulation driver -> `unsafe extern "C"` FFI over `ISO_C_BINDING` facades (`src/suews_bridge/c_api/`).
- **Contract**: Follow `.claude/rules/rust/bridge-contract.md` (BR-01 through BR-10). Keep equations in Fortran, defaults and migrations in Python (`src/supy/data_model/`), and marshalling/shape validation in the bridge. Never invent silent numeric fallbacks in Rust for missing inputs.
- **Linting**: `cargo fmt` and `cargo clippy --all-features --all-targets -- -D warnings` (enforced by `.github/workflows/rust-clippy.yml`).

## Documentation Standards

- Comments explain **why** (physical basis, invariant, edge case), not **what**.
- Document physical units in square brackets using ASCII notation:
  ```fortran
  REAL(KIND(1D0)) :: rainfall     ! [mm h-1]
  REAL(KIND(1D0)) :: temperature  ! [K]
  ```
- Never hand-edit generated RST or JSON schema artefacts; update the generator or Pydantic field descriptions instead (see `.claude/rules/docs/conventions.md`).

## Testing Requirements

See **[testing/TESTING_GUIDELINES.md](testing/TESTING_GUIDELINES.md)** and [`.claude/rules/tests/patterns.md`](../.claude/rules/tests/patterns.md).

### 6.1 Test File Organisation

All Python tests MUST live under `test/` and declare a module-level nature marker (`pytestmark = pytest.mark.api` or `pytestmark = pytest.mark.physics`):
```text
test/
|-- core/           # Core model contracts, sample output, exact reference gate
|-- data_model/     # Pydantic schema, validators, YAML converters
|-- physics/        # Scientific and option-sweep validation
|-- io_tests/       # Input/output and resampling contracts
|-- cmd/            # CLI entry points (suews run/convert/validate/schema)
|-- mcp/            # MCP server and packaging manifests
|-- docs/           # Documentation sanity checks
|-- umep/           # UMEP/QGIS compatibility contracts
`-- fixtures/       # Vendored test fixtures and reference outputs
```

### 6.2 Running Tests

```bash
# Fast critical-path validation before every commit (~30-60 s)
make test-smoke

# Everyday development suite (core, data_model, physics, io_tests; excludes slow & peripheral dirs)
make test

# Complete test suite including slow tests and peripheral surfaces (cmd, mcp, docs, knowledge, umep)
make test-all

# UMEP/QGIS compatibility suite (targets Windows + Python 3.12; skips elsewhere)
make test-qgis
```

### 6.3 Native Mutable-State Inventory

- `src/suews_bridge/src/lib.rs` exposes the active Rust entry points, and `src/suews_bridge/c_api/driver.f95::suews_cal_multitsteps_c` materialises `SUEWS_TIMER`, `SUEWS_CONFIG`, `SUEWS_SITE`, and `SUEWS_STATE` as call-local values.
- `src/suews/src/suews_ctrl_error.f95::module_ctrl_error_state` stores fatal error flags, codes, and messages in C11 thread-local storage (`src/suews/src/suews_ctrl_error_tls.c`, GH#1736), and `suews_cal_multitsteps_c` calls `reset_supy_error()` at every driver entry; non-fatal warnings accumulate per grid on `modState%errorstate` (GH#1737).
- `src/suews/src/suews_phys_anohm.f95` still contains legacy `SAVE` module declarations, while AnOHM remains disabled outside the active multi-grid path; removing or moving that state into `SUEWS_STATE` is a gate for reactivating it.

## Version Control and CHANGELOG Practices

### 7.1 Git Remote and Merge Queue
- Use `origin` (`git@github.com:UMEP-dev/SUEWS.git`) as the sole remote.
- Never push directly to `master`. All changes enter `master` via pull requests and the GitHub merge queue (`gh pr merge --auto` or `/queue-pr`).

### 7.2 Commit Messages

Follow conventional commit format:
```text
type(scope): brief description

Longer explanation if needed.

Fixes #123
```
Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`. Do not add AI agent branding prefixes to PR titles.

### 7.3 `CHANGELOG.md` Format

Follow [`.claude/rules/changelog/format.md`](../.claude/rules/changelog/format.md) (or run `/log-changes`):
- Date heading: `### DD Mon YYYY` (no leading zero on single-digit days, three-letter month, actual commit date; for example `### 6 May 2026` or `### 26 Nov 2025`).
- Entry format: `- [category] Description (#PR)` or `- [category][status] Description (#PR)`.
- Categories: `[feature]`, `[bugfix]`, `[change]`, `[maintenance]`, `[doc]`.
- Status tags (required on `[feature]` and `[change]`): `[experimental]` (default for new features), `[stable]`, or `[internal]`.

```markdown
### 26 Nov 2025

- [feature][experimental] Added new radiation scheme (#123)
- [feature][stable] Added OOP interface for output data (#456)
- [bugfix] Fixed snow melt energy balance (#789)
- [maintenance] Updated CI matrix configuration (abc1234)
```

## Configuration and Data Model Guidelines

### 8.1 Separation of Concerns

High-level classes (`SUEWSSimulation`) parse `SUEWSConfig` and unwrap `RefValue` fields; low-level functions accept explicit, primitive/typed arguments rather than config objects (see `.claude/rules/python/config-patterns.md`):
```python
# Good: high-level class extracts concrete values
def save_outputs(self, path_out: Path) -> None:
    freq_s = self._config.output.freq.value if self._config else 3600
    save_df_output(self._df_output, path_out=path_out, freq_s=int(freq_s))

# Bad: passing config object into low-level helper
def save_df_output(df_output: pd.DataFrame, config: SUEWSConfig) -> None: ...
```

### 8.2 Pydantic Models

Use Pydantic v2 models in `src/supy/data_model/` with `Literal[...]` / enum types, `X | None` optional annotations, and `snake_case` field names:
```python
from typing import Literal
from pydantic import BaseModel, Field


class StabilityConfig(BaseModel):
    scheme: Literal["W16", "K75"] = Field(
        default="W16",
        description="Atmospheric stability scheme selection.",
    )
    z0m_fallback: float | None = Field(
        default=None,
        description="Optional momentum roughness override [m].",
    )
```

Any structural schema change must follow [`.claude/rules/python/schema-versioning.md`](../.claude/rules/python/schema-versioning.md).

## Automated Formatting and Linting

### 9.1 Local Formatting and Style Checks

Format Python and Fortran together with `make format`, and audit style with `/lint-code` (or the underlying tools directly):

```bash
# Format Python (ruff format src test) and Fortran (fprettify via .fprettify.rc)
make format

# Check Python lint + NumPy docstrings (configured in .ruff.toml)
ruff check .
ruff format --check src test

# Check Fortran formatting without modifying files
fprettify --diff src/suews/src/*.f95

# Check Rust bridge formatting and lints (lint-only mode needs no Fortran link)
cd src/suews_bridge && cargo fmt --check
cd src/suews_bridge && SUEWS_BRIDGE_LINT_ONLY=1 cargo clippy --all-features --all-targets -- -D warnings
```

In Claude Code or Codex sessions, invoke `/lint-code` to run `ruff`, `fprettify --diff`, changed-file docstring checks, and SUEWS convention checks together.

### 9.2 Fortran `.fprettify.rc` Settings

`fprettify` reads `.fprettify.rc` from the repository root automatically:
- `indent = 3` and `line-length = 132` (compiler flags also pass `-ffree-line-length-none`).
- `case = [0, 0, 0, 0]`: **preserves existing case** for keywords, procedures/modules, derived types, and intrinsics rather than forcing uppercase or lowercase rewrites across mixed-case files. Do not pass `--case` overrides on the command line.

### 9.3 CI Enforcement

Formatting, linting, and invariants are verified on pull requests and in the merge queue before code lands on `master`:
- Python lint, formatting, and encoding checks (`ruff`, `.github/workflows/encoding-audit.yml`).
- Rust clippy gate (`.github/workflows/rust-clippy.yml`).
- Repository lint scripts under `scripts/lint/` (including `check_saved_fortran_locals.py`, `check_test_markers.py`, `check_packaged_data_paths.py`, and `check_schema_version_bump.py`).
