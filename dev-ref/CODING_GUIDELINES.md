# SUEWS Coding Guidelines and Policy

This document defines the coding standards and policies for the SUEWS (Surface Urban Energy and Water Balance Scheme) project. These guidelines apply to both human developers and AI assistants (including Claude Code) when contributing to the project.

## Table of Contents
1. [General Principles](#general-principles)
2. [Python Guidelines](#python-guidelines)
3. [Fortran Guidelines](#fortran-guidelines)
4. [Documentation Standards](#documentation-standards)
5. [Testing Requirements](#testing-requirements)
6. [Version Control Practices](#version-control-practices)

## General Principles

### 1.1 Language and Spelling
- Use **British English** for all documentation, comments, and user-facing text
- Variable names may use common scientific abbreviations (e.g., `temp` for temperature)

### 1.2 File Organisation
- Keep related functionality together in modules
- Separate concerns between configuration, physics, and utilities
- Use clear, descriptive file names that indicate purpose

### 1.3 Code Quality
- Write self-documenting code with clear variable names
- Add comments only when the code's purpose is not immediately obvious
- Prefer clarity over cleverness
- Follow the principle of least surprise

## Python Guidelines

### 2.1 Import Organisation

Order imports as follows:
```python
# 1. Standard library imports
import os
import sys
from pathlib import Path
from typing import Dict, List, Optional

# 2. Third-party imports
import numpy as np
import pandas as pd

# 3. Local imports (use relative imports)
from .supy_driver import suews_driver as sd
from ._load import df_var_info
```

### 2.2 Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Functions | snake_case | `run_supy`, `check_forcing` |
| Variables | snake_case | `df_forcing`, `dict_state` |
| Classes | PascalCase | `SUEWSConfig`, `BaseModel` |
| Constants | UPPER_CASE | `DEFAULT_TIMESTEP`, `NSURF` |
| Private items | Leading underscore | `_internal_function` |
| Module names | lowercase with underscores | `data_model.py` |

**Special Prefixes:**
- DataFrames: `df_` (e.g., `df_forcing`)
- Dictionaries: `dict_` (e.g., `dict_state`)
- Lists: `list_` (e.g., `list_grid`)
- Series: `ser_` (e.g., `ser_var`)
- Paths: `path_` (e.g., `path_runcontrol`)

### 2.3 Type Hints

Always use type hints for function signatures:
```python
from typing import Tuple, Optional, Union

def run_supy_ser(
    df_forcing: pd.DataFrame,
    df_state_init: pd.DataFrame,
    save_state: bool = False,
    chunk_day: int = 3660,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Perform supy simulation."""
    ...
```

### 2.4 Documentation

Use **NumPy-style docstrings** for all public functions and classes:
```python
def function_name(param1: type, param2: type) -> return_type:
    """Brief description of function.

    Longer description if needed.

    Parameters
    ----------
    param1 : type
        Description of param1.
    param2 : type
        Description of param2.

    Returns
    -------
    return_type
        Description of return value.

    Examples
    --------
    >>> example_usage()
    expected_output
    """
```

### 2.5 Error Handling

Use structured error handling with informative messages:
```python
try:
    result = risky_operation()
except SpecificError as e:
    logger_supy.error(f"Operation failed: {e}")
    raise
except Exception:
    logger_supy.exception("Unexpected error in operation")
    raise
```

### 2.6 Best Practices

1. **Path Handling**: Use `pathlib.Path` instead of `os.path`
2. **Logging**: Use `logger_supy` instead of `print()`
3. **Deep Copying**: Use `copy.deepcopy()` for mutable state
4. **Configuration**: Keep configuration parsing separate from implementation
5. **Validation**: Validate inputs early and provide clear error messages

## Fortran Guidelines

### 3.1 Module Structure

```fortran
MODULE module_name
    USE other_module
    IMPLICIT NONE

    ! Parameters
    REAL(KIND(1D0)), PARAMETER :: CONSTANT_NAME = value

    ! Type definitions
    TYPE, PUBLIC :: TypeName
        REAL(KIND(1D0)) :: component = 0.0D0  ! Always initialise
    END TYPE TypeName

CONTAINS
    ! Subroutines and functions
END MODULE module_name
```

### 3.2 Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Modules | Descriptive with `_module` suffix | `AtmMoistStab_module` |
| Subroutines | Mixed case with underscores | `SUEWS_update_atmState` |
| Functions | Mixed case with underscores | `sat_vap_press` |
| Parameters | UPPERCASE with underscores | `MOLMASS_AIR` |
| Variables | lowercase with underscores | `dens_dry`, `temp_c` |
| Types | Mixed case | `OHM_STATE` |

### 3.3 Variable Declarations

```fortran
! Always use explicit precision
REAL(KIND(1D0)) :: temperature = 0.0D0  ! [K] Always include units
INTEGER :: surface_type = 0             ! Always initialise
CHARACTER(LEN=50) :: site_name = ''     ! Specify length

! Arrays with clear dimensions
REAL(KIND(1D0)), DIMENSION(nsurf) :: surface_fraction
```

### 3.4 Documentation

```fortran
! Module header with change log
! Original: sg feb 2012
! Modified: lj jun 2012 - added snow calculations
! Modified: hw oct 2014 - restructured for clarity

!===============================================
! Subroutine: calculate_something
! Purpose: Brief description
! Input: var1 - description [units]
!        var2 - description [units]
! Output: result - description [units]
!===============================================
SUBROUTINE calculate_something(var1, var2, result)
```

### 3.5 Floating-Point Comparisons

Never use exact equality for floating-point numbers:
```fortran
! Wrong
IF (H == 0.0) THEN

! Correct
REAL(KIND(1D0)), PARAMETER :: eps_fp = 1.0E-12
IF (ABS(H) <= eps_fp) THEN
```

### 3.6 Best Practices

1. **Always use `IMPLICIT NONE`**
2. **Initialise all variables in type definitions**
3. **Document physical units in comments**
4. **Use ASSOCIATE blocks for clarity**
5. **Define array dimensions as parameters**
6. **Group related functionality in modules**

## Documentation Standards

### 4.1 Code Comments

- Comments should explain **why**, not **what**
- Update comments when code changes
- Remove commented-out code before committing
- Use British English in all comments

### 4.2 Physical Units

Always document units in square brackets:
```fortran
REAL(KIND(1D0)) :: rainfall = 0.0D0  ! [mm h-1]
REAL(KIND(1D0)) :: temperature = 0.0D0  ! [K]
```

### 4.3 README Files

Each major component should have a README explaining:
- Purpose and functionality
- Dependencies
- Usage examples
- Key algorithms or references

## Testing Requirements

### 5.1 Test File Organisation

Tests are organised by functionality:
```
test/
├── core/               # Essential functionality tests
├── data_model/         # Configuration and validation tests
├── physics/            # Scientific validation tests
├── io_tests/           # Input/output tests
└── fixtures/           # Test data and configurations
```

### 5.2 Test Naming Conventions

- **Files**: `test_*.py` (e.g., `test_supy.py`, `test_benchmark.py`)
- **Classes**: `TestFeatureName` using `unittest.TestCase`
- **Methods**: Descriptive names explaining what's tested
  ```python
  def test_energy_balance_closure(self):
      """Test that energy balance is conserved."""
  
  def test_yaml_loading_no_spurious_warnings(self):
      """Test that loading YAML doesn't produce warnings."""
  ```

### 5.3 Test Structure

#### Class-based Tests (unittest)
```python
class TestSuPy(TestCase):
    def setUp(self):
        """Set up test fixtures."""
        warnings.simplefilter("ignore", category=ImportWarning)
        self.df_input = sp.load_SampleData()
    
    def test_normal_operation(self):
        """Test feature under normal conditions."""
        result = sp.run_supy(self.df_input)
        self.assertIsNotNone(result)
    
    def tearDown(self):
        """Clean up after tests."""
        # Cleanup if needed
```

#### Function-based Tests (pytest)
```python
def test_sample_output_validation():
    """Validate sample output against expected results."""
    df_input = sp.load_SampleData()
    result = sp.run_supy(df_input)
    assert result is not None
```

### 5.4 Assertion Patterns

- **unittest**: `self.assertTrue()`, `self.assertEqual()`, `self.assertAlmostEqual()`
- **pytest**: Simple `assert` statements
- **Pandas**: `pd.testing.assert_frame_equal()` for DataFrames
- **NumPy**: `np.testing.assert_array_almost_equal()` for arrays
- **Physical bounds**: Always check outputs are within realistic ranges

### 5.5 Test Data Management

- **Sample data**: Use `sp.load_SampleData()` for consistent test data
- **Fixtures**: Store test data in `test/fixtures/` directory
- **Temporary files**: Use `tempfile.TemporaryDirectory()` for file operations
- **Benchmark data**: Store expected results in `.pkl` files for regression testing

### 5.6 Physics and Scientific Testing

When testing scientific functionality:
```python
def test_energy_balance_closure(self):
    """Test energy balance: Rn = QH + QE + QS + QF."""
    result = sp.run_supy(df_input)
    
    # Extract fluxes
    rn = result.SUEWS['Rn']
    qh = result.SUEWS['QH']
    qe = result.SUEWS['QE']
    qs = result.SUEWS['QS']
    qf = result.SUEWS['QF']
    
    # Check energy balance within tolerance
    balance = rn - (qh + qe + qs - qf)
    tolerance = 10.0  # W/m²
    self.assertTrue(
        abs(balance.mean()) < tolerance,
        f"Energy balance error: {balance.mean():.2f} W/m²"
    )
```

### 5.7 State Isolation

Ensure tests don't pollute each other:
- Load fresh data in each test
- Use `setUp()` and `tearDown()` methods
- Be aware of Fortran state persistence issues
- Run tests individually and in suite to detect pollution

### 5.8 Best Practices

1. **Descriptive names**: Test names should explain what's being tested
2. **Docstrings**: Add docstrings explaining the test purpose
3. **One concept per test**: Each test should verify one specific behaviour
4. **Fast tests**: Keep individual tests fast (< 1 second where possible)
5. **Reproducibility**: Tests should produce identical results on repeated runs
6. **Platform awareness**: Consider platform-specific behaviour
7. **Warning handling**: Suppress expected warnings, fail on unexpected ones

### 5.9 Running Tests

```bash
# Run all tests
make test

# Run specific test file
python -m pytest test/test_supy.py -v

# Run with coverage
python -m pytest test --cov=supy

# Check for state pollution
python -m pytest test/test_specific.py -v  # Run alone
python -m pytest test -v                   # Run in suite
```

### 5.10 Test Execution Order

Critical tests run first via `conftest.py`:
- Sample output validation
- Benchmark tests
- Core functionality
This ensures fundamental features are tested before complex scenarios.

## Version Control Practices

### 6.1 Commit Messages

Follow conventional commit format:
```
type(scope): brief description

Longer explanation if needed.

Fixes #123
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

### 6.2 Branch Naming

Use descriptive branch names:
- `feature/add-new-physics-option`
- `fix/qe-qh-discrepancy`
- `docs/update-user-guide`

### 6.3 Pull Request Guidelines

1. Update relevant documentation
2. Add/update tests
3. Run full test suite
4. Update CHANGELOG if applicable:
   - [feature]: New user-facing functionality
   - [bugfix]: Bug fixes (create GitHub issue)
   - [change]: User-facing behaviour changes
   - [maintenance]: Internal/dev tooling (including Claude Code aspects)
   - [doc]: Documentation updates
5. Request review from appropriate team members

## Configuration and Data Model Guidelines

### 7.1 Separation of Concerns

Configuration objects should be handled at high levels:
```python
# Good: High-level extracts configuration
def high_level_method(self):
    freq_s = self._config.output.freq.value if self._config else 3600
    save_supy(df_output, df_state, freq_s=freq_s)

# Bad: Low-level accepts configuration
def save_supy(df_output, df_state, config):  # Don't do this: including config in the function signature is bad practice
```

### 7.2 Pydantic Models

Use Pydantic for configuration with proper validation:
```python
class PhysicsConfig(BaseModel):
    scheme: Literal["W16", "K75"] = Field(
        default="W16",
        description="Physics scheme selection"
    )

    @field_validator("scheme")
    def validate_scheme(cls, v):
        if v not in ["W16", "K75"]:
            raise ValueError(f"Unknown scheme: {v}")
        return v
```

## Automated Formatting and Linting

### 8.1 Python - Ruff

SUEWS uses [Ruff](https://docs.astral.sh/ruff/) for Python formatting and linting.

**Installation:**
```bash
pip install ruff
# or
mamba install -c conda-forge ruff
```

**Usage:**
```bash
# Format code
ruff format .

# Lint code
ruff check .

# Fix auto-fixable issues
ruff check --fix .
```

**Pre-commit Integration:**
```bash
# Install pre-commit hooks
pip install pre-commit
pre-commit install

# Run manually
pre-commit run --all-files
```

### 8.2 Fortran - fprettify

SUEWS uses [fprettify](https://github.com/pseewald/fprettify) for Fortran formatting.

**Installation:**
```bash
pip install fprettify
# or
mamba install -c conda-forge fprettify
```

**Usage:**
```bash
# Format a single file
fprettify src/suews/src/suews_phys_snow.f95

# Format all Fortran files
find src/suews/src -name "*.f95" -o -name "*.f90" | xargs fprettify --indent 3 --line-length 132

# Check without modifying
fprettify --diff src/suews/src/suews_phys_snow.f95
```

**Recommended fprettify settings:**
```bash
fprettify \
    --indent 3 \                    # 3 spaces for indentation
    --line-length 132 \             # Allow longer lines for scientific code
    --whitespace 2 \                # Adjust whitespace around operators
    --strict-indent \               # Strict indentation rules
    --enable-decl \                 # Format declarations
    --case 1 1 1 1 \               # Keywords lowercase, intrinsics lowercase
    src/suews/src/*.f95
```

### 8.3 Editor Integration

**VS Code:**
- Install "Ruff" extension for Python
- Install "Modern Fortran" extension with fprettify support
- Configure settings.json:
```json
{
    "[python]": {
        "editor.formatOnSave": true,
        "editor.defaultFormatter": "charliermarsh.ruff"
    },
    "[fortran]": {
        "editor.formatOnSave": true,
        "fprettify.arguments": [
            "--indent", "3",
            "--line-length", "132",
            "--whitespace", "2"
        ]
    }
}
```

### 8.4 CI/CD Integration

**Automated Formatting Philosophy**: Let machines handle formatting so developers can focus on functionality.

#### GitHub Actions Workflow: `auto-format.yml`

The master branch is automatically formatted after every merge:

- **Triggers**: On push to master containing Python or Fortran files
- **Actions**: 
  - Formats Python code with ruff
  - Formats Fortran code with fprettify
  - Only creates commit if changes are needed
  - Uses `[skip ci]` to avoid build loops
- **Benefits**:
  - Zero friction for contributors
  - Guaranteed consistency on master
  - Clear formatting commits in history

#### Local Development:

While formatting is optional for contributors, these tools are available:

```bash
make format      # Format all code locally
make lint        # Check code style without modifying

# Or use pre-commit hooks (optional but recommended)
pip install pre-commit
pre-commit install
```

**Note**: The master branch is the single source of truth for code formatting. All code merged to master will be automatically formatted to ensure consistency.

## Enforcement and Review

1. All code must pass automated checks before merging
2. Pre-commit hooks enforce formatting locally
3. GitHub Actions verify formatting in CI
4. Code reviews should verify adherence to these guidelines
5. AI assistants should reference this document when generating code
6. Guidelines should be updated through team consensus

---

## Revision History

- 18 Jul 2025, [TS](@sunt05): Initial version
- 18 Jul 2025, [TS](@sunt05): Added automated formatting tools (ruff, fprettify)
