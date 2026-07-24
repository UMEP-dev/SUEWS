# SUEWS Testing Guidelines Reference

This document consolidates all testing guidelines and patterns for permanent reference by both human developers and AI assistants.

## Overview

These guidelines are extracted from the planning documents and represent best practices and patterns to follow when writing tests for SUEWS.

## 1. Test Design Philosophy (from TEST_DESIGN_DISCUSSION.md)

### Types of Tests
- **Unit Tests**: Test individual functions/methods in isolation
- **Integration Tests**: Test how components work together
- **Scientific Validation Tests**: Verify physics/science (energy balance, water conservation)
- **Regression Tests**: Ensure changes don't break existing functionality
- **Edge Case Tests**: Handle extreme/unusual inputs

### Key Questions When Designing Tests
1. **What am I testing?** (One clear objective)
2. **What's the expected behaviour?** (Define success)
3. **What could go wrong?** (Failure modes)
4. **How do I know it worked?** (Assertions)
5. **Is this test reproducible?** (Deterministic)
6. **Is this test fast enough?** (Performance)

### FIRST Principles
- **F**ast - Tests should run quickly
- **I**ndependent - No dependencies between tests
- **R**epeatable - Same results every time
- **S**elf-validating - Clear pass/fail
- **T**imely - Written with the code

### AAA Pattern
- **A**rrange - Set up test data
- **A**ct - Execute the functionality
- **A**ssert - Verify the results

## 2. Fortran Test Patterns (from FORTRAN_TEST_PATTERNS.md)

### Historical f90wrap modules (removed)
The f90wrap interface has been removed. Fortran physics is now accessed
exclusively via the Rust bridge. The module list previously here is retained
in `FORTRAN_TEST_PATTERNS.md` for historical reference.

### Common Test Patterns

#### Pattern 1: Direct Physics Function Testing
Test individual physics calculations without full model overhead.

#### Pattern 2: State Object Testing
Test Fortran derived types and state management.

#### Pattern 3: Array Handling Tests
Test array operations between Python and Fortran.

#### Pattern 4: Edge Case Testing
Test numerical edge cases and boundary conditions.

#### Pattern 5: Performance Testing
Test performance with batch operations.

#### Pattern 6: Physics Validation Testing
Validate physics calculations against known solutions.

#### Pattern 7: Error Detection Testing
Verify error handling and invalid input detection.

### Best Practices
1. **Test Naming**: Use descriptive names indicating what is being tested
2. **Input Validation**: Always test with physically realistic values
3. **Output Validation**: Check physical constraints, verify units
4. **Documentation**: Document the physics being tested with references
5. **Performance**: Minimize Python-Fortran transitions

### Common Pitfalls and Solutions
- **Uninitialized Variables**: Always check initialization
- **Array Indexing**: Remember Fortran uses 1-based indexing
- **Floating Point Comparison**: Use `np.allclose()` or tolerance
- **State Pollution**: Reset state or use fresh instances
- **Memory Leaks**: Test with memory profiling

## 3. Error Handling Patterns (from ERROR_HANDLING_PATTERNS.md)

### Core Principles
1. **Helpful for debugging** - Precise indication of cause
2. **Friendly for user experience** - Easy to understand what went wrong

### Dual-Layer Error Strategy
- **Technical Layer**: Exact error location, stack trace, variable states
- **User Layer**: What went wrong, why it matters, how to fix it

### Error Message Structure
```python
class SUEWSError(Exception):
    def __init__(self, user_message, debug_info=None, fix_hint=None):
        self.user_message = user_message
        self.debug_info = debug_info or {}
        self.fix_hint = fix_hint
```

### Best Practices
**DO**:
- Provide clear user messages in plain language
- Include actionable fix hints
- Capture debug context for investigation
- Use error hierarchies appropriately
- Test error paths

**DON'T**:
- Don't expose stack traces to users by default
- Don't use technical jargon in user messages
- Don't lose context when re-raising exceptions
- Don't ignore errors
- Don't assume user technical knowledge

## 4. Test Organisation Standards

### Python Test Structure
```
test/
├── unit/                    # Fast, focused tests
│   ├── test_data_model/    # Configuration validation
│   ├── test_fortran_interface/  # Type conversions
│   └── test_utilities/     # Helper functions
├── integration/            # Component interaction tests
│   ├── test_temporal/      # Time-series tests
│   └── test_weather/       # Weather scenarios
├── physics/                # Scientific validation
│   ├── test_energy_balance/
│   ├── test_water_balance/
│   └── test_atmospheric_stability/
└── performance/            # Performance benchmarks
```

### Fortran Test Organisation (by Physics)
```
test/physics/
├── test_atmospheric_stability/
├── test_evaporation/
├── test_energy_balance/
└── test_radiation/
```

## 5. pytest Markers and Configuration

### Two-Axis Marker System (gh#1300)

Markers sit on two orthogonal axes, declared in `pyproject.toml` (see
`.claude/rules/tests/patterns.md` for the full authoring guide):

- **Nature axis** -- what the test actually exercises. Every test file must
  declare exactly one (or, rarely, both) at module level via `pytestmark`:
  - `physics` -- numerical/binary correctness; sufficient to run once per
    `(OS, arch)` on the canonical build Python (the compiled artefact
    determines the result, not the interpreter).
  - `api` -- Python wrapper correctness (pandas/numpy/pydantic, CLI,
    `SUEWSSimulation`); run across `(platform x Python)` because the
    dependency surface varies per interpreter.
- **Tier axis** -- how fast/expensive the test is, applied as a per-test
  decorator on top of the nature marker:
  - `smoke` -- minimal wheel validation (~6 tests, ~60s)
  - `smoke_bridge` -- bridge-loading subset run across `cp312..cp3xx` after a
    single abi3 build
  - `core` -- core physics and logic tests (Fortran, driver)
  - `cfg` -- config/schema validation tests
  - `util` -- utility function tests (non-critical)
  - `rust` -- Rust bridge backend tests (requires `suews_bridge` built with
    the `physics` feature)
  - `slow` -- tests taking >30s individually
  - `qgis` -- UMEP plugin integration tests in `test/umep/`, targeting
    Windows + Python 3.12 (current QGIS 3 LTR / QGIS 4 runtime); auto-applied
    by `test/umep/conftest.py`, stays out of normal PR/merge-queue tiers

```python
# pyproject.toml (abbreviated -- see pyproject.toml for the authoritative list)
[tool.pytest.ini_options]
markers = [
    # Nature axis
    "physics: Numerical/binary correctness; sufficient to run once per (OS, arch) on the canonical Python",
    "api: Python wrapper correctness; run across (platform x Python) because pandas/numpy/pydantic surface varies",
    # Tier axis
    "smoke: Minimal wheel validation (~6 tests, ~60s)",
    "smoke_bridge: Bridge-loading subset run across cp312..cp3xx after a single abi3 build",
    "core: Core physics & logic tests (Fortran, driver)",
    "rust: Rust bridge backend tests (requires suews_bridge with physics feature)",
    "util: Utility function tests (non-critical)",
    "cfg: Config/schema validation tests",
    "slow: Tests taking >30s individually",
    "qgis: UMEP plugin integration tests in test/umep/ (Windows + Python 3.12 target)",
]
```

A CI expression composes the two axes, e.g. `-m "api and smoke"` or
`-m "physics and not slow"`. A static lint
(`scripts/lint/check_test_markers.py`) plus a `pytest_collection_finish` hook
in `test/conftest.py` fail any PR that adds a test file without a nature
marker.

### Tier -> CI Mapping

Tier names map onto CI wall-clock budgets and the events that run them (see
`.claude/rules/ci/conventions.md` "Test Tier Definitions" and "Test Tier
Assignment by Event" for the authoritative mapping):

| Tier | Approx. duration | Runs on |
|------|-------------------|---------|
| `smoke` | ~60s | Draft PRs (python/util/ci/tests-only changes), ready PRs (ci/tests-only) |
| `core` / `cfg` | ~2-3 min | Draft PRs (fortran/rust -> core; build-system -> cfg) |
| `standard` | ~5-10 min | Ready PRs, merge queue |
| `physics-full` | physics axis widens to include `slow`; api axis stays `standard` | Ready PR / merge queue when the PR carries `0-physics:change` (gh#1576) |
| `all` | ~15-30 min | Nightly schedule, tag push, manual dispatch (`full`) |

`qgis`-tier tests run only under the `all` tier (nightly/release/manual
`full`); every other tier expression explicitly excludes them
(`and not qgis`), so they never gate a PR or the merge queue.

## 6. Coverage Standards

### Coverage Targets
- **Overall Goal**: 80% coverage
- **Critical Paths**: 95-100% coverage required
- **Core Functions**: 85-90% coverage target
- **Utilities**: 70-80% coverage target
- **UI/CLI**: 60-70% coverage acceptable

### Pull Request Rules
1. No PR can decrease overall coverage
2. New code must have ≥ 80% coverage
3. Critical path changes require 95% coverage
4. Coverage report required in PR

## 7. Test Documentation Standards

Tests should serve as documentation:
```python
def test_urban_canyon_radiation(self):
    """
    Test radiation calculations in urban canyon.

    This validates equation 3.2 from Grimmond & Oke (1999)
    which calculates net radiation considering:
    - Multiple reflections between buildings
    - Sky view factor
    - Building height/width ratio

    Expected accuracy: ±5 W/m²
    """
```

## 8. CI Test Tier Principles

### Tier Structure

CI tiers are driven by the two-axis marker system in Section 5, not a
separate taxonomy. The `determine_matrix` job in
`.github/workflows/build-publish_to_pypi.yml` assigns a tier per event; see
`.claude/rules/ci/conventions.md` ("Test Tier Definitions" and "Test Tier
Assignment by Event") for the authoritative mapping:

- **Draft PR**: `smoke`, `core`, or `cfg` depending on what changed -- fastest
  feedback (~60s-3 min)
- **Ready PR / merge queue**: `standard` (all non-`slow` tests, ~5-10 min), or
  `physics-full` when the PR carries `0-physics:change` (gh#1576)
- **Nightly / tag push / manual dispatch (`full`)**: `all` -- the complete
  suite including `slow` and `qgis` (~15-30 min)

### Test Selection Criteria for PR
- Fast (< 10 seconds per test)
- Focused (test one functionality)
- Stable (no flaky failures)
- Critical (block merging if fail)

### Session-Scoped Sample Fixtures (Required for Sample Runs)

Tests that need the bundled sample simulation should use the session-scoped
fixtures in `test/conftest.py` rather than building or running a fresh
`SUEWSSimulation` per test:

- `sample_data_loaded` -- the bundled `(df_state_init, df_forcing)`, loaded
  once per session (read-only; `.copy()` before mutating)
- `completed_sample_sim` -- a short `SUEWSSimulation` built from the sample
  YAML with `.run()` already called (read-only; do not `.reset()`, mutate, or
  re-run)
- `sample_run_cached` -- a session-scoped factory memoising functional-API
  (`supy.run_supy`) sample runs by forcing window

Use these for any test that only *reads* a completed run's config, forcing,
state, or output. Tests exercising running, mutation, or lifecycle behaviour
(reset, checkpoint continuation, chunking, parallelism, before/after-run
state) must still construct and run their own instance -- see each fixture's
docstring in `test/conftest.py` for the full contract.

---

## For AI Assistants (Claude Code)

When writing tests for SUEWS:
1. Follow the patterns documented above
2. Use the Rust bridge for Fortran integration testing
3. Keep tests AI-friendly and human-manageable
4. Always use British English in test documentation
5. Prioritise clarity over cleverness
6. Include physics references where applicable
7. Ensure tests are deterministic and reproducible
8. Treat supy as an external package - use proper package data access methods

## 9. Package Data Access Standards

### Accessing supy Package Data in Tests

Tests should treat supy as an external package. When accessing package data (e.g., sample configurations, test data), use the standard Python approach:

#### Recommended Approach: `importlib.resources` (Python 3.9+)
```python
from importlib.resources import files, as_file
import supy as sp
import yaml

# Method 1: Direct reading
sample_config = files("supy") / "sample_data" / "sample_config.yml"
with sample_config.open() as f:
    config_data = yaml.safe_load(f)

# Method 2: Extract to temporary path for functions expecting file paths
with as_file(files("supy") / "sample_data" / "sample_config.yml") as config_path:
    df_state_init = sp.init_supy(config_path)
```

#### Why This Approach?
- **Works in all environments**: zipfiles, eggs, wheels, frozen applications
- **Package-agnostic**: Doesn't assume filesystem layout
- **Future-proof**: Standard Python way to access package data
- **Type-safe**: Modern typing support
- **Portable**: No assumptions about installation method

#### Acceptable Alternative (with limitations)
```python
from pathlib import Path
import supy as sp

# This works but is less portable
config_path = Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
```

#### Use supy's API When Available
```python
# Preferred: Use supy's built-in functions
df_state_init, df_forcing = sp.load_sample_data()

# Instead of manually accessing files
```

### Best Practices
1. **Use supy's API first**: If supy provides a function, use it
2. **Use importlib.resources for package data**: When you need direct file access
3. **Never use relative paths from repo root**: Don't assume repo structure
4. **Don't traverse to `src/`**: Treat supy as installed package

## For Human Developers

1. Write tests alongside code development
2. Use the test patterns as templates
3. Document why a test exists, not just what it does
4. Keep individual tests focused and fast
5. Use meaningful test names that describe the scenario
6. Ensure tests work across all supported platforms
7. Review coverage reports to identify gaps
8. Use `importlib.resources` for accessing package data
