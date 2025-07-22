# Feature: Refine All Tests

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2025-07-17

## Context
Comprehensive test suite refinement to improve test quality, reliability, and maintainability across the entire SUEWS/SuPy codebase.

## GitHub Issues
- None (internal quality improvement)

## Status
- **Current**: doing
- **Outcome**: pending
- **Completed**: [YYYY-MM-DD]
- **Main PR**: #[pr-number]

## PR Strategy: Multiple Sub-PRs → Main PR

This is a large refactoring that will be split into multiple smaller PRs:

### Sub-PR 1: Enable All Tests (COMPLETED)
- **Branch**: `fix/enable-all-tests`
- **Scope**: Remove platform restrictions from test_supy.py
- **Size**: ~10 lines changed (3 insertions, 19 deletions)
- **PR**: #513
- **Commit**: 2e36d189

### Sub-PR 2: Fix Critical Warnings (HIGH PRIORITY)
- **Branch**: `fix/numpy-pandas-warnings`
- **Scope**: Fix NumPy 2.0 and Pandas deprecation warnings
- **Size**: ~100-200 lines across multiple files
- **PR**: #[number]

### Sub-PR 3: Create Test Structure (FOUNDATION)
- **Branch**: `test/reorganize-structure`
- **Scope**: Create directories, move existing tests, add __init__.py files
- **Size**: File moves, minimal code changes
- **PR**: #[number]

### Sub-PR 4: Add Missing Core Tests (ENHANCEMENT)
- **Branch**: `test/add-core-tests`
- **Scope**: Add test_load.py, test_post.py
- **Size**: New test files (~200-300 lines each)
- **PR**: #[number]

### Sub-PR 5: Add Utility Tests (OPTIONAL)
- **Branch**: `test/add-utility-tests`
- **Scope**: Add tests for atm, gs, ohm utilities
- **Size**: New test files
- **PR**: #[number]

### Main PR: Merge All Test Refinements
- **Branch**: `feature/refine-all-tests` (current)
- **Scope**: Merge all sub-PRs, final cleanup
- **Merge strategy**: Rebase sub-PRs onto this branch

## Progress Tracking

### Phase 1: Ensure All Essential Tests Are Running
- [x] Identify why 3 tests are skipped (full test markers)
  - Found: test_gen_forcing, test_is_supy_save_working, test_is_smd_veg_weighted
  - These use @skipUnless(flag_full_test) which only runs on specific platforms
  - flag_full_test = True only for Python 3.12 macOS ARM64 or Python 3.13 Linux x86_64
- [x] **ACTION: Remove platform restrictions - enable all tests for all platforms**
  - COMPLETED: Set flag_full_test = True unconditionally
  - Removed all @skipUnless decorators from 3 tests
  - Tests now run on all platforms and Python versions
- [x] Review if any tests are conditionally skipped in CI
  - CI runs on multiple platforms: Linux x86_64, macOS ARM64, Windows AMD64
  - Full tests run on: Linux x86_64 + Python 3.13, macOS ARM64 + Python 3.12
  - Other combinations skip the 3 "full test" tests
- [x] Check for tests marked as xfail that should be fixed
  - No xfail tests found - good!
- [x] Ensure benchmark tests are running properly
  - Benchmark tests moved to test_sample_output.py::test_sample_output_validation
  - Running properly and testing key physics variables
- [x] Verify all physics validation tests are active
  - Comprehensive physics tests in test_core_physics.py (8 tests)
  - Water balance test in test_supy.py active
  - All physics validation tests are running
- [ ] Create missing tests for uncovered functionality

### Phase 1.5: Test Reorganization by Functionality
<!-- TASK: Review and comment on this proposed test organization -->

#### Proposed Test Structure (Core-Focused)
<!-- TS:looks good -->
```
test/
├── core/           # Core SUEWS engine tests
├── data_model/     # Configuration and data model tests
├── physics/        # Physics validation tests
├── io/             # Essential I/O (forcing, output, state)
└── fixtures/       # Keep existing test data
```

#### Test Categorization Plan

**Core Tests (test/core/)**
<!-- Essential core functionality tests -->
- **CRUCIAL TESTS (highest priority):**
  - test_sample_output.py (benchmark validation)
  - test_fortran_state_persistence.py (state isolation)
  - test_floating_point_stability.py (numerical stability)
- Core parts of test_supy.py:
  - test_is_supy_running_multi_step
  - test_is_driver_connected
- test_suews_simulation.py (high-level interface)

**Data Model Tests (test/data_model/)**
<!-- Q: Should we split test_data_model.py into smaller files? -->
- test_data_model.py
- test_precheck.py (essential pre-run checks)
- test_conditional_validation.py
- test_validation_topdown.py
- test_validation_utils.py
- test_flexible_refvalue_clean.py

**Physics Tests (test/physics/)**
<!-- Physics and scientific validation tests -->
- test_core_physics.py
- Physics-related tests from test_supy.py:
  - test_water_balance_closed
  - test_is_smd_veg_weighted (if kept)

**I/O Tests (test/io/)**
<!-- Q: Are all these I/O tests essential? -->
<!-- TS: yes; besides, some of the validation tests are also I/O tests as they are checking the input data validity -->
- test_output_config.py
- test_save_supy.py (core output saving)
- test_resample_output.py
- test_dailystate_output.py
- test_forcing_file_list.py
- test_yaml_annotation.py

#### Missing Core Tests to Create
<!-- Prioritized list of missing tests -->
- [ ] test_load.py - Test forcing and state loading (HIGH PRIORITY)
- [ ] test_post.py - Test essential post-processing (MEDIUM PRIORITY)

#### Core Utility Tests to Create
<!-- Utilities that are part of core SUEWS functionality -->
- [ ] test_util_atm.py - Atmospheric calculations (KEEP - core model)
- [ ] test_util_gs.py - Surface conductance calculations (KEEP - core model)  
- [ ] test_util_ohm.py - OHM calculations (KEEP - core model)
- [ ] test_cmd.py - Command line tools (KEEP - user interface)
- [ ] test_util_wrf.py - WRF integration (KEEP for now)

#### Non-Essential Tests to Skip
<!-- Confirmed non-core utilities -->
- ❌ ERA5 processing - REMOVE
- ❌ TMY handling - REMOVE
- ❌ Gap filling - REMOVE
- ❌ Plotting utilities - REMOVE

### Phase 2: Address Deprecation Warnings
- [ ] Fix NumPy 2.0 deprecation warnings (array scalar conversion)
- [ ] Fix Pandas future warnings (Series getitem, downcasting)
- [ ] Fix multiprocessing fork warnings
- [ ] Document required dependency version constraints

### Phase 3: Fix Performance Warnings
- [ ] Address DataFrame fragmentation warnings
- [ ] Fix indexing past lexsort depth issues
- [ ] Optimize DataFrame operations in data_model/site.py
- [ ] Review and optimize DataFrame creation patterns

### Phase 4: Clean Up Test Warnings
- [ ] Fix conditional validation warnings
- [ ] Reduce noise from Pydantic serialization warnings
- [ ] Investigate and fix UserWarnings in tests
- [ ] Create warning suppression strategy for expected warnings

### Phase 5: Improve Test Quality
- [ ] Add missing test docstrings
- [ ] Improve test naming consistency
- [ ] Add parametrized tests where appropriate
- [ ] Review and improve test coverage

### Phase 6: Test Infrastructure
- [ ] Configure pytest to better handle warnings
- [ ] Set up warning filters for CI
- [ ] Create test performance benchmarks
- [ ] Document test best practices

## Key Decisions
- **Warning Strategy**: Fix root causes rather than suppress warnings
- **Compatibility**: Maintain compatibility with NumPy 2.0 and Pandas 2.x
- **Performance**: Prioritize fixing performance warnings that affect test speed

## Implementation Notes
- Test suite shows 38,993 warnings (majority from Pydantic data model validation)
- Many warnings are repeated across multiple tests
- DataFrame indexing issues appear systemic in data_model/site.py
- NumPy 2.0 compatibility needs attention
- Focus on fixing actual issues, not suppressing Pydantic validation warnings

## Files to Modify
- `src/supy/data_model/site.py` - Fix DataFrame indexing warnings
- `src/supy_driver/supy_driver.py` - Fix NumPy array conversion
- `src/supy/_check.py` - Fix Pandas downcasting behavior
- `src/supy/util/_io.py` - Fix Series position indexing
- `test/conftest.py` - (create) Configure pytest warning handling
- Various test files - Add proper warning handling

## Testing Strategy
- Run tests with strict warning mode to catch new issues
- Create specific tests for warning-free execution
- Benchmark test performance before/after fixes
- Ensure no functionality regression

## Documentation Updates
- Document required dependency versions
- Add testing best practices guide
- Update contributor guidelines with warning policies

## Abandonment Notes
(Only if abandoned) Reason for abandonment and any learnings for future reference.