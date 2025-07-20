# Testing Implementation Tasks for SUEWS

This document contains draft GitHub issues for implementing the test infrastructure. These are extracted from the planning documents and ready for review before posting.

---

## Issue 1: Implement Core Configuration and Validation Tests

**Title**: feat(test): Implement configuration validation and YAML loading tests

**Labels**: `enhancement`, `testing`, `priority:high`

**Description**:
Implement comprehensive tests for configuration loading and validation as the foundation of our testing infrastructure.

**Tasks**:
- [ ] Create `test/unit/test_data_model/test_config_validation.py`
  - [ ] Test valid YAML with all parameters
  - [ ] Test valid YAML with minimal parameters
  - [ ] Test invalid YAML syntax handling
  - [ ] Test missing required parameters
  - [ ] Test invalid parameter values
  - [ ] Test invalid parameter combinations
- [ ] Create `test/unit/test_data_model/test_yaml_loading.py`
  - [ ] Test YAML parsing
  - [ ] Test default value application
  - [ ] Test type conversions
- [ ] Create `test/integration/test_config_loading/test_config_scenarios.py`
  - [ ] Test real-world configuration examples
  - [ ] Test migration from old formats

**Acceptance Criteria**:
- All configuration edge cases covered
- Clear error messages for invalid configurations
- Tests run in < 5 seconds total

**References**:
- See TESTING_GUIDELINES_REFERENCE.md for patterns
- Priority: Critical - blocks all functionality if broken

---

## Issue 2: Implement Fortran-Python Interface Tests

**Title**: feat(test): Add comprehensive Fortran-Python interface testing

**Labels**: `enhancement`, `testing`, `priority:high`

**Description**:
Test the critical interface between Python and Fortran to ensure type safety and memory management.

**Tasks**:
- [ ] Create `test/unit/test_fortran_interface/test_type_conversions.py`
  - [ ] Test numeric type conversions (real*8 ↔ float64)
  - [ ] Test array dimension handling
  - [ ] Test string handling between languages
  - [ ] Test missing/NaN value propagation
- [ ] Create `test/unit/test_fortran_interface/test_memory_management.py`
  - [ ] Test large array transfers
  - [ ] Test repeated allocations/deallocations
  - [ ] Add memory leak detection
  - [ ] Verify state isolation
- [ ] Create `test/unit/test_fortran_interface/test_array_handling.py`
  - [ ] Test 1D, 2D, 3D array passing
  - [ ] Test array slicing
  - [ ] Test Fortran vs Python indexing

**Acceptance Criteria**:
- No memory leaks detected
- All type conversions validated
- State isolation confirmed

**References**:
- Use existing f90wrap interface
- See FORTRAN_TEST_PATTERNS.md for examples

---

## Issue 3: Implement Energy Balance Physics Tests

**Title**: feat(test): Add energy balance component tests with edge cases

**Labels**: `enhancement`, `testing`, `physics`, `priority:high`

**Description**:
Implement tests for all energy balance components, including the critical H=0 edge case.

**Tasks**:
- [ ] Create `test/physics/test_radiation/test_net_radiation.py`
  - [ ] Test shortwave radiation
  - [ ] Test longwave radiation
  - [ ] Test albedo effects
- [ ] Create `test/physics/test_energy_balance/test_sensible_heat.py`
  - [ ] Test QH calculations
  - [ ] **Critical**: Test H=0 stability case
  - [ ] Test extreme temperatures
- [ ] Create `test/physics/test_energy_balance/test_latent_heat.py`
  - [ ] Test QE calculations
  - [ ] Test vapor pressure deficit effects
  - [ ] Test surface resistance impact
- [ ] Create `test/physics/test_energy_balance/test_edge_cases.py`
  - [ ] Test near-zero wind speeds
  - [ ] Test extreme stability conditions
  - [ ] Test numerical boundaries

**Acceptance Criteria**:
- Energy balance closure within tolerance
- All edge cases handled gracefully
- No numerical instabilities

**References**:
- Critical for fixing QE/QH discrepancies
- Must test epsilon-based comparisons

---

## Issue 4: Implement Water Balance and Conservation Tests

**Title**: feat(test): Add water balance tests with conservation checks

**Labels**: `enhancement`, `testing`, `physics`, `priority:high`

**Description**:
Ensure water mass conservation and correct water balance calculations.

**Tasks**:
- [ ] Create `test/physics/test_water_balance/test_evaporation.py`
  - [ ] Test potential evaporation
  - [ ] Test actual evaporation
  - [ ] Test soil moisture limitations
- [ ] Create `test/physics/test_water_balance/test_runoff.py`
  - [ ] Test surface runoff
  - [ ] Test drainage
  - [ ] Test impervious surface behaviour
- [ ] Create `test/physics/test_water_balance/test_conservation.py`
  - [ ] Test mass balance closure
  - [ ] Test precipitation partitioning
  - [ ] Test storage changes

**Acceptance Criteria**:
- Water balance closes to within 0.1%
- All pathways properly tested
- Conservation laws validated

---

## Issue 5: Implement Multi-Day Time Series Tests

**Title**: feat(test): Add multi-day simulation tests with various scenarios

**Labels**: `enhancement`, `testing`, `integration`

**Description**:
Test realistic multi-day simulations as specified in requirements.

**Tasks**:
- [ ] Create `test/integration/test_temporal/test_multi_day_runs.py`
  - [ ] Test 3-day continuous runs
  - [ ] Test 7-day runs with varying conditions
  - [ ] Test runs across different seasons
  - [ ] Test wet/dry period transitions
- [ ] Create `test/integration/test_temporal/test_seasonal_variation.py`
  - [ ] Test 5 days each season
  - [ ] Test seasonal transitions
- [ ] Create `test/integration/test_temporal/test_timestep_handling.py`
  - [ ] Test 5-minute timesteps
  - [ ] Test 30-minute timesteps
  - [ ] Test 1-hour timesteps

**Acceptance Criteria**:
- All temporal scenarios complete successfully
- Results consistent across timesteps
- Memory usage remains stable

---

## Issue 6: Implement Weather Scenario Tests

**Title**: feat(test): Add comprehensive weather scenario testing

**Labels**: `enhancement`, `testing`, `integration`

**Description**:
Test model behaviour under various weather conditions.

**Tasks**:
- [ ] Create `test/integration/test_weather/test_precipitation_scenarios.py`
  - [ ] Test no rainfall periods
  - [ ] Test light rain (< 1 mm/hr)
  - [ ] Test heavy rain (> 10 mm/hr)
  - [ ] Test intermittent rainfall
- [ ] Create `test/integration/test_weather/test_extreme_conditions.py`
  - [ ] Test heatwave conditions
  - [ ] Test cold snaps
  - [ ] Test high wind events
  - [ ] Test drought periods

**Acceptance Criteria**:
- Model remains stable under all conditions
- Physical bounds respected
- Appropriate responses to extremes

---

## Issue 7: Setup Cross-Platform Testing Infrastructure

**Title**: feat(test): Implement cross-platform testing matrix

**Labels**: `enhancement`, `testing`, `infrastructure`

**Description**:
Ensure SUEWS works correctly across all supported platforms.

**Tasks**:
- [ ] Configure GitHub Actions matrix for:
  - [ ] Windows, Linux, macOS
  - [ ] Python 3.9, 3.10, 3.11, 3.12
  - [ ] Different gfortran versions
- [ ] Add platform-specific test markers
- [ ] Create platform compatibility test suite
- [ ] Document platform-specific issues

**Acceptance Criteria**:
- All platforms tested in CI
- Platform-specific issues identified
- Clear documentation of limitations

---

## Issue 8: Implement Performance Testing Framework

**Title**: feat(test): Add performance testing and regression detection

**Labels**: `enhancement`, `testing`, `performance`

**Description**:
Track performance metrics and detect regressions.

**Tasks**:
- [ ] Create `test/performance/test_execution_time/`
  - [ ] Test single grid performance
  - [ ] Test scaling with grid size
  - [ ] Test timestep impact
- [ ] Create `test/performance/test_scaling/`
  - [ ] Test 1, 10, 100, 1000 grid points
  - [ ] Verify linear scaling where expected
- [ ] Implement performance tracking
  - [ ] Store baseline metrics
  - [ ] Detect regressions
  - [ ] Generate reports

**Acceptance Criteria**:
- Performance baselines established
- Regression detection working
- Reports generated automatically

---

## Issue 9: Setup Test Coverage Infrastructure

**Title**: feat(test): Implement test coverage tracking and reporting

**Labels**: `enhancement`, `testing`, `infrastructure`

**Description**:
Setup comprehensive coverage tracking to reach 80% target.

**Tasks**:
- [ ] Configure pytest-cov
- [ ] Setup coverage.py configuration
- [ ] Integrate codecov.io
- [ ] Add coverage badges to README
- [ ] Configure PR coverage checks
- [ ] Document coverage goals

**Acceptance Criteria**:
- Coverage tracked on every PR
- Reports easily accessible
- 80% target clearly tracked

---

## Issue 10: Create CI Test Tier Configuration

**Title**: feat(ci): Implement tiered testing strategy in GitHub Actions

**Labels**: `enhancement`, `ci/cd`, `infrastructure`

**Description**:
Setup the 4-tier testing strategy for optimal CI performance.

**Tasks**:
- [ ] Configure Tier 1 (PR tests, < 5 min)
- [ ] Configure Tier 2 (Merge tests, < 15 min)
- [ ] Configure Tier 3 (Nightly comprehensive, < 2 hours)
- [ ] Configure Tier 4 (Weekly platform matrix, < 4 hours)
- [ ] Add appropriate test markers
- [ ] Configure parallel execution

**Acceptance Criteria**:
- PR feedback within 5 minutes
- Comprehensive validation without blocking development
- Clear tier assignment for all tests

---

## Issue 11: Implement Error Handling Framework

**Title**: feat(error): Implement user-friendly error handling patterns

**Labels**: `enhancement`, `error-handling`, `ux`

**Description**:
Create dual-layer error handling for both debugging and user experience.

**Tasks**:
- [ ] Create base error classes with user/debug layers
- [ ] Implement error context system
- [ ] Add fix hints to all error types
- [ ] Create error formatting utilities
- [ ] Add error handling tests
- [ ] Document error patterns

**Acceptance Criteria**:
- All errors have user-friendly messages
- Debug information available when needed
- Fix hints provided where possible

---

## Priority Order

1. **High Priority - Foundation**: Issues #1, #2, #9, #10
   - Core configuration, Fortran interface, coverage tracking, CI setup
2. **High Priority - Core Functionality**: Issues #3, #4, #11
   - Energy balance, water balance, error handling
3. **Medium Priority - Integration**: Issues #5, #6
   - Multi-day simulations, weather scenarios
4. **Medium Priority - Robustness**: Issues #7, #8
   - Cross-platform support, performance testing

## Notes for Review

- These issues can be adjusted based on existing GitHub issues
- Some may be merged with existing work
- Priority labels can be adjusted based on current needs
- Dependencies between issues are minimised for parallel work
- Foundation issues should be completed before moving to dependent work