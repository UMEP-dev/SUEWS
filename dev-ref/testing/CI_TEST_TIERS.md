# CI Test Tiers for SUEWS

This document defines the test execution tiers for continuous integration, balancing fast feedback on pull requests with comprehensive validation in nightly runs.

## Test Tier Overview

### Tier 1: PR Tests (Essential Correctness)
**Trigger**: Every pull request and push
**Time Budget**: < 5 minutes
**Purpose**: Fast feedback on code correctness

### Tier 2: Merge Tests (Extended Validation)
**Trigger**: On merge to master
**Time Budget**: < 15 minutes
**Purpose**: Validate integration before accepting changes

### Tier 3: Nightly Tests (Comprehensive)
**Trigger**: Daily at midnight UTC
**Time Budget**: < 2 hours
**Purpose**: Full validation including expensive tests

### Tier 4: Weekly Tests (Platform Matrix)
**Trigger**: Weekly on Sundays
**Time Budget**: < 4 hours
**Purpose**: Cross-platform and compatibility testing

## Tier 1: PR Tests (Essential Correctness)

### Execution Strategy
- Run on every commit in PR
- Fail fast on first error
- Parallel execution where possible
- Skip on `[skip ci]` in commit message

### Test Selection Criteria
Tests included in PR runs must be:
- **Fast**: < 10 seconds per test
- **Focused**: Test one specific functionality
- **Stable**: No flaky or intermittent failures
- **Critical**: Block merging if they fail

### Included Tests

#### Configuration Validation
```yaml
test/unit/test_data_model/test_config_validation.py::test_required_fields
test/unit/test_data_model/test_config_validation.py::test_invalid_values
test/unit/test_data_model/test_yaml_loading.py::test_valid_minimal_config
```

#### Core Interface Tests
```yaml
test/unit/test_fortran_interface/test_type_conversions.py::test_numeric_types
test/unit/test_fortran_interface/test_array_handling.py::test_dimension_consistency
```

#### Critical Physics
```yaml
test/physics/test_energy_balance/test_conservation.py::test_energy_closure
test/physics/test_water_balance/test_conservation.py::test_mass_balance
test/physics/test_atmospheric_stability/test_edge_cases.py::test_zero_heat_flux
```

#### Smoke Tests
```yaml
test/integration/test_minimal_run.py::test_single_timestep
test/integration/test_minimal_run.py::test_single_day
```

### GitHub Actions Configuration
```yaml
name: PR Tests
on:
  pull_request:
    types: [opened, synchronize, reopened]
  push:
    branches: [master]

jobs:
  essential-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'
      - name: Install dependencies
        run: |
          pip install -e ".[test]"
      - name: Run essential tests
        run: |
          pytest test -v \
            -k "essential or critical or smoke" \
            --tb=short \
            --maxfail=1
```

## Tier 2: Merge Tests (Extended Validation)

### Additional Tests Beyond Tier 1

#### Extended Physics Validation
```yaml
test/physics/test_radiation/test_net_radiation.py
test/physics/test_evaporation/test_penman_monteith.py
test/physics/test_runoff/test_surface_runoff.py
```

#### Integration Scenarios
```yaml
test/integration/test_temporal/test_three_day_run.py
test/integration/test_weather/test_rainfall_event.py
```

#### Error Handling
```yaml
test/unit/test_error_handling/test_invalid_inputs.py
test/unit/test_error_handling/test_missing_data.py
```

### Configuration
```yaml
name: Merge Tests
on:
  push:
    branches: [master]

jobs:
  extended-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 20
    steps:
      - uses: actions/checkout@v4
      - name: Run extended test suite
        run: |
          pytest test -v \
            --cov=supy \
            --cov-report=xml \
            --ignore=test/performance \
            --ignore=test/multi_day
```

## Tier 3: Nightly Tests (Comprehensive)

### Full Test Suite Components

#### Multi-Day Simulations
```yaml
test/integration/test_temporal/test_weekly_run.py
test/integration/test_temporal/test_seasonal_transitions.py
test/integration/test_temporal/test_year_long_run.py
```

#### Weather Scenario Matrix
```yaml
test/integration/test_weather/test_extreme_heat.py
test/integration/test_weather/test_drought_conditions.py
test/integration/test_weather/test_storm_events.py
```

#### Performance Benchmarks
```yaml
test/performance/test_execution_time/test_single_grid_performance.py
test/performance/test_scaling/test_multi_grid_scaling.py
test/performance/test_memory/test_memory_usage.py
```

#### Numerical Stability
```yaml
test/physics/test_numerical_stability/test_long_runs.py
test/physics/test_numerical_stability/test_extreme_conditions.py
```

### Nightly Workflow
```yaml
name: Nightly Comprehensive Tests
on:
  schedule:
    - cron: '0 0 * * *'  # Midnight UTC
  workflow_dispatch:  # Manual trigger

jobs:
  comprehensive-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 120
    steps:
      - uses: actions/checkout@v4
      - name: Run full test suite
        run: |
          pytest test -v \
            --cov=supy \
            --cov-report=html \
            --cov-report=xml
      - name: Performance regression check
        run: |
          python scripts/check_performance_regression.py
      - name: Upload coverage
        uses: codecov/codecov-action@v3
```

## Tier 4: Weekly Tests (Platform Matrix)

### Platform Coverage

#### Operating Systems
- Ubuntu 20.04, 22.04, 24.04
- Windows Server 2019, 2022
- macOS 12, 13, 14

#### Python Versions
- Python 3.9, 3.10, 3.11, 3.12
- Python 3.13 (experimental)

#### Compiler Versions
- gfortran 9, 10, 11, 12
- Intel Fortran (if available)

### Weekly Workflow
```yaml
name: Weekly Platform Tests
on:
  schedule:
    - cron: '0 0 * * 0'  # Sunday midnight UTC
  workflow_dispatch:

jobs:
  platform-matrix:
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-20.04, ubuntu-22.04, windows-2022, macos-13]
        python: ['3.9', '3.10', '3.11', '3.12']
        exclude:
          - os: macos-13
            python: '3.9'  # Not supported
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - name: Run platform tests
        run: |
          pytest test -v -k "not performance"
```

## Test Selection Markers

### pytest Markers
```python
# pyproject.toml
[tool.pytest.ini_options]
markers = [
    "essential: Core tests that must pass (Tier 1)",
    "critical: Critical functionality tests (Tier 1)",
    "smoke: Basic smoke tests (Tier 1)",
    "extended: Extended validation tests (Tier 2)",
    "integration: Integration tests (Tier 2+)",
    "slow: Tests that take > 10 seconds (Tier 3)",
    "performance: Performance benchmarks (Tier 3)",
    "platform: Platform-specific tests (Tier 4)",
    "experimental: Experimental features (optional)",
]
```

### Test Marking Examples
```python
@pytest.mark.essential
def test_energy_conservation():
    """Must pass on every PR."""
    pass

@pytest.mark.slow
@pytest.mark.performance
def test_annual_simulation():
    """Only runs in nightly tests."""
    pass

@pytest.mark.platform
@pytest.mark.skipif(sys.platform != 'win32', reason="Windows only")
def test_windows_path_handling():
    """Platform-specific test."""
    pass
```

## Performance Tracking

### Metrics Collection
```python
# test/performance/conftest.py
@pytest.fixture
def performance_tracker():
    """Track test execution metrics."""
    start_time = time.perf_counter()
    start_memory = get_memory_usage()
    
    yield
    
    duration = time.perf_counter() - start_time
    memory_delta = get_memory_usage() - start_memory
    
    # Store metrics for regression detection
    store_performance_metrics(
        test_name=request.node.name,
        duration=duration,
        memory_delta=memory_delta
    )
```

### Regression Detection
```yaml
# .github/performance-thresholds.yml
thresholds:
  test_single_day_simulation:
    max_duration: 5.0  # seconds
    max_memory: 100    # MB
  test_multi_grid_scaling:
    scaling_factor: 1.1  # Max 10% worse than linear
```

## CI Time Optimization

### Parallel Execution
```yaml
# Split tests across multiple workers
pytest-xdist:
  workers: auto  # Use all available cores
  groups:
    - physics: test/physics
    - integration: test/integration
    - unit: test/unit
```

### Caching Strategy
```yaml
cache:
  - key: pip-${{ runner.os }}-${{ hashFiles('**/requirements*.txt') }}
  - key: test-data-${{ hashFiles('test/data/**') }}
  - key: fortran-build-${{ hashFiles('src/suews/**/*.f90') }}
```

### Early Termination
```yaml
fail-fast: true  # Stop on first failure in PR tests
max-parallel: 4   # Limit concurrent jobs
timeout-minutes:  # Enforce time limits
  pr: 10
  merge: 20
  nightly: 120
  weekly: 240
```

## Test Result Reporting

### PR Comments
```yaml
- name: Comment test results
  uses: actions/github-script@v6
  if: always()
  with:
    script: |
      const summary = `
      ## Test Results
      - Essential Tests: ${{ steps.essential.outcome }}
      - Coverage: ${{ steps.coverage.outputs.percentage }}%
      - Duration: ${{ steps.duration.outputs.time }}
      `;
      github.issues.createComment({
        issue_number: context.issue.number,
        body: summary
      });
```

### Failure Notifications
```yaml
- name: Notify on nightly failure
  if: failure() && github.event_name == 'schedule'
  uses: actions/github-script@v6
  with:
    script: |
      github.issues.create({
        title: 'Nightly tests failed',
        body: 'See ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}',
        labels: ['test-failure', 'nightly']
      });
```

## Maintenance and Updates

### Monthly Review
1. Analyze test execution times
2. Review flaky test reports
3. Update tier assignments based on:
   - Test stability
   - Execution time changes
   - Criticality reassessment

### Metrics to Track
- Average PR test duration
- Nightly test success rate
- Platform-specific failure patterns
- Performance regression frequency

### Tier Promotion/Demotion Criteria
- **Promote to Tier 1**: Stable, fast, catches real issues
- **Demote from Tier 1**: Slow (>10s), flaky, or low value
- **Add to Tier 3**: New expensive validation tests
- **Remove entirely**: Redundant or obsolete tests

---

**Key Principle**: Fast feedback on PRs while ensuring comprehensive validation through tiered testing. Essential correctness tests block merging, while expensive tests run asynchronously to catch edge cases and regressions.