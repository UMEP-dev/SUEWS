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

### Available Modules via f90wrap
```python
from supy_driver import (
    Atmmoiststab_Module,     # Atmospheric stability
    Evap_Module,             # Evaporation calculations
    Resist_Module,           # Resistance calculations
    Snow_Module,             # Snow processes
    Waterdist_Module,        # Water distribution
    Ohm_Module,              # Objective Hysteresis Model
    Estm_Module,             # ESTM calculations
    Suews_Def_Dts,          # Derived type definitions
)
```

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

### Standard Markers
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
]
```

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
- **Tier 1 (PR)**: < 5 minutes, essential correctness
- **Tier 2 (Merge)**: < 15 minutes, extended validation
- **Tier 3 (Nightly)**: < 2 hours, comprehensive
- **Tier 4 (Weekly)**: < 4 hours, platform matrix

### Test Selection Criteria for PR
- Fast (< 10 seconds per test)
- Focused (test one functionality)
- Stable (no flaky failures)
- Critical (block merging if fail)

---

## For AI Assistants (Claude Code)

When writing tests for SUEWS:
1. Follow the patterns documented above
2. Use existing f90wrap interface - don't create new Fortran test interfaces
3. Keep tests AI-friendly and human-manageable
4. Always use British English in test documentation
5. Prioritise clarity over cleverness
6. Include physics references where applicable
7. Ensure tests are deterministic and reproducible

## For Human Developers

1. Write tests alongside code development
2. Use the test patterns as templates
3. Document why a test exists, not just what it does
4. Keep individual tests focused and fast
5. Use meaningful test names that describe the scenario
6. Ensure tests work across all supported platforms
7. Review coverage reports to identify gaps