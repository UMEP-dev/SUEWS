# Feature: Fix Water Balance Test Failure on macOS ARM64

## Context
Fix the failing water balance test `test_water_balance_closed` in `test_supy.py` that's returning NaN values instead of numeric differences on macOS ARM64 Python 3.11. This test is critical for validating SUEWS water balance closure and is currently blocking CI builds.

## GitHub Issues
- #508 - Water balance test failure on macOS ARM64 Python 3.11: NaN values in balance calculation (PRIMARY)

## Progress Tracking
- [x] Create worktree and development environment
- [ ] Investigate the failing test locally
- [ ] Identify root cause of NaN values and object dtype
- [ ] Implement fix for data type consistency
- [ ] Verify fix works on macOS ARM64
- [ ] Run full test suite to ensure no regressions
- [ ] Document the fix and root cause

## Key Decisions
- **Approach**: Focus on data type consistency in water balance calculation
- **Testing**: Reproduce the issue locally first, then verify fix
- **Scope**: Fix the specific test failure without changing core physics

## Implementation Notes
- Test failure occurs in `test_supy.py:594` in water balance calculation
- Issue: `max_diff` becomes `nan` due to `object` dtype in difference series
- Expected: Numeric difference < 1e-6 tolerance
- The problem appears to be in pandas arithmetic operations returning object dtype instead of float

## Files to Modify
- `test/test_supy.py` - Fix water balance test calculation
- Possibly related files if data type issue is in core model output

## Investigation Areas
1. **Data Types**: Check dtypes of `ser_totalstore_change` and `ser_water_balance`
2. **Pandas Operations**: Verify arithmetic operations preserve numeric types
3. **Model Output**: Ensure SUEWS output has consistent numeric types
4. **Platform Differences**: Test if issue is ARM64-specific or general

## Environment Setup
- **Worktree**: `worktrees/fix-water-balance-test-macos-arm64`
- **Branch**: `feature/fix-water-balance-test-macos-arm64`
- **Python**: Virtual environment with uv
- **Dependencies**: pandas, scipy, matplotlib, numpy, pytest, etc.

## Testing Strategy
- Run the specific failing test in isolation
- Add debug prints to understand data types
- Test on local macOS ARM64 environment
- Verify fix with full test suite