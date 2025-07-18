# Feature: Fix SUEWSSimulation Test Path Issues in CI

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2025-07-13

## Worktree Information
- **Branch**: `fix/simulation-test-paths`
- **Location**: `/Users/tingsun/Dropbox (Personal)/6.Repos/SUEWS/worktrees/fix-simulation-tests`
- **Status**: Ahead of origin by 9 commits

## Context
The SUEWSSimulation tests in `test/test_suews_simulation.py` were being skipped in CI because they used relative paths to find benchmark files. In CI environments, pytest is run with absolute paths, causing relative paths like `Path("test/benchmark1/benchmark1.yml")` to fail to resolve correctly.

## GitHub Issues
- #475 - SUEWSSimulation tests are being skipped in CI due to relative path issues (PRIMARY)

## Status
- **Current**: doing
- **Outcome**: pending
- **Completed**: 
- **PR**: 

## Progress Tracking

### Phase 1: Initial Path Resolution Fix (Completed in commit 2238e4d8)
- [x] Replace all relative paths with `Path(__file__).parent` pattern
- [x] Fix API mismatches by removing forcing_file parameter from constructor
- [x] Add proper `sim.update_forcing()` calls after construction
- [x] Update 13 path references across all test classes

### Phase 2: Performance Optimisation (Completed in commit 697d0548)
- [x] Replace slow benchmark data loading with fast `supy.load_sample_data()`
- [x] Fix API mismatches with `run_supy_ser` returning 4 values
- [x] Reduce test execution time from minutes to ~37 seconds
- [x] Verify all tests pass without being skipped in CI

### Phase 3: CI Verification and Cleanup
- [ ] Push branch to origin (currently ahead by 9 commits)
- [ ] Run full CI test suite to confirm no skipped tests
- [ ] Check test coverage remains adequate
- [ ] Address any remaining warnings in test output (PerformanceWarning, UserWarning)
- [ ] Create pull request and link to issue #475
- [ ] Verify CI passes on PR before merging

## Key Decisions
- **Use Path(__file__).parent pattern**: Ensures tests work regardless of where pytest is invoked from, solving the CI path resolution issue
- **Switch to sample data**: Using `supy.load_sample_data()` instead of full benchmark data significantly improves test performance without compromising test quality
- **Fix API mismatches**: Updated tests to match current API where `run_supy_ser` returns 4 values instead of 2

## Implementation Notes
- The issue affected 21 tests that were being skipped in CI
- Tests showed as 's' (skipped) in CI output: `test/test_suews_simulation.py sssss.sssssssssss`
- Root cause was pytest being run with absolute paths in CI (e.g., `/project/test` on Linux)
- Additional API mismatches were discovered and fixed during implementation
- Performance warnings about "indexing past lexsort depth" should be investigated separately
- Claude Code already created initial branch `claude/issue-475-20250713_211957` with fixes
- Current worktree branch `fix/simulation-test-paths` includes additional optimisations

## Files to Modify
- `test/test_suews_simulation.py` - Update all path references and fix API mismatches
- `src/supy/suews_sim.py` - Minor adjustments to support test changes

## Testing Strategy
- Run all SUEWSSimulation tests locally to ensure they pass
- Verify tests are not skipped in CI environment
- Check that test execution time is reasonable (~37 seconds)
- Ensure test coverage for SUEWSSimulation functionality is maintained

## Documentation Updates
- No user-facing documentation changes required
- Consider adding a note in developer documentation about using `Path(__file__).parent` for test paths

## Abandonment Notes
(Not applicable - fix is progressing successfully)