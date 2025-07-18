# Feature: Pragmatic Robustness Testing

## Context
Implement comprehensive robustness testing for SUEWS focusing on scientific validity rather than bit-for-bit determinism. This pragmatic approach ensures model outputs are physically reasonable and scientifically consistent across platforms within appropriate tolerances.

## GitHub Issues
- None directly related (new feature)

## Progress Tracking

### Phase 1: Build Infrastructure
- [x] Create deterministic math module with Kahan summation
- [x] Add deterministic build flags to meson.build
- [x] Add deterministic build flags to Makefile.gfortran
- [x] Set up dedicated worktree with uv environment
- [x] Test basic compilation with deterministic flags

### Phase 2: Code Modifications
- [ ] Replace SUM() with det_sum() in critical modules
- [ ] Standardize numerical tolerances
- [ ] Replace transcendental functions with portable versions
- [ ] Fix random number seeding
- [ ] Handle big-endian conversion consistently

### Phase 3: Testing Infrastructure
- [x] Create cross-platform test cases
- [x] Implement numerical fingerprinting
- [ ] Add CI matrix for Linux/macOS/Windows
- [ ] Create Docker test environment
- [ ] Document performance impact

### Phase 4: Integration
- [ ] Update cibuildwheel configuration
- [ ] Add deterministic mode to documentation
- [ ] Create user guide for reproducible runs
- [ ] Add performance benchmarks

## Key Decisions
- Use Kahan summation for all critical accumulations
- Standardize on -O2 optimization for deterministic mode
- Use statistical fingerprinting instead of bit-for-bit comparison
- Make deterministic mode optional to preserve performance

## Implementation Notes
- Deterministic module created at `src/suews/suews_util_deterministic.f95`
- Compiler flags disable FP contraction and fast math
- Need to handle big-endian conversion flag impact
- Windows ARM testing deferred to later phase

## Files to Modify
- `src/suews/suews_util_deterministic.f95` (created)
- `meson_options.txt` (modified)
- `meson.build` (modified) 
- `src/supy_driver/meson.build` (modified)
- `src/suews/Makefile` (modified)
- `src/suews/Makefile.gfortran` (modified)
- Physics modules for SUM replacement (pending)
- Test suite additions (pending)

## Current Status

### Completed Work

1. **Infrastructure**: Successfully set up deterministic build mode with compiler flags
2. **Testing**: Created comprehensive robustness testing framework based on pragmatic approach
3. **Documentation**: Documented the scientific robustness approach

### Key Decision: Pragmatic Robustness Over Determinism

After analysis, pursuing bit-for-bit determinism is over-engineering for SUEWS. Instead, implemented:

1. **Tolerance-based testing** with platform-specific configurations
2. **Physical bounds validation** for all outputs  
3. **Energy balance closure tests** with appropriate tolerances
4. **Numerical stability tests** under extreme conditions
5. **Statistical validation** of model behavior

The 0.8% tolerance currently used is scientifically appropriate given measurement and model uncertainties.

### Files Created

**Robustness Testing**:
- `test/test_robustness.py` - Core robustness tests
- `test/test_enhanced_validation.py` - Platform-aware validation
- `test/test_tolerances.yml` - Platform-specific tolerance configuration
- `test/tolerance_utils.py` - Helper utilities
- `docs/robustness_approach.md` - Documentation of approach

**Deterministic Build** (for future use if needed):
- `src/suews/suews_util_deterministic.f95` - Kahan summation module
- Build configurations in meson and Makefile