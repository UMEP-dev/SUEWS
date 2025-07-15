# Feature: Fortran Robustness - OHM Numerical Stability

## Lead Developer
- **GitHub**: @sunt05
- **Started**: 2025-07-13

## Context
The pragmatic testing framework identified systematic numerical divergence in SUEWS OHM (Objective Hysteresis Model) calculations affecting specific Python/NumPy version combinations. The root cause was floating-point precision differences near decision thresholds (particularly 10°C), causing different platforms to select different OHM coefficients and leading to storage heat flux differences up to 55.6 W/m².

## GitHub Issues
- #473 - Numerical divergence in OHM calculations with specific Python/NumPy versions (PRIMARY)
- #468 - Pragmatic testing framework (identified the issue)
- #471 - Initial tolerance-based fix attempt

## Status
- **Current**: doing
- **Outcome**: pending
- **Completed**: [pending]
- **PR**: [pending]

## Progress Tracking

### Phase 1: Smooth Transition Implementation
- [x] Analyse root cause of numerical divergence
- [x] Design smooth transition approach using tanh functions
- [x] Implement smooth transitions for temperature thresholds (2K width)
- [x] Implement smooth transitions for soil moisture thresholds (0.1 width)
- [x] Create unit tests for transition verification
- [x] Verify platform consistency across Windows/Linux/macOS

### Phase 2: Validation and Testing
- [ ] Run full benchmark tests on all affected platforms
- [ ] Verify energy balance closure is maintained
- [ ] Compare results with original step function approach
- [ ] Test edge cases near transition regions
- [ ] Document performance impact (if any)

### Phase 3: Integration and Documentation
- [ ] Update model documentation with smooth transition details
- [ ] Add technical notes on numerical stability approach
- [ ] Create PR with comprehensive test results
- [ ] Review with team for physics implications

## Key Decisions
- **Smooth Transitions**: Replaced hard threshold comparisons with tanh-based smooth transitions to eliminate platform-dependent behaviour from floating-point precision
- **Transition Widths**: Selected 2K for temperature and 0.1 (10%) for soil moisture based on physical significance and sensor precision
- **Backward Compatibility**: Ensured smooth transitions produce similar results to step functions outside transition regions

## Implementation Notes
- The smooth transition approach eliminates the root cause of divergence by making the model continuous across thresholds
- Tanh function provides a mathematically smooth transition that's differentiable
- Transition widths chosen to be physically meaningful while maintaining model behaviour
- Performance impact expected to be minimal (tanh calculations are efficient)
- This approach is more robust than tolerance-based comparisons as it fundamentally removes discontinuities

## Files to Modify
- `src/suews/src/suews_phys_ohm.f95` - Implemented smooth transitions for OHM coefficient selection
- `test/test_ohm_robustness.py` - (created) Unit tests for smooth transition verification
- `docs/source/physics/ohm.rst` - Document smooth transition approach
- `test/test_supy.py` - May need tolerance adjustments for benchmark tests

## Testing Strategy
- Unit tests verify smooth transitions behave correctly near thresholds
- Platform consistency tests ensure identical results across OS/Python versions
- Benchmark tests validate overall model performance is maintained
- Physics validation confirms energy balance closure
- Edge case testing for extreme temperature/moisture conditions

## Documentation Updates
- OHM physics documentation to explain smooth transition approach
- Technical notes on numerical stability improvements
- Update model equations to show tanh-based transitions
- Add guidance for future threshold implementations

## Abandonment Notes
(Not applicable - feature is actively being developed)