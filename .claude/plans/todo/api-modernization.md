# Plan: API Modernization & Migration

**Status**: TODO  
**Scope**: Deprecate table-based loading, promote SUEWSSimulation class  
**Languages**: Python, potentially Fortran interfaces  
**Duration**: 4-6 weeks  
**Priority**: High  

## Background
The SUEWS/SuPy project currently supports both legacy table-based configuration and modern YAML-based configuration through the SUEWSSimulation class. We need to deprecate the legacy approach while ensuring a smooth transition for existing users.

## Objectives
- Deprecate `init_supy()` and related table-based functions
- Make SUEWSSimulation the primary user interface
- Provide smooth migration path for existing users
- Maintain backward compatibility during transition period

## Key Deliverables

### 1. Deprecation Implementation
- Add deprecation warnings to legacy functions
  - `init_supy()`
  - `load_forcing_grid()`
  - Table-based configuration loading
- Create compatibility layer for gradual transition
- Document deprecation timeline (2 major versions)

### 2. API Enhancement
- Enhance SUEWSSimulation class with any missing features from legacy API
- Create factory methods for common use cases
  - `SUEWSSimulation.from_sample_data()`
  - `SUEWSSimulation.from_legacy_tables()`
- Implement improved validation and error handling
- Add convenience methods for common workflows

### 3. Migration Tools
- Table-to-YAML converter script
  - Automated conversion of .nml files to .yml
  - Validation of converted configurations
- Migration guide with detailed examples
- Automated migration testing suite
- Compatibility checker tool

## Implementation Steps

### Week 1-2: Add Deprecation Warnings
- [ ] Identify all legacy functions to deprecate
- [ ] Add deprecation decorators with clear messages
- [ ] Create compatibility layer to maintain functionality
- [ ] Update tests to suppress deprecation warnings

### Week 3-4: Enhance SUEWSSimulation Class
- [ ] Audit missing features from legacy API
- [ ] Implement factory methods
- [ ] Add validation improvements
- [ ] Create comprehensive examples

### Week 5-6: Create Migration Tools
- [ ] Develop table-to-YAML converter
- [ ] Write migration guide
- [ ] Create validation tools
- [ ] Set up migration testing

## Technical Considerations
- Ensure all deprecation warnings include migration instructions
- Maintain full backward compatibility
- Consider performance implications
- Update all documentation and examples

## Success Metrics
- Zero breaking changes for existing users
- 100% feature parity between APIs
- Clear migration path documented
- All examples updated to use new API
- Positive user feedback on migration process

## Dependencies
- Current test suite must pass
- Documentation infrastructure ready
- Team consensus on deprecation timeline

## Risks and Mitigation
- **Risk**: User resistance to change
  - **Mitigation**: Extensive communication, clear benefits documentation
- **Risk**: Hidden dependencies on legacy code
  - **Mitigation**: Comprehensive testing, gradual rollout
- **Risk**: Performance regression
  - **Mitigation**: Benchmarking, optimization as needed

## Open Questions
- [ ] Exact deprecation timeline?
- [ ] Should we maintain a legacy branch?
- [ ] How to handle third-party tools using old API?

## Notes for Review
<!-- Please add your comments below -->