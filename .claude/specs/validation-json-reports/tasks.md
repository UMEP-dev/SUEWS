# Tasks: Validation JSON Reports

## Implementation Tasks

### Phase 1: Core Infrastructure (Day 1-2)

#### 1.1 Create ValidationReporter Class [3h]
- [ ] Create `validation_reporter.py` in pipeline directory
- [ ] Define core JSON data structure for validation results
- [ ] Implement methods: add_error, add_warning, add_info
- [ ] Implement merge method for combining reporters
- **Requirement**: JSON as single source of truth for validation data

#### 1.2 Create TextReportGenerator [2h]
- [ ] Create `text_report_generator.py` in pipeline directory
- [ ] Implement generate method to create text from JSON
- [ ] Maintain exact format of current text reports
- [ ] Support all phase combinations (A, B, C, AB, AC, BC, ABC)
- **Requirement**: Text reports maintain exact same format

#### 1.3 Define JSON Schema [1h]
- [ ] Document JSON structure in schema file
- [ ] Include all fields from current reports
- [ ] Add schema version for future compatibility
- **Requirement**: Comprehensive JSON structure

### Phase 2: Phase Refactoring (Day 2-3)

#### 2.1 Refactor Phase A [3h]
- [ ] Modify `phase_a_parameter_update.py` to use ValidationReporter
- [ ] Build JSON structure during validation
- [ ] Generate text report from JSON using TextReportGenerator
- [ ] Save both JSON and text reports
- [ ] Return ValidationReporter instance
- **Requirement**: Phase A uses JSON as core data structure

#### 2.2 Refactor Phase B [3h]
- [ ] Modify `phase_b_science_check.py` to use ValidationReporter
- [ ] Build JSON structure during science checks
- [ ] Generate text report from JSON
- [ ] Save both JSON and text reports
- [ ] Return ValidationReporter instance
- **Requirement**: Phase B uses JSON as core data structure

#### 2.3 Refactor Phase C [2h]
- [ ] Modify Phase C functions to use ValidationReporter
- [ ] Build JSON structure from Pydantic validation errors
- [ ] Generate text report from JSON
- [ ] Save both JSON and text reports
- [ ] Return ValidationReporter instance
- **Requirement**: Phase C uses JSON as core data structure

### Phase 3: Orchestrator Integration (Day 3-4)

#### 3.1 Update Orchestrator [3h]
- [ ] Modify orchestrator to work with ValidationReporter instances
- [ ] Implement reporter aggregation for multi-phase workflows
- [ ] Generate consolidated reports for combined phases (AB, AC, BC, ABC)
- [ ] Maintain existing file naming conventions
- [ ] Clean up intermediate files as before
- **Requirement**: Orchestrator aggregates JSON from all phases

#### 3.2 CLI Wizard Integration [2h]
- [ ] Create JSON report loader in CLI wizard
- [ ] Parse validation results structure
- [ ] Display errors with structured formatting
- [ ] Use JSON data for guided workflow
- **Requirement**: CLI wizard can parse and use reports

#### 3.3 Error Context Enhancement [2h]
- [ ] Add YAML line/column tracking where possible
- [ ] Include fix suggestions in JSON structure
- [ ] Add documentation URLs for parameters
- [ ] Provide value ranges and examples
- **Requirement**: Rich error context for user guidance

### Phase 4: Testing & Documentation (Day 4-5)

#### 4.1 Unit Tests [2h]
- [ ] Test ValidationReporter class
- [ ] Test text report parsing
- [ ] Test JSON generation
- [ ] Test backward compatibility
- **Requirement**: Modular and testable code

#### 4.2 Integration Tests [2h]
- [ ] Test full pipeline with JSON reporter
- [ ] Test with all phase combinations (A, B, C, AB, AC, BC, ABC)
- [ ] Verify JSON output structure
- [ ] Test CLI wizard consumption
- **Requirement**: All existing tests pass

#### 4.3 Documentation [1h]
- [ ] Document JSON schema
- [ ] Add usage examples to README
- [ ] Update CLI wizard documentation
- [ ] Add sample JSON reports
- **Requirement**: Documentation with examples

## Estimated Total: ~28 hours (5-6 days)

## Dependencies
- Existing validation pipeline must be stable
- CLI wizard structure must be finalised
- Text report format must be preserved exactly

## Risks & Mitigations
1. **Risk**: Breaking text report format compatibility
   - **Mitigation**: Careful testing of text generation, comparison with existing reports

2. **Risk**: Performance overhead from JSON structure
   - **Mitigation**: Efficient data structures, minimal memory footprint

3. **Risk**: Complex refactoring of existing phases
   - **Mitigation**: Incremental refactoring, maintain backward compatibility

## Architecture Benefits
1. **Single Source of Truth**: JSON structure contains all validation data
2. **Maintainability**: Adding new fields only requires JSON structure updates
3. **Extensibility**: Easy to add new output formats (HTML, Markdown, etc.)
4. **Reliability**: Text reports always consistent with JSON data
5. **Testability**: Can test JSON structure independently of text formatting

## Success Metrics
- [ ] JSON reports generated successfully
- [ ] CLI wizard consumes reports effectively
- [ ] Error resolution time reduced by 50%
- [ ] User satisfaction with error messages improved
- [ ] No regression in validation performance