# Tests to Run for Validation Pipeline Changes

After making changes to the validation pipeline (phase naming, removing redundant dictionaries), run these key tests to ensure everything still works:

## Setup Test Environment

```bash
# If you don't have a venv with pytest
uv venv
source .venv/bin/activate
make dev  # This installs test dependencies
```

## Key Test Files

### 1. Main Validation Pipeline Tests
```bash
# Full test suite for YAML processing (includes Phase A, B, C tests)
pytest test/data_model/test_yaml_processing.py -v

# This is the comprehensive test file (4483 lines) that tests:
# - TestPhaseAUptoDateYaml: Phase A parameter detection tests
# - TestPhaseBScienceCheck: Phase B physics validation tests
# - TestPhaseCPydanticValidation: Phase C consistency tests
# - TestSuewsYamlProcessorOrchestrator: Multi-phase workflow tests
```

### 2. Specific Test Classes to Focus On

```bash
# Phase A (Configuration structure check) tests
pytest test/data_model/test_yaml_processing.py::TestPhaseAUptoDateYaml -v

# Phase B (Physics validation check) tests
pytest test/data_model/test_yaml_processing.py::TestPhaseBScienceCheck -v

# Phase C (Configuration consistency check) tests
pytest test/data_model/test_yaml_processing.py::TestPhaseCPydanticValidation -v
pytest test/data_model/test_yaml_processing.py::TestPhaseCReporting -v

# Orchestrator (multi-phase workflows) tests
pytest test/data_model/test_yaml_processing.py::TestSuewsYamlProcessorOrchestrator -v

# End-to-end workflow tests
pytest test/data_model/test_yaml_processing.py::TestEndToEndWorkflow -v
```

### 3. Quick Smoke Tests

If you want to quickly verify the changes work:

```bash
# Test basic validation functionality
pytest test/data_model/test_validation.py -v

# Test report generation
pytest test/data_model/test_yaml_processing.py::TestPhaseCReporting::test_phase_c_report_format -v

# Test consolidated report generation
pytest test/data_model/test_yaml_processing.py::TestSuewsYamlProcessorOrchestrator::test_consolidated_report_generation -v
```

## What Our Changes Affected

1. **Terminal Output Messages**: Changed from "Phase A: Up-to-date YAML check..." to "Configuration structure check..."
2. **Report Titles**: All phases now use "SUEWS Validation Report"
3. **Removed Redundant Dictionaries**: Simplified code in:
   - `orchestrator.py`
   - `phase_a_parameter_update.py`
   - `phase_b_science_check.py`
   - `phase_c_pydantic_report.py`

## Expected Test Results

The tests should all pass. If any fail, check for:
- Tests that expect specific phase names in output
- Tests that check for exact report titles
- Tests that verify console messages

Most tests should be unaffected as we only changed user-facing strings, not the core logic or internal variable names.