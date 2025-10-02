# SUEWS YAML Processor Orchestrator

## Overview

The orchestrator module (`orchestrator.py`) coordinates the three-phase validation system for SUEWS YAML configuration files. It manages workflow execution, file handling, and report generation across all validation phases.

## Architecture

### Core Components

- **Phase Execution**: Manages individual phase execution (A, B, C)
- **Workflow Coordination**: Handles combined workflows (AB, AC, BC, ABC)
- **File Management**: Controls intermediate and final file generation
- **Report Consolidation**: Merges reports from multiple phases
- **Error Recovery**: Ensures partial results are preserved on failure

### Key Functions

```python
def run_yaml_processor(
    user_file: str,
    standard_file: str = None,
    phase: str = "ABC",
    mode: str = "public"
) -> Dict[str, Any]:
    """Main orchestration function for YAML processing pipeline."""
```

## Workflow Execution

### Phase Combinations

The orchestrator supports seven distinct workflows:

1. **A**: Configuration structure checks only
2. **B**: Physics validation checks only
3. **C**: Configuration consistency checks only
4. **AB**: Configuration structure checks + Physics validation checks
5. **AC**: Configuration structure checks + Configuration consistency checks
6. **BC**: Physics validation checks + Configuration consistency checks
7. **ABC**: Complete three-phase validation

### Execution Flow

```python
# Phase execution mapping
phase_map = {
    "A": [("A", run_phase_a)],
    "B": [("B", run_phase_b)],
    "C": [("C", run_phase_c)],
    "AB": [("A", run_phase_a), ("B", run_phase_b)],
    "AC": [("A", run_phase_a), ("C", run_phase_c)],
    "BC": [("B", run_phase_b), ("C", run_phase_c)],
    "ABC": [("A", run_phase_a), ("B", run_phase_b), ("C", run_phase_c)]
}
```

### File Flow Management

Each phase processes the configuration internally, producing standardised final output:

```text
User Input: config.yml
    ↓
Internal: Phase A → Phase B → Phase C
    ↓
Final Output: updated_config.yml, report_config.txt
```

## File Naming Conventions

### Final Output Files

All workflows produce standardised output files:

- **Updated YAML**: `updated_config.yml` 
- **Consolidated Report**: `report_config.txt` 

The final YAML represents the output from the last successful validation phase(s). For example:
- **AB pipeline**: If A succeeds but B fails, `updated_config.yml` contains Phase A output
- **ABC pipeline**: If all phases succeed, `updated_config.yml` contains Phase A, B and C output

### Cleanup Strategy

The orchestrator ensures only final user-facing files remain:

```python
def create_final_user_files(user_yaml_file, phase_yaml, phase_report):
    """Move phase outputs to standardised final file names."""
    # Standardise file names regardless of pipeline
    # Ensure clean, consistent output for users
```

## Report Consolidation

### Report Merging Logic

The orchestrator consolidates reports from multiple phases into a single, unified document:

```python
def create_consolidated_report(phases_run, no_action_messages, final_report_file, mode):
    """Create consolidated report combining findings from all validation phases."""
    # Merge NO ACTION NEEDED messages from all successful phases
    # Remove duplicate messages automatically
    # Filter orphaned headers (headers with no content)
    # Exclude generic "all passed" messages when specific changes exist
    # Present unified view with consistent formatting
```

**Consolidation Features:**

1. **Deduplication**: Same message appearing in multiple phases is shown only once
2. **Orphaned Header Removal**: Headers with no following content are filtered out
3. **Generic Message Filtering**: Generic "all validations passed" messages excluded when specific changes are listed
4. **Consistent Formatting**: Standardised header format across all pipelines

### Report Structure

The consolidated report uses a harmonised format regardless of pipeline:

```text
# SUEWS Validation Report
# ==================================================
# Mode: {mode}
# ==================================================

## ACTION NEEDED
[Critical issues requiring user attention]

## NO ACTION NEEDED
[Automatic updates and informational items from all phases]

# ==================================================
```
### Recovery Patterns

```python
try:
    phase_c_success = run_phase_c(...)
    if not phase_c_success:
        # Phase C failed - consolidate Phase B messages into Phase C error report
        phase_b_messages = extract_no_action_messages_from_report(phase_b_report)
        append_messages_to_report(phase_c_report, phase_b_messages)
        create_final_user_files(user_yaml, phase_b_yaml, phase_c_report)
except Exception as e:
    # Unexpected error - preserve diagnostic information
    log_error(e)
    create_error_report()
```

## Mode-Specific Behavior

### Public Mode

- Standard validation rules
- User-friendly error messages
- Extra parameters reported as ACTION NEEDED
- No experimental features
- **Pre-validation checks**: Blocks execution if experimental features are detected (stebbsmethod ≠ 0, snowuse ≠ 0)

### Developer Mode

- Same validation logic
- Extra parameters allowed in specific locations
- Experimental features enabled (STEBBS, Snow calculations, SPARTACUS)
- Enhanced diagnostic information
- **No pre-validation restrictions**: All experimental features allowed

## Integration Points

### Phase A Integration

```python
def run_phase_a(user_file, standard_file, output_file, report_file, mode):
    """Execute Phase A: Configuration structure checks and validation."""
    from phase_a_parameter_update import annotate_missing_parameters
    return annotate_missing_parameters(...)
```

### Phase B Integration

```python
def run_phase_b(input_file, output_file, report_file, mode):
    """Execute Phase B: Physics validation checks."""
    from phase_b_science_check import run_science_check
    return run_science_check(...)
```

### Phase C Integration

```python
def run_phase_c(input_file, output_file, report_file, mode):
    """Execute Phase C: Configuration consistency checks and validation."""
    from phase_c_pydantic_report import run_pydantic_validation
    return run_pydantic_validation(...)
```

## Command Line Interface

### Basic Usage

```bash
# Run complete validation
python orchestrator.py config.yml

# Run specific phase
python orchestrator.py config.yml --phase B

# Use developer mode
python orchestrator.py config.yml --mode dev
```

### Options

- `--phase`: Select workflow (A, B, C, AB, AC, BC, ABC)
- `--mode`: Select mode (public, dev)
- `--standard`: Override standard config file path

### Experimental Features Validation

The CLI enforces experimental features restrictions in public mode, providing clear feedback when restricted features are detected:

```bash
$ suews-validate --pipeline ABC config.yml
✗ Configuration contains experimental features restricted in public mode:
  • STEBBS method is enabled (stebbsmethod != 0)

Options to resolve:
  1. Switch to dev mode: --mode dev
  2. Disable experimental features in your YAML file and rerun
     Example: Set stebbsmethod: {value: 0}
```

### Harmonised Terminal Output

Terminal output has been standardised across all pipelines for consistency:

**Success Output:**

```bash
$ suews-validate --pipeline AB config.yml
✓ Validation successful
Report: report_config.txt
Updated YAML: updated_config.yml
```

**Failure Output (shows final file locations regardless of which phase failed):**

```bash
$ suews-validate --pipeline ABC config.yml
✗ Validation failed
Report: report_config.txt
Updated YAML: updated_config.yml
```

## Performance Considerations

### Optimization Strategies

1. **Lazy Loading**: Import phases only when needed
2. **File Caching**: Reuse parsed YAML across phases
3. **Parallel Validation**: Independent checks run concurrently
4. **Early Exit**: Stop on critical errors when appropriate

### Resource Management

```python
# Efficient file handling
with open(file_path, 'r') as f:
    data = yaml.safe_load(f)
    
# Memory-conscious processing
del large_intermediate_data
gc.collect()
```

## Testing

### Test Coverage

- Unit tests for each phase function
- Integration tests for workflows
- File handling edge cases
- Error recovery scenarios

### Test Patterns

```python
def test_workflow_abc():
    """Test complete ABC workflow."""
    result = run_yaml_processor(
        "test_config.yml",
        phase="ABC",
        mode="public"
    )
    assert result["success"]
    assert os.path.exists("updated_test_config.yml")
```

## Best Practices

### For Users

1. Start with ABC workflow for comprehensive validation
2. Use specific phases for targeted validation
3. Review ACTION NEEDED items first
4. Keep intermediate files for debugging

### For Developers

1. Maintain phase independence
2. Preserve backward compatibility
3. Document workflow changes
4. Test error recovery paths

## Future Enhancements

### Planned Features

- Parallel phase execution
- Incremental validation
- Custom phase plugins
- Web service interface

### Extension Points

```python
# Plugin architecture
class ValidationPhase:
    def validate(self, config: dict) -> ValidationResult:
        pass

# Register custom phases
register_phase("custom", CustomValidationPhase())
```

## Related Documentation

- [README](README.md) - System overview
- [Phase A Detailed](PHASE_A_DETAILED.md) - Configuration structure checks and validation
- [Phase B Detailed](PHASE_B_DETAILED.md) - Physics validation checks
- [Phase C Detailed](PHASE_C_DETAILED.md) - Configuration consistency checks and validation