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

1. **A**: Up-to-date YAML check only
2. **B**: Scientific validation only
3. **C**: Pydantic validation only
4. **AB**: Up-to-date check + Scientific validation
5. **AC**: Up-to-date check + Pydantic validation
6. **BC**: Scientific validation + Pydantic validation
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

Each phase generates specific files:

```text
User Input: config.yml
    ↓
Phase A: updatedA_config.yml, reportA_config.txt
    ↓
Phase B: updatedB_config.yml, reportB_config.txt
    ↓
Phase C: updatedC_config.yml, reportC_config.txt
    ↓
Final: updated{workflow}_config.yml, report{workflow}_config.txt
```

## File Naming Conventions

### Intermediate Files

- **Phase A**: `updatedA_*.yml`, `reportA_*.txt`
- **Phase B**: `updatedB_*.yml`, `reportB_*.txt`
- **Phase C**: `updatedC_*.yml`, `reportC_*.txt`

### Final Output Files

- **Workflow Output**: `updated_*.yml` for full validation (e.g., `updated_config.yml`)
- **Consolidated Report**: `report_*.txt` for full validation (e.g., `report_config.txt`)

### Cleanup Strategy

```python
def cleanup_intermediate_files(base_name: str, phases_to_clean: List[str]):
    """Remove intermediate files after successful workflow completion."""
    # Only cleanup on success
    # Preserve files on error for debugging
```

## Report Consolidation

### Report Merging Logic

The orchestrator intelligently merges reports from multiple phases:

```python
def consolidate_reports(reports: Dict[str, str], workflow: str) -> str:
    """Merge multiple phase reports into single consolidated report."""
    # Extract ACTION NEEDED from all phases
    # Combine NO ACTION NEEDED sections
    # Preserve phase-specific information
```

### Report Structure

```text
# SUEWS - Phase {workflow} Report
# ==================================================
# Mode: {mode}
# ==================================================

## ACTION NEEDED
[Critical issues from all phases]

## NO ACTION NEEDED
[Automatic updates and information from all phases]

# ==================================================
```

## Error Handling

### Phase Failure Behavior

1. **Partial Success**: Earlier phase outputs preserved
2. **Error Reporting**: Detailed error information in reports
3. **File Retention**: Intermediate files kept for debugging
4. **Graceful Degradation**: Best effort to complete validation

### Recovery Patterns

```python
try:
    result = phase_function(...)
    if not result:
        # Phase failed but continue with available data
        preserve_intermediate_files()
        generate_error_report()
except Exception as e:
    # Unexpected error - preserve all files
    log_error(e)
    preserve_all_files()
```

## Mode-Specific Behavior

### Public Mode

- Standard validation rules
- User-friendly error messages
- Extra parameters reported as ACTION NEEDED
- No experimental features

### Developer Mode

- Same validation logic
- Extra parameters allowed in specific locations
- Experimental features enabled (STEBBS, SPARTACUS)
- Enhanced diagnostic information

## Integration Points

### Phase A Integration

```python
def run_phase_a(user_file, standard_file, output_file, report_file, mode):
    """Execute Phase A: Up-to-date YAML check."""
    from phase_a_parameter_update import annotate_missing_parameters
    return annotate_missing_parameters(...)
```

### Phase B Integration

```python
def run_phase_b(input_file, output_file, report_file, mode):
    """Execute Phase B: Scientific validation."""
    from phase_b_science_check import run_science_check
    return run_science_check(...)
```

### Phase C Integration

```python
def run_phase_c(input_file, output_file, report_file, mode):
    """Execute Phase C: Pydantic validation."""
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

### Enhanced Error Reporting

When phases fail, the CLI now shows generated report files so users can find detailed error information:

```bash
$ suews-validate --pipeline ABC config.yml
✗ Phase B failed
Report: reportB_config.txt
```

The key improvement is that Phase B now generates comprehensive error reports even when initialization fails, with individual errors listed separately for better clarity.

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
- [Phase A Detailed](PHASE_A_DETAILED.md) - Parameter validation
- [Phase B Detailed](PHASE_B_DETAILED.md) - Scientific validation
- [Phase C Detailed](PHASE_C_DETAILED.md) - Pydantic validation