# Design: Validation JSON Reports

## Architecture Overview

```
┌─────────────────────────────────────────────┐
│            CLI Wizard                       │
└─────────────┬───────────────────────────────┘
              │ Consumes JSON Report
              ▼
┌─────────────────────────────────────────────┐
│           Orchestrator                      │
│  - Coordinates phase execution              │
│  - Aggregates JSON results                  │
│  - Saves both JSON and text reports         │
└─────────────┬───────────────────────────────┘
              │
┌─────────────▼───────────────────────────────┐
│         ValidationReporter                  │
│  - Maintains central JSON structure         │
│  - Generates text from JSON                 │
│  - Provides unified API for all phases      │
└─────────────┬───────────────────────────────┘
              │ Used by
    ┌─────────┴──────────┬──────────────────┐
    ▼                    ▼                   ▼
┌─────────┐      ┌─────────────┐     ┌─────────────┐
│ Phase A │      │   Phase B   │     │   Phase C   │
│Parameter│      │Science Check│     │  Pydantic   │
│ Update  │      │             │     │   Report    │
└─────────┘      └─────────────┘     └─────────────┘
    │                    │                   │
    ▼                    ▼                   ▼
JSON Data           JSON Data           JSON Data
    │                    │                   │
    └────────────┬───────┴───────────────────┘
                 ▼
         Unified JSON Structure
                 │
         ┌───────┴───────┐
         ▼               ▼
    Text Report     JSON Report
    (generated)     (saved as-is)
```

## Core Components

### 1. ValidationReporter Class
**Location**: `src/supy/data_model/validation/pipeline/validation_reporter.py`

```python
class ValidationReporter:
    def __init__(self):
        self.json_data = {
            "schema_version": "1.0.0",
            "phases": {},
            "errors": [],
            "warnings": [],
            "info": [],
            "suggestions": []
        }
        
    def add_phase_results(self, phase: str, results: dict) -> None
    def add_error(self, error: dict) -> None
    def add_warning(self, warning: dict) -> None
    def add_info(self, info: dict) -> None
    def get_json_report(self) -> dict
    def generate_text_report(self, phase: str, mode: str) -> str
    def save_reports(self, base_path: str, phase: str, mode: str) -> tuple[str, str]
```

### 2. JSON Schema Structure

```json
{
  "schema_version": "1.0.0",
  "timestamp": "2025-08-26T10:30:00Z",
  "validation_id": "uuid",
  "source_file": "path/to/config.yml",
  "summary": {
    "total_errors": 5,
    "total_warnings": 3,
    "total_info": 10,
    "phases_completed": ["A", "B", "C"],
    "validation_passed": false
  },
  "phases": {
    "parameter_update": {
      "status": "completed",
      "duration_ms": 150,
      "transformations": [],
      "errors": [],
      "warnings": []
    },
    "science_check": {
      "status": "completed",
      "duration_ms": 200,
      "validations": [],
      "errors": [],
      "warnings": []
    },
    "pydantic_validation": {
      "status": "failed",
      "duration_ms": 100,
      "schema_errors": [],
      "field_errors": []
    }
  },
  "errors": [
    {
      "id": "err_001",
      "severity": "critical",
      "phase": "science_check",
      "type": "value_out_of_range",
      "field_path": "metforcing.temp_c",
      "yaml_location": {
        "line": 45,
        "column": 12,
        "line_content": "  temp_c: -300"
      },
      "message": "Temperature value -300°C is below absolute zero",
      "context": {
        "current_value": -300,
        "valid_range": [-273.15, 100],
        "units": "celsius"
      },
      "suggestions": [
        {
          "action": "replace_value",
          "proposed_value": 20,
          "confidence": 0.8,
          "explanation": "Using typical room temperature as default"
        }
      ]
    }
  ],
  "suggestions": {
    "auto_fixes": [],
    "manual_fixes": [],
    "documentation_refs": []
  }
}
```

### 3. Integration Points

#### Phase A Integration
```python
# In phase_a_parameter_update.py - refactor to use reporter
def annotate_missing_parameters(..., reporter: ValidationReporter = None):
    if not reporter:
        reporter = ValidationReporter()
    
    # Existing logic collects data
    for param in renamed_params:
        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": param["old_path"],
            "details": param
        })
    
    for param in missing_params:
        reporter.add_warning({
            "phase": "A", 
            "type": "missing_parameter",
            "field_path": param["path"],
            "details": param
        })
    
    # Generate text report from JSON
    text_report = reporter.generate_text_report("A", mode)
    with open(report_file, 'w') as f:
        f.write(text_report)
    
    # Also save JSON
    json_file = report_file.replace('.txt', '.json')
    with open(json_file, 'w') as f:
        json.dump(reporter.get_json_report(), f, indent=2)
```

#### Phase B Integration
```python
# In phase_b_science_check.py - refactor to use reporter
def run_science_check(..., reporter: ValidationReporter = None):
    if not reporter:
        reporter = ValidationReporter()
    
    # Existing validation logic...
    if validation_failed:
        reporter.add_error({
            "phase": "B",
            "type": "science_validation_error",
            "field_path": field_path,
            "message": error_msg,
            "severity": "critical",
            "context": {"physics_constraint": constraint}
        })
    
    # Generate both reports from JSON
    text_report = reporter.generate_text_report("B", mode)
    json_report = reporter.get_json_report()
    
    return reporter  # Return reporter for orchestrator to aggregate
```

#### Phase C Integration
```python
# In phase_c_pydantic_report.py - refactor to use reporter
def run_pydantic_validation(..., reporter: ValidationReporter = None):
    if not reporter:
        reporter = ValidationReporter()
    
    try:
        # Pydantic validation
        validated = SUEWSConfig.from_yaml(input_file)
    except ValidationError as e:
        for error in e.errors():
            reporter.add_error({
                "phase": "C",
                "type": "pydantic_validation_error",
                "field_path": ".".join(str(p) for p in error["loc"]),
                "message": error["msg"],
                "severity": "critical",
                "context": error
            })
    
    # Generate reports
    text_report = reporter.generate_text_report("C", mode)
    json_report = reporter.get_json_report()
    
    return reporter
```

#### Orchestrator Integration
```python
# In orchestrator.py - aggregate results from all phases
def run_yaml_processor(user_file, standard_file, phase="ABC", mode="public"):
    master_reporter = ValidationReporter()
    
    # Phase A
    if "A" in phase:
        phase_a_reporter = run_phase_a(...)
        master_reporter.merge(phase_a_reporter)
    
    # Phase B
    if "B" in phase:
        phase_b_reporter = run_phase_b(...)
        master_reporter.merge(phase_b_reporter)
    
    # Phase C
    if "C" in phase:
        phase_c_reporter = run_phase_c(...)
        master_reporter.merge(phase_c_reporter)
    
    # Save consolidated reports
    text_path, json_path = master_reporter.save_reports(base_path, phase, mode)
    
    return {"success": True, "text_report": text_path, "json_report": json_path}
```

### 4. Text Report Generation

```python
class TextReportGenerator:
    """Generates human-readable text reports from JSON structure"""
    
    def generate(self, json_data: dict, phase: str, mode: str) -> str:
        """Generate text report maintaining exact current format"""
        lines = []
        
        # Header
        lines.append(f"# SUEWS - Phase {phase} Report")
        lines.append("# " + "=" * 50)
        lines.append(f"# Mode: {mode.title()}")
        lines.append("# " + "=" * 50)
        lines.append("")
        
        # ACTION NEEDED section
        action_items = self._extract_action_items(json_data)
        if action_items:
            lines.append("## ACTION NEEDED")
            for item in action_items:
                lines.append(f"- {item}")
            lines.append("")
        
        # NO ACTION NEEDED section
        info_items = self._extract_info_items(json_data)
        if info_items:
            lines.append("## NO ACTION NEEDED")
            for item in info_items:
                lines.append(f"- {item}")
            lines.append("")
        
        lines.append("# " + "=" * 50)
        
        return "\n".join(lines)
```

### 5. Suggestion Engine

```python
class SuggestionEngine:
    def generate_suggestions(self, error: ValidationError) -> List[Suggestion]:
        """Generate actionable suggestions for errors"""
        suggestions = []
        
        if error.type == "value_out_of_range":
            suggestions.append(self.suggest_range_correction(error))
        elif error.type == "missing_required":
            suggestions.append(self.suggest_default_value(error))
        elif error.type == "incompatible_options":
            suggestions.append(self.suggest_compatible_combo(error))
            
        return suggestions
```

## Implementation Strategy

### Phase 1: Core Reporter Infrastructure
1. Create ValidationReporter class with JSON as core data structure
2. Implement TextReportGenerator to produce text from JSON
3. Define JSON schema that captures all validation information

### Phase 2: Phase Refactoring
1. Refactor Phase A to build JSON structure, then generate text
2. Refactor Phase B to build JSON structure, then generate text
3. Refactor Phase C to build JSON structure, then generate text
4. Each phase returns a ValidationReporter instance

### Phase 3: Orchestrator Integration
1. Update orchestrator to aggregate ValidationReporter instances
2. Merge JSON structures from multiple phases
3. Generate consolidated text and JSON reports
4. Maintain backward compatibility with existing file naming

### Phase 4: CLI Wizard Integration
1. Load and parse JSON reports in CLI wizard
2. Use structured data for interactive error resolution
3. Implement guided fixes based on error types and suggestions

## Performance Considerations

- Use incremental report building to avoid memory spikes
- Cache YAML parsing results for location mapping
- Lazy-load suggestions only when requested
- Use streaming JSON generation for large reports

## Testing Strategy

1. Unit tests for each component
2. Integration tests for pipeline flow
3. Schema validation tests
4. Performance benchmarks
5. CLI wizard consumption tests

## Backward Compatibility

- Reporter is optional parameter to phases
- Text output still available via flags
- Existing return values unchanged
- New functionality behind feature flag initially