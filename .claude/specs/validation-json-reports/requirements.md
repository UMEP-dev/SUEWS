# Requirements: Validation JSON Reports

## Context
The validation pipeline currently generates human-readable text reports directly from validation logic, making it difficult to extract structured data for programmatic consumption. While `yaml_annotator.py` uses JSON internally for YAML manipulation, the validation results themselves are not structured as JSON.

## Overview
Refactor the validation pipeline to use JSON as the core data structure for all validation results, then generate both human-readable text reports and machine-readable JSON reports from this single source of truth. This enables the CLI wizard to programmatically consume validation results while maintaining backward compatibility with existing text report formats.

## Functional Requirements (EARS Notation)

### Core Validation Reporting
- **WHEN** the validation pipeline executes **THE SYSTEM SHALL** first generate structured JSON data containing all validation results
- **WHEN** JSON data is generated **THE SYSTEM SHALL** derive human-readable text reports from this JSON structure
- **WHEN** validation errors occur **THE SYSTEM SHALL** store detailed error context with line numbers, field paths, and values in the JSON structure
- **WHEN** validation warnings are detected **THE SYSTEM SHALL** categorise them by severity (critical, warning, info) in the JSON structure
- **WHEN** reports are saved **THE SYSTEM SHALL** generate both `report{phase}_{name}.txt` (from JSON) and `report{phase}_{name}.json` (raw structure)

### Phase Integration
- **WHEN** Phase A (Parameter Update) runs **THE SYSTEM SHALL** return structured JSON data containing all parameter transformations and updates
- **WHEN** Phase B (Science Check) runs **THE SYSTEM SHALL** return structured JSON data containing all physics validation results and cross-dependencies
- **WHEN** Phase C (Pydantic Report) runs **THE SYSTEM SHALL** return structured JSON data containing all schema validation errors with field-level details
- **WHEN** multiple phases run **THE SYSTEM SHALL** merge their JSON results into a consolidated structure before generating reports

### Error Context & Suggestions
- **WHEN** a validation error occurs **THE SYSTEM SHALL** provide the YAML path to the problematic field
- **WHEN** a value is out of range **THE SYSTEM SHALL** include the valid range and suggested corrections
- **WHEN** a required field is missing **THE SYSTEM SHALL** provide a template value and documentation reference
- **WHEN** incompatible options are detected **THE SYSTEM SHALL** explain the conflict and suggest compatible alternatives

### CLI Wizard Integration
- **WHEN** the CLI wizard requests validation **THE SYSTEM SHALL** return results in a consistent JSON schema
- **WHEN** the wizard needs error locations **THE SYSTEM SHALL** provide line and column numbers in the source YAML
- **WHEN** the wizard requests suggestions **THE SYSTEM SHALL** provide actionable fix proposals with confidence scores

### Backward Compatibility
- **WHEN** existing validation functions are called **THE SYSTEM SHALL** maintain their current return signatures
- **WHEN** text reports are generated **THE SYSTEM SHALL** maintain the exact same format as current reports
- **WHEN** legacy code calls validation **THE SYSTEM SHALL** transparently handle the JSON-to-text conversion
- **WHEN** JSON structure is updated **THE SYSTEM SHALL** maintain backward-compatible text output format

### Performance Requirements
- **WHEN** generating JSON reports **THE SYSTEM SHALL** complete within 10% of current validation time
- **WHEN** storing validation results **THE SYSTEM SHALL** use incremental updates to minimise memory usage

### Output Requirements
- **WHEN** saving JSON reports **THE SYSTEM SHALL** use a versioned schema for forward compatibility
- **WHEN** including file paths **THE SYSTEM SHALL** use relative paths from the project root
- **WHEN** formatting timestamps **THE SYSTEM SHALL** use ISO 8601 format

## Non-Functional Requirements

### Usability
- JSON reports must be human-readable with proper indentation
- Error messages must be actionable and user-friendly
- Documentation must include examples of report structure

### Maintainability
- Report generation code must be modular and testable
- JSON schema must be documented and versioned
- Integration points must be clearly defined

### Reliability
- Report generation must not affect validation logic
- Errors in reporting must not block validation
- Reports must be deterministic for the same input

## Success Criteria
1. CLI wizard can parse and use validation reports
2. Users receive more helpful error messages
3. Validation time increase is less than 10%
4. All existing tests continue to pass
5. JSON schema is documented and validated