# SUEWS Validation Documentation - CSV Files Summary

## Overview

This directory contains three CSV files documenting all validation rules for SUEWS parameters:

1. **main_rulebook.csv** - Complete parameter inventory with validation rules
2. **c2_rulebook.csv** - Detailed C2 (complex) rule descriptions
3. **descriptions_rulebook.csv** - Rule type and rule name definitions

## File Descriptions

### 1. main_rulebook.csv

**Purpose**: Central inventory of all 393 unique SUEWS parameters with their validation rules.

**Structure**: 
- **Header row 1**: Column descriptions
- **Header row 2**: Column names
- **Data rows**: One row per (parameter, mother_class) combination (404 rows)

**Key Columns**:
- `parameter_name`: Parameter identifier (e.g., lat, lng, snowalb)
- `category`: Functional category (e.g., morphology, radiation, thermal_property) -- STILL WORKING IN PROGRESS!
- `model`: SUEWS or STEBBS
- `module`: Python module location (e.g., site, model, state)
- `mother_class`: Pydantic class containing the parameter
- `pipeline`: Validation pipeline(s): A (structure), B (scientific), C/Pydantic (Pydantic validation)
- `rule_class`: C0 (fundamental), C1 (simple range), C2 (complex), CM (no validation)
- `rule_type`: Type of validation (e.g., fraction_0_1, cross_parameter, scientific_adjustment)
- `rule_name`: Specific rule identifier (e.g., land_cover_consistency, temperature_adjustment)
- `formula`: Mathematical constraint (e.g., [0,1], >=0)
- `notes`: Additional information (e.g., PR numbers for pending features)

**Usage**: Look up any parameter to see all its validation rules and constraints.

### 2. c2_rulebook.csv

**Purpose**: Detailed documentation of all 25 C2 (complex) validation rules across validation pipelines.

**Structure**:
- **Header row**: Column names
- **Data rows**: One row per C2 rule (25 rows)

**Key Columns**:
- `pipeline`: Where rule is executed (A, B, or C/Pydantic)
- `rule_type`: Type of complex validation (e.g., cross_parameter, scientific_adjustment)
- `rule_name`: Unique rule identifier
- `trigger_condition`: When the rule is activated
- `validation_logic`: What the rule checks or adjusts
- `validation_source`: Source code location (e.g., phase_b.py:validate_irrigation_parameters)
- `parameters_affected`: List of parameters involved in this rule

**Pipeline Breakdown**:
- **Phase A (2 rules)**: Structure and forcing data validation
- **Phase B (8 rules)**: Scientific adjustments and physics validation
- **Phase C/Pydantic (15 rules)**: Pydantic field validation and transformations

**Usage**: Look up detailed logic for any C2 rule referenced in the main rulebook.

### 3. descriptions_rulebook.csv

**Purpose**: Dictionary defining all rule types and rule names used in the main rulebook.

**Structure**:
- **Header row**: Column names
- **Data rows**: Rule type definitions followed by rule name definitions

**Key Columns**:
- `rule_type`: Type of validation (e.g., fraction_0_1, cross_parameter)
- `description_rule_type`: Explanation of the rule type category
- `rule_name`: Specific rule identifier (if applicable)
- `description_rule_name`: Detailed explanation of what the rule does

**Rule Class Definitions**:
- **C0 (Fundamental)**: fraction_0_1, positive, non_negative
- **C1 (Non-fundamental)**: range, min_value
- **C2 (Complex)**: conditional_required, cross_parameter, field_transformation, physical_constraint, data_validation, scientific_adjustment
- **CM (Class Missing)**: rules are missing for this specific parameter

**Usage**: Look up definitions for any rule_type or rule_name to understand what it means.

## How the Files Are Linked

1. **Start with main_rulebook.csv** to find parameters and their validation rules
2. **For rule_type or rule_name definitions**, consult descriptions_rulebook.csv
3. **For detailed C2 rule logic**, look up the rule_name in c2_rulebook.csv

### Example Workflow:

**Question**: What validation rules apply to parameter `ie_start`?

1. Look up `ie_start` in **main_rulebook.csv**:
   - pipeline: B
   - rule_class: C2
   - rule_type: cross_parameter
   - rule_name: irrigation_validation

2. Check **descriptions_rulebook.csv** for `cross_parameter`:
   - "Validation rules that check relationships or consistency between multiple parameters"

3. Find `irrigation_validation` in **c2_rulebook.csv**:
   - Detailed logic: "validates irrigation timing: DOY range (1-365/366), both params must be set together or both disabled, hemisphere-aware seasonal restrictions"
   - Also affects: ie_end, lat

## Quick Reference

**Validation Pipelines**:
- **Phase A**: Structure checks (parameter presence, array dimensions, forcing data)
- **Phase B**: Scientific adjustments (temperatures, seasonal params, geographic calculations)
- **Phase C**: Pydantic validation (field constraints, type conversions, conditional requirements)

**Rule Classes**:
- **C0**: Fundamental ranges ([0,1], >=0, >0) - 142 parameters
- **C1**: Non-fundamental ranges - 7 parameters
- **C2**: Complex multi-parameter or conditional rules - 99 parameters
- **CM**: No validation - 245 parameters

## File Maintenance

These files wew initially generated via `scripts/generate_validation_overview_csv.py`, but then maintained manually by SR.
