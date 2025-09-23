# Phase A: Up-to-Date YAML Consistency Guide

## Overview

Phase A is the first stage of SUEWS configuration validation that ensures your YAML file contains all required parameters and handles outdated parameter names. This comprehensive guide covers all aspects of Phase A operation.

## Table of Contents

- [Architecture and Design](#architecture-and-design)
- [Technical Implementation](#technical-implementation)
- [Processing Modes and Behaviour](#processing-modes-and-behaviour)
- [Parameter Classification Logic](#parameter-classification-logic)
- [Outdated Parameter Handling](#outdated-parameter-handling)
- [Not In Standard Parameter Handling](#not-in-standard-parameter-handling)
- [Output Files Structure](#output-files-structure)
- [Error Handling and Edge Cases](#error-handling-and-edge-cases)
- [Integration with Other Phases](#integration-with-other-phases)
- [Testing and Validation](#testing-and-validation)
- [Mode Selection Guidelines](#mode-selection-guidelines)
- [Git Branch Validation](#git-branch-validation-and-development-workflow-safety)
- [Best Practices](#best-practices)
- [Troubleshooting](#troubleshooting)

## Architecture and Design

Phase A implements a systematic comparison algorithm that:

1. **Recursively traverses** both user and standard YAML structures
2. **Identifies missing parameters** at all nesting levels
3. **Classifies parameters** by importance (critical vs optional)
4. **Handles outdated names** with automatic renaming
5. **Preserves user parameter customisations** not in standard

## Technical Implementation

### Core Functions

- `find_missing_parameters()`: Recursive parameter detection
- `find_missing_parameters_in_lists()`: Missing parameter detection in list structures
- `handle_renamed_parameters()`: Outdated parameter renaming
- `find_extra_parameters()`: NOT IN STANDARD detection
- `find_extra_parameters_in_lists()`: Extra parameter detection in list structures
- `categorise_extra_parameters()`: Classify extra parameters by Pydantic location constraints
- `get_allowed_nested_sections_in_properties()`: **Dynamic introspection** for nested sections that allow extra parameters
- `extract_meaningful_parameter_name()`: Extract contextual parameter names from full paths for user-friendly reporting
- `remove_extra_parameters_from_yaml()`: Remove extra parameters in public mode
- `is_physics_option()`: Critical parameter classification logic
- `create_uptodate_yaml_with_missing_params()`: Mode-dependent clean YAML generation
- `validate_standard_file()`: **Git branch validation** and standard file consistency checks
- `get_current_git_branch()`: Git branch detection for development workflow safety
- `check_file_differs_from_master()`: File comparison against master branch version

### Key Data Structures

```python
# Physics options automatically classified as critical
PHYSICS_OPTIONS = {
    'netradiationmethod', 'emissionsmethod', 'storageheatmethod',
    'ohmincqf', 'roughlenmommethod', 'roughlenheatmethod',
    'stabilitymethod', 'smdmethod', 'waterusemethod',
    'rslmethod', 'faimethod', 'rsllevel',
    'gsmodel', 'snowuse', 'stebbsmethod'
}

# Outdated parameter mappings
RENAMED_PARAMS = {
    'cp': 'rho_cp',
    'diagmethod': 'rslmethod',
    'localclimatemethod': 'rsllevel'
}
```

## Processing Modes and Behaviour

### Mode-Dependent Behaviour

Phase A implements different strategies for handling extra parameters (NOT IN STANDARD) based on the selected processing mode:

#### Public Mode (`--mode public`)

- **Strategy**: Preserve extra parameters in output YAML but report as ACTION_NEEDED
- **Purpose**: Keep user data intact while highlighting non-standard parameters
- **Reporting**: Lists extra parameters as "Found (X) not allowed extra parameter name(s)" in ACTION_NEEDED section with guidance to switch to dev mode or remove parameters
- **Target Users**: General users requiring stable, validated configurations

#### Developer Mode (`--mode dev`)

- **Strategy**: Preserve extra parameters in Pydantic-allowed locations
- **Purpose**: Allow experimental features and custom parameter extensions
- **Reporting**: Lists found parameters as "Found (X) parameter(s) not in standard"
- **Target Users**: Developers and researchers using experimental features

### Extra Parameter Classification

```python
def categorise_extra_parameters(extra_params: list) -> dict:
    """Categorise extra parameters by Pydantic location constraints."""
    # ACTION_NEEDED: Parameters in forbidden locations (sites[].properties)
    # NO_ACTION_NEEDED: Parameters in allowed locations

def get_allowed_nested_sections_in_properties():
    """Dynamically discover nested sections that allow extra parameters.
    
    Uses introspection to find BaseModel fields within classes that have
    extra="forbid" configuration. Automatically discovers new nested
    sections as data model evolves.
    
    Returns: Sorted list of field names (e.g., ["irrigation", "snow", "stebbs"])
    """
```

### Dynamic Nested Section Discovery

The system automatically discovers which nested sections allow extra parameters using introspection:

```python
def get_allowed_nested_sections_in_properties():
    """Dynamic introspection across all data model modules."""
    # 1. Import all data model modules (hydro, site, model, etc.)
    # 2. Find BaseModel classes with extra="forbid" configuration
    # 3. Inspect their nested BaseModel fields
    # 4. Check if nested models allow extra parameters
    # 5. Return sorted list of allowed section names

# Automatically discovers sections like:
# ["anthropogenic_emissions", "building_archetype", "irrigation",
#  "snow", "stebbs", "conductance", ...]

# Replaces hardcoded lists - stays in sync with data model changes
```

### Technical Implementation Details

The dynamic introspection system operates through several key components:

```python
def get_allowed_nested_sections_in_properties():
    """Main introspection function with robust fallback mechanism."""
    # 1. Module Discovery Phase
    data_model_modules = [
        'hydro', 'human_activity', 'model', 'state', 'site', 'core',
        'ohm', 'profile', 'surface', 'timezone_enum', 'type'
    ]
    
    # 2. Class Scanning Phase
    for module_name in data_model_modules:
        module = importlib.import_module(f'.{module_name}',
                                       package='supy.data_model')
        
        # Find BaseModel classes with extra="forbid"
        for attr_name in dir(module):
            if is_forbidden_model(attr):
                # 3. Field Analysis Phase
                for field_name, field_info in attr.model_fields.items():
                    nested_model = _extract_nested_model_type(field_info.annotation)
                    if nested_model and _allows_extra_parameters(nested_model):
                        allowed_sections.add(field_name)
    
    # 4. Validation & Fallback Phase
    if not allowed_sections:
        # Use validated static sections as fallback
        return validate_against_actual_model(static_sections)
    
    return sorted(allowed_sections)
```

### Helper Functions

```python
def _extract_nested_model_type(annotation):
    """Extract BaseModel types from complex annotations."""
    # Handles: Dict[str, BaseModel], List[BaseModel],
    #          Union[BaseModel, str], Optional[BaseModel]

def _allows_extra_parameters(model_class):
    """Check if model allows extra parameters."""
    # Returns: True if extra != "forbid"
```

### Discovery Results

Currently discovers these nested sections automatically:

- **anthropogenic_emissions**: AnthropogenicEmissions model
- **building_archetype**: ArchetypeProperties model
- **conductance**: ConductanceParams model
- **irrigation**: IrrigationParams model
- **snow**: SnowParams model
- **stebbs**: StebbsProperties model
- Additional sections as data model evolves

### Error Handling

The system includes comprehensive error handling:

- **Import Failures**: Gracefully skips modules that can't be imported
- **Missing Attributes**: Handles classes without model_config safely
- **Type Extraction Errors**: Falls back to None for unrecognisable types
- **Complete Failure**: Uses validated static sections as ultimate fallback

### Public Mode Extra Parameter Handling

In public mode, extra parameters are now **preserved** in the output YAML but reported differently:

```python
# In public mode, ALL extra parameters are reported as ACTION_NEEDED
if mode == "public" and extra_count > 0:
    report_lines.append(f"- Found ({extra_count}) not allowed extra parameter name(s):")
    for param_path in extra_params:
        param_name = param_path.split(".")[-1]
        report_lines.append(f"-- {param_name} at level {param_path}")
        report_lines.append("   Suggested fix: You selected Public mode. Consider either to switch to Dev mode, or remove this extra parameter since this is not in the standard yaml.")
```

## Parameter Classification Logic

### Critical Missing Parameters (ACTION NEEDED)

Parameters classified as critical when:

- Located under `model.physics.*` path
- Parameter name exists in `PHYSICS_OPTIONS` set
- Required for basic model physics calculations
- Listed in **ACTION NEEDED** section of report

### Optional Missing Parameters (NO ACTION NEEDED)

Parameters classified as optional when:

- Located outside `model.physics.*` path
- Include site properties, initial states, etc.
- Model can run with nulls or defaults
- Listed in **NO ACTION NEEDED** section of report

### Example Classification

```text
ACTION NEEDED (Critical):
├── model.physics.netradiationmethod
├── model.physics.emissionsmethod
└── model.physics.stabilitymethod

NO ACTION NEEDED (Optional):
├── sites[0].properties.irrigation.wuprofm_24hr.holiday
├── sites[0].initial_states.soilstore_id
└── model.control.output_file.groups
```

## Outdated Parameter Handling

### Automatic Renaming Process

1. **Detection Phase:**
   - Scans YAML content line by line
   - Matches parameter names against `RENAMED_PARAMS` keys
   - Preserves original indentation and values

2. **Renaming Phase:**
   - Replaces old parameter name with new name
   - Adds temporary inline comment during processing
   - Maintains original parameter value

3. **Clean-up Phase:**
   - Removes temporary inline comments for clean output
   - Final YAML contains no processing markers

4. **Documentation Phase:**
   - Records all renamings in analysis report
   - Provides old→new mapping for user verification

### Example Renaming

```yaml
# Before Phase A processing (user file with outdated parameter names)
model:
  physics:
    diagmethod:
      value: 2

# After Phase A processing (clean YAML output with updated names)
model:
  physics:
    rslmethod:
      value: 2
```

## Not In Standard Parameter Handling

Phase A identifies parameters that exist in your configuration but not in the standard and handles them based on processing mode:

### Detection Criteria

- Parameter name exists in user YAML
- Same name does not exist in standard YAML
- Includes both custom parameters and typos

### Mode-Dependent Handling

#### Public Mode Strategy

- **Preserved** in output YAML (parameters remain in the file)
- **Documented** as "Found (X) not allowed extra parameter name(s)" in ACTION_NEEDED section
- **Suggestion** provided to switch to dev mode or remove the extra parameters

#### Developer Mode Strategy

- **Preserved** in output YAML (allows experimental features)
- **Categorised** by Pydantic location constraints:
  - **NO_ACTION_NEEDED**: Parameters in allowed locations (preserved)
  - **ACTION_NEEDED**: Parameters in forbidden locations (SiteProperties)

### Examples by Mode

```yaml
# Public mode: These parameters would be PRESERVED but reported as ACTION_NEEDED
model:
  control:
    custom_simulation_name: "My_SUEWS_Run"  # → Preserved (but ACTION_NEEDED in report)
    debug_mode: true                        # → Preserved (but ACTION_NEEDED in report)
sites:
- properties:
    custom_param: 1.5                       # → Preserved (but ACTION_NEEDED in report)

# Dev mode: Location-dependent handling
model:
  control:
    custom_simulation_name: "My_SUEWS_Run"  # → Preserved (allowed location)
sites:
- properties:
    custom_param: 1.5                       # → ACTION_NEEDED (forbidden location)
    stebbs:
      experimental_param: 2.0               # → Preserved (allowed nested section)
```

## Output Files Structure

### Updated YAML File (`updatedA_<filename>.yml`)

```yaml
# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

name: Updated User Configuration
model:
  control:
    tstep: 300
    custom_param: "user_value"
  physics:
    netradiationmethod:
      value: null
    emissionsmethod:
      value: 2
    rho_cp:
      value: 1005
```

### Analysis Report Structure

Phase A generates mode-dependent comprehensive reports with enhanced user-friendly parameter naming and clear success indication:

- **ACTION NEEDED**: Critical physics parameters that must be set by the user (YAML contains null values)
  - In **Dev Mode**: Also includes extra parameters in forbidden locations
  - In **Public Mode**: Critical missing parameters AND extra parameters (extra parameters now reported as ACTION_NEEDED)

- **NO ACTION NEEDED**: All updates automatically applied including:
  - Optional missing parameters updated with null values
  - Parameter renamings applied
  - Mode-dependent extra parameter handling:
    - **Public Mode**: No extra parameters in NO ACTION NEEDED section (all moved to ACTION_NEEDED)
    - **Dev Mode**: "Found (X) parameter(s) not in standard" (for allowed locations)

- **Phase A Success Indication**: When Phase A completes successfully without any issues to report, the report displays "Phase A passed" to clearly indicate successful completion

### Analysis Report Examples

#### Public Mode Report (`reportA_<filename>.txt`)

```text
# SUEWS - Phase A (Up-to-date YAML check) Report
# Generated: 2024-01-15 14:30:00
# Mode: Public
# ==================================================

## ACTION NEEDED
- Found (1) critical missing parameter(s):
-- netradiationmethod has been added to updatedA_user.yml and set to null
   Suggested fix: Set appropriate value based on SUEWS documentation

- Found (2) not allowed extra parameter name(s):
-- startdate at level model.control.startdate
   Suggested fix: You selected Public mode. Consider either to switch to Dev mode, or remove this extra parameter since this is not in the standard yaml.
-- test at level sites[0].properties.test
   Suggested fix: You selected Public mode. Consider either to switch to Dev mode, or remove this extra parameter since this is not in the standard yaml.

## NO ACTION NEEDED
- Updated (3) optional missing parameter(s) with null values:
-- holiday added to updatedA_user.yml and set to null
-- wetthresh added to updatedA_user.yml and set to null

- Updated (2) renamed parameter(s):
-- diagmethod changed to rslmethod
-- cp changed to rho_cp

# ==================================================
```

#### Developer Mode Report (`reportA_<filename>.txt`)

```text
# SUEWS - Phase A (Up-to-date YAML check) Report
# Generated: 2024-01-15 14:30:00
# Mode: Developer
# ==================================================

## ACTION NEEDED
- Found (1) critical missing parameter(s):
-- netradiationmethod has been added to updatedA_user.yml and set to null
   Suggested fix: Set appropriate value based on SUEWS documentation

- Found (1) parameter(s) in forbidden locations:
-- test at level sites[0].properties.test
   Reason: Extra parameters not allowed in SiteProperties
   Suggested fix: Remove parameter or move to allowed nested section (stebbs, irrigation, snow)

## NO ACTION NEEDED
- Updated (3) optional missing parameter(s) with null values:
-- holiday added to updatedA_user.yml and set to null
-- wetthresh added to updatedA_user.yml and set to null

- Updated (2) renamed parameter(s):
-- diagmethod changed to rslmethod
-- cp changed to rho_cp

- Found (1) parameter(s) not in standard:
-- startdate at level model.control.startdate

# ==================================================
```

## Error Handling and Edge Cases

### File Access Errors

```python
# Phase A handles common file errors gracefully
try:
    with open(user_file, 'r') as f:
        user_data = yaml.safe_load(f)
except FileNotFoundError as e:
    print(f"Error: File not found - {e}")
    return None
except yaml.YAMLError as e:
    print(f"Error: Invalid YAML syntax in '{user_file}': {e}")
    return None
```

### Malformed YAML Structures

- **Empty files**: Handled with appropriate error messages
- **Invalid syntax**: YAML parsing errors caught and reported
- **Missing sections**: Detected and documented in missing parameters

#### Successful Completion with No Issues

When Phase A completes successfully without finding any issues, the report clearly indicates success:

```text
# SUEWS Validation Report
# ==================================================
# Mode: Public
# ==================================================

Phase A passed

# ==================================================
```

This clear success indication is particularly valuable in multi-phase workflows where Phase A succeeds but later phases fail.

## Integration with Other Phases

Phase A output serves as input to subsequent phases in the validation pipeline:

### File Handoff

```bash
# Phase A generates
updatedA_user_config.yml    # → Input to Phase B/C
reportA_user_config.txt     # → Phase A analysis

# Workflow combinations process Phase A output:
updatedA_user_config.yml    # ← Phase A output
↓
updatedAB_user_config.yml   # → AB workflow final output
updatedAC_user_config.yml   # → AC workflow final output
updated_user_config.yml  # → Complete pipeline output
```

## Testing and Validation

Phase A includes comprehensive test coverage:

### Test Categories

- **Parameter Detection**: Missing, renamed, and extra parameters
- **File Handling**: Various file formats and error conditions
- **Classification Logic**: Critical vs optional parameter sorting
- **Output Generation**: YAML and report file creation
- **Edge Cases**: Empty files, malformed YAML, permission errors

### Example Test

```python
def test_urgent_parameter_classification():
    """Test that physics parameters are classified as critical."""
    user_config = {
        'model': {
            'physics': {'emissionsmethod': {'value': 2}}
            # netradiationmethod missing
        }
    }
    
    missing_params = find_missing_parameters(user_config, standard_config)
    urgent_params = [path for path, val, is_urgent in missing_params if is_urgent]
    
    assert 'model.physics.netradiationmethod' in urgent_params
```

## Mode Selection Guidelines

### When to Use Public Mode

- **General users** requiring stable, validated configurations
- **Production runs** with standard SUEWS features only
- **Clean output files** needed for sharing or archival
- **Standard compliance** is important for your use case

### When to Use Developer Mode

- **Experimental features** like STEBBS method are required
- **Custom parameters** need to be preserved during validation
- **Research applications** using non-standard configurations
- **Development work** on new SUEWS features

### Mode Restrictions

```text
Public Mode Restrictions:
├── stebbsmethod != 0        # Triggers pre-validation error
├── Extra parameters         # Preserved but reported as ACTION_NEEDED
└── Future: SPARTACUS method # Will be restricted

Developer Mode Allowances:
├── All experimental features # No pre-validation restrictions
├── Extra parameters         # Preserved in allowed locations
└── Enhanced diagnostics     # Additional reporting information
```

## Git Branch Validation and Development Workflow Safety

Phase A includes **sophisticated git-based validation** to ensure configuration consistency across development branches and prevent validation against modified standard files.

### Git Validation System

```python
def validate_standard_file(standard_file: str) -> bool:
    """Validate standard file exists and matches master branch."""
    print("Validating standard configuration file...")
    
    # Check file exists
    if not os.path.exists(standard_file):
        print(f"❌ ERROR: Standard file not found: {standard_file}")
        return False
    
    current_branch = get_current_git_branch()
    
    if current_branch != "master":
        file_differs = check_file_differs_from_master(standard_file)
        if file_differs:
            print(f"⚠️  WARNING: You are on branch '{current_branch}' and sample_config.yml differs from master")
            print("   RECOMMENDED:")
            print("   1. Switch to master branch: git checkout master") 
            print("   2. OR update your sample_config.yml to match master:")
            print(f"      git checkout master -- {standard_file}")
```

### Core Git Functions

```python
def get_current_git_branch() -> str:
    """Get current git branch using git branch --show-current."""
    result = subprocess.run(["git", "branch", "--show-current"], 
                           capture_output=True, text=True, check=True)
    return result.stdout.strip() or "unknown"

def check_file_differs_from_master(file_path: str) -> bool:
    """Check if file differs from master branch version using git diff master."""
    result = subprocess.run(["git", "diff", "master", "--", file_path],
                           capture_output=True, text=True, check=True)
    return len(result.stdout.strip()) > 0
```

### Warning Message Format

```text
⚠️  WARNING: You are on branch 'feature-new-physics' and sample_config.yml differs from master
   This may cause inconsistent parameter detection.
   RECOMMENDED:
   1. Switch to master branch: git checkout master
   2. OR update your sample_config.yml to match master:
      git checkout master -- src/supy/sample_data/sample_config.yml
```

## Best Practices

### For Users

1. **Start with public mode** for standard validation needs
2. **Switch to dev mode** only when experimental features are required
3. **Address critical parameters** immediately in ACTION NEEDED section
4. **Review mode-specific messaging** in reports for guidance
5. **Use complete ABC workflow** for thorough validation

### For Developers

1. **Use dev mode** when working with experimental features
2. **Update PHYSICS_OPTIONS** when adding new physics parameters
3. **Add RENAMED_PARAMS entries** when deprecating parameters
4. **Test both modes** to ensure consistent behaviour
5. **Update allowed nested sections** when extending Pydantic model

## Troubleshooting

### Common Issues

**Issue**: "Standard file not found"

```text
Solution: Ensure sample_data/sample_config.yml exists
Check: ls sample_data/sample_config.yml
Fix: Update SUEWS installation or specify correct path
```

**Issue**: "YAML syntax error in user file"

```text
Solution: Validate YAML syntax
Check: python -c "import yaml; yaml.safe_load(open('user.yml'))"
Fix: Correct indentation, quotes, or structure
```

**Issue**: "All parameters marked as critical"

```text
Solution: Check PHYSICS_OPTIONS set in uptodate_yaml.py
Check: Parameter classification logic
Fix: Update PHYSICS_OPTIONS or parameter paths
```

### Advanced Usage

```python
# Direct Python usage
from supy.data_model.uptodate_yaml import annotate_missing_parameters

# Public mode usage (default)
result = annotate_missing_parameters(
    user_file="my_config.yml",
    standard_file="sample_data/sample_config.yml",
    uptodate_file="updatedA_my_config.yml",
    report_file="reportA_my_config.txt",
    mode="public",  # Public mode - preserves extra parameters but reports as ACTION NEEDED
    phase="A"
)

# Developer mode usage
result = annotate_missing_parameters(
    user_file="my_config.yml",
    standard_file="sample_data/sample_config.yml",
    uptodate_file="updatedA_my_config.yml",
    report_file="reportA_my_config.txt",
    mode="dev",    # Developer mode preserves extra parameters
    phase="A"
)

if result:
    print("✅ Phase A completed successfully")
else:
    print("❌ Phase A encountered errors")
```

### Command Line Usage

```bash
# Public mode (default) - preserves extra parameters but reports as ACTION NEEDED
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase A --mode public

# Developer mode - preserves extra parameters
python src/supy/data_model/suews_yaml_processor.py user_config.yml --phase A --mode dev
```

## Related Documentation

### Three-Phase Validation System
- [README](README.md) - Overview of the complete three-phase validation system
- [Orchestrator](ORCHESTRATOR.md) - Implementation and workflow coordination

### Other Validation Phases
- [Phase B Detailed](PHASE_B_DETAILED.md) - Phase B scientific validation and automatic corrections
- [Phase C Detailed](PHASE_C_DETAILED.md) - Phase C Pydantic validation and conditional rules

### SUEWS Configuration
- Complete parameter specifications and validation details in the main SUEWS documentation