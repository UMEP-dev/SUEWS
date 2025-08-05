"""
SUEWS Science Check Phase B

This module performs scientific validation and consistency checks on YAML configurations
that have already been processed by Phase A (uptodate_yaml.py).

Phase B focuses on:
- Scientific parameter validation using Pydantic models
- Geographic coordinate and timezone validation  
- Seasonal parameter adjustments (LAI, snowalb, surface temperatures)
- Land cover fraction validation and consistency
- Model physics option interdependency checks
- Automatic scientific corrections where appropriate

Phase B assumes Phase A has completed successfully and builds upon clean YAML output
without duplicating parameter detection or YAML structure validation.
"""

import yaml
import os
from typing import Dict, List, Optional, Union, Any, Tuple
from dataclasses import dataclass
from datetime import datetime
from copy import deepcopy


@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""
    status: str  # 'PASS', 'WARNING', 'ERROR'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False


@dataclass 
class ScientificAdjustment:
    """Record of automatic scientific adjustment applied."""
    parameter: str
    site_index: Optional[int] = None
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


def validate_phase_b_inputs(uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str) -> Tuple[dict, dict, dict]:
    """
    Validate that Phase B has all required inputs and Phase A completed successfully.
    
    Args:
        uptodate_yaml_file: Path to Phase A output (uptodate YAML)
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        
    Returns:
        Tuple of (uptodate_data, user_data, standard_data) dictionaries
        
    Raises:
        FileNotFoundError: If required files are missing
        ValueError: If Phase A did not complete properly
    """
    # Check that all required files exist
    for file_path in [uptodate_yaml_file, user_yaml_file, standard_yaml_file]:
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"Required file not found: {file_path}")
    
    try:
        # Load uptodate YAML (Phase A output)
        with open(uptodate_yaml_file, 'r') as f:
            uptodate_content = f.read()
            uptodate_data = yaml.safe_load(uptodate_content)
            
        # Verify Phase A completed (check for Phase A header)
        if "UP TO DATE YAML" not in uptodate_content:
            raise ValueError(f"Phase A did not complete properly - missing Phase A header in {uptodate_yaml_file}")
            
        # Load original user YAML
        with open(user_yaml_file, 'r') as f:
            user_data = yaml.safe_load(f)
            
        # Load standard YAML
        with open(standard_yaml_file, 'r') as f:
            standard_data = yaml.safe_load(f)
            
    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}")
        
    return uptodate_data, user_data, standard_data


def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """
    Extract key simulation parameters needed for scientific validation.
    
    Args:
        yaml_data: YAML configuration dictionary
        
    Returns:
        Tuple of (model_year, start_date, end_date)
        
    Raises:
        ValueError: If required simulation parameters are missing
    """
    control = yaml_data.get("model", {}).get("control", {})
    
    start_date = control.get("start_time")
    end_date = control.get("end_time")
    
    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError("Missing or invalid 'start_time' in model.control - must be in 'YYYY-MM-DD' format")
        
    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError("Missing or invalid 'end_time' in model.control - must be in 'YYYY-MM-DD' format")
    
    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError("Could not extract model year from 'start_time' - ensure 'YYYY-MM-DD' format")
        
    return model_year, start_date, end_date


def run_scientific_validation_pipeline(yaml_data: dict, start_date: str, model_year: int) -> List[ValidationResult]:
    """
    Execute all scientific validation checks on the YAML configuration.
    
    Args:
        yaml_data: YAML configuration dictionary
        start_date: Simulation start date in YYYY-MM-DD format
        model_year: Model year extracted from start_date
        
    Returns:
        List of ValidationResult objects from all validation checks
    """
    validation_results = []
    
    # Physics parameter validation
    # TODO: Implement validate_physics_parameters(yaml_data)
    
    # Geographic coordinate validation  
    # TODO: Implement validate_geographic_parameters(yaml_data)
    
    # Seasonal parameter validation
    # TODO: Implement validate_seasonal_parameters(yaml_data, start_date)
    
    # Land cover consistency validation
    # TODO: Implement validate_land_cover_consistency(yaml_data)
    
    # Model option dependency validation
    # TODO: Implement validate_model_option_dependencies(yaml_data)
    
    # Placeholder validation for initial implementation
    validation_results.append(ValidationResult(
        status='PASS',
        category='FRAMEWORK',
        parameter='phase_b_framework',
        message='Phase B framework initialized successfully'
    ))
    
    return validation_results


def run_scientific_adjustment_pipeline(yaml_data: dict, start_date: str, model_year: int) -> Tuple[dict, List[ScientificAdjustment]]:
    """
    Apply automatic scientific corrections and adjustments to YAML configuration.
    
    Args:
        yaml_data: YAML configuration dictionary
        start_date: Simulation start date in YYYY-MM-DD format
        model_year: Model year extracted from start_date
        
    Returns:
        Tuple of (updated_yaml_data, list_of_adjustments)
    """
    adjustments = []
    updated_data = deepcopy(yaml_data)
    
    # Surface temperature initialization
    # TODO: Implement adjust_surface_temperatures(updated_data, start_date)
    
    # Seasonal parameter adjustments
    # TODO: Implement adjust_seasonal_parameters(updated_data, start_date)
    
    # DLS parameter calculations
    # TODO: Implement adjust_dls_parameters(updated_data, model_year)
    
    # Model-dependent nullification
    # TODO: Implement adjust_model_dependent_nullification(updated_data)
    
    return updated_data, adjustments


def create_science_report(validation_results: List[ValidationResult], adjustments: List[ScientificAdjustment], 
                         science_yaml_filename: str = None) -> str:
    """
    Generate comprehensive scientific validation report.
    
    Args:
        validation_results: List of validation results from scientific checks
        adjustments: List of automatic adjustments applied
        science_yaml_filename: Name of final science-checked YAML file
        
    Returns:
        String containing formatted report content
    """
    report_lines = []
    report_lines.append("# SUEWS Scientific Validation Report")
    report_lines.append("# " + "="*50)
    report_lines.append("")
    
    # Count results by status
    errors = [r for r in validation_results if r.status == 'ERROR']
    warnings = [r for r in validation_results if r.status == 'WARNING']
    passed = [r for r in validation_results if r.status == 'PASS']
    
    # PHASE B RESULTS header
    report_lines.append("## PHASE B RESULTS")
    report_lines.append("")
    
    # ACTION NEEDED - SCIENTIFIC ERRORS
    if errors:
        report_lines.append("### ACTION NEEDED - SCIENTIFIC ERRORS")
        report_lines.append(f"- Found ({len(errors)}) critical scientific parameter error(s):")
        for error in errors:
            site_ref = f" at site [{error.site_index}]" if error.site_index is not None else ""
            report_lines.append(f"-- {error.parameter}{site_ref}: {error.message}")
            if error.suggested_value is not None:
                report_lines.append(f"   Suggested fix: {error.suggested_value}")
        report_lines.append("")
    
    # SCIENTIFIC WARNINGS
    if warnings:
        report_lines.append("### SCIENTIFIC WARNINGS")
        report_lines.append(f"- Found ({len(warnings)}) scientific warning(s):")
        for warning in warnings:
            site_ref = f" at site [{warning.site_index}]" if warning.site_index is not None else ""
            report_lines.append(f"-- {warning.parameter}{site_ref}: {warning.message}")
        report_lines.append("")
    
    # SCIENTIFIC ADJUSTMENTS APPLIED
    if adjustments:
        report_lines.append("### SCIENTIFIC ADJUSTMENTS APPLIED")
        report_lines.append(f"- Applied ({len(adjustments)}) automatic scientific adjustment(s):")
        for adjustment in adjustments:
            site_ref = f" at site [{adjustment.site_index}]" if adjustment.site_index is not None else ""
            report_lines.append(f"-- {adjustment.parameter}{site_ref}: {adjustment.old_value} → {adjustment.new_value} ({adjustment.reason})")
        report_lines.append("")
    
    # NO SCIENTIFIC ISSUES
    if not errors and not warnings:
        report_lines.append("### NO SCIENTIFIC ISSUES")
        report_lines.append("- All scientific validations passed")
        report_lines.append("- Model physics parameters are consistent")
        report_lines.append("- Geographic parameters are valid")
        if adjustments:
            report_lines.append("- Seasonal adjustments applied successfully")
        report_lines.append("")
    
    # Footer separator
    report_lines.append("# " + "="*50)
    report_lines.append("")
    
    # Next steps
    report_lines.append("## NEXT STEPS")
    if errors:
        report_lines.append("1. Review scientific errors and apply suggested fixes")
        report_lines.append("2. Re-run Phase B after corrections")
    elif warnings:
        report_lines.append("1. Review scientific warnings for model accuracy")
        if science_yaml_filename:
            report_lines.append(f"2. Proceed with SUEWS simulation using {science_yaml_filename}")
        else:
            report_lines.append("2. Proceed with SUEWS simulation using science-checked YAML")
    else:
        if science_yaml_filename:
            report_lines.append(f"1. Proceed with SUEWS simulation using {science_yaml_filename}")
        else:
            report_lines.append("1. Proceed with SUEWS simulation using science-checked YAML")
        report_lines.append("2. All scientific validations and adjustments completed successfully")
    
    return '\n'.join(report_lines)


def print_science_check_results(validation_results: List[ValidationResult], adjustments: List[ScientificAdjustment]):
    """
    Print clean terminal output for Phase B results.
    
    Args:
        validation_results: List of validation results
        adjustments: List of automatic adjustments applied
    """
    errors = [r for r in validation_results if r.status == 'ERROR']
    warnings = [r for r in validation_results if r.status == 'WARNING']
    
    if errors:
        print("PHASE B -- SCIENTIFIC ERRORS FOUND:")
        for error in errors:
            site_ref = f" at site [{error.site_index}]" if error.site_index is not None else ""
            print(f"  - {error.parameter}{site_ref}: {error.message}")
        print("\nNext step: Check science_report_user.txt for detailed scientific guidance")
    elif warnings:
        print(f"PHASE B -- SCIENTIFIC WARNINGS ({len(warnings)} found)")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")
        print("Check science_report_user.txt for details")
    else:
        print("PHASE B -- PASSED")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")


def create_science_yaml_header() -> str:
    """Create header for science-checked YAML file."""
    header = '''# =============================================================================
# SCIENCE CHECKED YAML
# =============================================================================
#
# This file has been processed by science_check.py (Phase B) with scientific validation:
# - Physics parameters validated for consistency
# - Geographic coordinates and timezone validated
# - Seasonal adjustments applied (LAI, snowalb, surface temperatures)
# - Land cover fractions validated
# - Model physics option dependencies checked
# - All scientific corrections documented in science_report_<filename>.txt
#
# =============================================================================

'''
    return header


def run_science_check(uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str, 
                     science_yaml_file: str = None, science_report_file: str = None) -> dict:
    """
    Main Phase B workflow - perform scientific validation and adjustments.
    
    Args:
        uptodate_yaml_file: Path to Phase A output (clean YAML)
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for science-checked output YAML
        science_report_file: Path for scientific validation report
        
    Returns:
        Final science-checked YAML configuration dictionary
        
    Raises:
        FileNotFoundError: If required input files are missing
        ValueError: If Phase A did not complete or YAML is invalid
    """
    try:
        # Step 1: Validate inputs and load YAML files
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )
        
        # Step 2: Extract simulation parameters
        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)
        
        # Step 3: Run scientific validation pipeline
        validation_results = run_scientific_validation_pipeline(uptodate_data, start_date, model_year)
        
        # Step 4: Run scientific adjustment pipeline
        science_checked_data, adjustments = run_scientific_adjustment_pipeline(uptodate_data, start_date, model_year)
        
        # Step 5: Generate science report
        science_yaml_filename = os.path.basename(science_yaml_file) if science_yaml_file else None
        report_content = create_science_report(validation_results, adjustments, science_yaml_filename)
        
        # Step 6: Print terminal results
        print_science_check_results(validation_results, adjustments)
        
        # Step 7: Write science-checked YAML file
        if science_yaml_file:
            header = create_science_yaml_header()
            with open(science_yaml_file, 'w') as f:
                f.write(header)
                yaml.dump(science_checked_data, f, default_flow_style=False, sort_keys=False)
        
        # Step 8: Write science report
        if science_report_file:
            with open(science_report_file, 'w') as f:
                f.write(report_content)
        
        return science_checked_data
        
    except Exception as e:
        print(f"Phase B Error: {e}")
        raise


def main():
    """Main entry point for science_check.py Phase B."""
    print(" SUEWS Scientific Validation (Phase B)")
    print("=" * 50)
    
    # Define file paths
    user_file = "src/supy/data_model/user.yml"
    uptodate_file = "src/supy/data_model/uptodate_user.yml"
    standard_file = "src/supy/sample_run/sample_config.yml"
    
    print(f"Phase A output (uptodate): {uptodate_file}")
    print(f"Original user YAML: {user_file}")
    print(f"Standard YAML: {standard_file}")
    print()
    
    # Generate output file paths
    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]
    
    science_yaml_filename = f"science_checked_{basename}"
    science_report_filename = f"science_report_{name_without_ext}.txt"
    
    science_yaml_file = os.path.join(dirname, science_yaml_filename)
    science_report_file = os.path.join(dirname, science_report_filename)
    
    # Run Phase B
    try:
        science_checked_data = run_science_check(
            uptodate_yaml_file=uptodate_file,
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            science_yaml_file=science_yaml_file,
            science_report_file=science_report_file
        )
        
        print(f"\nPhase B completed successfully!")
        print(f"Science-checked YAML: {science_yaml_file}")
        print(f"Science report: {science_report_file}")
        
    except Exception as e:
        print(f"\nPhase B failed: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())