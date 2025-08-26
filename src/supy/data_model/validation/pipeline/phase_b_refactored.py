"""
Phase B Science Validation - Refactored to use ValidationReporter.

This module performs scientific validation and consistency checks on YAML configurations
using the JSON-based ValidationReporter for structured reporting.
"""

import os
import yaml
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
from copy import deepcopy
import re

try:
    from .validation_reporter import ValidationReporter
    from .text_report_generator import TextReportGenerator
    from .phase_b_science_check import (
        ValidationResult,
        ScientificAdjustment,
        DLSCheck,
        get_value_safe,
        validate_phase_b_inputs,
        extract_simulation_parameters,
        validate_physics_parameters,
        validate_model_option_dependencies,
        validate_land_cover_consistency,
        validate_geographic_parameters,
        run_scientific_validation_pipeline,
        get_mean_monthly_air_temperature,
        adjust_surface_temperatures,
        adjust_land_cover_fractions,
        adjust_model_dependent_nullification,
        adjust_seasonal_parameters,
        run_scientific_adjustment_pipeline,
        create_science_yaml_header,
        print_critical_halt_message,
        print_science_check_results,
    )
except ImportError:
    # For direct execution
    from validation_reporter import ValidationReporter
    from text_report_generator import TextReportGenerator
    from phase_b_science_check import (
        ValidationResult,
        ScientificAdjustment,
        DLSCheck,
        get_value_safe,
        validate_phase_b_inputs,
        extract_simulation_parameters,
        validate_physics_parameters,
        validate_model_option_dependencies,
        validate_land_cover_consistency,
        validate_geographic_parameters,
        run_scientific_validation_pipeline,
        get_mean_monthly_air_temperature,
        adjust_surface_temperatures,
        adjust_land_cover_fractions,
        adjust_model_dependent_nullification,
        adjust_seasonal_parameters,
        run_scientific_adjustment_pipeline,
        create_science_yaml_header,
        print_critical_halt_message,
        print_science_check_results,
    )


def build_phase_b_reporter(
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    mode: str = "public",
) -> ValidationReporter:
    """
    Build a ValidationReporter with Phase B validation results.
    
    Args:
        validation_results: List of ValidationResult objects from science checks
        adjustments: List of ScientificAdjustment objects for auto-corrections
        mode: Validation mode ('public' or 'dev')
    
    Returns:
        ValidationReporter with Phase B results
    """
    reporter = ValidationReporter()
    
    # Set metadata
    reporter.set_metadata("phase", "B")
    reporter.set_metadata("mode", mode)
    
    # Process validation results
    error_count = 0
    warning_count = 0
    pass_count = 0
    
    for result in validation_results:
        if result.status == "ERROR":
            error_count += 1
            reporter.add_error({
                "phase": "B",
                "type": f"science_{result.category.lower()}_error",
                "field_path": result.parameter,
                "message": result.message,
                "details": {
                    "category": result.category,
                    "site_index": result.site_index,
                    "suggested_value": result.suggested_value,
                    "requires_action": True
                },
                "suggestions": [result.suggested_value] if result.suggested_value else []
            })
        
        elif result.status == "WARNING":
            warning_count += 1
            reporter.add_warning({
                "phase": "B",
                "type": f"science_{result.category.lower()}_warning",
                "field_path": result.parameter,
                "message": result.message,
                "details": {
                    "category": result.category,
                    "site_index": result.site_index,
                    "suggested_value": result.suggested_value,
                    "requires_action": False
                },
                "suggestions": [result.suggested_value] if result.suggested_value else []
            })
        
        elif result.status == "PASS":
            pass_count += 1
            # Don't add individual pass items to reduce clutter
            # Just track in phase results
    
    # Process adjustments as info items
    for adjustment in adjustments:
        details = {
            "parameter": adjustment.parameter,
            "site_index": adjustment.site_index,
            "old_value": adjustment.old_value,
            "new_value": adjustment.new_value,
            "reason": adjustment.reason,
            "auto_corrected": True
        }
        
        # Determine adjustment type based on parameter
        if "temperature" in adjustment.parameter or "tsfc" in adjustment.parameter or "tin" in adjustment.parameter:
            adjustment_type = "temperature_adjustment"
        elif "lai" in adjustment.parameter:
            adjustment_type = "seasonal_lai_adjustment"
        elif "snowalb" in adjustment.parameter:
            adjustment_type = "seasonal_snow_adjustment"
        elif "sfr" in adjustment.parameter:
            adjustment_type = "land_cover_fraction_correction"
        elif "startdls" in adjustment.parameter or "enddls" in adjustment.parameter:
            adjustment_type = "daylight_saving_calculation"
        elif "timezone" in adjustment.parameter:
            adjustment_type = "timezone_calculation"
        elif "stebbs" in adjustment.parameter:
            adjustment_type = "model_dependent_nullification"
        else:
            adjustment_type = "scientific_adjustment"
        
        reporter.add_info({
            "phase": "B",
            "type": adjustment_type,
            "field_path": adjustment.parameter,
            "message": f"Applied automatic adjustment: {adjustment.reason}",
            "details": details
        })
    
    # Add phase results summary
    reporter.add_phase_results("B", {
        "total_validations": len(validation_results),
        "errors": error_count,
        "warnings": warning_count,
        "passed": pass_count,
        "adjustments_applied": len(adjustments),
        "validation_mode": mode,
        "categories_checked": list(set(r.category for r in validation_results))
    })
    
    return reporter


def generate_phase_b_text_report(json_data: Dict[str, Any], phase: str, mode: str) -> str:
    """
    Generate Phase B specific text report from JSON data.
    
    This maintains the exact format of the original Phase B reports.
    """
    lines = []
    
    # Header - match original format exactly
    phase_titles = {
        "B": "SUEWS - Phase B (Scientific Validation) Report",
        "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
        "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
        "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report",
    }
    
    title = phase_titles.get(phase, "SUEWS Scientific Validation Report")
    
    lines.append(f"# {title}")
    lines.append("# " + "=" * 50)
    lines.append(f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}")
    lines.append("# " + "=" * 50)
    lines.append("")
    
    # Extract Phase B specific data
    phase_b_errors = [e for e in json_data.get("errors", []) if e.get("phase") == "B"]
    phase_b_warnings = [w for w in json_data.get("warnings", []) if w.get("phase") == "B"]
    phase_b_info = [i for i in json_data.get("info", []) if i.get("phase") == "B"]
    
    # ACTION NEEDED section
    if phase_b_errors:
        lines.append("## ACTION NEEDED")
        lines.append(f"- Found ({len(phase_b_errors)}) critical scientific parameter error(s):")
        
        for error in phase_b_errors:
            site_index = error.get("details", {}).get("site_index")
            site_ref = f" at site [{site_index}]" if site_index is not None else ""
            field_path = error.get("field_path", "unknown")
            message = error.get("message", "")
            
            lines.append(f"-- {field_path}{site_ref}: {message}")
            
            suggestions = error.get("suggestions", [])
            if suggestions and suggestions[0]:
                lines.append(f"   Suggested fix: {suggestions[0]}")
        
        lines.append("")
    
    # NO ACTION NEEDED section
    lines.append("## NO ACTION NEEDED")
    
    # Adjustments (info items with auto_corrected flag)
    adjustments = [i for i in phase_b_info if i.get("details", {}).get("auto_corrected")]
    
    if adjustments:
        # Count total parameters changed
        total_params_changed = 0
        for adj in adjustments:
            old_value = adj.get("details", {}).get("old_value", "")
            reason = adj.get("details", {}).get("reason", "")
            
            if "temperature, tsfc, tin" in str(old_value):
                total_params_changed += 3
            elif "nullified" in reason:
                match = re.search(r"nullified (\d+)", reason)
                if match:
                    total_params_changed += int(match.group(1))
                else:
                    total_params_changed += 1
            else:
                total_params_changed += 1
        
        lines.append(f"- Updated ({total_params_changed}) parameter(s):")
        
        for adj in adjustments:
            site_index = adj.get("details", {}).get("site_index")
            site_ref = f" at site [{site_index}]" if site_index is not None else ""
            
            field_path = adj.get("field_path", "")
            old_value = adj.get("details", {}).get("old_value", "")
            new_value = adj.get("details", {}).get("new_value", "")
            reason = adj.get("details", {}).get("reason", "")
            
            lines.append(f"-- {field_path}{site_ref}: {old_value} → {new_value} ({reason})")
        
        lines.append("")
    
    # Warnings
    if phase_b_warnings:
        lines.append(f"- Revise ({len(phase_b_warnings)}) warnings:")
        for warning in phase_b_warnings:
            site_index = warning.get("details", {}).get("site_index")
            site_ref = f" at site [{site_index}]" if site_index is not None else ""
            field_path = warning.get("field_path", "unknown")
            message = warning.get("message", "")
            
            lines.append(f"-- {field_path}{site_ref}: {message}")
        lines.append("")
    
    # If no errors, warnings, or adjustments
    if not phase_b_errors and not phase_b_warnings and not adjustments:
        lines.append("- All scientific validations passed")
        lines.append("- Model physics parameters are consistent")
        lines.append("- Geographic parameters are valid")
        lines.append("")
    
    # Footer
    lines.append("# " + "=" * 50)
    
    return "\n".join(lines)


def run_science_check_refactored(
    uptodate_yaml_file: str,
    user_yaml_file: str,
    standard_yaml_file: str,
    science_yaml_file: str = None,
    science_report_file: str = None,
    phase_a_report_file: str = None,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
) -> Tuple[dict, ValidationReporter]:
    """
    Refactored version of run_science_check using ValidationReporter.
    
    This function performs Phase B validation and returns a ValidationReporter
    containing structured validation results. It maintains backward compatibility
    by also generating the same text reports and YAML files as before.
    
    Args:
        uptodate_yaml_file: Path to Phase A output (clean YAML)
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for science-checked output YAML
        science_report_file: Path for scientific validation report
        phase_a_report_file: Path to Phase A report file (if available)
        phase_a_performed: Whether Phase A was performed before Phase B
        mode: Validation mode ('public' or 'dev')
        phase: Phase identifier
        
    Returns:
        Tuple of (science-checked YAML data, ValidationReporter)
    """
    # Initialize reporter
    reporter = ValidationReporter()
    reporter.set_metadata("source_file", user_yaml_file)
    reporter.set_metadata("uptodate_file", uptodate_yaml_file)
    reporter.set_metadata("standard_file", standard_yaml_file)
    
    try:
        # Validate inputs and load YAML files
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )
        
        # Extract simulation parameters
        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)
        
        # Run scientific validation pipeline
        validation_results = run_scientific_validation_pipeline(
            uptodate_data, start_date, model_year
        )
        
        # Check for critical errors
        critical_errors = [r for r in validation_results if r.status == "ERROR"]
        
        if not critical_errors:
            # Apply scientific adjustments if no critical errors
            science_checked_data, adjustments = run_scientific_adjustment_pipeline(
                uptodate_data, start_date, model_year
            )
        else:
            # Don't apply adjustments if there are critical errors
            science_checked_data = deepcopy(uptodate_data)
            adjustments = []
        
        # Build reporter with results
        reporter = build_phase_b_reporter(
            validation_results,
            adjustments,
            mode
        )
        
    except FileNotFoundError as e:
        reporter.add_error({
            "phase": "B",
            "type": "file_not_found",
            "field_path": "",
            "message": f"File not found: {e}",
            "severity": "critical"
        })
        return {}, reporter
        
    except ValueError as e:
        reporter.add_error({
            "phase": "B",
            "type": "validation_error",
            "field_path": "",
            "message": str(e),
            "severity": "critical"
        })
        return uptodate_data if 'uptodate_data' in locals() else {}, reporter
    
    # Generate and write text report if path provided
    if science_report_file:
        # Generate text report from JSON structure
        text_report = generate_phase_b_text_report(reporter.get_json_report(), phase, mode)
        
        with open(science_report_file, "w") as f:
            f.write(text_report)
        reporter.set_metadata("report_file", science_report_file)
    
    # Also save JSON report alongside text report
    if science_report_file:
        json_file = science_report_file.replace('.txt', '.json')
        reporter.save_json_report(Path(json_file))
    
    # Print terminal output (maintain existing behavior)
    if critical_errors:
        print_critical_halt_message(critical_errors)
        raise ValueError("Critical scientific errors detected - Phase B halted")
    
    print_science_check_results(validation_results, adjustments)
    
    # Write science-checked YAML if requested and no critical errors
    if science_yaml_file and not critical_errors:
        header = create_science_yaml_header(phase_a_performed)
        with open(science_yaml_file, "w") as f:
            f.write(header)
            yaml.dump(
                science_checked_data, f, default_flow_style=False, sort_keys=False
            )
        reporter.set_metadata("science_yaml_file", science_yaml_file)
    
    return science_checked_data, reporter


def main():
    """Main entry point for refactored Phase B."""
    from pathlib import Path
    
    print(" SUEWS Scientific Validation (Phase B) - Refactored")
    print("=" * 50)
    
    user_file = "src/supy/data_model/user.yml"
    uptodate_file = "src/supy/data_model/uptodate_user.yml"
    standard_file = "src/supy/sample_data/sample_config.yml"
    
    print(f"Phase A output (uptodate): {uptodate_file}")
    print(f"Original user YAML: {user_file}")
    print(f"Standard YAML: {standard_file}")
    print()
    
    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]
    
    science_yaml_filename = f"science_checked_{basename}"
    science_report_filename = f"science_report_{name_without_ext}.txt"
    
    science_yaml_file = os.path.join(dirname, science_yaml_filename)
    science_report_file = os.path.join(dirname, science_report_filename)
    
    try:
        science_checked_data, reporter = run_science_check_refactored(
            uptodate_yaml_file=uptodate_file,
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            science_yaml_file=science_yaml_file,
            science_report_file=science_report_file,
            phase_a_performed=True,
            phase="B",
        )
        
        print(f"\nPhase B completed successfully!")
        print(f"Science-checked YAML: {science_yaml_file}")
        print(f"Science report: {science_report_file}")
        
        # Display JSON summary
        json_data = reporter.get_json_report()
        print(f"\nValidation Summary:")
        print(f"  - Errors: {json_data['summary']['total_errors']}")
        print(f"  - Warnings: {json_data['summary']['total_warnings']}")
        print(f"  - Info: {json_data['summary']['total_info']}")
        print(f"  - Validation Passed: {json_data['summary']['validation_passed']}")
        
    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            # Critical errors already printed by print_critical_halt_message
            return 1
        else:
            print(f"\nPhase B failed: {e}")
            return 1
    except Exception as e:
        print(f"\nPhase B failed with unexpected error: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())