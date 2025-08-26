"""
Phase A Parameter Update - Refactored to use ValidationReporter.

This module handles Phase A validation (up-to-date YAML check) using the
JSON-based ValidationReporter for structured reporting.
"""

import yaml
import os
from typing import List, Tuple, Dict, Any, Optional
from pathlib import Path

try:
    from .validation_reporter import ValidationReporter
    from .text_report_generator import TextReportGenerator
    from .phase_a_parameter_update import (
        RENAMED_PARAMS,
        PHYSICS_OPTIONS,
        handle_renamed_parameters,
        is_physics_option,
        get_allowed_nested_sections_in_properties,
        find_missing_parameters,
        find_extra_parameters,
        create_uptodate_yaml_with_missing_params,
        create_uptodate_yaml_header,
        categorise_extra_parameters,
    )
except ImportError:
    # For direct execution
    from validation_reporter import ValidationReporter
    from text_report_generator import TextReportGenerator
    from phase_a_parameter_update import (
        RENAMED_PARAMS,
        PHYSICS_OPTIONS,
        handle_renamed_parameters,
        is_physics_option,
        get_allowed_nested_sections_in_properties,
        find_missing_parameters,
        find_extra_parameters,
        create_uptodate_yaml_with_missing_params,
        create_uptodate_yaml_header,
        categorise_extra_parameters,
    )


def build_phase_a_reporter(
    missing_params: List[Tuple[str, Any, bool]],
    renamed_replacements: List[Tuple[str, str]],
    extra_params: List[str],
    mode: str = "public",
) -> ValidationReporter:
    """
    Build a ValidationReporter with Phase A validation results.
    
    Args:
        missing_params: List of (path, standard_value, is_physics) tuples
        renamed_replacements: List of (old_name, new_name) tuples
        extra_params: List of extra parameter paths
        mode: Validation mode ('public' or 'dev')
    
    Returns:
        ValidationReporter with Phase A results
    """
    reporter = ValidationReporter()
    
    # Set metadata
    reporter.set_metadata("phase", "A")
    reporter.set_metadata("mode", mode)
    
    # Process missing parameters
    critical_count = 0
    optional_count = 0
    
    for param_path, standard_value, is_physics in missing_params:
        param_name = param_path.split(".")[-1]
        
        if is_physics:
            # Critical physics parameter - requires action
            critical_count += 1
            reporter.add_error({
                "phase": "A",
                "type": "missing_required_parameter",
                "field_path": param_path,
                "message": f"Missing critical physics parameter: {param_name}",
                "details": {
                    "parameter_name": param_name,
                    "is_physics_option": True,
                    "standard_value": standard_value,
                    "requires_action": True
                },
                "suggestions": [
                    f"Add {param_name} to your YAML configuration",
                    f"Consult documentation for valid values: https://suews.readthedocs.io/en/latest"
                ]
            })
        else:
            # Optional parameter - informational
            optional_count += 1
            reporter.add_info({
                "phase": "A",
                "type": "missing_optional_parameter",
                "field_path": param_path,
                "message": f"Optional parameter {param_name} missing, will be set to null",
                "details": {
                    "parameter_name": param_name,
                    "is_physics_option": False,
                    "standard_value": standard_value,
                    "default_value": None
                }
            })
    
    # Process renamed parameters
    for old_name, new_name in renamed_replacements:
        reporter.add_info({
            "phase": "A",
            "type": "parameter_renamed",
            "field_path": f"model.physics.{old_name}",
            "message": f"Parameter renamed: {old_name} → {new_name}",
            "details": {
                "old_name": old_name,
                "new_name": new_name,
                "auto_corrected": True
            }
        })
    
    # Process extra parameters
    if extra_params:
        categorised = categorise_extra_parameters(extra_params)
        forbidden_extras = categorised.get("ACTION_NEEDED", [])
        allowed_extras = categorised.get("NO_ACTION_NEEDED", [])
        
        # Forbidden locations require action
        for param_path in forbidden_extras:
            param_name = param_path.split(".")[-1]
            
            if mode == "public":
                reporter.add_error({
                    "phase": "A",
                    "type": "extra_parameter_forbidden",
                    "field_path": param_path,
                    "message": f"Extra parameter not allowed in public mode: {param_name}",
                    "details": {
                        "parameter_name": param_name,
                        "location": param_path,
                        "forbidden_in_mode": "public",
                        "requires_action": True
                    },
                    "suggestions": [
                        "Remove this parameter from your configuration",
                        "Switch to dev mode if this parameter is needed"
                    ]
                })
            else:
                reporter.add_warning({
                    "phase": "A",
                    "type": "extra_parameter_forbidden_location",
                    "field_path": param_path,
                    "message": f"Extra parameter in forbidden location: {param_name}",
                    "details": {
                        "parameter_name": param_name,
                        "location": param_path,
                        "requires_action": True
                    },
                    "suggestions": [
                        "Remove this parameter",
                        "Move to an allowed nested section",
                        "Update data model to allow extra parameters here"
                    ]
                })
        
        # Allowed locations (dev mode only)
        for param_path in allowed_extras:
            if mode != "public":
                param_name = param_path.split(".")[-1]
                reporter.add_info({
                    "phase": "A",
                    "type": "extra_parameter_allowed",
                    "field_path": param_path,
                    "message": f"Extra parameter found (allowed in dev mode): {param_name}",
                    "details": {
                        "parameter_name": param_name,
                        "location": param_path,
                        "allowed_in_mode": "dev"
                    }
                })
            else:
                # In public mode, all extras are errors (already handled above)
                param_name = param_path.split(".")[-1]
                reporter.add_error({
                    "phase": "A",
                    "type": "extra_parameter_forbidden",
                    "field_path": param_path,
                    "message": f"Extra parameter not allowed in public mode: {param_name}",
                    "details": {
                        "parameter_name": param_name,
                        "location": param_path,
                        "forbidden_in_mode": "public",
                        "requires_action": True
                    },
                    "suggestions": [
                        "Remove this parameter from your configuration",
                        "Switch to dev mode if this parameter is needed"
                    ]
                })
    
    # Add phase results summary
    reporter.add_phase_results("A", {
        "missing_critical": critical_count,
        "missing_optional": optional_count,
        "renamed_parameters": len(renamed_replacements),
        "extra_parameters": len(extra_params),
        "validation_mode": mode
    })
    
    return reporter


def annotate_missing_parameters_refactored(
    user_file: str,
    standard_file: str,
    uptodate_file: Optional[str] = None,
    report_file: Optional[str] = None,
    mode: str = "public",
    phase: str = "A",
) -> ValidationReporter:
    """
    Refactored version of annotate_missing_parameters using ValidationReporter.
    
    This function performs Phase A validation and returns a ValidationReporter
    containing structured validation results. It maintains backward compatibility
    by also generating the same text reports and YAML files as before.
    
    Args:
        user_file: Path to user's YAML configuration
        standard_file: Path to standard YAML configuration
        uptodate_file: Optional path for updated YAML output
        report_file: Optional path for text report output
        mode: Validation mode ('public' or 'dev')
        phase: Phase identifier (default 'A')
        
    Returns:
        ValidationReporter with Phase A validation results
    """
    # Initialize reporter
    reporter = ValidationReporter()
    reporter.set_metadata("source_file", user_file)
    reporter.set_metadata("standard_file", standard_file)
    
    try:
        # Load and process YAML files
        with open(user_file, "r") as f:
            original_yaml_content = f.read()
        
        # Handle renamed parameters
        updated_yaml_content, renamed_replacements = handle_renamed_parameters(
            original_yaml_content
        )
        
        # Parse YAML
        user_data = yaml.safe_load(updated_yaml_content)
        with open(standard_file, "r") as f:
            standard_data = yaml.safe_load(f)
            
    except FileNotFoundError as e:
        reporter.add_error({
            "phase": "A",
            "type": "file_not_found",
            "field_path": "",
            "message": f"File not found: {e}",
            "severity": "critical"
        })
        return reporter
        
    except yaml.YAMLError as e:
        reporter.add_error({
            "phase": "A",
            "type": "yaml_parse_error",
            "field_path": "",
            "message": f"Invalid YAML: {e}",
            "severity": "critical"
        })
        return reporter
    
    # Find discrepancies
    missing_params = find_missing_parameters(user_data, standard_data)
    extra_params = find_extra_parameters(user_data, standard_data)
    
    # Build reporter with results
    reporter = build_phase_a_reporter(
        missing_params,
        renamed_replacements,
        extra_params,
        mode
    )
    
    # Generate updated YAML content
    if missing_params or renamed_replacements or extra_params:
        uptodate_content = create_uptodate_yaml_with_missing_params(
            updated_yaml_content, missing_params, extra_params, mode
        )
    else:
        uptodate_content = create_uptodate_yaml_header() + updated_yaml_content
    
    # Write updated YAML file if path provided
    if uptodate_file:
        with open(uptodate_file, "w") as f:
            f.write(uptodate_content)
        reporter.set_metadata("uptodate_file", uptodate_file)
    
    # Generate and write text report if path provided
    if report_file:
        # Generate text report from JSON structure
        text_generator = TextReportGenerator()
        
        # We need to enhance the text generator for Phase A specific format
        # For now, generate using the enhanced generator
        text_report = generate_phase_a_text_report(reporter.get_json_report(), phase, mode)
        
        with open(report_file, "w") as f:
            f.write(text_report)
        reporter.set_metadata("report_file", report_file)
    
    # Also save JSON report alongside text report
    if report_file:
        json_file = report_file.replace('.txt', '.json')
        reporter.save_json_report(Path(json_file))
    
    # Print terminal output (maintain existing behavior)
    print_phase_a_terminal_output(reporter)
    
    return reporter


def generate_phase_a_text_report(json_data: Dict[str, Any], phase: str, mode: str) -> str:
    """
    Generate Phase A specific text report from JSON data.
    
    This maintains the exact format of the original Phase A reports.
    """
    lines = []
    
    # Header - match original format exactly
    lines.append("# SUEWS - Phase A (Up-to-date YAML check) Report")
    lines.append("# " + "=" * 50)
    lines.append(f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}")
    lines.append("# " + "=" * 50)
    lines.append("")
    
    # Extract counts from phase results
    phase_results = json_data.get("phases", {}).get("A", {}).get("results", {})
    critical_count = phase_results.get("missing_critical", 0)
    optional_count = phase_results.get("missing_optional", 0)
    renamed_count = phase_results.get("renamed_parameters", 0)
    extra_count = phase_results.get("extra_parameters", 0)
    
    # Determine if we have action items
    has_action_items = critical_count > 0 or (mode == "public" and extra_count > 0)
    
    # ACTION NEEDED section
    if has_action_items:
        lines.append("## ACTION NEEDED")
        
        # Critical missing parameters
        if critical_count > 0:
            lines.append(f"- Found ({critical_count}) critical missing parameter(s):")
            for error in json_data.get("errors", []):
                if error.get("type") == "missing_required_parameter":
                    param_name = error.get("details", {}).get("parameter_name", "unknown")
                    lines.append(f"-- {param_name} has been added to the updated YAML and set to null")
                    lines.append(f"   Suggested fix: Set appropriate value based on SUEWS documentation -- https://suews.readthedocs.io/latest/")
            lines.append("")
        
        # Extra parameters in public mode
        if mode == "public" and extra_count > 0:
            lines.append(f"- Found ({extra_count}) not allowed extra parameter name(s):")
            for error in json_data.get("errors", []):
                if error.get("type") == "extra_parameter_forbidden":
                    param_name = error.get("details", {}).get("parameter_name", "unknown")
                    field_path = error.get("field_path", "")
                    lines.append(f"-- {param_name} at level {field_path}")
                    lines.append(
                        "   Suggested fix: You selected Public mode. Consider either to switch to Dev mode, "
                        "or remove this extra parameter since this is not in the standard yaml."
                    )
            lines.append("")
    
    # NO ACTION NEEDED section
    has_no_action_items = optional_count > 0 or renamed_count > 0 or (mode != "public" and extra_count > 0)
    
    if has_no_action_items:
        lines.append("## NO ACTION NEEDED")
        
        # Optional missing parameters
        if optional_count > 0:
            lines.append(f"- Updated ({optional_count}) optional missing parameter(s) with null values:")
            for info in json_data.get("info", []):
                if info.get("type") == "missing_optional_parameter":
                    param_name = info.get("details", {}).get("parameter_name", "unknown")
                    lines.append(f"-- {param_name} added to the updated YAML and set to null")
            lines.append("")
        
        # Renamed parameters
        if renamed_count > 0:
            lines.append(f"- Updated ({renamed_count}) renamed parameter(s):")
            for info in json_data.get("info", []):
                if info.get("type") == "parameter_renamed":
                    old_name = info.get("details", {}).get("old_name", "unknown")
                    new_name = info.get("details", {}).get("new_name", "unknown")
                    lines.append(f"-- {old_name} changed to {new_name}")
            lines.append("")
        
        # Extra parameters in dev mode
        if mode != "public" and extra_count > 0:
            allowed_extras = [
                info for info in json_data.get("info", [])
                if info.get("type") == "extra_parameter_allowed"
            ]
            if allowed_extras:
                lines.append(f"- Found ({len(allowed_extras)}) parameter(s) not in standard:")
                for info in allowed_extras:
                    param_name = info.get("details", {}).get("parameter_name", "unknown")
                    field_path = info.get("field_path", "")
                    lines.append(f"-- {param_name} at level {field_path}")
                lines.append("")
    
    # Footer
    lines.append("# " + "=" * 50)
    
    return "\n".join(lines)


def print_phase_a_terminal_output(reporter: ValidationReporter) -> None:
    """Print terminal output for Phase A validation."""
    json_data = reporter.get_json_report()
    
    # Check for critical errors
    critical_errors = [
        e for e in json_data.get("errors", [])
        if e.get("type") == "missing_required_parameter"
    ]
    
    if critical_errors:
        print(f"Action needed: CRITICAL parameters missing:")
        for error in critical_errors:
            param_name = error.get("details", {}).get("parameter_name", "unknown")
            print(f"  - {param_name}")
        print("")
        
        # Get report file location from metadata
        report_file = json_data.get("metadata", {}).get("report_file", "report file")
        if report_file != "report file":
            report_filename = os.path.basename(report_file)
            report_location = os.path.dirname(report_file) or "current directory"
            print(
                f"Next step: Check {report_filename} report file located {report_location} "
                "on what to do to resolve this"
            )
        else:
            print("Next step: Check the report file for resolution steps")
    else:
        print("PHASE A -- PASSED")