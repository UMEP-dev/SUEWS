"""
Phase C Pydantic Validation - Refactored to use ValidationReporter.

This module performs Pydantic model validation on YAML configurations
using the JSON-based ValidationReporter for structured reporting.
"""

import os
import re
from typing import Dict, List, Optional, Any, Tuple
from pathlib import Path

try:
    from .validation_reporter import ValidationReporter
    from .text_report_generator import TextReportGenerator
    from .phase_c_pydantic_report import PHASE_TITLES
except ImportError:
    # For direct execution
    from validation_reporter import ValidationReporter
    from text_report_generator import TextReportGenerator
    from phase_c_pydantic_report import PHASE_TITLES


def parse_pydantic_error(error: dict) -> Tuple[str, str, str, Optional[str]]:
    """
    Parse a single Pydantic error into structured components.

    Args:
        error: Pydantic error dictionary

    Returns:
        Tuple of (field_path, field_name, error_message, error_type)
    """
    error_type = error.get("type", "unknown")
    field_path = ".".join(str(loc) for loc in error.get("loc", []))
    error_msg = error.get("msg", "Unknown error")

    if not field_path:
        # Try to extract field path from error message
        field_match = re.search(r"Required field '(\w+)' has no value", error_msg)
        if field_match:
            field_name = field_match.group(1)
            if field_name in ["lat", "lon", "alt"]:
                field_path = f"sites[0].properties.{field_name}"
            else:
                field_path = f"model.{field_name}"
        else:
            field_path = "root_validation"
            field_name = "configuration"
    else:
        field_name = field_path.split(".")[-1] if "." in field_path else field_path

    # Build complete error message
    full_error_parts = [error_msg]

    if error_type != "unknown":
        type_parts = [f"type={error_type}"]
        if "input" in error:
            input_value = str(error.get("input", ""))
            if len(input_value) > 100:
                input_value = input_value[:97] + "..."
            type_parts.append(f"input_value={input_value}")
        if "input_type" in error:
            type_parts.append(f"input_type={error.get('input_type')}")
        full_error_parts.append(f"[{', '.join(type_parts)}]")

    if "url" in error:
        full_error_parts.append(f"For further information visit {error.get('url')}")

    complete_error_msg = " ".join(full_error_parts)

    return field_path, field_name, complete_error_msg, error_type


def parse_critical_validation_error(error_msg: str) -> List[Dict[str, str]]:
    """
    Parse combined critical validation error into individual issues.

    Args:
        error_msg: Combined error message string

    Returns:
        List of parsed error dictionaries
    """
    issues = []

    # Extract the part after "Critical validation failed: "
    if "Critical validation failed: " in error_msg:
        split_parts = error_msg.split("Critical validation failed: ", 1)
        combined_message = split_parts[1] if len(split_parts) > 1 else error_msg
    else:
        combined_message = error_msg

    individual_issues = [issue.strip() for issue in combined_message.split(";")]

    for issue in individual_issues:
        if not issue:
            continue

        # Determine the field name and path based on the issue content
        if "is set to null and will cause runtime crash" in issue:
            param_name = issue.split(" is set to null")[0]
            issues.append({
                "field_name": param_name,
                "field_path": f"model.physics.{param_name}",
                "message": issue,
                "type": "null_physics_parameter",
            })

        elif " → " in issue:
            # StorageHeat parameter format
            parts = issue.split(" → ", 1)
            if len(parts) == 2:
                site_part = parts[0].strip()
                param_part = parts[1].strip()
                site_name = (
                    site_part.split(":")[0].strip() if ":" in site_part else "Unknown"
                )
                field_name = param_part.split(" must be")[0].strip()
                issues.append({
                    "field_name": field_name,
                    "field_path": f"sites[{site_name}].properties.{field_name}",
                    "message": issue,
                    "type": "storage_heat_dependency",
                })

        elif "must be set" in issue and ":" in issue:
            # RSL parameter format
            parts = issue.split(":", 1)
            if len(parts) == 2:
                site_name = parts[0].strip()
                param_desc = parts[1].strip()
                if "faibldg" in param_desc:
                    issues.append({
                        "field_name": "bldgs.faibldg",
                        "field_path": f"sites[{site_name}].properties.land_cover.bldgs.faibldg",
                        "message": issue,
                        "type": "rsl_dependency",
                    })
                else:
                    issues.append({
                        "field_name": "RSL parameter",
                        "field_path": f"sites[{site_name}]",
                        "message": issue,
                        "type": "rsl_dependency",
                    })
        else:
            # Generic issue
            issues.append({
                "field_name": "validation issue",
                "field_path": "configuration",
                "message": issue,
                "type": "validation_error",
            })

    return issues


def build_phase_c_reporter(
    validation_error: Exception,
    mode: str = "public",
) -> ValidationReporter:
    """
    Build a ValidationReporter with Phase C Pydantic validation results.

    Args:
        validation_error: Pydantic ValidationError exception
        mode: Validation mode ('public' or 'dev')

    Returns:
        ValidationReporter with Phase C results
    """
    reporter = ValidationReporter()

    # Set metadata
    reporter.set_metadata("phase", "C")
    reporter.set_metadata("mode", mode)

    error_count = 0

    # Check if this is a Pydantic ValidationError with errors() method
    pydantic_errors = None
    if hasattr(validation_error, "errors"):
        errors_attr = validation_error.errors
        pydantic_errors = errors_attr() if callable(errors_attr) else errors_attr

    if pydantic_errors:
        # Process each Pydantic error
        for error in pydantic_errors:
            field_path, field_name, error_msg, error_type = parse_pydantic_error(error)

            # Check if this is a combined critical validation error
            if "Critical validation failed: " in error_msg:
                issues = parse_critical_validation_error(error_msg)
                for issue in issues:
                    error_count += 1
                    reporter.add_error({
                        "phase": "C",
                        "type": f"pydantic_{issue['type']}",
                        "field_path": issue["field_path"],
                        "message": issue["message"],
                        "details": {
                            "field_name": issue["field_name"],
                            "validation_type": issue["type"],
                            "requires_action": True,
                        },
                        "suggestions": [
                            "Review and correct the parameter value",
                            "Ensure all required dependencies are satisfied",
                        ],
                    })
            else:
                # Regular Pydantic error
                error_count += 1

                # Determine error category based on type
                if error_type in ["missing", "required"]:
                    error_category = "missing_required_field"
                elif error_type in ["type_error", "value_error"]:
                    error_category = "type_validation_error"
                elif error_type == "assertion_error":
                    error_category = "constraint_violation"
                else:
                    error_category = "validation_error"

                reporter.add_error({
                    "phase": "C",
                    "type": f"pydantic_{error_category}",
                    "field_path": field_path,
                    "message": error_msg,
                    "details": {
                        "field_name": field_name,
                        "error_type": error_type,
                        "input_value": error.get("input"),
                        "input_type": error.get("input_type"),
                        "requires_action": True,
                    },
                    "suggestions": [
                        "Check the field value and type",
                        "Ensure the value meets all constraints",
                        error.get("url") if "url" in error else None,
                    ],
                })
    else:
        # Handle non-Pydantic validation errors
        error_str = str(validation_error)

        # Check if this is a combined critical validation error
        if error_str.startswith("Critical validation failed: "):
            issues = parse_critical_validation_error(error_str)
            for issue in issues:
                error_count += 1
                reporter.add_error({
                    "phase": "C",
                    "type": f"pydantic_{issue['type']}",
                    "field_path": issue["field_path"],
                    "message": issue["message"],
                    "details": {
                        "field_name": issue["field_name"],
                        "validation_type": issue["type"],
                        "requires_action": True,
                    },
                    "suggestions": [
                        "Review and correct the parameter value",
                        "Ensure all required dependencies are satisfied",
                    ],
                })
        else:
            # Generic validation error
            error_count += 1
            reporter.add_error({
                "phase": "C",
                "type": "pydantic_validation_error",
                "field_path": "configuration",
                "message": error_str,
                "details": {"field_name": "general", "requires_action": True},
                "suggestions": [
                    "Review the validation error and fix the configuration"
                ],
            })

    # Add phase results summary
    reporter.add_phase_results(
        "C",
        {
            "pydantic_errors": error_count,
            "validation_mode": mode,
            "validation_passed": error_count == 0,
        },
    )

    return reporter


def generate_phase_c_text_report(
    json_data: Dict[str, Any], phase: str, mode: str
) -> str:
    """
    Generate Phase C specific text report from JSON data.

    This maintains the exact format of the original Phase C reports.
    """
    lines = []

    # Header - match original format exactly
    title = PHASE_TITLES.get(phase, "SUEWS Phase C (Pydantic Validation) Report")

    lines.append(f"# {title}")
    lines.append("# " + "=" * 50)
    lines.append(f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}")
    lines.append("# " + "=" * 50)
    lines.append("")

    # Extract Phase C specific data
    phase_c_errors = [e for e in json_data.get("errors", []) if e.get("phase") == "C"]

    # ACTION NEEDED section
    if phase_c_errors:
        lines.append("## ACTION NEEDED")
        lines.append(
            f"- Found ({len(phase_c_errors)}) critical Pydantic validation error(s):"
        )

        for error in phase_c_errors:
            field_name = error.get("details", {}).get("field_name", "unknown")
            message = error.get("message", "Unknown error")
            field_path = error.get("field_path", "")

            lines.append(f"-- {field_name}: {message}")
            if field_path and field_path != "configuration":
                lines.append(f"   Location: {field_path}")

        lines.append("")

    # Include previous phase results if this is a combined phase
    if len(phase) > 1:
        # Extract info from other phases for NO ACTION NEEDED section
        phase_a_info = [i for i in json_data.get("info", []) if i.get("phase") == "A"]
        phase_b_info = [i for i in json_data.get("info", []) if i.get("phase") == "B"]
        phase_b_warnings = [
            w for w in json_data.get("warnings", []) if w.get("phase") == "B"
        ]

        no_action_items = []

        # Phase A renamed parameters
        renamed = [i for i in phase_a_info if i.get("type") == "parameter_renamed"]
        if renamed:
            no_action_items.append(
                f"- Updated ({len(renamed)}) renamed parameter(s) to current standards:"
            )
            for item in renamed:
                old_name = item.get("details", {}).get("old_name", "")
                new_name = item.get("details", {}).get("new_name", "")
                no_action_items.append(f"-- {old_name} changed to {new_name}")

        # Phase A optional missing parameters
        optional = [
            i for i in phase_a_info if i.get("type") == "missing_optional_parameter"
        ]
        if optional:
            no_action_items.append(
                f"- Updated ({len(optional)}) optional missing parameter(s) with null values:"
            )
            for item in optional:
                param_name = item.get("details", {}).get("parameter_name", "")
                no_action_items.append(
                    f"-- {param_name} added to the updated YAML and set to null"
                )

        # Phase B scientific warnings
        if phase_b_warnings:
            no_action_items.append(
                f"- Found ({len(phase_b_warnings)}) scientific warning(s) for information:"
            )
            for warning in phase_b_warnings:
                field_path = warning.get("field_path", "")
                message = warning.get("message", "")
                no_action_items.append(f"-- {field_path}: {message}")

        if no_action_items:
            lines.append("## NO ACTION NEEDED")
            lines.extend(no_action_items)
            lines.append("")

    # If no errors
    if not phase_c_errors:
        lines.append(f"Phase {phase} passed")
        lines.append("")

    # Footer
    lines.append("# " + "=" * 50)

    return "\n".join(lines)


def generate_phase_c_report_refactored(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
    existing_reporter: ValidationReporter = None,
) -> ValidationReporter:
    """
    Refactored version of generate_phase_c_report using ValidationReporter.

    This function performs Phase C validation and returns/updates a ValidationReporter
    containing structured validation results. It maintains backward compatibility
    by also generating the same text reports as before.

    Args:
        validation_error: Pydantic ValidationError exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
        mode: Validation mode ('public' or 'dev')
        phase_a_report_file: Path to Phase A report (for backward compatibility)
        phases_run: List of phases run (e.g., ['A', 'B', 'C'])
        existing_reporter: Optional existing reporter to merge with

    Returns:
        ValidationReporter with Phase C results (or merged results)
    """
    # Build Phase C reporter
    phase_c_reporter = build_phase_c_reporter(validation_error, mode)

    # Set metadata
    phase_c_reporter.set_metadata("input_file", input_yaml_file)
    phase_c_reporter.set_metadata("report_file", output_report_file)

    # If we have an existing reporter (from previous phases), merge
    if existing_reporter:
        existing_reporter.merge(phase_c_reporter)
        reporter = existing_reporter
    else:
        reporter = phase_c_reporter

    # Determine phase string
    phase_str = "".join(phases_run) if phases_run else "C"

    # Generate text report from JSON structure
    text_report = generate_phase_c_text_report(
        reporter.get_json_report(), phase_str, mode
    )

    # Write text report
    with open(output_report_file, "w") as f:
        f.write(text_report)

    # Also save JSON report alongside text report
    json_file = output_report_file.replace(".txt", ".json")
    reporter.save_json_report(Path(json_file))

    return reporter


def generate_fallback_report_refactored(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
) -> ValidationReporter:
    """
    Generate fallback report using ValidationReporter when structured report generation fails.

    Args:
        validation_error: The validation error that occurred
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
        mode: Validation mode
        phase_a_report_file: Path to Phase A report (unused but kept for compatibility)
        phases_run: List of phases run

    Returns:
        ValidationReporter with fallback error
    """
    reporter = ValidationReporter()

    phase_str = "".join(phases_run) if phases_run else "C"

    reporter.set_metadata("phase", phase_str)
    reporter.set_metadata("mode", mode)
    reporter.set_metadata("input_file", input_yaml_file)
    reporter.set_metadata("report_file", output_report_file)

    # Add the error
    reporter.add_error({
        "phase": "C",
        "type": "pydantic_fallback_error",
        "field_path": "configuration",
        "message": f"Validation error: {str(validation_error)}",
        "details": {
            "field_name": "general",
            "error_type": "fallback",
            "requires_action": True,
        },
        "suggestions": [
            "Review and fix validation errors above",
            f"Check input file: {input_yaml_file}",
        ],
    })

    # Generate text report
    title = PHASE_TITLES.get(phase_str, "SUEWS Phase C (Pydantic Validation) Report")
    mode_title = "Public" if mode.lower() == "public" else mode.title()

    text_report = f"""# {title}
# {"=" * 50}
# Mode: {mode_title}
# {"=" * 50}

## ACTION NEEDED
- Found (1) critical Pydantic validation error(s):
-- validation_error: {str(validation_error)}
   Suggested fix: Review and fix validation errors above
   Location: {input_yaml_file}

# {"=" * 50}
"""

    # Write text report
    with open(output_report_file, "w") as f:
        f.write(text_report)

    # Also save JSON report
    json_file = output_report_file.replace(".txt", ".json")
    reporter.save_json_report(Path(json_file))

    return reporter


if __name__ == "__main__":
    # Example usage for testing
    print("Phase C Pydantic Validation - Refactored")
    print("This module should be imported and used by the validation pipeline.")
