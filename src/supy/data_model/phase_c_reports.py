"""
Phase C Report Generation

Standalone module for generating Phase C validation reports in ACTION NEEDED format.
This module is separate from core.py to minimize merge conflicts with master branch.
"""

import os


def generate_phase_c_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "user",
    phase_a_report_file: str = None,
    phases_run: list = None,
) -> None:
    """
    Generate Phase C validation report following Phase B consolidation pattern.

    Args:
        validation_error: The Pydantic validation exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
        mode: Processing mode ('user' or 'dev')
        phase_a_report_file: Path to Phase A or Phase B report file for consolidation (optional)
    """
    report_lines = []

    # Generate phase-specific title
    if phases_run:
        phase_str = "".join(phases_run)
    else:
        phase_str = "C"  # Default to Phase C only
    
    phase_titles = {
        "A": "SUEWS - Phase A (Up-to-date YAML check) Report",
        "B": "SUEWS - Phase B (Scientific Validation) Report", 
        "C": "SUEWS - Phase C (Pydantic Validation) Report",
        "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
        "AC": "SUEWS - Phase AC (Up-to-date YAML check and Pydantic Validation) Report", 
        "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
        "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report"
    }
    
    title = phase_titles.get(phase_str, "SUEWS Phase C (Pydantic Validation) Report")

    # Header (matching Phase B format)
    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(f"# Mode: {mode.title()}")
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    # Extract Phase A/B information if available (following Phase B pattern)
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []
    phase_b_adjustments = []
    report_type = "unknown"

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                report_content = f.read()

            # Detect report type
            if "SUEWS Scientific Validation Report" in report_content:
                report_type = "phase_b"
            elif "SUEWS Configuration Analysis Report" in report_content:
                report_type = "phase_a"

            # Parse Phase A or Phase B report for relevant information
            lines = report_content.split("\n")
            current_section = None

            for line in lines:
                line = line.strip()
                if report_type == "phase_a":
                    # Parse Phase A content
                    if "Updated (" in line and "renamed parameter" in line:
                        current_section = "renames"
                    elif "Updated (" in line and "optional missing parameter" in line:
                        current_section = "optional"
                    elif "parameter(s) not in standard" in line:
                        current_section = "not_standard"
                    elif line.startswith("--") and current_section == "renames":
                        phase_a_renames.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "optional":
                        phase_a_optional_missing.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "not_standard":
                        phase_a_not_in_standard.append(line[2:].strip())
                elif report_type == "phase_b":
                    # Parse Phase B content (scientific validation)
                    if "Updated (" in line and "renamed parameter" in line:
                        current_section = "renames"
                    elif "Updated (" in line and "optional missing parameter" in line:
                        current_section = "optional"
                    elif "parameter(s) not in standard" in line:
                        current_section = "not_standard"
                    elif "scientific warning" in line:
                        current_section = "warnings"
                    elif line.startswith("--") and current_section == "renames":
                        phase_a_renames.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "optional":
                        phase_a_optional_missing.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "not_standard":
                        phase_a_not_in_standard.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "warnings":
                        phase_b_science_warnings.append(line[2:].strip())
        except Exception:
            # If we can't read previous phase report, continue without it
            pass

    # Parse validation errors from Pydantic ValidationError
    action_needed_items = []
    pydantic_errors = None

    # Try multiple ways to detect and extract Pydantic errors
    if hasattr(validation_error, "errors"):
        # Check if errors is a method (pydantic_core) or property (older pydantic)
        if callable(validation_error.errors):
            # pydantic_core ValidationError - errors is a method
            pydantic_errors = validation_error.errors()
        else:
            # Older pydantic - errors is a property
            pydantic_errors = validation_error.errors
    elif "ValidationError" in str(type(validation_error)):
        # Sometimes the errors attribute might exist but be empty initially
        if hasattr(validation_error, "errors"):
            errors_attr = getattr(validation_error, "errors", None)
            if callable(errors_attr):
                pydantic_errors = errors_attr()
            else:
                pydantic_errors = errors_attr

    if pydantic_errors:
        for error in pydantic_errors:
            error_type = error.get("type", "unknown")
            field_path = ".".join(str(loc) for loc in error.get("loc", []))
            error_msg = error.get("msg", "Unknown error")

            # Build complete Pydantic error message with all available details
            full_error_parts = [error_msg]

            # Add type information
            if error_type != "unknown":
                full_error_parts.append(f"[type={error_type}")

                # Add input_value if available
                if "input" in error:
                    input_value = str(error.get("input", ""))
                    if len(input_value) > 100:  # Truncate very long inputs
                        input_value = input_value[:97] + "..."
                    full_error_parts.append(f"input_value={input_value}")

                # Add input_type if available
                if "input_type" in error:
                    full_error_parts.append(f"input_type={error.get('input_type')}")

                full_error_parts.append("]")

            # Add Pydantic docs URL if available
            if "url" in error:
                full_error_parts.append(
                    f"For further information visit {error.get('url')}"
                )

            complete_error_msg = " ".join(full_error_parts)

            field_name = field_path.split(".")[-1] if "." in field_path else field_path
            action_needed_items.append({
                "field": field_name,
                "path": field_path,
                "error": complete_error_msg,
            })
    else:
        # Fallback for non-Pydantic errors
        action_needed_items.append({
            "field": "general",
            "path": "configuration",
            "error": str(validation_error),
        })

    # ACTION NEEDED section (following Phase B pattern)
    if action_needed_items:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(
            f"- Found ({len(action_needed_items)}) critical Pydantic validation error(s):"
        )

        for item in action_needed_items:
            report_lines.append(f"-- {item['field']}: {item['error']}")
            if item["path"] != "configuration":
                report_lines.append(f"   Location: {item['path']}")

        report_lines.append("")

    # NO ACTION NEEDED section (following Phase B pattern)
    report_lines.append("## NO ACTION NEEDED")

    # Previous phase information (Phase A or B) - following Phase B pattern exactly
    previous_phase_items = []
    if phase_a_renames:
        previous_phase_items.append(
            f"- Updated ({len(phase_a_renames)}) renamed parameter(s) to current standards:"
        )
        for rename in phase_a_renames:
            previous_phase_items.append(f"-- {rename}")

    if phase_a_optional_missing:
        previous_phase_items.append(
            f"- Updated ({len(phase_a_optional_missing)}) optional missing parameter(s) with null values:"
        )
        for param in phase_a_optional_missing:
            previous_phase_items.append(f"-- {param}")

    if phase_a_not_in_standard:
        previous_phase_items.append(
            f"- Found ({len(phase_a_not_in_standard)}) parameter(s) not in standard:"
        )
        for param in phase_a_not_in_standard:
            previous_phase_items.append(f"-- {param}")

    # Add Phase B specific information if available
    if phase_b_science_warnings:
        previous_phase_items.append(
            f"- Found ({len(phase_b_science_warnings)}) scientific warning(s) for information:"
        )
        for warning in phase_b_science_warnings:
            previous_phase_items.append(f"-- {warning}")

    # Add previous phase items if any exist
    if previous_phase_items:
        report_lines.extend(previous_phase_items)
        # Add spacing before Pydantic validation status
        report_lines.append("")

    # Add Pydantic validation status
    if not action_needed_items:
        if (
            not previous_phase_items
        ):  # Only show if no previous phase items already shown
            report_lines.append("- All Pydantic validation checks passed")
            report_lines.append("- Model physics method compatibility validated")
            report_lines.append("- Conditional parameter requirements satisfied")
        else:
            report_lines.append("- Pydantic validation completed successfully")

    # Footer
    report_lines.append("")
    report_lines.append("# " + "=" * 50)

    # Write report file
    report_content = "\n".join(report_lines)
    with open(output_report_file, "w") as f:
        f.write(report_content)


def generate_fallback_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "user",
    phase_a_report_file: str = None,
    phases_run: list = None,
) -> None:
    """
    Generate a simple fallback report when structured report generation fails.

    Args:
        validation_error: The validation exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
        mode: Processing mode ('user' or 'dev')
        phase_a_report_file: Path to Phase A or Phase B report file for consolidation (optional)
    """
    # Extract Phase A/B information if available (same logic as main report)
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                report_content = f.read()

            # Detect report type
            report_type = "unknown"
            if "SUEWS Scientific Validation Report" in report_content:
                report_type = "phase_b"
            elif "SUEWS Configuration Analysis Report" in report_content:
                report_type = "phase_a"

            # Parse Phase A or Phase B report for relevant information
            lines = report_content.split("\n")
            current_section = None

            for line in lines:
                line = line.strip()
                if report_type == "phase_a":
                    # Parse Phase A content
                    if "Updated (" in line and "renamed parameter" in line:
                        current_section = "renames"
                    elif "Updated (" in line and "optional missing parameter" in line:
                        current_section = "optional"
                    elif "parameter(s) not in standard" in line:
                        current_section = "not_standard"
                    elif line.startswith("--") and current_section == "renames":
                        phase_a_renames.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "optional":
                        phase_a_optional_missing.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "not_standard":
                        phase_a_not_in_standard.append(line[2:].strip())
                elif report_type == "phase_b":
                    # Parse Phase B content (scientific validation)
                    if "Updated (" in line and "renamed parameter" in line:
                        current_section = "renames"
                    elif "Updated (" in line and "optional missing parameter" in line:
                        current_section = "optional"
                    elif "parameter(s) not in standard" in line:
                        current_section = "not_standard"
                    elif "scientific warning" in line:
                        current_section = "warnings"
                    elif line.startswith("--") and current_section == "renames":
                        phase_a_renames.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "optional":
                        phase_a_optional_missing.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "not_standard":
                        phase_a_not_in_standard.append(line[2:].strip())
                    elif line.startswith("--") and current_section == "warnings":
                        phase_b_science_warnings.append(line[2:].strip())
        except Exception:
            pass

    # Build previous phase consolidation section (following Phase B pattern)
    previous_phase_consolidation = ""
    previous_phase_items = []

    if phase_a_renames:
        previous_phase_items.append(
            f"- Updated ({len(phase_a_renames)}) renamed parameter(s) to current standards:"
        )
        for rename in phase_a_renames:
            previous_phase_items.append(f"-- {rename}")

    if phase_a_optional_missing:
        previous_phase_items.append(
            f"- Updated ({len(phase_a_optional_missing)}) optional missing parameter(s) with null values:"
        )
        for param in phase_a_optional_missing:
            previous_phase_items.append(f"-- {param}")

    if phase_a_not_in_standard:
        previous_phase_items.append(
            f"- Found ({len(phase_a_not_in_standard)}) parameter(s) not in standard:"
        )
        for param in phase_a_not_in_standard:
            previous_phase_items.append(f"-- {param}")

    # Add Phase B specific information if available
    if phase_b_science_warnings:
        previous_phase_items.append(
            f"- Found ({len(phase_b_science_warnings)}) scientific warning(s) for information:"
        )
        for warning in phase_b_science_warnings:
            previous_phase_items.append(f"-- {warning}")

    if previous_phase_items:
        previous_phase_consolidation = "\n\n## NO ACTION NEEDED\n" + "\n".join(
            previous_phase_items
        )

    # Generate phase-specific title (same logic as main report)
    if phases_run:
        phase_str = "".join(phases_run)
    else:
        phase_str = "C"  # Default to Phase C only
    
    phase_titles = {
        "A": "SUEWS - Phase A (Up-to-date YAML check) Report",
        "B": "SUEWS - Phase B (Scientific Validation) Report", 
        "C": "SUEWS - Phase C (Pydantic Validation) Report",
        "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
        "AC": "SUEWS - Phase AC (Up-to-date YAML check and Pydantic Validation) Report", 
        "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
        "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report"
    }
    
    title = phase_titles.get(phase_str, "SUEWS Phase C (Pydantic Validation) Report")

    error_report = f"""# {title}
# ============================================
# Mode: {mode.title()}
# ============================================

## ACTION NEEDED
- Found (1) critical Pydantic validation error(s):
-- validation_error: {str(validation_error)}
   Suggested fix: Review and fix validation errors above
   Location: {input_yaml_file}{previous_phase_consolidation}

# ==================================================
"""

    with open(output_report_file, "w") as f:
        f.write(error_report)
