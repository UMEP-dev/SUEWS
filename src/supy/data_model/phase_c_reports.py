"""Phase C report generation for Pydantic validation errors."""

import os
import re

PHASE_TITLES = {
    "A": "SUEWS - Phase A (Up-to-date YAML check) Report",
    "B": "SUEWS - Phase B (Scientific Validation) Report",
    "C": "SUEWS - Phase C (Pydantic Validation) Report",
    "AB": "SUEWS - Phase AB (Up-to-date YAML check and Scientific Validation) Report",
    "AC": "SUEWS - Phase AC (Up-to-date YAML check and Pydantic Validation) Report",
    "BC": "SUEWS - Phase BC (Scientific Validation and Pydantic Validation) Report",
    "ABC": "SUEWS - Phase ABC (Up-to-date YAML check, Scientific Validation and Pydantic Validation) Report",
}


def _parse_previous_phase_report(report_content: str):
    """Parse previous phase report content to extract relevant information."""
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []

    report_type = "unknown"
    if "SUEWS Scientific Validation Report" in report_content:
        report_type = "phase_b"
    elif "SUEWS Configuration Analysis Report" in report_content:
        report_type = "phase_a"

    lines = report_content.split("\n")
    current_section = None

    for line in lines:
        line = line.strip()
        if "Updated (" in line and "renamed parameter" in line:
            current_section = "renames"
        elif "Updated (" in line and "optional missing parameter" in line:
            current_section = "optional"
        elif "parameter(s) not in standard" in line:
            current_section = "not_standard"
        elif report_type == "phase_b" and "scientific warning" in line:
            current_section = "warnings"
        elif line.startswith("--"):
            if current_section == "renames":
                phase_a_renames.append(line[2:].strip())
            elif current_section == "optional":
                phase_a_optional_missing.append(line[2:].strip())
            elif current_section == "not_standard":
                phase_a_not_in_standard.append(line[2:].strip())
            elif current_section == "warnings":
                phase_b_science_warnings.append(line[2:].strip())

    return (
        phase_a_renames,
        phase_a_optional_missing,
        phase_a_not_in_standard,
        phase_b_science_warnings,
    )


def generate_phase_c_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
) -> None:
    """Generate Phase C validation report with previous phase consolidation."""
    report_lines = []

    phase_str = "".join(phases_run) if phases_run else "C"
    title = PHASE_TITLES.get(phase_str, "SUEWS Phase C (Pydantic Validation) Report")

    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(
        f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}"
    )
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                report_content = f.read()
            (
                phase_a_renames,
                phase_a_optional_missing,
                phase_a_not_in_standard,
                phase_b_science_warnings,
            ) = _parse_previous_phase_report(report_content)
        except Exception:
            pass

    action_needed_items = []
    pydantic_errors = None

    if hasattr(validation_error, "errors"):
        errors_attr = validation_error.errors
        pydantic_errors = errors_attr() if callable(errors_attr) else errors_attr

    if pydantic_errors:
        for error in pydantic_errors:
            error_type = error.get("type", "unknown")
            field_path = ".".join(str(loc) for loc in error.get("loc", []))
            error_msg = error.get("msg", "Unknown error")

            if not field_path:
                field_match = re.search(
                    r"Required field '(\w+)' has no value", error_msg
                )
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
                field_name = (
                    field_path.split(".")[-1] if "." in field_path else field_path
                )

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
                full_error_parts.append(
                    f"For further information visit {error.get('url')}"
                )

            complete_error_msg = " ".join(full_error_parts)
            
            # Check if this is our combined critical validation error
            if "Critical validation failed: " in error_msg:
                # Split the combined error message into individual issues
                # Extract the part after "Critical validation failed: "
                split_parts = error_msg.split("Critical validation failed: ", 1)
                combined_message = split_parts[1] if len(split_parts) > 1 else error_msg
                individual_issues = [issue.strip() for issue in combined_message.split(";")]
                
                for issue in individual_issues:
                    if issue:  # Skip empty issues
                        # Determine the field name and path based on the issue content
                        if "is set to null and will cause runtime crash" in issue:
                            param_name = issue.split(" is set to null")[0]
                            issue_field_name = param_name
                            issue_path = f"model.physics.{param_name}"
                        elif " → " in issue:
                            # StorageHeat parameter format: "site: storageheatmethod=6 → properties.lambda_c must be set"
                            parts = issue.split(" → ", 1)
                            if len(parts) == 2:
                                site_part = parts[0].strip()
                                param_part = parts[1].strip()
                                # Extract site name (before the colon)
                                site_name = site_part.split(":")[0].strip() if ":" in site_part else "Unknown"
                                issue_field_name = param_part.split(" must be")[0].strip()
                                issue_path = f"sites[{site_name}].properties.{issue_field_name}"
                            else:
                                issue_field_name = "StorageHeat parameter"
                                issue_path = "configuration"
                        elif "must be set" in issue and ":" in issue:
                            # RSL parameter format: "site: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set"
                            parts = issue.split(":", 1)
                            if len(parts) == 2:
                                site_name = parts[0].strip()
                                param_desc = parts[1].strip()
                                if "faibldg" in param_desc:
                                    issue_field_name = "bldgs.faibldg"
                                    issue_path = f"sites[{site_name}].properties.land_cover.bldgs.faibldg"
                                else:
                                    issue_field_name = "RSL parameter"
                                    issue_path = f"sites[{site_name}]"
                            else:
                                issue_field_name = "RSL parameter"
                                issue_path = "configuration"
                        else:
                            # Generic issue
                            issue_field_name = "validation issue"
                            issue_path = "configuration"
                        
                        action_needed_items.append({
                            "field": issue_field_name,
                            "path": issue_path,
                            "error": issue,
                        })
            else:
                # Original single error handling
                action_needed_items.append({
                    "field": field_name,
                    "path": field_path,
                    "error": complete_error_msg,
                })
    else:
        error_str = str(validation_error)
        
        # Check if this is our combined critical validation error
        if error_str.startswith("Critical validation failed: "):
            # Split the combined error message into individual issues
            combined_message = error_str.replace("Critical validation failed: ", "")
            individual_issues = [issue.strip() for issue in combined_message.split(";")]
            
            for issue in individual_issues:
                if issue:  # Skip empty issues
                    # Determine the field name and path based on the issue content
                    if "is set to null and will cause runtime crash" in issue:
                        param_name = issue.split(" is set to null")[0]
                        field_name = param_name
                        path = f"model.physics.{param_name}"
                    elif " → " in issue:
                        # StorageHeat parameter format: "site: storageheatmethod=6 → properties.lambda_c must be set"
                        parts = issue.split(" → ", 1)
                        if len(parts) == 2:
                            site_part = parts[0].strip()
                            param_part = parts[1].strip()
                            # Extract site name (before the colon)
                            site_name = site_part.split(":")[0].strip() if ":" in site_part else "Unknown"
                            field_name = param_part.split(" must be")[0].strip()
                            path = f"sites[{site_name}].properties.{field_name}"
                        else:
                            field_name = "StorageHeat parameter"
                            path = "configuration"
                    elif "must be set" in issue and ":" in issue:
                        # RSL parameter format: "site: for rslmethod=2 and bldgs.sfr=0.38, bldgs.faibldg must be set"
                        parts = issue.split(":", 1)
                        if len(parts) == 2:
                            site_name = parts[0].strip()
                            param_desc = parts[1].strip()
                            if "faibldg" in param_desc:
                                field_name = "bldgs.faibldg"
                                path = f"sites[{site_name}].properties.land_cover.bldgs.faibldg"
                            else:
                                field_name = "RSL parameter"
                                path = f"sites[{site_name}]"
                        else:
                            field_name = "RSL parameter"
                            path = "configuration"
                    else:
                        # Generic issue
                        field_name = "validation issue"
                        path = "configuration"
                    
                    action_needed_items.append({
                        "field": field_name,
                        "path": path,
                        "error": issue,
                    })
        else:
            # Original single error handling
            action_needed_items.append({
                "field": "general",
                "path": "configuration", 
                "error": error_str,
            })

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

    previous_phase_items = []

    phase_sections = [
        (phase_a_renames, "renamed parameter(s) to current standards"),
        (phase_a_optional_missing, "optional missing parameter(s) with null values"),
        (phase_a_not_in_standard, "parameter(s) not in standard"),
        (phase_b_science_warnings, "scientific warning(s) for information"),
    ]

    for items, description in phase_sections:
        if items:
            action = (
                "Updated"
                if "renamed" in description or "optional" in description
                else "Found"
            )
            previous_phase_items.append(f"- {action} ({len(items)}) {description}:")
            previous_phase_items.extend(f"-- {item}" for item in items)

    if previous_phase_items:
        report_lines.append("## NO ACTION NEEDED")
        report_lines.extend(previous_phase_items)
        report_lines.append("")

    if not action_needed_items and not previous_phase_items:
        report_lines.append(f"Phase {phase_str} passed")

    report_lines.extend(["", "# " + "=" * 50])

    with open(output_report_file, "w") as f:
        f.write("\n".join(report_lines))


def generate_fallback_report(
    validation_error: Exception,
    input_yaml_file: str,
    output_report_file: str,
    mode: str = "public",
    phase_a_report_file: str = None,
    phases_run: list = None,
) -> None:
    """Generate simple fallback report when structured report generation fails."""
    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []
    phase_b_science_warnings = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                report_content = f.read()
            (
                phase_a_renames,
                phase_a_optional_missing,
                phase_a_not_in_standard,
                phase_b_science_warnings,
            ) = _parse_previous_phase_report(report_content)
        except Exception:
            pass

    previous_phase_items = []

    phase_sections = [
        (phase_a_renames, "renamed parameter(s) to current standards"),
        (phase_a_optional_missing, "optional missing parameter(s) with null values"),
        (phase_a_not_in_standard, "parameter(s) not in standard"),
        (phase_b_science_warnings, "scientific warning(s) for information"),
    ]

    for items, description in phase_sections:
        if items:
            action = (
                "Updated"
                if "renamed" in description or "optional" in description
                else "Found"
            )
            previous_phase_items.append(f"- {action} ({len(items)}) {description}:")
            previous_phase_items.extend(f"-- {item}" for item in items)

    previous_phase_consolidation = (
        f"\n\n## NO ACTION NEEDED\n{chr(10).join(previous_phase_items)}"
        if previous_phase_items
        else ""
    )

    phase_str = "".join(phases_run) if phases_run else "C"
    title = PHASE_TITLES.get(phase_str, "SUEWS Phase C (Pydantic Validation) Report")
    mode_title = "Public" if mode.lower() == "public" else mode.title()

    error_report = f"""# {title}
# ============================================
# Mode: {mode_title}
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
