"""
Phase C Report Generation

Standalone module for generating Phase C validation reports in ACTION NEEDED format.
This module is separate from core.py to minimize merge conflicts with master branch.
"""


def generate_phase_c_report(
    validation_error: Exception, input_yaml_file: str, output_report_file: str
) -> None:
    """
    Generate Phase C validation report in ACTION NEEDED format.

    Args:
        validation_error: The Pydantic validation exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
    """
    report_lines = []

    # Header
    report_lines.append("# SUEWS Phase C (Pydantic Validation) Report")
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    # Parse validation errors from Pydantic ValidationError
    action_needed_items = []

    # Debug: Print validation error details (disabled)
    # print(f"DEBUG: validation_error type: {type(validation_error)}")
    # print(f"DEBUG: validation_error has errors attr: {hasattr(validation_error, 'errors')}")
    # if hasattr(validation_error, 'errors'):
    #     print(f"DEBUG: validation_error.errors is callable: {callable(validation_error.errors)}")
    #     if callable(validation_error.errors):
    #         errors_result = validation_error.errors()
    #         print(f"DEBUG: validation_error.errors(): {errors_result}")
    #         print(f"DEBUG: errors() length: {len(errors_result) if errors_result else 'None'}")
    #     else:
    #         print(f"DEBUG: validation_error.errors: {validation_error.errors}")
    #         print(f"DEBUG: validation_error.errors length: {len(validation_error.errors) if validation_error.errors else 'None'}")

    # Improved Pydantic ValidationError detection
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
                "error": complete_error_msg,  # Use complete Pydantic error message
            })
    else:
        # Fallback for non-Pydantic errors
        action_needed_items.append({
            "field": "general",
            "path": "configuration",
            "error": str(validation_error),
        })

    # ACTION NEEDED section (critical errors)
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

    # Add context information
    if not action_needed_items:
        report_lines.append("## NO ACTION NEEDED")
        report_lines.append("- No validation errors found")
        report_lines.append("")

    # Footer
    report_lines.append("# " + "=" * 50)

    # Write report file
    report_content = "\n".join(report_lines)
    with open(output_report_file, "w") as f:
        f.write(report_content)


def generate_fallback_report(
    validation_error: Exception, input_yaml_file: str, output_report_file: str
) -> None:
    """
    Generate a simple fallback report when structured report generation fails.

    Args:
        validation_error: The validation exception
        input_yaml_file: Path to input YAML file
        output_report_file: Path for output report file
    """
    error_report = f"""# SUEWS Phase C (Pydantic Validation) Report  
# ============================================

## ACTION NEEDED
- Found (1) critical Pydantic validation error(s):
-- validation_error: {str(validation_error)}
   Suggested fix: Review and fix validation errors above
   Location: {input_yaml_file}

# ==================================================
"""

    with open(output_report_file, "w") as f:
        f.write(error_report)
