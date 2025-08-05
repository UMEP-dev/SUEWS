import yaml
import os
import subprocess

RENAMED_PARAMS = {
    "cp": "rho_cp",
    "diagmethod": "rslmethod",
    "localclimatemethod": "rsllevel",
}

PHYSICS_OPTIONS = {
    "netradiationmethod",
    "emissionsmethod",
    "storageheatmethod",
    "roughlenmommethod",
    "roughlenheatmethod",
    "stabilitymethod",
    "smdmethod",
    "waterusemethod",
    "rslmethod",
    "faimethod",
    "gsmodel",
    "snowuse",
    "stebbsmethod",
}


def handle_renamed_parameters(yaml_content: str):
    lines = yaml_content.split("\n")
    replacements = []
    for i, line in enumerate(lines):
        stripped = line.strip()
        for old_key, new_key in RENAMED_PARAMS.items():
            if stripped.startswith(f"{old_key}:"):
                indent = line[: len(line) - len(stripped)]
                value = stripped.split(":", 1)[1].strip()
                lines[i] = (
                    f'{indent}{new_key}: {value}  #RENAMED IN STANDARD - Found "{old_key}" and changed into "{new_key}"'
                )
                replacements.append((old_key, new_key))
    return "\n".join(lines), replacements


def is_physics_option(param_path):
    param_name = param_path.split(".")[-1]
    return "model.physics" in param_path and param_name in PHYSICS_OPTIONS


def find_extra_parameters(user_data, standard_data, current_path=""):
    """Find parameters that exist in user data but not in standard data."""
    extra_params = []
    if isinstance(user_data, dict) and isinstance(standard_data, dict):
        for key, user_value in user_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if key not in standard_data:
                # This parameter exists in user but not in standard
                extra_params.append(full_path)
            elif isinstance(user_value, dict) and isinstance(
                standard_data.get(key), dict
            ):
                # Recursively check nested dictionaries
                nested_extra = find_extra_parameters(
                    user_value, standard_data[key], full_path
                )
                extra_params.extend(nested_extra)
            elif isinstance(user_value, list) and isinstance(
                standard_data.get(key), list
            ):
                # Handle lists/arrays
                nested_extra = find_extra_parameters_in_lists(
                    user_value, standard_data[key], full_path
                )
                extra_params.extend(nested_extra)
    elif isinstance(user_data, list) and isinstance(standard_data, list):
        nested_extra = find_extra_parameters_in_lists(
            user_data, standard_data, current_path
        )
        extra_params.extend(nested_extra)
    return extra_params


def find_extra_parameters_in_lists(user_list, standard_list, current_path=""):
    """Find extra parameters in list structures."""
    extra_params = []
    for i, user_item in enumerate(user_list):
        item_path = f"{current_path}[{i}]" if current_path else f"[{i}]"
        if i < len(standard_list):
            # Compare with corresponding standard item
            standard_item = standard_list[i]
            nested_extra = find_extra_parameters(user_item, standard_item, item_path)
            extra_params.extend(nested_extra)
        # Note: We don't flag entire array items as "extra" if they exceed standard length
        # as this might be valid (user has more array items than standard)
    return extra_params


def find_missing_parameters(user_data, standard_data, current_path=""):
    missing_params = []
    if isinstance(standard_data, dict):
        user_dict = user_data if isinstance(user_data, dict) else {}
        for key, standard_value in standard_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if key not in user_dict:
                is_physics = is_physics_option(full_path)
                missing_params.append((full_path, standard_value, is_physics))
            elif isinstance(standard_value, dict) and isinstance(
                user_dict.get(key), dict
            ):
                nested_missing = find_missing_parameters(
                    user_dict[key], standard_value, full_path
                )
                missing_params.extend(nested_missing)
            elif isinstance(standard_value, list) and isinstance(
                user_dict.get(key), list
            ):
                nested_missing = find_missing_parameters_in_lists(
                    user_dict[key], standard_value, full_path
                )
                missing_params.extend(nested_missing)
    elif isinstance(standard_data, list):
        user_list = user_data if isinstance(user_data, list) else []
        nested_missing = find_missing_parameters_in_lists(
            user_list, standard_data, current_path
        )
        missing_params.extend(nested_missing)
    return missing_params


def find_missing_parameters_in_lists(user_list, standard_list, current_path=""):
    missing_params = []
    for i, standard_item in enumerate(standard_list):
        item_path = f"{current_path}[{i}]" if current_path else f"[{i}]"
        if i < len(user_list):
            user_item = user_list[i]
            nested_missing = find_missing_parameters(
                user_item, standard_item, item_path
            )
            missing_params.extend(nested_missing)
        else:
            if isinstance(standard_item, dict):
                flattened_missing = flatten_missing_dict(standard_item, item_path)
                missing_params.extend(flattened_missing)
            else:
                is_physics = is_physics_option(item_path)
                missing_params.append((item_path, standard_item, is_physics))
    return missing_params


def flatten_missing_dict(data, current_path=""):
    missing_params = []
    if isinstance(data, dict):
        for key, value in data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if isinstance(value, dict):
                nested_missing = flatten_missing_dict(value, full_path)
                missing_params.extend(nested_missing)
            else:
                is_physics = is_physics_option(full_path)
                missing_params.append((full_path, value, is_physics))
    else:
        is_physics = is_physics_option(current_path)
        missing_params.append((current_path, data, is_physics))
    return missing_params


def find_insertion_point(lines, path_parts):
    if len(path_parts) < 2:
        return None

    parent_section = path_parts[-2]

    # Handle array indices in parent section (e.g., "walls[2]" -> "walls")
    if "[" in parent_section and "]" in parent_section:
        array_name = parent_section.split("[")[0]
        array_index = int(parent_section.split("[")[1].split("]")[0])
        return find_array_item_insertion_point(
            lines, path_parts, array_name, array_index
        )

    section_indent = None
    section_start = None
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == f"{parent_section}:" or stripped.endswith(f":{parent_section}:"):
            section_indent = len(line) - len(line.lstrip())
            section_start = i
            break
    if section_start is None:
        return None
    child_indent = section_indent + 2
    last_parameter_end = section_start

    # Find the last line that belongs to this section at the child indent level
    for i in range(section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        # If we encounter a line at the same level or less indented than the parent section,
        # we've reached the end of this section
        if line_indent <= section_indent and line.strip():
            break

        # If this line is at the correct child indent level and not a comment
        if line_indent == child_indent and not line.strip().startswith("#"):
            # Update our potential insertion point to after this parameter
            last_parameter_end = i

            # Look ahead to find the end of this parameter (including any nested content)
            for j in range(i + 1, len(lines)):
                next_line = lines[j]
                if not next_line.strip():
                    continue
                next_indent = len(next_line) - len(next_line.lstrip())

                # If we find another parameter at the same level or a section end, stop
                if next_indent <= child_indent and next_line.strip():
                    if next_indent == child_indent and not next_line.strip().startswith(
                        "#"
                    ):
                        # This is another parameter at the same level
                        break
                    elif next_indent <= section_indent:
                        # This is the end of the section
                        break
                else:
                    # This line belongs to the current parameter, update end position
                    last_parameter_end = j

    return last_parameter_end + 1


def find_array_item_insertion_point(lines, path_parts, array_name, array_index):
    """Find insertion point for a parameter within a specific array item."""
    # Find the array section
    array_section_start = None
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == f"{array_name}:" or stripped.endswith(f":{array_name}:"):
            array_section_start = i
            array_indent = len(line) - len(line.lstrip())
            break

    if array_section_start is None:
        return None

    # Find the specific array item (index)
    current_item = -1
    item_start = None
    item_indent = None

    for i in range(array_section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        # Found an array item marker "-" (should be at specific indent level and followed by key:)
        if (
            line.strip().startswith("-")
            and line_indent == array_indent  # Must be exactly at array indent level
            and ":" in line
        ):  # Must contain a key (like "- alb:")
            current_item += 1
            if current_item == array_index:
                item_start = i
                item_indent = line_indent
                break

        # End of array section (non-array-item at same or less indent)
        elif (
            line_indent <= array_indent
            and line.strip()
            and not line.strip().startswith("-")
        ):
            break

    if item_start is None:
        return None

    # Find the end of this specific array item
    last_parameter_end = item_start
    for i in range(item_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        # If we encounter another array item or end of section
        if line_indent <= item_indent and line.strip():
            if line.strip().startswith("-") or line_indent <= array_indent:
                break

        # Update end position if this line belongs to the current item
        if line_indent > item_indent:
            last_parameter_end = i

    return last_parameter_end + 1


def get_section_indent(lines, position, target_indent_level=None):
    # If we have a target indent level, use it to find the correct parent indent
    if target_indent_level is not None:
        return " " * target_indent_level

    # Otherwise fall back to the old behavior
    for i in range(position - 1, -1, -1):
        line = lines[i]
        if line.strip() and not line.strip().startswith("#"):
            return line[: len(line) - len(line.lstrip())]
    return ""


def calculate_array_item_indent(lines, insert_position, array_name):
    """Calculate the correct indentation for a parameter within an array item."""

    # First approach: find the commented wetthresh and use its indentation
    # This is the most reliable method since we know exactly what we want to replace
    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        if "#wetthresh:" in line:
            return line[: len(line) - len(line.lstrip())]

    # Second approach: look for existing parameters at the same level
    # Find parameters that are direct children of the array item, not value lines
    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        stripped = line.strip()

        # Skip empty lines and comments
        if not stripped or stripped.startswith("#"):
            continue

        # Skip array item markers (lines starting with "-")
        if stripped.startswith("-"):
            continue

        line_indent = len(line) - len(line.lstrip())

        # Look for parameter lines that end with `:` and are followed by indented values
        if stripped.endswith(":") and not stripped.startswith("value:"):
            # Check if next non-empty line is more indented (indicating this is a parent parameter)
            for j in range(i + 1, min(len(lines), i + 5)):
                if (
                    j < len(lines)
                    and lines[j].strip()
                    and not lines[j].strip().startswith("#")
                ):
                    next_indent = len(lines[j]) - len(lines[j].lstrip())
                    if next_indent > line_indent:
                        # This is a parent parameter, use its indentation
                        return " " * line_indent
                    break

    # Third fallback: calculate based on array structure
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == f"{array_name}:" or stripped.endswith(f":{array_name}:"):
            array_indent = len(line) - len(line.lstrip())
            return " " * (array_indent + 2)

    return ""


def format_yaml_key(key):
    """Format a key for YAML output, ensuring numeric strings are properly quoted."""
    # If the key is a string that looks like a number (like '1', '2', etc.), keep it quoted
    if isinstance(key, str) and key.isdigit():
        return f"'{key}'"
    # If the key is an integer but should be a string (common with YAML parsing), quote it
    elif isinstance(key, int):
        return f"'{key}'"
    # Otherwise, return as-is
    else:
        return str(key)


def create_uptodate_yaml_header():
    header = """# =============================================================================
# UP TO DATE YAML
# =============================================================================
#
# This file has been automatically updated by uptodate_yaml.py with all necessary changes:
# - Missing in standard parameters have been added with null values
# - Renamed in standard parameters have been updated to current naming conventions
# - All changes are reported in report_<yourfilename>.txt
#
# =============================================================================

"""
    return header


def create_clean_missing_param_annotation(param_name, standard_value):
    """Create missing parameter annotation without inline comments for clean YAML."""
    lines = []
    if isinstance(standard_value, dict):
        lines.append(f"{param_name}:")
        for key, value in standard_value.items():
            formatted_key = format_yaml_key(key)
            if isinstance(value, dict):
                lines.append(f"  {formatted_key}:")
                for subkey, subvalue in value.items():
                    formatted_subkey = format_yaml_key(subkey)
                    # All missing parameters get null values
                    default_value = get_null_placeholder()
                    lines.append(f"    {formatted_subkey}: {default_value}")
            else:
                # All missing parameters get null values
                default_value = get_null_placeholder()
                lines.append(f"  {formatted_key}: {default_value}")
    else:
        # All missing parameters get null values
        default_value = get_null_placeholder()
        lines.append(f"{param_name}: {default_value}")
    return lines


def get_null_placeholder():
    """Return null placeholder for missing parameters.

    All missing parameters get null values regardless of type.
    Users are expected to replace null with appropriate values.
    """
    return "null"


def cleanup_renamed_comments(yaml_content):
    """Remove renamed in standard comments from YAML content for clean output."""
    lines = yaml_content.split("\n")
    cleaned_lines = []

    for line in lines:
        # Remove renamed in standard comments but keep the parameter line
        if "#RENAMED IN STANDARD" in line:
            # Extract the part before the comment
            clean_line = line.split("#RENAMED IN STANDARD")[0].rstrip()
            cleaned_lines.append(clean_line)
        else:
            cleaned_lines.append(line)

    return "\n".join(cleaned_lines)


def create_uptodate_yaml_with_missing_params(
    yaml_content, missing_params, extra_params=None
):
    """Create clean YAML with missing parameters added but no inline comments."""
    # First, clean up any renamed in standard comments from the yaml_content
    clean_yaml_content = cleanup_renamed_comments(yaml_content)

    if not missing_params:
        header = create_uptodate_yaml_header()
        return header + clean_yaml_content

    lines = clean_yaml_content.split("\n")
    missing_params.sort(key=lambda x: x[0].count("."), reverse=True)

    for param_path, standard_value, is_physics in missing_params:
        path_parts = param_path.split(".")
        param_name = path_parts[-1]
        insert_position = find_insertion_point(lines, path_parts)
        if insert_position is not None:
            # Calculate the correct indentation
            parent_section = path_parts[-2] if len(path_parts) >= 2 else None
            if parent_section:
                # Handle array indices in parent section
                if "[" in parent_section and "]" in parent_section:
                    array_name = parent_section.split("[")[0]
                    indent = calculate_array_item_indent(
                        lines, insert_position, array_name
                    )
                else:
                    # Find the parent section and calculate child indent
                    for i, line in enumerate(lines):
                        stripped = line.strip()
                        if stripped == f"{parent_section}:" or stripped.endswith(
                            f":{parent_section}:"
                        ):
                            parent_indent = len(line) - len(line.lstrip())
                            child_indent_level = parent_indent + 2
                            indent = get_section_indent(
                                lines, insert_position, child_indent_level
                            )
                            break
                    else:
                        indent = get_section_indent(lines, insert_position)
            else:
                indent = get_section_indent(lines, insert_position)

            # Create clean annotation lines (without comments)
            annotation_lines = create_clean_missing_param_annotation(
                param_name, standard_value
            )
            # Apply proper indentation to each line
            indented_lines = []
            for line in annotation_lines:
                if line.strip():  # Don't indent empty lines
                    indented_lines.append(indent + line)
                else:
                    indented_lines.append(line)

            # Insert the lines
            for i, annotation_line in enumerate(reversed(indented_lines)):
                lines.insert(insert_position, annotation_line)

    header = create_uptodate_yaml_header()
    content_with_lines = "\n".join(lines)

    # Note: We don't mark extra parameters in the clean YAML - it should have no inline comments
    # Extra parameters are only reported in the analysis report

    clean_content = header + content_with_lines
    return clean_content


def create_analysis_report(
    missing_params, renamed_replacements, extra_params=None, uptodate_filename=None
):
    """Create analysis report with summary of changes."""
    report_lines = []
    report_lines.append("# SUEWS Configuration Analysis Report")
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    # Count parameters by type
    urgent_count = sum(1 for _, _, is_physics in missing_params if is_physics)
    optional_count = len(missing_params) - urgent_count
    renamed_count = len(renamed_replacements)
    extra_count = len(extra_params) if extra_params else 0

    # ACTION NEEDED section - only critical/urgent parameters
    if urgent_count > 0:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(f"- Found ({urgent_count}) critical missing parameter(s):")
        for param_path, standard_value, is_physics in missing_params:
            if is_physics:
                param_name = param_path.split(".")[-1]
                uptodate_file_ref = (
                    uptodate_filename if uptodate_filename else "uptodate YAML file"
                )
                report_lines.append(
                    f"-- {param_name} has been added to {uptodate_file_ref} and set to null"
                )
                report_lines.append(
                    f"   Suggested fix: Set appropriate value based on SUEWS documentation -- https://suews.readthedocs.io/latest/"
                )
        report_lines.append("")

    # NO ACTION NEEDED section - optional and informational items
    has_no_action_items = optional_count > 0 or extra_count > 0 or renamed_count > 0
    if has_no_action_items:
        report_lines.append("## NO ACTION NEEDED")

        # Updated optional missing parameters
        if optional_count > 0:
            report_lines.append(
                f"- Updated ({optional_count}) optional missing parameter(s) with null values:"
            )
            for param_path, standard_value, is_physics in missing_params:
                if not is_physics:
                    param_name = param_path.split(".")[-1]
                    uptodate_file_ref = (
                        uptodate_filename if uptodate_filename else "uptodate YAML file"
                    )
                    report_lines.append(f"-- {param_name} added to {uptodate_file_ref} and set to null")
            report_lines.append("")

        # Renamed parameters
        if renamed_count > 0:
            report_lines.append(f"- Updated ({renamed_count}) renamed parameter(s):")
            for old_name, new_name in renamed_replacements:
                report_lines.append(f"-- {old_name} changed to {new_name}")
            report_lines.append("")

        # NOT IN STANDARD parameters
        if extra_count > 0:
            report_lines.append(
                f"- Found ({extra_count}) parameter(s) not in standard:"
            )
            for param_path in extra_params:
                param_name = param_path.split(".")[-1]
                report_lines.append(f"-- {param_name} at level {param_path}")
            report_lines.append("")

    # Footer separator
    report_lines.append("# " + "=" * 50)

    return "\n".join(report_lines)


def annotate_missing_parameters(
    user_file, standard_file, uptodate_file=None, report_file=None
):
    try:
        with open(user_file, "r") as f:
            original_yaml_content = f.read()
        original_yaml_content, renamed_replacements = handle_renamed_parameters(
            original_yaml_content
        )
        user_data = yaml.safe_load(original_yaml_content)
        with open(standard_file, "r") as f:
            standard_data = yaml.safe_load(f)
    except FileNotFoundError as e:
        print(f"Error: File not found - {e}")
        return
    except yaml.YAMLError as e:
        print(f"Error: Invalid YAML - {e}")
        return
    missing_params = find_missing_parameters(user_data, standard_data)
    extra_params = find_extra_parameters(user_data, standard_data)

    # Generate content for both files
    if missing_params or renamed_replacements or extra_params:
        # Create uptodate YAML (clean, with NOT IN STANDARD markers)
        uptodate_content = create_uptodate_yaml_with_missing_params(
            original_yaml_content, missing_params, extra_params
        )

        # Create analysis report
        uptodate_filename = os.path.basename(uptodate_file) if uptodate_file else None
        report_content = create_analysis_report(
            missing_params, renamed_replacements, extra_params, uptodate_filename
        )
    else:
        print("No missing in standard or renamed in standard parameters found!")
        # Still create clean files
        uptodate_content = create_uptodate_yaml_header() + original_yaml_content
        uptodate_filename = os.path.basename(uptodate_file) if uptodate_file else None
        report_content = create_analysis_report([], [], [], uptodate_filename)

    # Print clean terminal output based on critical parameters
    critical_params = [
        (path, val, is_phys) for path, val, is_phys in missing_params if is_phys
    ]

    if critical_params:
        print(f"Action needed: CRITICAL parameters missing:")
        for param_path, standard_value, _ in critical_params:
            param_name = param_path.split(".")[-1]
            print(f"  - {param_name}")
        print("")
        report_filename = (
            os.path.basename(report_file) if report_file else "report file"
        )
        report_location = (
            os.path.dirname(report_file) if report_file else "current directory"
        )
        print(
            f"Next step: Check {report_filename} report file located {report_location} on what to do to resolve this"
        )
    else:
        print("PHASE A -- PASSED")

    # Write output files
    if uptodate_file:
        with open(uptodate_file, "w") as f:
            f.write(uptodate_content)
        # print(f"\n Clean YAML written to: {uptodate_file}")

    if report_file:
        with open(report_file, "w") as f:
            f.write(report_content)
        # print(f" Analysis report written to: {report_file}")


def get_current_git_branch() -> str:
    """
    Get the current git branch name.

    Returns:
        Current branch name or 'unknown' if not in a git repo
    """
    try:
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "unknown"


def check_file_differs_from_master(file_path: str) -> bool:
    """
    Check if a file differs from its version in the master branch.

    Args:
        file_path: Path to the file to check

    Returns:
        True if file differs from master, False if same or on error
    """
    try:
        # Check if file differs from master branch version
        result = subprocess.run(
            ["git", "diff", "master", "--", file_path],
            capture_output=True,
            text=True,
            check=True,
        )
        # If diff output is empty, files are the same
        return len(result.stdout.strip()) > 0
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False


def validate_standard_file(standard_file: str) -> bool:
    """
    Validate that the standard file exists and is up to date with master branch.

    Args:
        standard_file: Path to the standard YAML file

    Returns:
        True if validation passes, False if warnings were issued
    """
    print("Validating standard configuration file...")

    # Check if file exists
    if not os.path.exists(standard_file):
        print(f"❌ ERROR: Standard file not found: {standard_file}")
        print(f"   Make sure you're running from the SUEWS root directory")
        return False

    current_branch = get_current_git_branch()

    if current_branch == "unknown":
        print(
            "⚠️  WARNING: Not in a git repository - cannot verify standard file is up to date"
        )
        return True

    if current_branch != "master":
        file_differs = check_file_differs_from_master(standard_file)

        if file_differs:
            print(
                f"⚠️  WARNING: You are on branch '{current_branch}' and {os.path.basename(standard_file)} differs from master"
            )
            print(f"   This may cause inconsistent parameter detection.")
            print(f"   RECOMMENDED:")
            print(f"   1. Switch to master branch: git checkout master")
            print(
                f"   2. OR update your {os.path.basename(standard_file)} to match master:"
            )
            print(f"      git checkout master -- {standard_file}")
            print()
            return False
        else:
            print(f"✓ Branch: {current_branch} (standard file matches master)")
    else:
        print(f"✓ Branch: {current_branch}")

    print(f"✓ Standard file: {standard_file}")
    return True


def main():
    print(" SUEWS YAML Configuration Analysis")
    print("=" * 50)

    standard_file = "src/supy/sample_run/sample_config.yml"
    user_file = "src/supy/data_model/user.yml"

    # Validate standard file is up to date with master branch
    validation_passed = validate_standard_file(standard_file)
    print()

    # Print user file info
    print(f"User YAML file: {user_file}")
    print()

    # If validation failed, we can still proceed but user should be aware
    if not validation_passed:
        print("⚠️  Proceeding with potentially outdated standard file...")
        print()

    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)

    # Generate file names
    name_without_ext = os.path.splitext(basename)[0]
    uptodate_filename = f"uptodate_{basename}"
    report_filename = f"report_{name_without_ext}.txt"

    uptodate_file = os.path.join(dirname, uptodate_filename)
    report_file = os.path.join(dirname, report_filename)

    annotate_missing_parameters(
        user_file=user_file,
        standard_file=standard_file,
        uptodate_file=uptodate_file,
        report_file=report_file,
    )


if __name__ == "__main__":
    main()
