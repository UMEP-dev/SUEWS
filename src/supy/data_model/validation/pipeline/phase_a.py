import yaml
import os
import subprocess

RENAMED_PARAMS = {
    "cp": "rho_cp",
    "diagmethod": "rslmethod",
    "localclimatemethod": "rsllevel",
    "chanohm": "ch_anohm",
    "cpanohm": "rho_cp_anohm",
    "kkanohm": "k_anohm",
}
PHYSICS_OPTIONS = {
    "netradiationmethod",
    "emissionsmethod",
    "storageheatmethod",
    "ohmincqf",
    "roughlenmommethod",
    "roughlenheatmethod",
    "stabilitymethod",
    "smdmethod",
    "waterusemethod",
    "rslmethod",
    "faimethod",
    "rsllevel",
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


def get_allowed_nested_sections_in_properties():
    """Get list of nested sections within models that allow extra parameters.

    This function dynamically introspects all data model classes to find nested
    BaseModel fields that do not have extra="forbid" configuration.
    """
    import importlib
    from pydantic import BaseModel
    from typing import get_origin, get_args

    # Data model modules to introspect
    data_model_modules = [
        "hydro",
        "human_activity",
        "model",
        "state",
        "site",
        "core",
        "ohm",
        "profile",
        "surface",
        "timezone_enum",
        "type",
    ]

    allowed_sections = set()

    for module_name in data_model_modules:
        try:
            # Import the module dynamically
            module = importlib.import_module(
                f".{module_name}", package="supy.data_model"
            )

            # Find all classes in the module that are BaseModel subclasses
            for attr_name in dir(module):
                attr = getattr(module, attr_name)
                if (
                    isinstance(attr, type)
                    and issubclass(attr, BaseModel)
                    and attr is not BaseModel
                    and hasattr(attr, "model_config")
                ):
                    # Check if this model has extra="forbid"
                    config = attr.model_config
                    # Handle both ConfigDict and dict cases
                    if isinstance(config, dict):
                        extra_setting = config.get("extra", None)
                    else:
                        extra_setting = getattr(config, "extra", None)
                    if extra_setting == "forbid":
                        # This is a model with extra="forbid" - check its nested fields
                        for field_name, field_info in attr.model_fields.items():
                            nested_model = _extract_nested_model_type(
                                field_info.annotation
                            )
                            if nested_model and _allows_extra_parameters(nested_model):
                                allowed_sections.add(field_name)

        except (ImportError, AttributeError) as e:
            # Skip modules that can't be imported or don't have expected structure
            continue

    # If dynamic introspection found nothing, use known static sections with validation
    if not allowed_sections:
        # Known sections from manual analysis - only actual BaseModel fields
        static_sections = {
            "stebbs",
            "irrigation",
            "snow",
            "lumps",
            "spartacus",
            "building_archetype",
        }

        # Validate these exist in the actual models (best effort)
        try:
            from .site import SiteProperties

            actual_fields = set(SiteProperties.model_fields.keys())
            validated_sections = static_sections.intersection(actual_fields)
            allowed_sections = (
                validated_sections if validated_sections else static_sections
            )
        except ImportError:
            allowed_sections = static_sections

    return sorted(allowed_sections)


def _extract_nested_model_type(annotation):
    """Extract the nested BaseModel type from a field annotation."""
    from pydantic import BaseModel
    from typing import get_origin, get_args
    import types

    # Handle direct BaseModel subclasses
    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return annotation

    # Handle generic types like Dict, List, Union, Optional
    origin = get_origin(annotation)
    if origin is not None:
        args = get_args(annotation)
        for arg in args:
            if isinstance(arg, type) and issubclass(arg, BaseModel):
                return arg
            # Handle nested generic types
            nested = _extract_nested_model_type(arg)
            if nested:
                return nested

    # Handle special cases like ForwardRef
    if hasattr(annotation, "__forward_arg__"):
        # This is a forward reference, we can't easily resolve it here
        return None

    return None


def _allows_extra_parameters(model_class):
    """Check if a model class allows extra parameters."""
    if not hasattr(model_class, "model_config"):
        return True  # Default Pydantic behavior allows extra parameters

    config = model_class.model_config
    # Handle both ConfigDict and dict cases
    if isinstance(config, dict):
        extra_setting = config.get("extra", None)
    else:
        extra_setting = getattr(config, "extra", None)
    return extra_setting != "forbid"


def is_path_in_forbidden_location(field_path: str) -> bool:
    """Check if field path is in a location that forbids extra parameters."""
    # Check if path contains properties under sites
    if "properties" in field_path and "sites" in field_path:
        # Make sure it's directly under properties, not in a nested allowed section
        path_parts = field_path.split(".")

        try:
            properties_index = path_parts.index("properties")
            if properties_index + 1 < len(path_parts):
                next_part = path_parts[properties_index + 1]
                allowed_nested_sections = get_allowed_nested_sections_in_properties()
                if next_part in allowed_nested_sections:
                    return False
            return True
        except ValueError:
            pass

    return False


def categorise_extra_parameters(extra_params: list) -> dict:
    """Categorise extra parameters into ACTION_NEEDED vs NO_ACTION_NEEDED based on locations."""
    categorised = {"ACTION_NEEDED": [], "NO_ACTION_NEEDED": []}

    for param_path in extra_params:
        if is_path_in_forbidden_location(param_path):
            categorised["ACTION_NEEDED"].append(param_path)
        else:
            categorised["NO_ACTION_NEEDED"].append(param_path)

    return categorised


def find_extra_parameters(user_data, standard_data, current_path=""):
    """Find parameters that exist in user data but not in standard data."""
    extra_params = []
    if isinstance(user_data, dict) and isinstance(standard_data, dict):
        for key, user_value in user_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if key not in standard_data:
                extra_params.append(full_path)
            elif isinstance(user_value, dict) and isinstance(
                standard_data.get(key), dict
            ):
                nested_extra = find_extra_parameters(
                    user_value, standard_data[key], full_path
                )
                extra_params.extend(nested_extra)
            elif isinstance(user_value, list) and isinstance(
                standard_data.get(key), list
            ):
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


def find_section_position(lines, section_name, start_pos=0):
    """Find the position of a section in the YAML lines, starting from start_pos."""
    for i, line in enumerate(lines[start_pos:], start_pos):
        stripped = line.strip()
        if stripped == f"{section_name}:" or stripped.endswith(f":{section_name}:"):
            return i
    return None


def find_array_section_position(lines, array_name, array_index, start_pos=0):
    """Find the position of a specific array item, starting from start_pos."""
    array_section_start = None
    array_indent = None

    # Find the array section
    for i, line in enumerate(lines[start_pos:], start_pos):
        stripped = line.strip()
        if stripped == f"{array_name}:" or stripped.endswith(f":{array_name}:"):
            array_section_start = i
            array_indent = len(line) - len(line.lstrip())
            break

    if array_section_start is None:
        return None

    # Find the specific array item
    current_item = -1
    for i in range(array_section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        if line.strip().startswith("-") and line_indent == array_indent and ":" in line:
            current_item += 1
            if current_item == array_index:
                return i

        elif (
            line_indent <= array_indent
            and line.strip()
            and not line.strip().startswith("-")
        ):
            break

    return None


def find_insertion_point(lines, path_parts):
    if len(path_parts) < 2:
        return None

    parent_section = path_parts[-2]

    if "[" in parent_section and "]" in parent_section:
        array_name = parent_section.split("[")[0]
        array_index = int(parent_section.split("[")[1].split("]")[0])
        return find_array_item_insertion_point(
            lines, path_parts, array_name, array_index
        )

    # Find the correct parent section by following the full path
    section_indent = None
    section_start = None
    current_position = 0

    # Navigate through the path parts to find the exact section
    for path_part in path_parts[:-2]:  # Exclude the parameter name and immediate parent
        if "[" in path_part and "]" in path_part:
            array_name = path_part.split("[")[0]
            array_index = int(path_part.split("[")[1].split("]")[0])
            current_position = find_array_section_position(
                lines, array_name, array_index, current_position
            )
        else:
            current_position = find_section_position(lines, path_part, current_position)

        if current_position is None:
            return None

    # Now find the immediate parent section from the current position
    for i, line in enumerate(lines[current_position:], current_position):
        stripped = line.strip()
        if stripped == f"{parent_section}:" or stripped.endswith(f":{parent_section}:"):
            section_indent = len(line) - len(line.lstrip())
            section_start = i
            break
    if section_start is None:
        return None
    child_indent = section_indent + 2
    last_parameter_end = section_start

    for i in range(section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        if line_indent <= section_indent and line.strip():
            break

        if line_indent == child_indent and not line.strip().startswith("#"):
            last_parameter_end = i

            for j in range(i + 1, len(lines)):
                next_line = lines[j]
                if not next_line.strip():
                    continue
                next_indent = len(next_line) - len(next_line.lstrip())

                if next_indent <= child_indent and next_line.strip():
                    if next_indent == child_indent and not next_line.strip().startswith(
                        "#"
                    ):
                        break
                    elif next_indent <= section_indent:
                        break
                else:
                    last_parameter_end = j

    return last_parameter_end + 1


def find_array_item_insertion_point(lines, path_parts, array_name, array_index):
    """Find insertion point for a parameter within a specific array item."""
    array_section_start = None
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == f"{array_name}:" or stripped.endswith(f":{array_name}:"):
            array_section_start = i
            array_indent = len(line) - len(line.lstrip())
            break

    if array_section_start is None:
        return None

    current_item = -1
    item_start = None
    item_indent = None

    for i in range(array_section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        if line.strip().startswith("-") and line_indent == array_indent and ":" in line:
            current_item += 1
            if current_item == array_index:
                item_start = i
                item_indent = line_indent
                break

        elif (
            line_indent <= array_indent
            and line.strip()
            and not line.strip().startswith("-")
        ):
            break

    if item_start is None:
        return None

    last_parameter_end = item_start
    for i in range(item_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())

        if line_indent <= item_indent and line.strip():
            if line.strip().startswith("-") or line_indent <= array_indent:
                break

        if line_indent > item_indent:
            last_parameter_end = i

    return last_parameter_end + 1


def get_section_indent(lines, position, target_indent_level=None):
    if target_indent_level is not None:
        return " " * target_indent_level

    for i in range(position - 1, -1, -1):
        line = lines[i]
        if line.strip() and not line.strip().startswith("#"):
            return line[: len(line) - len(line.lstrip())]
    return ""


def calculate_array_item_indent(lines, insert_position, array_name):
    """Calculate the correct indentation for a parameter within an array item."""
    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        if "#wetthresh:" in line:
            return line[: len(line) - len(line.lstrip())]

    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        stripped = line.strip()

        if not stripped or stripped.startswith("#"):
            continue

        if stripped.startswith("-"):
            continue

        line_indent = len(line) - len(line.lstrip())

        if stripped.endswith(":") and not stripped.startswith("value:"):
            for j in range(i + 1, min(len(lines), i + 5)):
                if (
                    j < len(lines)
                    and lines[j].strip()
                    and not lines[j].strip().startswith("#")
                ):
                    next_indent = len(lines[j]) - len(lines[j].lstrip())
                    if next_indent > line_indent:
                        return " " * line_indent
                    break

    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == f"{array_name}:" or stripped.endswith(f":{array_name}:"):
            array_indent = len(line) - len(line.lstrip())
            return " " * (array_indent + 2)

    return ""


def format_yaml_key(key):
    """Format a key for YAML output, ensuring numeric strings are properly quoted."""
    if isinstance(key, str) and key.isdigit():
        return f"'{key}'"
    elif isinstance(key, int):
        return f"'{key}'"
    else:
        return str(key)


def create_uptodate_yaml_header():
    header = """# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

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
                    default_value = get_null_placeholder()
                    lines.append(f"    {formatted_subkey}: {default_value}")
            else:
                default_value = get_null_placeholder()
                lines.append(f"  {formatted_key}: {default_value}")
    else:
        default_value = get_null_placeholder()
        lines.append(f"{param_name}: {default_value}")
    return lines


def get_null_placeholder():
    """Return null placeholder for missing parameters."""
    return "null"


def cleanup_renamed_comments(yaml_content):
    """Remove renamed in standard comments from YAML content for clean output."""
    lines = yaml_content.split("\n")
    cleaned_lines = []

    for line in lines:
        if "#RENAMED IN STANDARD" in line:
            clean_line = line.split("#RENAMED IN STANDARD")[0].rstrip()
            cleaned_lines.append(clean_line)
        else:
            cleaned_lines.append(line)

    return "\n".join(cleaned_lines)


def remove_extra_parameters_from_yaml(yaml_content, extra_params):
    """Remove extra parameters from YAML content for public mode."""
    if not extra_params:
        return yaml_content

    lines = yaml_content.split("\n")
    lines_to_remove = set()

    # Convert extra_params paths to line numbers to remove
    for param_path in extra_params:
        param_name = param_path.split(".")[-1]

        for i, line in enumerate(lines):
            stripped = line.strip()
            if stripped.startswith(f"{param_name}:"):
                lines_to_remove.add(i)
                break

    for line_num in sorted(lines_to_remove, reverse=True):
        lines.pop(line_num)

    return "\n".join(lines)


def create_uptodate_yaml_with_missing_params(
    yaml_content, missing_params, extra_params=None, mode="public"
):
    """Create clean YAML with missing parameters added but no inline comments."""
    clean_yaml_content = cleanup_renamed_comments(yaml_content)
    # Note: Extra parameters are now preserved in both public and dev mode
    # In public mode, they will be reported as ACTION NEEDED items in the report

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
            parent_section = path_parts[-2] if len(path_parts) >= 2 else None
            if parent_section:
                if "[" in parent_section and "]" in parent_section:
                    array_name = parent_section.split("[")[0]
                    indent = calculate_array_item_indent(
                        lines, insert_position, array_name
                    )
                else:
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

            annotation_lines = create_clean_missing_param_annotation(
                param_name, standard_value
            )
            indented_lines = []
            for line in annotation_lines:
                if line.strip():
                    indented_lines.append(indent + line)
                else:
                    indented_lines.append(line)

            for i, annotation_line in enumerate(reversed(indented_lines)):
                lines.insert(insert_position, annotation_line)

    header = create_uptodate_yaml_header()
    content_with_lines = "\n".join(lines)

    clean_content = header + content_with_lines
    return clean_content


def create_analysis_report(
    missing_params,
    renamed_replacements,
    extra_params=None,
    uptodate_filename=None,
    mode="public",
    phase="A",
):
    """Create analysis report with summary of changes."""
    report_lines = []

    # Generate phase-specific title
    # Use unified report title for all validation phases
    title = "SUEWS Validation Report"

    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(
        f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}"
    )
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    # Count parameters by type
    urgent_count = sum(1 for _, _, is_physics in missing_params if is_physics)
    optional_count = len(missing_params) - urgent_count
    renamed_count = len(renamed_replacements)
    extra_count = len(extra_params) if extra_params else 0

    # ACTION NEEDED section - critical/urgent parameters AND forbidden extra parameters
    # Categorise extra parameters to find forbidden ones
    forbidden_extras = []
    if extra_count > 0 and mode != "public":  # Only show forbidden extras in dev mode
        categorised = categorise_extra_parameters(extra_params)
        forbidden_extras = categorised["ACTION_NEEDED"]

    total_action_needed = urgent_count + len(forbidden_extras)
    has_action_items = total_action_needed > 0

    if has_action_items:
        report_lines.append("## ACTION NEEDED")

        # Critical missing parameters
        if urgent_count > 0:
            report_lines.append(
                f"- Found ({urgent_count}) critical missing parameter(s):"
            )
            for param_path, standard_value, is_physics in missing_params:
                if is_physics:
                    param_name = param_path.split(".")[-1]
                    report_lines.append(
                        f"-- {param_name} has been added to the updated YAML and set to null"
                    )
                    report_lines.append(
                        f"   Suggested fix: Set appropriate value based on SUEWS documentation -- https://suews.readthedocs.io/latest/"
                    )
            if forbidden_extras:
                report_lines.append("")  # Add spacing if both sections present

        # Forbidden extra parameters
        if forbidden_extras:
            report_lines.append(
                f"- Found ({len(forbidden_extras)}) parameter(s) in forbidden locations:"
            )
            allowed_sections = ", ".join(get_allowed_nested_sections_in_properties())
            for param_path in forbidden_extras:
                param_name = param_path.split(".")[-1]
                report_lines.append(f"-- {param_name} at level {param_path}")
                report_lines.append(
                    f"   Reason: Extra parameters not allowed in SiteProperties"
                )

                # Add mode-specific messaging
                if mode.lower() == "public":
                    report_lines.append(
                        f"   Suggested fix: This param name is not allowed in Phase C and will raise a validation error."
                    )
                    report_lines.append(
                        f"   You selected --mode public. Consider to either remove these names or switch to --mode dev"
                    )
                else:
                    report_lines.append(
                        f"   Suggested fix: Remove parameter, change code in data_model to allow extra parameters in this location, \n or move to an allowed nested section ({allowed_sections})"
                    )

        report_lines.append("")

    # Handle extra parameters in ACTION NEEDED section for public mode
    if mode == "public" and extra_count > 0:
        # In public mode, show ALL extra parameters as ACTION NEEDED items
        if not has_action_items:
            report_lines.append("## ACTION NEEDED")
            has_action_items = True

        report_lines.append(
            f"- Found ({extra_count}) not allowed extra parameter name(s):"
        )
        for param_path in extra_params:
            param_name = param_path.split(".")[-1]
            report_lines.append(f"-- {param_name} at level {param_path}")
            report_lines.append(
                f"   Suggested fix: You selected Public mode. Consider either to switch to Dev mode, or remove this extra parameter since this is not in the standard yaml."
            )
        report_lines.append("")

    # NO ACTION NEEDED section - optional and informational items
    # Calculate allowed extra parameters (those not in forbidden locations)
    # In public mode, extra parameters are now handled as ACTION NEEDED items
    # In dev mode, only allowed extra parameters are counted
    if mode == "public":  # Public mode
        allowed_extras_count = (
            0  # No extra parameters in NO ACTION NEEDED for public mode
        )
    else:  # Dev mode
        allowed_extras_count = (
            extra_count - len(forbidden_extras) if extra_count > 0 else 0
        )

    has_no_action_items = (
        optional_count > 0 or allowed_extras_count > 0 or renamed_count > 0
    )

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
                    report_lines.append(
                        f"-- {param_name} added to the updated YAML and set to null"
                    )
            report_lines.append("")

        # Renamed parameters
        if renamed_count > 0:
            report_lines.append(f"- Updated ({renamed_count}) renamed parameter(s):")
            for old_name, new_name in renamed_replacements:
                report_lines.append(f"-- {old_name} changed to {new_name}")
            report_lines.append("")

        # NOT IN STANDARD parameters - Dev mode handling only
        if extra_count > 0 and mode != "public":
            # In public mode, extra parameters are handled in ACTION NEEDED section above
            # In dev mode, show found parameters (current behavior)
            categorised = categorise_extra_parameters(extra_params)
            no_action_extras = categorised["NO_ACTION_NEEDED"]

            # Show allowed location extra parameters first (NO ACTION NEEDED)
            if no_action_extras:
                report_lines.append(
                    f"- Found ({len(no_action_extras)}) parameter(s) not in standard:"
                )
                for param_path in no_action_extras:
                    param_name = param_path.split(".")[-1]
                    report_lines.append(f"-- {param_name} at level {param_path}")
                report_lines.append("")

                # Show forbidden location extra parameters as ACTION NEEDED
                # (These will be moved to the ACTION NEEDED section below)
                # We'll handle this below when updating that section

    # If neither ACTION NEEDED nor NO ACTION NEEDED sections were added,
    # indicate that Phase A passed without issues
    if not has_action_items and not has_no_action_items:
        if phase == "A":
            report_lines.append("Configuration structure check passed")
        elif "A" in phase:  # Multi-phase like "AB", "AC", "ABC"
            report_lines.append("Configuration structure check passed")
        report_lines.append("")

    # Footer separator
    report_lines.append("# " + "=" * 50)

    return "\n".join(report_lines)


def annotate_missing_parameters(
    user_file,
    standard_file,
    uptodate_file=None,
    report_file=None,
    mode="public",
    phase="A",
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
            original_yaml_content, missing_params, extra_params, mode
        )

        # Create analysis report
        uptodate_filename = os.path.basename(uptodate_file) if uptodate_file else None
        report_content = create_analysis_report(
            missing_params,
            renamed_replacements,
            extra_params,
            uptodate_filename,
            mode,
            phase,
        )
    else:
        print("No missing in standard or renamed in standard parameters found!")
        # Still create clean files
        uptodate_content = create_uptodate_yaml_header() + original_yaml_content
        uptodate_filename = os.path.basename(uptodate_file) if uptodate_file else None
        report_content = create_analysis_report(
            [], [], [], uptodate_filename, mode, phase
        )

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
            print(f"[OK] Branch: {current_branch} (standard file matches master)")
    else:
        print(f"[OK] Branch: {current_branch}")

    print(f"[OK] Standard file: {standard_file}")
    return True


def main():
    print(" SUEWS YAML Configuration Analysis")
    print("=" * 50)

    standard_file = "src/supy/sample_data/sample_config.yml"
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
