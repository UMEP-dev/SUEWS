import pandas as pd
import geopandas as gpd
import numpy as np
import f90nml as nml
import yaml
import os
import supy as sp
from supy.data_model import SUEWSConfig
from yaml.representer import SafeRepresenter

DEPRECATED_PARAMS = {
    'cp': 'rho_cp',
    'diagmethod': 'rslmethod',
    'localclimatemethod': 'rsllevel'
}

PHYSICS_OPTIONS = {
    'netradiationmethod',
    'emissionsmethod', 
    'storageheatmethod',
    'roughlenmommethod',
    'roughlenheatmethod',
    'stabilitymethod',
    'smdmethod',
    'waterusemethod',
    'rslmethod',
    'faimethod',
    'gsmodel',
    'snowuse',
    'stebbsmethod'
}

class CustomDumper(yaml.SafeDumper):
    pass

def missing_comment_representer(dumper, data):
    node = dumper.represent_scalar('tag:yaml.org,2002:null', 'null')
    node.comment = ' MISSING!'
    return node

CustomDumper.add_representer(type(None), missing_comment_representer)

def handle_deprecated_parameters(yaml_content: str):
    lines = yaml_content.split('\n')
    replacements = []
    for i, line in enumerate(lines):
        stripped = line.strip()
        for old_key, new_key in DEPRECATED_PARAMS.items():
            if stripped.startswith(f"{old_key}:"):
                indent = line[:len(line) - len(stripped)]
                value = stripped.split(":", 1)[1].strip()
                lines[i] = f"{indent}{new_key}: {value}  #RENAMED IN STANDARD - Found \"{old_key}\" and changed into \"{new_key}\""
                replacements.append((old_key, new_key))
    return '\n'.join(lines), replacements

def is_physics_option(param_path):
    param_name = param_path.split('.')[-1]
    return 'model.physics' in param_path and param_name in PHYSICS_OPTIONS

def find_extra_parameters(user_data, standard_data, current_path=""):
    """Find parameters that exist in user data but not in standard data."""
    extra_params = []
    if isinstance(user_data, dict) and isinstance(standard_data, dict):
        for key, user_value in user_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if key not in standard_data:
                # This parameter exists in user but not in standard
                extra_params.append(full_path)
            elif isinstance(user_value, dict) and isinstance(standard_data.get(key), dict):
                # Recursively check nested dictionaries
                nested_extra = find_extra_parameters(user_value, standard_data[key], full_path)
                extra_params.extend(nested_extra)
            elif isinstance(user_value, list) and isinstance(standard_data.get(key), list):
                # Handle lists/arrays
                nested_extra = find_extra_parameters_in_lists(user_value, standard_data[key], full_path)
                extra_params.extend(nested_extra)
    elif isinstance(user_data, list) and isinstance(standard_data, list):
        nested_extra = find_extra_parameters_in_lists(user_data, standard_data, current_path)
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
            elif isinstance(standard_value, dict) and isinstance(user_dict.get(key), dict):
                nested_missing = find_missing_parameters(user_dict[key], standard_value, full_path)
                missing_params.extend(nested_missing)
            elif isinstance(standard_value, list) and isinstance(user_dict.get(key), list):
                nested_missing = find_missing_parameters_in_lists(user_dict[key], standard_value, full_path)
                missing_params.extend(nested_missing)
    elif isinstance(standard_data, list):
        user_list = user_data if isinstance(user_data, list) else []
        nested_missing = find_missing_parameters_in_lists(user_list, standard_data, current_path)
        missing_params.extend(nested_missing)
    return missing_params

def find_missing_parameters_in_lists(user_list, standard_list, current_path=""):
    missing_params = []
    for i, standard_item in enumerate(standard_list):
        item_path = f"{current_path}[{i}]" if current_path else f"[{i}]"
        if i < len(user_list):
            user_item = user_list[i]
            nested_missing = find_missing_parameters(user_item, standard_item, item_path)
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
    if '[' in parent_section and ']' in parent_section:
        array_name = parent_section.split('[')[0]
        array_index = int(parent_section.split('[')[1].split(']')[0])
        return find_array_item_insertion_point(lines, path_parts, array_name, array_index)
    
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
        if line_indent == child_indent and not line.strip().startswith('#'):
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
                    if next_indent == child_indent and not next_line.strip().startswith('#'):
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
        if (line.strip().startswith('-') and 
            line_indent == array_indent and  # Must be exactly at array indent level
            ':' in line):  # Must contain a key (like "- alb:")
            current_item += 1
            if current_item == array_index:
                item_start = i
                item_indent = line_indent
                break
        
        # End of array section (non-array-item at same or less indent)
        elif line_indent <= array_indent and line.strip() and not line.strip().startswith('-'):
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
            if line.strip().startswith('-') or line_indent <= array_indent:
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
        if line.strip() and not line.strip().startswith('#'):
            return line[:len(line) - len(line.lstrip())]
    return ""

def calculate_array_item_indent(lines, insert_position, array_name):
    """Calculate the correct indentation for a parameter within an array item."""
    
    # First approach: find the commented wetthresh and use its indentation
    # This is the most reliable method since we know exactly what we want to replace
    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        if '#wetthresh:' in line:
            return line[:len(line) - len(line.lstrip())]
    
    # Second approach: look for existing parameters at the same level
    # Find parameters that are direct children of the array item, not value lines
    for i in range(insert_position - 1, -1, -1):
        line = lines[i]
        stripped = line.strip()
        
        # Skip empty lines and comments
        if not stripped or stripped.startswith('#'):
            continue
        
        # Skip array item markers (lines starting with "-")
        if stripped.startswith('-'):
            continue
        
        line_indent = len(line) - len(line.lstrip())
        
        # Look for parameter lines that end with `:` and are followed by indented values
        if stripped.endswith(':') and not stripped.startswith('value:'):
            # Check if next non-empty line is more indented (indicating this is a parent parameter)
            for j in range(i + 1, min(len(lines), i + 5)):
                if j < len(lines) and lines[j].strip() and not lines[j].strip().startswith('#'):
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
    header = '''# =============================================================================
# UP TO DATE YAML
# =============================================================================
#
# This file has been automatically updated by uptodate_yaml.py with all necessary changes:
# - Missing in standard parameters have been added with appropriate default values
# - Renamed in standard parameters have been updated to current naming conventions
# - All changes applied without inline comments for clean usage
#
# =============================================================================

'''
    return header

def create_clean_missing_param_annotation(param_name, standard_value, is_physics=False):
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
                    # Use null for physics parameters, appropriate defaults for others
                    default_value = get_default_value(subvalue, is_physics)
                    lines.append(f"    {formatted_subkey}: {default_value}")
            else:
                # Use null for physics parameters, appropriate defaults for others
                default_value = get_default_value(value, is_physics)
                lines.append(f"  {formatted_key}: {default_value}")
    else:
        # Use null for physics parameters, appropriate defaults for others
        default_value = get_default_value(standard_value, is_physics)
        lines.append(f"{param_name}: {default_value}")
    return lines

def get_default_value(standard_value, is_physics=False):
    """Get appropriate default value based on standard value."""
    # All missing parameters (both physics and non-physics) get null values
    # Users will set the appropriate values themselves
    return "null"

def mark_extra_parameters(yaml_content, extra_params):
    """Mark parameters that are NOT IN STANDARD in the YAML content."""
    if not extra_params:
        return yaml_content
    
    lines = yaml_content.split('\n')
    
    for param_path in extra_params:
        # Find the line containing this parameter
        param_name = param_path.split('.')[-1]
        
        # Handle array indices (e.g., walls[2].wetthresh -> wetthresh)
        if '[' in param_name and ']' in param_name:
            param_name = param_name.split('.')[-1]  # Get the actual parameter name after array index
        
        for i, line in enumerate(lines):
            stripped = line.strip()
            # Look for lines that define this parameter (key: value format)
            if stripped.startswith(f"{param_name}:") and not stripped.endswith("#NOT IN STANDARD"):
                # Add the NOT IN STANDARD comment
                lines[i] = line + "  #NOT IN STANDARD"
                break
    
    return '\n'.join(lines)

def cleanup_deprecated_comments(yaml_content):
    """Remove renamed in standard comments from YAML content for clean output."""
    lines = yaml_content.split('\n')
    cleaned_lines = []
    
    for line in lines:
        # Remove renamed in standard comments but keep the parameter line
        if '#RENAMED IN STANDARD' in line:
            # Extract the part before the comment
            clean_line = line.split('#RENAMED IN STANDARD')[0].rstrip()
            cleaned_lines.append(clean_line)
        else:
            cleaned_lines.append(line)
    
    return '\n'.join(cleaned_lines)

def create_uptodate_yaml_with_missing_params(yaml_content, missing_params, extra_params=None):
    """Create clean YAML with missing parameters added but no inline comments."""
    # First, clean up any renamed in standard comments from the yaml_content
    clean_yaml_content = cleanup_deprecated_comments(yaml_content)
    
    if not missing_params:
        header = create_uptodate_yaml_header()
        return header + clean_yaml_content
    
    lines = clean_yaml_content.split('\n')
    missing_params.sort(key=lambda x: x[0].count('.'), reverse=True)
    
    for param_path, standard_value, is_physics in missing_params:
        path_parts = param_path.split('.')
        param_name = path_parts[-1]
        insert_position = find_insertion_point(lines, path_parts)
        if insert_position is not None:
            # Calculate the correct indentation
            parent_section = path_parts[-2] if len(path_parts) >= 2 else None
            if parent_section:
                # Handle array indices in parent section
                if '[' in parent_section and ']' in parent_section:
                    array_name = parent_section.split('[')[0]
                    indent = calculate_array_item_indent(lines, insert_position, array_name)
                else:
                    # Find the parent section and calculate child indent
                    for i, line in enumerate(lines):
                        stripped = line.strip()
                        if stripped == f"{parent_section}:" or stripped.endswith(f":{parent_section}:"):
                            parent_indent = len(line) - len(line.lstrip())
                            child_indent_level = parent_indent + 2
                            indent = get_section_indent(lines, insert_position, child_indent_level)
                            break
                    else:
                        indent = get_section_indent(lines, insert_position)
            else:
                indent = get_section_indent(lines, insert_position)
            
            # Create clean annotation lines (without comments)
            annotation_lines = create_clean_missing_param_annotation(param_name, standard_value, is_physics)
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
    content_with_lines = '\n'.join(lines)
    
    # Note: We don't mark extra parameters in the clean YAML - it should have no inline comments
    # Extra parameters are only reported in the analysis report
    
    clean_content = header + content_with_lines
    return clean_content

def create_analysis_report(missing_params, deprecated_replacements, extra_params=None):
    """Create analysis report with summary of changes."""
    report_lines = []
    report_lines.append("# SUEWS Configuration Analysis Report")
    report_lines.append("# " + "="*50)
    report_lines.append("")
    
    # Count parameters by type
    urgent_count = sum(1 for _, _, is_physics in missing_params if is_physics)
    optional_count = len(missing_params) - urgent_count
    deprecated_count = len(deprecated_replacements)
    extra_count = len(extra_params) if extra_params else 0
    
    report_lines.append(f"## Summary")
    report_lines.append(f"- Found {len(missing_params)} MISSING IN STANDARD parameters")
    if urgent_count > 0:
        report_lines.append(f"-- URGENT: {urgent_count} physics options require immediate attention")
    if optional_count > 0:
        report_lines.append(f"-- {optional_count} optional parameters found")
    if deprecated_count > 0:
        report_lines.append(f"- Found {deprecated_count} RENAMED IN STANDARD parameters")
    if extra_count > 0:
        report_lines.append(f"- Found {extra_count} NOT IN STANDARD parameters")
    report_lines.append("")
    
    # Detailed breakdown - combined MISSING IN STANDARD section with urgent ones first
    if missing_params:
        report_lines.append("## MISSING IN STANDARD Parameters")
        report_lines.append("Missing in standard parameters found (urgent ones listed first):")
        report_lines.append("")
        
        # First, list all URGENT-MISSING parameters (physics options)
        urgent_found = False
        for param_path, standard_value, is_physics in missing_params:
            if is_physics:
                if not urgent_found:
                    report_lines.append("**URGENT - Physics options (MUST be set or precheck will fail):**")
                    urgent_found = True
                param_name = param_path.split('.')[-1]
                report_lines.append(f"- {param_name} at level {param_path}")
        
        if urgent_found:
            report_lines.append("")
        
        # Then, list all optional MISSING parameters
        optional_found = False
        for param_path, standard_value, is_physics in missing_params:
            if not is_physics:
                if not optional_found:
                    report_lines.append("**Optional parameters (may affect model behavior):**")
                    optional_found = True
                param_name = param_path.split('.')[-1]
                report_lines.append(f"- {param_name} at level {param_path}")
        
        report_lines.append("")
    
    if extra_count > 0:
        report_lines.append("## NOT IN STANDARD Parameters")
        report_lines.append("These parameters exist in your configuration but are not in the standard:")
        for param_path in extra_params:
            param_name = param_path.split('.')[-1]
            report_lines.append(f"- {param_name} at level {param_path}")
        report_lines.append("")
    
    if deprecated_count > 0:
        report_lines.append("## RENAMED IN STANDARD Parameters")
        report_lines.append("These parameters were renamed to current conventions:")
        for old_name, new_name in deprecated_replacements:
            report_lines.append(f"- {old_name} -> {new_name}")
        report_lines.append("")
    
    # Usage instructions
    report_lines.append("## Next Steps")
    if urgent_count > 0:
        report_lines.append("1. Review URGENT-MISSING IN STANDARD parameters and set appropriate values")
        report_lines.append("2. These are required for SUEWS physics calculations")
    if optional_count > 0:
        report_lines.append("3. Review MISSING IN STANDARD parameters and set values based on your study requirements")
        report_lines.append("4. These have default behavior but may affect model results")
    report_lines.append("5. Use the uptodate_user.yml file as your updated configuration")
    
    return '\n'.join(report_lines)


def annotate_missing_parameters(user_file, standard_file, uptodate_file=None, report_file=None):
    try:
        with open(user_file, 'r') as f:
            original_yaml_content = f.read()
        original_yaml_content, deprecated_replacements = handle_deprecated_parameters(original_yaml_content)
        user_data = yaml.safe_load(original_yaml_content)
        with open(standard_file, 'r') as f:
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
    if missing_params or deprecated_replacements or extra_params:
        # Create uptodate YAML (clean, with NOT IN STANDARD markers)
        uptodate_content = create_uptodate_yaml_with_missing_params(original_yaml_content, missing_params, extra_params)
        
        # Create analysis report
        report_content = create_analysis_report(missing_params, deprecated_replacements, extra_params)
    else:
        print("No missing in standard or renamed in standard parameters found!")
        # Still create clean files
        uptodate_content = create_uptodate_yaml_header() + original_yaml_content
        report_content = create_analysis_report([], [], [])
    
    # Print terminal output (kept for backward compatibility)
    if missing_params:
        physics_params = [(path, val, is_phys) for path, val, is_phys in missing_params if is_phys]
        regular_params = [(path, val, is_phys) for path, val, is_phys in missing_params if not is_phys]
        print(f" Found {len(missing_params)} missing in standard parameters:")
        if physics_params:
            print(f"\n URGENT: {len(physics_params)} physics options require immediate attention:")
            for param_path, standard_value, _ in physics_params:
                print(f"   - {param_path} (currently missing - precheck will fail!)")
        if regular_params:
            print(f"\n -  {len(regular_params)} optional parameters found:")
            for param_path, standard_value, _ in regular_params:
                print(f"   - {param_path}")
    if deprecated_replacements:
        print(f"\n RENAMED IN STANDARD parameters:")
        for old_key, new_key in deprecated_replacements:
            print(f"   - {old_key} -> {new_key}")
    if extra_params:
        print(f"\n NOT IN STANDARD parameters:")
        for param_path in extra_params:
            print(f"   - {param_path}")
    
    # Write output files
    if uptodate_file:
        with open(uptodate_file, 'w') as f:
            f.write(uptodate_content)
        print(f"\n Clean YAML written to: {uptodate_file}")
    
    if report_file:
        with open(report_file, 'w') as f:
            f.write(report_content)
        print(f" Analysis report written to: {report_file}")

def main():
    print(" SUEWS YAML Configuration Analysis")
    print("=" * 50)

    ## MISSING LOGIC - check that standard_file is up to date!

    standard_file = "src/supy/sample_run/sample_config.yml"
    user_file = "src/supy/data_model/sample2.yml"

    print(f"Standard YAML file: {standard_file}")
    print(f"User YAML file: {user_file}")
    print()

    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    
    # Generate file names
    name_without_ext = os.path.splitext(basename)[0]
    uptodate_filename = f"uptodate_{basename}"
    report_filename = f"report_{name_without_ext}.txt"
    
    uptodate_file = os.path.join(dirname, uptodate_filename)
    report_file = os.path.join(dirname, report_filename)

    print(" Step 2: Creating output files...")
    annotate_missing_parameters(
        user_file=user_file,
        standard_file=standard_file,
        uptodate_file=uptodate_file,
        report_file=report_file
    )
    print()
    print(" Analysis complete!")
    print(" Output files:")
    print(f"   - {uptodate_file}: Clean YAML file with all changes applied")
    print(f"   - {report_file}: Analysis report with summary of changes")
    print("\n Usage Guide:")
    print(f"  1. Use {uptodate_file} as your YAML configuration file")
    print(f"  2. Review {report_file} for details about changes made")
    print(f"  3. Optionally run precheck validation with the uptodate file")


if __name__ == "__main__":
    main()
