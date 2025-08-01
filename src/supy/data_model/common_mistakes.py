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
                lines[i] = f"{indent}{new_key}: {value}  #DEPRECATED! - Found \"{old_key}\" and changed into \"{new_key}\""
                replacements.append((old_key, new_key))
    return '\n'.join(lines), replacements

def is_physics_option(param_path):
    param_name = param_path.split('.')[-1]
    return 'model.physics' in param_path and param_name in PHYSICS_OPTIONS

def find_missing_parameters(user_data, reference_data, current_path=""):
    missing_params = []
    if isinstance(reference_data, dict):
        user_dict = user_data if isinstance(user_data, dict) else {}
        for key, ref_value in reference_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            if key not in user_dict:
                is_physics = is_physics_option(full_path)
                missing_params.append((full_path, ref_value, is_physics))
            elif isinstance(ref_value, dict) and isinstance(user_dict.get(key), dict):
                nested_missing = find_missing_parameters(user_dict[key], ref_value, full_path)
                missing_params.extend(nested_missing)
            elif isinstance(ref_value, list) and isinstance(user_dict.get(key), list):
                nested_missing = find_missing_parameters_in_lists(user_dict[key], ref_value, full_path)
                missing_params.extend(nested_missing)
    elif isinstance(reference_data, list):
        user_list = user_data if isinstance(user_data, list) else []
        nested_missing = find_missing_parameters_in_lists(user_list, reference_data, current_path)
        missing_params.extend(nested_missing)
    return missing_params

def find_missing_parameters_in_lists(user_list, reference_list, current_path=""):
    missing_params = []
    for i, ref_item in enumerate(reference_list):
        item_path = f"{current_path}[{i}]" if current_path else f"[{i}]"
        if i < len(user_list):
            user_item = user_list[i]
            nested_missing = find_missing_parameters(user_item, ref_item, item_path)
            missing_params.extend(nested_missing)
        else:
            if isinstance(ref_item, dict):
                flattened_missing = flatten_missing_dict(ref_item, item_path)
                missing_params.extend(flattened_missing)
            else:
                is_physics = is_physics_option(item_path)
                missing_params.append((item_path, ref_item, is_physics))
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

def create_missing_param_annotation(param_name, ref_value, base_indent, is_physics=False):
    lines = []
    if isinstance(ref_value, dict):
        lines.append(f"{base_indent}{param_name}:")
        for key, value in ref_value.items():
            # Ensure numeric keys are properly quoted as strings
            formatted_key = format_yaml_key(key)
            if isinstance(value, dict):
                lines.append(f"{base_indent}  {formatted_key}:")
                for subkey, subvalue in value.items():
                    formatted_subkey = format_yaml_key(subkey)
                    comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
                    lines.append(f"{base_indent}    {formatted_subkey}: null{comment}")
            else:
                comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
                lines.append(f"{base_indent}  {formatted_key}: null{comment}")
    else:
        comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
        lines.append(f"{base_indent}{param_name}: null{comment}")
    return lines

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

def create_yaml_legend():
    legend = '''# =============================================================================
# SUEWS Configuration File - Missing Parameters Guide
# =============================================================================
#
# This file has been automatically processed by common_mistakes.py to highlight missing parameters.
# Please review and update the following annotations:
#
# LEGEND:
# -------
# parameter: null #URGENT-MISSING!  <- Physics option requiring immediate attention
#                                       You MUST set a valid value or precheck will fail!
#
# parameter: null #MISSING!         <- Optional parameter with default behavior
#                                       You can leave as null or set a specific value.
#
# parameter: value  #DEPRECATED!    <- Found "deprecated_naming" and changed into "new_naming"
#                                       This parameter has been renamed in SUEWS.
#
# HOW TO USE:
# -----------
# 1. Search for "#URGENT-MISSING!" - These MUST be manually updated before running SUEWS
# 2. Search for "#MISSING!" - These are optional but may affect model behavior. 
#                             Replace "null" with appropriate values based on your study requirements.
# 3. Search for "#DEPRECATED!" - These are changed into new naming convention.
# 4. Remove the comment annotations after setting values
#
# PHYSICS OPTIONS GUIDE:
# ----------------------
# Most physics methods accept integer values (0, 1, 2, 3, etc.)
# Refer to SUEWS documentation for valid options for each parameter
#
# =============================================================================

'''
    return legend

def annotate_yaml_with_missing_params(yaml_content, missing_params):
    if not missing_params:
        legend = create_yaml_legend()
        return legend + yaml_content
    lines = yaml_content.split('\n')
    missing_params.sort(key=lambda x: x[0].count('.'), reverse=True)
    for param_path, ref_value, is_physics in missing_params:
        path_parts = param_path.split('.')
        param_name = path_parts[-1]
        insert_position = find_insertion_point(lines, path_parts)
        if insert_position is not None:
            # Calculate the correct indentation based on the parent section
            parent_section = path_parts[-2] if len(path_parts) >= 2 else None
            if parent_section:
                # Handle array indices in parent section
                if '[' in parent_section and ']' in parent_section:
                    array_name = parent_section.split('[')[0]
                    # For array items, find the item and calculate appropriate indent
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
            annotation_lines = create_missing_param_annotation(param_name, ref_value, indent, is_physics)
            for i, annotation_line in enumerate(reversed(annotation_lines)):
                lines.insert(insert_position, annotation_line)
    legend = create_yaml_legend()
    annotated_content = legend + '\n'.join(lines)
    return annotated_content

def annotate_missing_parameters(user_file, reference_file, output_file):
    try:
        with open(user_file, 'r') as f:
            original_yaml_content = f.read()
        original_yaml_content, deprecated_replacements = handle_deprecated_parameters(original_yaml_content)
        user_data = yaml.safe_load(original_yaml_content)
        with open(reference_file, 'r') as f:
            reference_data = yaml.safe_load(f)
    except FileNotFoundError as e:
        print(f"Error: File not found - {e}")
        return
    except yaml.YAMLError as e:
        print(f"Error: Invalid YAML - {e}")
        return
    missing_params = find_missing_parameters(user_data, reference_data)
    if not missing_params and not deprecated_replacements:
        print("No missing or deprecated parameters found!")
        legend = create_yaml_legend()
        with open(output_file, 'w') as f:
            f.write(legend + original_yaml_content)
        return
    if missing_params:
        physics_params = [(path, val, is_phys) for path, val, is_phys in missing_params if is_phys]
        regular_params = [(path, val, is_phys) for path, val, is_phys in missing_params if not is_phys]
        print(f" Found {len(missing_params)} missing parameters:")
        if physics_params:
            print(f"\n URGENT: {len(physics_params)} physics options require immediate attention:")
            for param_path, ref_value, _ in physics_params:
                print(f"   - {param_path} (currently missing - precheck will fail!)")
        if regular_params:
            print(f"\n -  {len(regular_params)} optional parameters found:")
            for param_path, ref_value, _ in regular_params:
                print(f"   - {param_path}")
    if deprecated_replacements:
        print(f"\n Deprecated parameters replaced:")
        for old_key, new_key in deprecated_replacements:
            print(f"   - {old_key} -> {new_key}")
    annotated_content = annotate_yaml_with_missing_params(original_yaml_content, missing_params)
    with open(output_file, 'w') as f:
        f.write(annotated_content)
    print(f"\n Annotated file written to: {output_file}")

def main():
    print(" SUEWS Configuration Analysis")
    print("=" * 50)

    reference_file = "src/supy/sample_run/sample_config.yml"
    user_file = "src/supy/data_model/sample2.yml"

    print(f"Reference file: {reference_file}")
    print(f"User file: {user_file}")
    print()

    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    commented_filename = f"commented_{basename}"
    output_file = os.path.join(dirname, commented_filename)

    print(" Step 2: Creating commented user file...")
    annotate_missing_parameters(
        user_file=user_file,
        reference_file=reference_file,
        output_file=output_file
    )
    print()
    print(" Analysis complete!")
    print(" Output file:")
    print(f"   - {output_file}: User file with missing parameters and legend")
    print("\n Usage Guide:")
    print("   1. Open the file and read the legend at the top")
    print("   2. Search for '#URGENT-MISSING!' and set valid values")
    print("   3. Review '#MISSING!' parameters and set if needed")
    print("   4. Review '#DEPRECATED!' parameters")
    print("   5. Run precheck validation after making changes")


if __name__ == "__main__":
    main()
