import pandas as pd
import geopandas as gpd
import numpy as np
import f90nml as nml
import yaml
import os
import supy as sp
from supy.data_model import SUEWSConfig

from yaml.representer import SafeRepresenter

class CustomDumper(yaml.SafeDumper):
    pass

def missing_comment_representer(dumper, data):
    node = dumper.represent_scalar('tag:yaml.org,2002:null', 'null')
    node.comment = ' MISSING!'
    return node

# Register representer
CustomDumper.add_representer(type(None), missing_comment_representer)


def compare_dicts(dicts):
    equal = {}
    diff = {}
    keys = set().union(*[d.keys() for d in dicts if d is not None])
    for key in keys:
        elements = [d[key] if d is not None and key in d else {"__missing__": True} for d in dicts]
        if all(elem == elements[0] for elem in elements):
            equal[key] = elements[0]
        else:
            if all(isinstance(elem, dict) and "__missing__" not in elem for elem in elements if elem):
                sub_equal, sub_diff = compare_dicts([
                    elem if "__missing__" not in elem else None for elem in elements
                ])
                if sub_equal:
                    equal[key] = sub_equal
                if sub_diff:
                    diff[key] = sub_diff
            elif all(isinstance(elem, list) for elem in elements if elem is not None) and any(isinstance(elem, list) for elem in elements):
                sub_equal, sub_diff = compare_lists([
                    elem if isinstance(elem, list) else [] for elem in elements
                ])
                if sub_equal:
                    equal[key] = sub_equal
                if sub_diff:
                    diff[key] = sub_diff
            else:
                diff[key] = {
                    f'file{i+1}': (None if isinstance(elem, dict) and "__missing__" in elem else elem)
                    for i, elem in enumerate(elements)
                }
    return equal, diff


def compare_lists(lists):
    equal = []
    diff = []
    max_len = max(len(lst) for lst in lists if lst is not None)
    for i in range(max_len):
        elements = [lst[i] if i < len(lst) else None for lst in lists if lst is not None]
        if all(elem == elements[0] for elem in elements):
            equal.append(elements[0])
        else:
            if all(isinstance(elem, dict) for elem in elements if elem is not None):
                sub_equal, sub_diff = compare_dicts(elements)
                if sub_equal:
                    equal.append(sub_equal)
                if sub_diff:
                    diff.append(sub_diff)
            elif all(isinstance(elem, list) for elem in elements if elem is not None):
                sub_equal, sub_diff = compare_lists(elements)
                if sub_equal:
                    equal.append(sub_equal)
                if sub_diff:
                    diff.append(sub_diff)
            else:
                diff.append({f'file{i+1}': elem for i, elem in enumerate(elements)})
    return equal, diff

def compare_yaml_files(files, output_equal, output_diff):
    yaml_data = []
    names = []

    for name, file_path in files:
        names.append(name)
        with open(file_path, 'r') as f:
            yaml_data.append(yaml.safe_load(f))  

    equal, diff = compare_dicts(yaml_data)

    equal_output = {'Comparison': " and ".join(names), 'Equal': equal}
    diff_output = {'Comparison': {f'file{i+1}': name for i, name in enumerate(names)}, 'Differences': diff}

    with open(output_equal, 'w') as f_equal:
        yaml.dump(equal_output, f_equal, sort_keys=False)

    with open(output_diff, 'w') as f_diff:
        yaml.dump(diff_output, f_diff, sort_keys=False, Dumper=CustomDumper)


def read_configs(dir_path: str):
    run_configs_dict = {}
    for file_name in os.listdir(dir_path):
        if file_name.endswith(".yml"):
            print("Reading in configuration file: ", file_name)
            config = sp.data_model.init_config_from_yaml(dir_path + file_name)
            run_configs_dict[file_name.split(".")[0]] = config
    return run_configs_dict


# Define physics options that require immediate user attention
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

def is_physics_option(param_path):
    """
    Check if a parameter is a model physics option that requires urgent attention.
    
    Args:
        param_path: Full parameter path (e.g., 'model.physics.storageheatmethod')
        
    Returns:
        bool: True if it's a physics option
    """
    param_name = param_path.split('.')[-1]
    return 'model.physics' in param_path and param_name in PHYSICS_OPTIONS

def find_missing_parameters(user_data, reference_data, current_path=""):
    """
    Find parameters that exist in reference but are missing in user data.
    
    Args:
        user_data: User's configuration data
        reference_data: Reference configuration data  
        current_path: Current path in the nested structure (for tracking location)
        
    Returns:
        List of tuples: (parameter_path, reference_value, is_physics_option)
    """
    missing_params = []
    
    if isinstance(reference_data, dict):
        user_dict = user_data if isinstance(user_data, dict) else {}
        
        for key, ref_value in reference_data.items():
            full_path = f"{current_path}.{key}" if current_path else key
            
            if key not in user_dict:
                # Parameter is completely missing
                is_physics = is_physics_option(full_path)
                missing_params.append((full_path, ref_value, is_physics))
            elif isinstance(ref_value, dict) and isinstance(user_dict.get(key), dict):
                # Both are dicts, recurse deeper
                nested_missing = find_missing_parameters(
                    user_dict[key], ref_value, full_path
                )
                missing_params.extend(nested_missing)
            elif isinstance(ref_value, list) and isinstance(user_dict.get(key), list):
                # Both are lists, compare elements
                nested_missing = find_missing_parameters_in_lists(
                    user_dict[key], ref_value, full_path
                )
                missing_params.extend(nested_missing)
                
    elif isinstance(reference_data, list):
        user_list = user_data if isinstance(user_data, list) else []
        nested_missing = find_missing_parameters_in_lists(
            user_list, reference_data, current_path
        )
        missing_params.extend(nested_missing)
            
    return missing_params


def find_missing_parameters_in_lists(user_list, reference_list, current_path=""):
    """
    Find missing parameters when comparing lists/arrays.
    
    Args:
        user_list: User's list data
        reference_list: Reference list data
        current_path: Current path in the nested structure
        
    Returns:
        List of tuples: (parameter_path, reference_value, is_physics_option)
    """
    missing_params = []
    
    # Compare each element in the reference list with corresponding user list element
    for i, ref_item in enumerate(reference_list):
        item_path = f"{current_path}[{i}]" if current_path else f"[{i}]"
        
        if i < len(user_list):
            # User has this array element, compare contents
            user_item = user_list[i]
            nested_missing = find_missing_parameters(user_item, ref_item, item_path)
            missing_params.extend(nested_missing)
        else:
            # User is missing this entire array element
            if isinstance(ref_item, dict):
                # Flatten the missing dict and add all its parameters
                flattened_missing = flatten_missing_dict(ref_item, item_path)
                missing_params.extend(flattened_missing)
            else:
                is_physics = is_physics_option(item_path)
                missing_params.append((item_path, ref_item, is_physics))
    
    return missing_params


def flatten_missing_dict(data, current_path=""):
    """
    Flatten a missing dictionary structure into individual parameter paths.
    
    Args:
        data: Dictionary data to flatten
        current_path: Current path in the nested structure
        
    Returns:
        List of tuples: (parameter_path, reference_value, is_physics_option)
    """
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
    """
    Find the best position to insert a missing parameter comment.
    
    Returns the line index after which to insert the missing parameter.
    """
    if len(path_parts) < 2:
        return None
        
    # Find the parent section (e.g., "physics:" for "model.physics.storageheatmethod")
    parent_section = path_parts[-2]
    section_indent = None
    section_start = None
    
    for i, line in enumerate(lines):
        stripped = line.strip()
        
        # Look for the parent section
        if stripped == f"{parent_section}:" or stripped.endswith(f":{parent_section}:"):
            section_indent = len(line) - len(line.lstrip())
            section_start = i
            break
    
    if section_start is None:
        return None
    
    # Find the end of this section (where to insert the missing parameter)
    child_indent = section_indent + 2  # Standard YAML 2-space indent
    last_parameter_end = section_start
    current_param_start = None
    
    for i in range(section_start + 1, len(lines)):
        line = lines[i]
        
        # Skip empty lines
        if not line.strip():
            continue
            
        line_indent = len(line) - len(line.lstrip())
        
        # If we've reached a line at the same level or less indented than parent, we're done
        if line_indent <= section_indent and line.strip():
            break
            
        # If this line is at the child level and not a comment, it's a new parameter
        if line_indent == child_indent and not line.strip().startswith('#'):
            # If we had a previous parameter, mark its end
            if current_param_start is not None:
                last_parameter_end = i - 1
            current_param_start = i
        # If this line is more indented than child level, it's part of the current parameter
        elif line_indent > child_indent and current_param_start is not None:
            # Update the end of the current parameter
            last_parameter_end = i
    
    # If we were tracking a parameter, make sure to end it properly
    if current_param_start is not None:
        # Find the actual end of the last parameter block
        for i in range(last_parameter_end, len(lines)):
            line = lines[i]
            if not line.strip():
                continue
            line_indent = len(line) - len(line.lstrip())
            # If we reach a line at or less indented than the parent, stop
            if line_indent <= section_indent and line.strip():
                break
            # If we reach a line at child level, it's a new parameter, stop
            elif line_indent == child_indent and not line.strip().startswith('#'):
                break
            else:
                last_parameter_end = i
    
    return last_parameter_end + 1


def get_section_indent(lines, position):
    """Get the appropriate indentation for the section."""
    # Look backwards for a non-empty line to determine indentation
    for i in range(position - 1, -1, -1):
        line = lines[i]
        if line.strip() and not line.strip().startswith('#'):
            return line[:len(line) - len(line.lstrip())]
    
    return ""  # Default to no indentation


def create_missing_param_annotation(param_name, ref_value, base_indent, is_physics=False):
    """
    Create annotation lines for a missing parameter.
    
    Args:
        param_name: Name of the missing parameter
        ref_value: Reference value from the complete config
        base_indent: Base indentation to use
        is_physics: Whether this is a physics option requiring urgent attention
        
    Returns:
        List of annotation lines to insert
    """
    lines = []
    
    # Add the parameter with null value and appropriate comment
    if isinstance(ref_value, dict):
        lines.append(f"{base_indent}{param_name}:")
        for key, value in ref_value.items():
            if isinstance(value, dict):
                lines.append(f"{base_indent}  {key}:")
                for subkey, subvalue in value.items():
                    comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
                    lines.append(f"{base_indent}    {subkey}: null{comment}")
            else:
                comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
                lines.append(f"{base_indent}  {key}: null{comment}")
    else:
        comment = " #URGENT-MISSING!" if is_physics else " #MISSING!"
        lines.append(f"{base_indent}{param_name}: null{comment}")
    
    return lines


def create_yaml_legend():
    """
    Create a comprehensive legend for the commented YAML file.
    
    Returns:
        String containing the legend to add at the top of the file
    """
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
# parameter: null #MISSING!          <- Optional parameter with default behavior
#                                       You can leave as null or set a specific value
#
# HOW TO USE:
# -----------
# 1. Search for "#URGENT-MISSING!" - these MUST be updated before running SUEWS
# 2. Search for "#MISSING!" - these are optional but may affect model behavior
# 3. Replace "null" with appropriate values based on your study requirements
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
    """
    Add missing parameters with null values and appropriate comments to the YAML content.
    
    Args:
        yaml_content: Original YAML content as string
        missing_params: List of (parameter_path, reference_value, is_physics) tuples
        
    Returns:
        Modified YAML content with missing parameters and legend
    """
    if not missing_params:
        # Still add legend even if no missing params
        legend = create_yaml_legend()
        return legend + yaml_content
        
    lines = yaml_content.split('\n')
    
    # Sort missing params by path depth (deepest first) to handle nested insertions correctly
    missing_params.sort(key=lambda x: x[0].count('.'), reverse=True)
    
    for param_path, ref_value, is_physics in missing_params:
        path_parts = param_path.split('.')
        param_name = path_parts[-1]
        
        # Find the parent section where this parameter should be added
        insert_position = find_insertion_point(lines, path_parts)
        
        if insert_position is not None:
            # Determine correct indentation
            indent = get_section_indent(lines, insert_position)
            
            # Create the missing parameter annotation
            annotation_lines = create_missing_param_annotation(param_name, ref_value, indent, is_physics)
            
            # Insert the annotation
            for i, annotation_line in enumerate(reversed(annotation_lines)):
                lines.insert(insert_position, annotation_line)
    
    # Add legend at the top
    legend = create_yaml_legend()
    annotated_content = legend + '\n'.join(lines)
    
    return annotated_content


def annotate_missing_parameters(user_file, reference_file, output_file):
    """
    Main function to annotate missing parameters in a user's YAML file.
    
    Args:
        user_file: Path to user's YAML file
        reference_file: Path to reference YAML file  
        output_file: Path to output annotated YAML file
    """
    
    # Load YAML files
    try:
        with open(user_file, 'r') as f:
            user_data = yaml.safe_load(f)
        
        with open(reference_file, 'r') as f:
            reference_data = yaml.safe_load(f)
            
        with open(user_file, 'r') as f:
            original_yaml_content = f.read()
            
    except FileNotFoundError as e:
        print(f"Error: File not found - {e}")
        return
    except yaml.YAMLError as e:
        print(f"Error: Invalid YAML - {e}")
        return
    
    # Find missing parameters
    missing_params = find_missing_parameters(user_data, reference_data)
    
    if not missing_params:
        print("No missing parameters found!")
        # Still write the output file with legend
        legend = create_yaml_legend()
        with open(output_file, 'w') as f:
            f.write(legend + original_yaml_content)
        return
    
    # Separate physics options from regular parameters
    physics_params = [(path, val, is_phys) for path, val, is_phys in missing_params if is_phys]
    regular_params = [(path, val, is_phys) for path, val, is_phys in missing_params if not is_phys]
    
    print(f" Found {len(missing_params)} missing parameters:")
    
    if physics_params:
        print(f"\n URGENT: {len(physics_params)} physics options require immediate attention:")
        for param_path, ref_value, _ in physics_params:
            print(f"   - {param_path} (currently missing - precheck will fail!)")
        print("\n  WARNING: You MUST set valid values for these physics options")
        print("   or the precheck validation will fail and SUEWS cannot run!")
        print("   These have been marked with #URGENT-MISSING! in the output file.")
    
    if regular_params:
        print(f"\nℹ  {len(regular_params)} optional parameters found:")
        for param_path, ref_value, _ in regular_params:
            print(f"   - {param_path}")
        print("   These have been marked with #MISSING! in the output file.")
    
    # Add comments for missing parameters
    annotated_content = annotate_yaml_with_missing_params(original_yaml_content, missing_params)
    
    # Write output file
    with open(output_file, 'w') as f:
        f.write(annotated_content)
    
    print(f" Annotated file written to: {output_file}")

def main():
    """
    Main function that runs both comparison and missing parameter annotation.
    """
    print(" SUEWS Configuration Analysis")
    print("=" * 50)
    
    # File paths
    reference_file = "./src/supy/sample_run/sample_config.yml"
    user_file = "./src/supy/data_model/sample2.yml"
    
    print(f"Reference file: {reference_file}")
    print(f"User file: {user_file}")
    print()
    
    # # Step 1: Compare files and generate diff/equal reports
    # print(" Step 1: Generating comparison reports...")
    # compare_yaml_files(
    #     files=[
    #         ("sample_config", reference_file),
    #         ("sample2", user_file)
    #     ],
    #     output_equal="./test_equal.yml",
    #     output_diff="./test_diff.yml"
    # )
    # print(" Comparison reports written to test_equal.yml and test_diff.yml")
    # print()
    
    # Annotate user file with missing parameters
    print(" Step 2: Creating commented user file...")
    annotate_missing_parameters(
        user_file=user_file,
        reference_file=reference_file,
        output_file="./commented_user.yml"
    )
    print()
    
    print("✨ Analysis complete!")
    print("📁 Output file:")
    #print("   - test_equal.yml: Parameters that match between files")
    #print("   - test_diff.yml: Parameters that differ between files") 
    print("   - commented_user.yml: User file with missing parameters and legend")
    print("\n📖 Usage Guide:")
    print("   1. Open commented_user.yml and read the legend at the top")
    print("   2. Search for '#URGENT-MISSING!' and set valid values")
    print("   3. Review '#MISSING!' parameters and set if needed")
    print("   4. Run precheck validation after making changes")


if __name__ == "__main__":
    main()