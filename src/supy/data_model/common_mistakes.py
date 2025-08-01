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

CustomDumper.add_representer(type(None), missing_comment_representer)

DEPRECATED_PARAMS = {
    'cp': 'rho_cp',
    'diagmethod': 'rslmethod',
    'localclimatemethod': 'rsllevel'
}

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
    current_param_start = None
    for i in range(section_start + 1, len(lines)):
        line = lines[i]
        if not line.strip():
            continue
        line_indent = len(line) - len(line.lstrip())
        if line_indent <= section_indent and line.strip():
            break
        if line_indent == child_indent and not line.strip().startswith('#'):
            if current_param_start is not None:
                last_parameter_end = i - 1
            current_param_start = i
        elif line_indent > child_indent and current_param_start is not None:
            last_parameter_end = i
    if current_param_start is not None:
        for i in range(last_parameter_end, len(lines)):
            line = lines[i]
            if not line.strip():
                continue
            line_indent = len(line) - len(line.lstrip())
            if line_indent <= section_indent and line.strip():
                break
            if line_indent == child_indent and not line.strip().startswith('#'):
                break
            else:
                last_parameter_end = i
    return last_parameter_end + 1

def get_section_indent(lines, position):
    for i in range(position - 1, -1, -1):
        line = lines[i]
        if line.strip() and not line.strip().startswith('#'):
            return line[:len(line) - len(line.lstrip())]
    return ""

def create_missing_param_annotation(param_name, ref_value, base_indent, is_physics=False):
    lines = []
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
    reference_file = "./src/supy/sample_run/sample_config.yml"
    user_file = "./src/supy/data_model/sample2.yml"
    print(f"Reference file: {reference_file}")
    print(f"User file: {user_file}")
    print()
    print(" Step 2: Creating commented user file...")
    annotate_missing_parameters(
        user_file=user_file,
        reference_file=reference_file,
        output_file="./commented_user.yml"
    )
    print()
    print(" Analysis complete!")
    print(" Output file:")
    print("   - commented_user.yml: User file with missing parameters and legend")
    print("\n Usage Guide:")
    print("   1. Open commented_user.yml and read the legend at the top")
    print("   2. Search for '#URGENT-MISSING!' and set valid values")
    print("   3. Review '#MISSING!' parameters and set if needed")
    print("   4. Review '#DEPRECATED!' parameters")
    print("   5. Run precheck validation after making changes")

if __name__ == "__main__":
    main()
