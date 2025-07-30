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


def find_missing_parameters(user_data, reference_data, current_path=""):
    """
    Find parameters that exist in reference but are missing in user data.
    
    Args:
        user_data: User's configuration data
        reference_data: Reference configuration data  
        current_path: Current path in the nested structure (for tracking location)
        
    Returns:
        List of tuples: (parameter_path, reference_value)
    """
    missing_params = []
    
    if not isinstance(reference_data, dict):
        return missing_params
        
    for key, ref_value in reference_data.items():
        full_path = f"{current_path}.{key}" if current_path else key
        
        if key not in user_data:
            # Parameter is completely missing
            missing_params.append((full_path, ref_value))
        elif isinstance(ref_value, dict) and isinstance(user_data.get(key), dict):
            # Both are dicts, recurse deeper
            nested_missing = find_missing_parameters(
                user_data[key], ref_value, full_path
            )
            missing_params.extend(nested_missing)
            
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
    last_child_line = section_start
    
    for i in range(section_start + 1, len(lines)):
        line = lines[i]
        
        # Skip empty lines
        if not line.strip():
            continue
            
        line_indent = len(line) - len(line.lstrip())
        
        # If this line is at the child level and not a comment, it's a parameter
        if line_indent == child_indent and not line.strip().startswith('#'):
            last_child_line = i
        # If we've reached a line at the same level or less indented than parent, we're done
        elif line_indent <= section_indent and line.strip():
            break
    
    return last_child_line + 1


def get_section_indent(lines, position):
    """Get the appropriate indentation for the section."""
    # Look backwards for a non-empty line to determine indentation
    for i in range(position - 1, -1, -1):
        line = lines[i]
        if line.strip() and not line.strip().startswith('#'):
            return line[:len(line) - len(line.lstrip())]
    
    return ""  # Default to no indentation


def create_missing_param_annotation(param_name, ref_value, base_indent):
    """
    Create annotation lines for a missing parameter.
    
    Args:
        param_name: Name of the missing parameter
        ref_value: Reference value from the complete config
        base_indent: Base indentation to use
        
    Returns:
        List of annotation lines to insert
    """
    lines = []
    
    # Add a comment indicating the missing parameter
    lines.append(f"{base_indent}# MISSING {param_name}")
    
    # Add a commented-out example of the parameter structure
    if isinstance(ref_value, dict):
        lines.append(f"{base_indent}#{param_name}:")
        for key, value in ref_value.items():
            if isinstance(value, dict):
                lines.append(f"{base_indent}#  {key}:")
                for subkey, subvalue in value.items():
                    lines.append(f"{base_indent}#    {subkey}: {subvalue}")
            else:
                lines.append(f"{base_indent}#  {key}: {value}")
    else:
        lines.append(f"{base_indent}#{param_name}: {ref_value}")
    
    return lines


def annotate_yaml_with_missing_params(yaml_content, missing_params):
    """
    Add comments for missing parameters to the YAML content.
    
    Args:
        yaml_content: Original YAML content as string
        missing_params: List of (parameter_path, reference_value) tuples
        
    Returns:
        Modified YAML content with missing parameter comments
    """
    if not missing_params:
        return yaml_content
        
    lines = yaml_content.split('\n')
    
    # Sort missing params by path depth (deepest first) to handle nested insertions correctly
    missing_params.sort(key=lambda x: x[0].count('.'), reverse=True)
    
    for param_path, ref_value in missing_params:
        path_parts = param_path.split('.')
        param_name = path_parts[-1]
        
        # Find the parent section where this parameter should be added
        insert_position = find_insertion_point(lines, path_parts)
        
        if insert_position is not None:
            # Determine correct indentation
            indent = get_section_indent(lines, insert_position)
            
            # Create the missing parameter annotation
            annotation_lines = create_missing_param_annotation(param_name, ref_value, indent)
            
            # Insert the annotation
            for i, annotation_line in enumerate(reversed(annotation_lines)):
                lines.insert(insert_position, annotation_line)
    
    return '\n'.join(lines)


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
        print("✅ No missing parameters found!")
        # Still write the output file with original content
        with open(output_file, 'w') as f:
            f.write(original_yaml_content)
        return
    
    print(f"⚠️  Found {len(missing_params)} missing parameters:")
    for param_path, ref_value in missing_params:
        print(f"   - {param_path}")
    
    # Add comments for missing parameters
    annotated_content = annotate_yaml_with_missing_params(original_yaml_content, missing_params)
    
    # Write output file
    with open(output_file, 'w') as f:
        f.write(annotated_content)
    
    print(f"📝 Annotated file written to: {output_file}")

def main():
    """
    Main function that runs both comparison and missing parameter annotation.
    """
    print("🔍 SUEWS Configuration Analysis")
    print("=" * 50)
    
    # File paths
    reference_file = "./src/supy/sample_run/sample_config.yml"
    user_file = "./src/supy/data_model/sample2.yml"
    
    print(f"Reference file: {reference_file}")
    print(f"User file: {user_file}")
    print()
    
    # Step 1: Compare files and generate diff/equal reports
    print("📊 Step 1: Generating comparison reports...")
    compare_yaml_files(
        files=[
            ("sample_config", reference_file),
            ("sample2", user_file)
        ],
        output_equal="./test_equal.yml",
        output_diff="./test_diff.yml"
    )
    print("   ✅ Comparison reports written to test_equal.yml and test_diff.yml")
    print()
    
    # Step 2: Annotate user file with missing parameters
    print("📝 Step 2: Annotating missing parameters...")
    annotate_missing_parameters(
        user_file=user_file,
        reference_file=reference_file,
        output_file="./sample2_annotated.yml"
    )
    print()
    
    print("✨ Analysis complete!")
    print("📁 Output files:")
    print("   - test_equal.yml: Parameters that match between files")
    print("   - test_diff.yml: Parameters that differ between files") 
    print("   - sample2_annotated.yml: User file with missing parameter annotations")


if __name__ == "__main__":
    main()