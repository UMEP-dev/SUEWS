#!/usr/bin/env python3
"""
Script to analyze initial state parameters from benchmark YAML file
and scan data_model for parameter usage, validations, and precheck logic.
"""

import yaml
import csv
import ast
import re
from pathlib import Path
from typing import Dict, List, Set, Tuple, Any
import inspect
import pandas as pd

def load_benchmark_yaml(yaml_path: Path) -> Dict:
    """Load and parse the benchmark YAML file."""
    with open(yaml_path, 'r') as f:
        return yaml.safe_load(f)

def extract_initial_state_parameters(yaml_data: Dict) -> Set[str]:
    """Extract all parameter names from the initial_states section."""
    parameters = set()
    
    def recursive_extract(obj, prefix=""):
        """Recursively extract parameter names from nested dictionaries."""
        if isinstance(obj, dict):
            for key, value in obj.items():
                if key == "value":
                    # Skip 'value' keys as they contain data, not parameter names
                    continue
                elif isinstance(value, dict):
                    # If it contains 'value' key, this is a parameter
                    if "value" in value:
                        param_name = f"{prefix}.{key}" if prefix else key
                        parameters.add(param_name)
                    else:
                        # Continue recursion
                        new_prefix = f"{prefix}.{key}" if prefix else key
                        recursive_extract(value, new_prefix)
                elif isinstance(value, list):
                    # Handle lists (like temperature arrays or roof/wall lists)
                    param_name = f"{prefix}.{key}" if prefix else key
                    parameters.add(param_name)
                else:
                    # Simple value parameter
                    param_name = f"{prefix}.{key}" if prefix else key
                    parameters.add(param_name)
        elif isinstance(obj, list):
            for i, item in enumerate(obj):
                if isinstance(item, dict):
                    recursive_extract(item, f"{prefix}[{i}]")
    
    # Extract from initial_states section
    if "sites" in yaml_data and yaml_data["sites"]:
        initial_states = yaml_data["sites"][0].get("initial_states", {})
        recursive_extract(initial_states)
    
    return parameters

def extract_properties_parameters(yaml_data: Dict) -> Set[str]:
    """Extract all parameter names from the properties section."""
    parameters = set()
    
    def recursive_extract(obj, prefix=""):
        """Recursively extract parameter names from nested dictionaries."""
        if isinstance(obj, dict):
            for key, value in obj.items():
                if key == "value":
                    # Skip 'value' keys as they contain data, not parameter names
                    continue
                elif isinstance(value, dict):
                    # If it contains 'value' key, this is a parameter
                    if "value" in value:
                        param_name = f"{prefix}.{key}" if prefix else key
                        parameters.add(param_name)
                    else:
                        # Continue recursion
                        new_prefix = f"{prefix}.{key}" if prefix else key
                        recursive_extract(value, new_prefix)
                elif isinstance(value, list):
                    # Handle lists 
                    param_name = f"{prefix}.{key}" if prefix else key
                    parameters.add(param_name)
                else:
                    # Simple value parameter
                    param_name = f"{prefix}.{key}" if prefix else key
                    parameters.add(param_name)
        elif isinstance(obj, list):
            for i, item in enumerate(obj):
                if isinstance(item, dict):
                    recursive_extract(item, f"{prefix}[{i}]")
    
    # Extract from properties section
    if "sites" in yaml_data and yaml_data["sites"]:
        properties = yaml_data["sites"][0].get("properties", {})
        recursive_extract(properties)
    
    return parameters

def scan_python_file_for_parameter(file_path: Path, parameter_name: str) -> Dict[str, Any]:
    """
    Scan a Python file for usage of a parameter, validation logic, descriptions, and metadata.
    
    Returns:
        Dict with keys: usage_locations, pydantic_info, precheck_info, descriptions, classes, precheck_found, pydantic_found
    """
    results = {
        "usage_locations": [],
        "pydantic_info": [],      # Pydantic class information
        "precheck_info": [],      # Precheck function information
        "descriptions": [],
        "classes": [],
        "precheck_found": False,
        "pydantic_found": False
    }
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Parse the AST to analyze the code
        try:
            tree = ast.parse(content)
        except SyntaxError:
            return results
        
        # More precise parameter name variations to search for
        param_variations = [
            parameter_name,  # Full dotted name like "bldgs.tin"
            parameter_name.replace(".", "_"),  # Underscore version like "bldgs_tin"
        ]
        
        # Only include the last part if it's specific enough and not overly common
        last_part = parameter_name.split(".")[-1]
        # Be very restrictive about short/common names that cause false positives
        common_short_names = {'id', 'wu', 'av', 'state', 'temp'}  # Keep 'state' restricted but allow 'tin'
        if len(last_part) > 2 and last_part not in common_short_names:
            param_variations.append(last_part)
        
        # We'll set precheck_found and pydantic_found later based on precise matching
        # Check for Pydantic usage
        if any(keyword in content.lower() for keyword in ['field(', 'basemodel', 'validator', 'pydantic']):
            if any(param_var in content for param_var in param_variations):
                results["pydantic_found"] = True
        
        # Use AST to find classes and their methods that use the parameter
        current_class = None
        lines = content.split('\n')
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                current_class = node.name
                # Check if this class contains the parameter in any field definitions
                class_start = node.lineno - 1
                class_end = getattr(node, 'end_lineno', len(lines)) - 1
                class_content = '\n'.join(lines[class_start:class_end + 1])
                
                for param_var in param_variations:
                    # Look for field definitions with more precise matching
                    for line_idx in range(class_start, min(class_end + 1, len(lines))):
                        line = lines[line_idx]
                        
                        # Use precise patterns for class field detection too
                        if len(param_var) <= 4:  # Short parameters like 'tin', 'temp'
                            field_patterns = [
                                f'{param_var}:',      # Field definition like tin:
                                f'{param_var} =',     # Field assignment like tin =
                                f'"{param_var}"',     # In string literals
                                f"'{param_var}'",     # In string literals
                            ]
                        else:  # Longer parameters - can use looser matching
                            field_patterns = [
                                f'{param_var}:',      # Field definition
                                f'{param_var} =',     # Field assignment
                                f'"{param_var}"',     # In string literals
                                f"'{param_var}'",     # In string literals
                            ]
                        
                        # Check if this is a field definition line and contains our parameter
                        if (any(pattern in line for pattern in field_patterns) and 
                            ('field(' in line.lower() or 'Field(' in line or ':' in line)):
                            line_num = line_idx + 1
                            results["usage_locations"].append(f"{file_path.name}:{line_num}")
                            results["classes"].append(current_class)
                            
                            # Add this class field definition to pydantic_info
                            pydantic_entry = f"{current_class} ({file_path.name}, {node.lineno})"
                            if pydantic_entry not in results["pydantic_info"]:  # Avoid duplicates
                                results["pydantic_info"].append(pydantic_entry)
                            
                            # Extract description
                            desc_match = re.search(r'description=["\'](.*?)["\']', line, re.IGNORECASE)
                            if desc_match:
                                results["descriptions"].append(f"{file_path.name}: {desc_match.group(1)}")
                            else:
                                # Look in next few lines
                                for offset in range(1, 4):
                                    if line_idx + offset < len(lines):
                                        next_line = lines[line_idx + offset]
                                        desc_match = re.search(r'description=["\'](.*?)["\']', next_line, re.IGNORECASE)
                                        if desc_match:
                                            results["descriptions"].append(f"{file_path.name}: {desc_match.group(1)}")
                                            break
            
            elif isinstance(node, ast.FunctionDef):
                func_name = node.name
                # Check for validation functions
                if any(keyword in func_name.lower() for keyword in ['validate', 'check', 'precheck']):
                    func_start = node.lineno - 1
                    func_end = getattr(node, 'end_lineno', len(lines)) - 1
                    func_content = '\n'.join(lines[func_start:func_end + 1])
                    
                    # More precise matching: look for the parameter in meaningful contexts
                    for param_var in param_variations:
                        # Use strict patterns but keep what was working for 'tin'
                        if len(param_var) <= 4:  # Short parameters like 'tin' need precise matching
                            precise_patterns = [
                                f'"{param_var}"',     # In string literals
                                f"'{param_var}'",     # In string literals  
                                f'["{param_var}"]',   # Dictionary access like obj["tin"]
                                f"['{param_var}']",   # Dictionary access like obj['tin']
                                f' {param_var} ',     # Surrounded by spaces
                                f'({param_var})',     # In parentheses
                                f'({param_var},',     # Function parameter with comma
                                f' {param_var},',     # Parameter in list with comma
                                f'.{param_var}',      # Attribute access (needed for tin)
                            ]
                        else:  # Longer parameters
                            precise_patterns = [
                                f'"{param_var}"',   # In string literals
                                f"'{param_var}'",   # In string literals
                                f'.{param_var}',    # Attribute access like obj.parameter
                                f'{param_var}:',    # Dictionary key
                                f'{param_var} ',    # Variable name followed by space
                                f'{param_var}=',    # Assignment
                                f'{param_var})',    # Function parameter
                                f'({param_var}',    # Function parameter
                            ]
                        
                        if any(pattern in func_content for pattern in precise_patterns):
                            if 'precheck' in func_name.lower():
                                # Precheck functions go to precheck_info
                                precheck_entry = f"{func_name} ({file_path.name}, {node.lineno})"
                                if precheck_entry not in results["precheck_info"]:  # Avoid duplicates
                                    results["precheck_info"].append(precheck_entry)
                                results["precheck_found"] = True
                            else:
                                # All other validation functions (validate_*, check_*, model validators) go to pydantic_info
                                validation_entry = f"{func_name} ({file_path.name}, {node.lineno})"
                                if validation_entry not in results["pydantic_info"]:  # Add to pydantic_info for general validation
                                    results["pydantic_info"].append(validation_entry)
                            break  # Only add the function once
        
    except Exception as e:
        results["usage_locations"].append(f"Error scanning file: {str(e)}")
    
    return results

def scan_data_model_directory(data_model_path: Path, parameters: Set[str]) -> Dict[str, Dict[str, Any]]:
    """
    Scan all Python files in data_model directory for parameter usage.
    
    Returns:
        Dict mapping parameter names to their usage information
    """
    results = {}
    
    python_files = list(data_model_path.glob("**/*.py"))
    
    for param in parameters:
        results[param] = {
            "pydantic_info": [],
            "precheck_info": [],
            "description": [],
            "classes": [],
            "precheck_found": False,
            "pydantic_found": False
        }
        
        for py_file in python_files:
            file_results = scan_python_file_for_parameter(py_file, param)
            
            if file_results["pydantic_info"]:
                # Add unique entries only
                for entry in file_results["pydantic_info"]:
                    if entry not in results[param]["pydantic_info"]:
                        results[param]["pydantic_info"].append(entry)
            
            if file_results["precheck_info"]:
                # Add unique entries only
                for entry in file_results["precheck_info"]:
                    if entry not in results[param]["precheck_info"]:
                        results[param]["precheck_info"].append(entry)
                    
            if file_results["descriptions"]:
                results[param]["description"].extend(file_results["descriptions"])
                
            if file_results["classes"]:
                results[param]["classes"].extend(file_results["classes"])
                
            if file_results["precheck_found"]:
                results[param]["precheck_found"] = True
                
            if file_results["pydantic_found"]:
                results[param]["pydantic_found"] = True
    
    return results

def assign_automatic_notes(param_name: str, existing_note: str) -> str:
    """Assign automatic note numbers based on parameter type if no existing note."""
    # Convert to string and handle NaN values from pandas
    existing_note = str(existing_note).strip() if existing_note is not None and str(existing_note).strip() != 'nan' else ''
    
    # Clean up floating point notation (1.0 -> 1, 2.0 -> 2)
    if existing_note and '.' in existing_note:
        try:
            # Convert to float then to int to remove decimal if it's a whole number
            float_val = float(existing_note)
            if float_val.is_integer():
                existing_note = str(int(float_val))
        except ValueError:
            pass  # Keep original if conversion fails
    
    # If there's already a note, preserve it (now cleaned)
    if existing_note:
        return existing_note
    
    # Check if parameter is temperature-related (note 1)
    temp_suffixes = ['.tin', '.temperature', '.tsfc']
    temp_names = ['tmin_id', 'tmax_id', 'tair_av']
    if (any(param_name.endswith(suffix) for suffix in temp_suffixes) or 
        any(temp_name in param_name for temp_name in temp_names)):
        return '1'
    
    # Check if parameter is snow/ice-related (note 2)
    snow_ice_keywords = ['snow', 'ice']
    if any(keyword in param_name.lower() for keyword in snow_ice_keywords):
        return '2'
    
    # Check if parameter is snow/ice-related (note 3)
    snow_ice_keywords = ['snow', 'ice']
    if any(keyword in param_name.lower() for keyword in snow_ice_keywords):
        return '3'
    
    # Check if parameter contains 'dd' in the name (note 4)
    if 'dd' in param_name.lower():
        return '4'
    
    # Check if parameter contains '.soilstore' or '.state' in the name (note 10) - precipitation-related
    if '.soilstore' in param_name.lower() or '.state' in param_name.lower():
        return '10'

    return ''

def create_analysis_dataframe(parameters: Set[str], analysis_results: Dict[str, Dict[str, Any]], 
                             existing_notes: Dict[str, str] = None) -> pd.DataFrame:
    """Convert analysis results to a pandas DataFrame, preserving existing notes."""
    
    data = []
    sorted_parameters = sorted(parameters)
    existing_notes = existing_notes or {}
    
    for param in sorted_parameters:
        param_data = analysis_results.get(param, {})
        
        # Extract data with defaults
        pydantic_info = param_data.get("pydantic_info", [])
        precheck_info = param_data.get("precheck_info", [])
        precheck_found = param_data.get("precheck_found", False)
        pydantic_found = param_data.get("pydantic_found", False)
        
        # Format the information
        pydantic_str = "; ".join(pydantic_info) if pydantic_info else "No Pydantic usage found"
        precheck_str = "; ".join(precheck_info) if precheck_info else "No precheck validation found"
        
        # Convert booleans to 1/0
        precheck_val = 1 if precheck_found else 0
        pydantic_val = 1 if pydantic_found else 0
        
        # Assign notes with automatic detection (temperature, snow/ice)
        existing_note = existing_notes.get(param, '')
        notes = assign_automatic_notes(param, existing_note)
        
        data.append({
            'parameter_name': param,
            'notes': notes,  # Preserve existing notes or assign temperature note
            'where_pydantic': pydantic_str,
            'where_precheck': precheck_str,
            'prechecked': precheck_val,
            'pydanted': pydantic_val
        })
    
    return pd.DataFrame(data)

def load_existing_notes(excel_path: Path) -> Dict[str, Dict[str, str]]:
    """Load existing notes from Excel file to preserve user annotations."""
    notes_by_sheet = {
        'initial_states': {},
        'properties_core': {},
        'properties_extended': {},
        'stebbs': {}
    }
    
    if not excel_path.exists():
        return notes_by_sheet
    
    try:
        # Read all sheets to extract existing notes
        for sheet_name in notes_by_sheet.keys():
            try:
                df = pd.read_excel(excel_path, sheet_name=sheet_name, engine='openpyxl')
                if 'parameter_name' in df.columns and 'notes' in df.columns:
                    # Create mapping of parameter_name -> notes
                    for _, row in df.iterrows():
                        param_name = row['parameter_name']
                        notes = row['notes'] if pd.notna(row['notes']) else ''
                        if param_name and notes:  # Only store non-empty notes
                            notes_by_sheet[sheet_name][param_name] = notes
            except (ValueError, KeyError):
                # Sheet doesn't exist or has different structure, skip
                continue
    except Exception:
        # File might be corrupted or locked, return empty notes
        pass
    
    return notes_by_sheet

def create_notes_legend_file(output_path: Path):
    """Create a notes legend file explaining what each note number means."""
    legend_content = """SUEWS Parameter Analysis - Notes Legend
========================================

Temperature-Related Notes (1-9):
--------------------------------
1. Parameters that are assigned with 2m monthly mean air temperature from precheck procedure.
Monthly mean air temperature is obtained from CRU dataset.

2. Snow and ice parameters that needs to be updated according to monthly mean air temperature.
If monthly mean air temperature > 4C°, these should be set to 0.

3. Snow and ice parameters that needs to be updated according to monthly mean air temperature.
If monthly mean air temperature < 4C°, these should be set to reasonable values. 
Ask Lena for suggestions.

4. Parameters related to degree days.

5. [Available for assignment]

6. [Available for assignment]

7. [Available for assignment]

8. [Available for assignment]

9. [Available for assignment]

10. Parameters related to precipitations.

Usage Instructions:
------------------
- Numbers 1-9 are reserved for temperature-related parameter categorization.
- Use higher numbers for non-temperature notes.
- Edit this file to update note descriptions.

Generated automatically by analyze_initial_states.py
"""
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(legend_content)

def create_blank_stebbs_dataframe() -> pd.DataFrame:
    """Create a blank stebbs dataframe with the standard columns."""
    return pd.DataFrame(columns=[
        'parameter_name', 'notes', 'where_pydantic', 'where_precheck', 'prechecked', 'pydanted'
    ])

def write_excel_report(initial_states_params: Set[str], initial_states_results: Dict[str, Dict[str, Any]], 
                      properties_params: Set[str], properties_results: Dict[str, Dict[str, Any]], 
                      output_path: Path):
    """Write the analysis results to an Excel file with multiple sheets."""
    
    # Load existing notes to preserve user annotations
    existing_notes = load_existing_notes(output_path)
    
    # Define core properties (the specific ones requested by user)
    core_properties = {
        'lat', 'lng', 'alt', 'timezone', 'surfacearea', 
        'z', 'z0m_in', 'zdm_in', 'pipecapacity', 'runofftowater', 'narp_trans_site'
    }
    
    # Split properties into core and extended
    properties_core_params = {param for param in properties_params if param in core_properties}
    properties_extended_params = properties_params  # All properties go to extended
    
    # Create DataFrames with preserved notes
    initial_states_df = create_analysis_dataframe(
        initial_states_params, initial_states_results, existing_notes['initial_states']
    )
    properties_core_df = create_analysis_dataframe(
        properties_core_params, properties_results, existing_notes['properties_core']
    )
    properties_extended_df = create_analysis_dataframe(
        properties_extended_params, properties_results, existing_notes['properties_extended']
    )
    
    # Create or preserve stebbs sheet
    stebbs_df = create_blank_stebbs_dataframe()
    # If stebbs sheet exists with data, preserve it completely
    if existing_notes['stebbs'] or output_path.exists():
        try:
            existing_stebbs = pd.read_excel(output_path, sheet_name='stebbs', engine='openpyxl')
            if not existing_stebbs.empty:
                stebbs_df = existing_stebbs
        except (ValueError, KeyError, FileNotFoundError):
            # Sheet doesn't exist or has issues, keep blank dataframe
            pass
    
    # Write to Excel with multiple sheets
    with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
        initial_states_df.to_excel(writer, sheet_name='initial_states', index=False)
        properties_core_df.to_excel(writer, sheet_name='properties_core', index=False)
        properties_extended_df.to_excel(writer, sheet_name='properties_extended', index=False)
        stebbs_df.to_excel(writer, sheet_name='stebbs', index=False)

def main():
    """Main function to orchestrate the analysis."""
    # Set up paths - find repository root from current script location
    script_dir = Path(__file__).parent
    
    # Navigate up to find the repository root (should contain meson.build)
    current_path = script_dir
    repo_root = None
    for _ in range(10):  # Prevent infinite loop
        if (current_path / "meson.build").exists() and (current_path / "test").exists():
            repo_root = current_path
            break
        current_path = current_path.parent
    
    if repo_root is None:
        # Fallback: assume we're in src/supy/data_model/initial_states
        repo_root = script_dir.parent.parent.parent.parent
    
    benchmark_yaml = repo_root / "test" / "fixtures" / "benchmark1" / "benchmark1.yml"
    data_model_dir = repo_root / "src" / "supy" / "data_model"
    output_csv = script_dir / "initial_states_analysis.csv"  # Keep CSV for backwards compatibility
    output_excel = script_dir / "parameter_analysis.xlsx"  # New Excel output
    output_notes_legend = script_dir / "notes_legend.txt"  # Notes legend file
    
    # Check if paths exist
    if not benchmark_yaml.exists():
        print(f"Error: Benchmark YAML file not found at {benchmark_yaml}")
        return 1
    
    if not data_model_dir.exists():
        print(f"Error: Data model directory not found at {data_model_dir}")
        return 1
    
    print("Loading benchmark YAML file...")
    yaml_data = load_benchmark_yaml(benchmark_yaml)
    
    print("Extracting initial state parameters...")
    initial_states_params = extract_initial_state_parameters(yaml_data)
    print(f"Found {len(initial_states_params)} initial state parameters")
    
    print("Extracting properties parameters...")
    properties_params = extract_properties_parameters(yaml_data)
    print(f"Found {len(properties_params)} properties parameters")
    
    print("Scanning data_model directory for initial states parameter usage...")
    initial_states_results = scan_data_model_directory(data_model_dir, initial_states_params)
    
    print("Scanning data_model directory for properties parameter usage...")
    properties_results = scan_data_model_directory(data_model_dir, properties_params)
    
    print("Writing Excel report with multiple sheets...")
    write_excel_report(initial_states_params, initial_states_results, 
                      properties_params, properties_results, output_excel)
    
    # Also write CSV for backwards compatibility (initial_states only)
    print("Writing CSV report (initial_states only for backwards compatibility)...")
    # Load existing notes for CSV too
    existing_csv_notes = {}
    if output_csv.exists():
        try:
            existing_csv_df = pd.read_csv(output_csv)
            if 'parameter_name' in existing_csv_df.columns and 'notes' in existing_csv_df.columns:
                for _, row in existing_csv_df.iterrows():
                    param_name = row['parameter_name']
                    notes = row['notes'] if pd.notna(row['notes']) else ''
                    if param_name and notes:
                        existing_csv_notes[param_name] = notes
        except Exception:
            pass
    
    initial_states_df = create_analysis_dataframe(initial_states_params, initial_states_results, existing_csv_notes)
    initial_states_df.to_csv(output_csv, index=False)
    
    # Create notes legend file
    print("Creating notes legend file...")
    create_notes_legend_file(output_notes_legend)
    
    # Count core properties for summary
    core_properties = {
        'lat', 'lng', 'alt', 'timezone', 'surfacearea', 
        'z', 'z0m_in', 'zdm_in', 'pipecapacity', 'runofftowater', 'narp_trans_site'
    }
    properties_core_params = {param for param in properties_params if param in core_properties}
    
    print(f"Analysis complete!")
    print(f"Excel report written to: {output_excel}")
    print(f"CSV report (initial_states only) written to: {output_csv}")
    print(f"Notes legend written to: {output_notes_legend}")
    print(f"\nFound {len(initial_states_params)} initial state parameters")
    print(f"Found {len(properties_core_params)} core properties parameters") 
    print(f"Found {len(properties_params)} total properties parameters (extended)")
    print(f"\nCore properties parameters:")
    for param in sorted(properties_core_params):
        print(f"  - {param}")
    print(f"\nSample initial state parameters:")
    for param in sorted(list(initial_states_params)[:10]):
        print(f"  - {param}")
    if len(initial_states_params) > 10:
        print(f"  ... and {len(initial_states_params) - 10} more")
    
    return 0

if __name__ == "__main__":
    exit(main())