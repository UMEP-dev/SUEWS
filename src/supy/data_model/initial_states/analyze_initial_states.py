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

def scan_python_file_for_parameter(file_path: Path, parameter_name: str) -> Dict[str, Any]:
    """
    Scan a Python file for usage of a parameter, validation logic, descriptions, and metadata.
    
    Returns:
        Dict with keys: usage_locations, validation_info, descriptions, classes, precheck_found, pydantic_found
    """
    results = {
        "usage_locations": [],
        "validation_info": [],
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
        
        # Simple parameter name variations to search for
        param_variations = [
            parameter_name,
            parameter_name.replace(".", "_"),
            parameter_name.split(".")[-1],  # Last part of dotted name
        ]
        
        # Check for precheck functions
        if 'precheck' in content.lower() and any(param_var in content for param_var in param_variations):
            results["precheck_found"] = True
            
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
                    if param_var in class_content:
                        # Look for field definitions
                        for line_idx in range(class_start, min(class_end + 1, len(lines))):
                            line = lines[line_idx]
                            if param_var in line and ('field(' in line.lower() or ':' in line):
                                line_num = line_idx + 1
                                results["usage_locations"].append(f"{file_path.name}:{line_num}")
                                results["classes"].append(current_class)
                                
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
                    
                    for param_var in param_variations:
                        if param_var in func_content:
                            results["validation_info"].append(f"{func_name} ({file_path.name}, {node.lineno})")
                            if 'precheck' in func_name.lower():
                                results["precheck_found"] = True
        
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
            "where": [],
            "what": [],
            "description": [],
            "classes": [],
            "precheck_found": False,
            "pydantic_found": False
        }
        
        for py_file in python_files:
            file_results = scan_python_file_for_parameter(py_file, param)
            
            if file_results["usage_locations"]:
                results[param]["where"].extend(file_results["usage_locations"])
            
            if file_results["validation_info"]:
                results[param]["what"].extend(file_results["validation_info"])
                    
            if file_results["descriptions"]:
                results[param]["description"].extend(file_results["descriptions"])
                
            if file_results["classes"]:
                results[param]["classes"].extend(file_results["classes"])
                
            if file_results["precheck_found"]:
                results[param]["precheck_found"] = True
                
            if file_results["pydantic_found"]:
                results[param]["pydantic_found"] = True
    
    return results

def write_csv_report(parameters: Set[str], analysis_results: Dict[str, Dict[str, Any]], output_path: Path):
    """Write the analysis results to a CSV file."""
    
    with open(output_path, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.writer(csvfile)
        
        # Write header
        writer.writerow(['parameter_name', 'where', 'prechecked', 'pydanted'])
        
        # Sort parameters for consistent output
        sorted_parameters = sorted(parameters)
        
        for param in sorted_parameters:
            param_data = analysis_results.get(param, {})
            
            # Extract data with defaults
            what_info = param_data.get("what", [])
            precheck_found = param_data.get("precheck_found", False)
            pydantic_found = param_data.get("pydantic_found", False)
            
            # Format validation info in the requested format: name_of_class_or_function (script.py, line_number)
            # The validation info is already in the correct format from scanning
            where_str = "; ".join(what_info) if what_info else "No validation found"
            
            # Convert booleans to 1/0
            precheck_val = 1 if precheck_found else 0
            pydantic_val = 1 if pydantic_found else 0
            
            writer.writerow([
                param, 
                where_str, 
                precheck_val, 
                pydantic_val
            ])

def main():
    """Main function to orchestrate the analysis."""
    # Set up paths
    script_dir = Path(__file__).parent
    benchmark_yaml = script_dir / "test/fixtures/benchmark1/benchmark1.yml"
    data_model_dir = script_dir / "src/supy/data_model"
    output_csv = script_dir / "initial_states_analysis.csv"
    
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
    parameters = extract_initial_state_parameters(yaml_data)
    print(f"Found {len(parameters)} parameters")
    
    print("Scanning data_model directory for parameter usage...")
    analysis_results = scan_data_model_directory(data_model_dir, parameters)
    
    print("Writing CSV report...")
    write_csv_report(parameters, analysis_results, output_csv)
    
    print(f"Analysis complete! Results written to {output_csv}")
    print(f"Found {len(parameters)} initial state parameters:")
    for param in sorted(parameters):
        print(f"  - {param}")
    
    return 0

if __name__ == "__main__":
    exit(main())