#!/usr/bin/env python3
"""
Script to analyze internal-only parameters in SUEWS data model.

This script:
1. Scans the data model Python files for parameters marked with internal_only=True
2. Checks which parameters are present in sample_config.yml
3. Generates a comprehensive table for issue #555

Usage:
    python analyze_internal_only_parameters.py

Output:
    - internal_only_parameters_table.csv
    - internal_only_parameters_report.md
"""

import ast
import csv
import os
import yaml
from pathlib import Path
from typing import Dict, List, Set, Tuple, Any
import re


class InternalOnlyAnalyzer:
    """Analyzer for internal-only parameters in SUEWS data model."""
    
    def __init__(self):
        self.script_dir = Path(__file__).parent
        self.repo_root = self.script_dir.parent.parent.parent.parent
        self.data_model_dir = self.script_dir.parent
        self.sample_config_path = self.repo_root / "src" / "supy" / "sample_run" / "sample_config.yml"
        
        self.internal_params = []
        self.sample_config_data = {}
        
    def find_internal_only_parameters(self) -> List[Dict[str, Any]]:
        """Find all parameters marked as internal_only in data model files."""
        python_files = [
            "state.py",
            "model.py", 
            "core.py",
            "human_activity.py",
            "hydro.py",
            "ohm.py",
            "precheck.py",
            "profile.py",
            "site.py",
            "surface.py",
            "type.py"
        ]
        
        internal_params = []
        
        for py_file in python_files:
            file_path = self.data_model_dir / py_file
            if not file_path.exists():
                continue
                
            print(f"Analyzing {py_file}...")
            params = self._extract_internal_params_from_file(file_path)
            internal_params.extend(params)
            
        return internal_params
    
    def _extract_internal_params_from_file(self, file_path: Path) -> List[Dict[str, Any]]:
        """Extract internal-only parameters from a single Python file."""
        params = []
        
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Split content into lines for line number tracking
        lines = content.split('\n')
        
        # Find Field definitions with internal_only=True
        field_pattern = r'(\w+):\s*[^=]*=\s*Field\s*\('
        
        for i, line in enumerate(lines, 1):
            # Look for Field definitions
            field_match = re.search(field_pattern, line)
            if field_match:
                param_name = field_match.group(1)
                
                # Look ahead to find the complete Field definition
                field_def = self._extract_complete_field_definition(lines, i-1)
                
                if 'internal_only' in field_def and 'True' in field_def:
                    # Extract description
                    desc_match = re.search(r'description=["\'](.*?)["\']', field_def, re.DOTALL)
                    description = desc_match.group(1) if desc_match else "No description"
                    
                    # Extract unit from json_schema_extra
                    unit_match = re.search(r'"unit":\s*["\'](.*?)["\']', field_def)
                    unit = unit_match.group(1) if unit_match else ""
                    
                    params.append({
                        'param_name': param_name,
                        'file_location': f"{file_path.name}:{i}",
                        'description': description,
                        'unit': unit
                    })
                    
        return params
    
    def _extract_complete_field_definition(self, lines: List[str], start_line: int) -> str:
        """Extract complete Field definition spanning multiple lines."""
        field_def = lines[start_line]
        line_idx = start_line + 1
        
        # Count parentheses to find complete definition
        paren_count = field_def.count('(') - field_def.count(')')
        
        while paren_count > 0 and line_idx < len(lines):
            field_def += '\n' + lines[line_idx]
            paren_count += lines[line_idx].count('(') - lines[line_idx].count(')')
            line_idx += 1
            
        return field_def
    
    def load_sample_config(self) -> Dict[str, Any]:
        """Load and parse the sample_config.yml file."""
        if not self.sample_config_path.exists():
            print(f"Warning: sample_config.yml not found at {self.sample_config_path}")
            return {}
            
        with open(self.sample_config_path, 'r', encoding='utf-8') as f:
            return yaml.safe_load(f)
    
    def _flatten_yaml_keys(self, data: Dict[str, Any], prefix: str = "") -> Set[str]:
        """Recursively flatten YAML structure to get all parameter names."""
        keys = set()
        
        if isinstance(data, dict):
            for key, value in data.items():
                full_key = f"{prefix}.{key}" if prefix else key
                keys.add(key)  # Add the key itself
                keys.add(full_key)  # Add the full path
                
                if isinstance(value, (dict, list)):
                    keys.update(self._flatten_yaml_keys(value, full_key))
        elif isinstance(data, list):
            for i, item in enumerate(data):
                if isinstance(item, (dict, list)):
                    keys.update(self._flatten_yaml_keys(item, f"{prefix}[{i}]"))
                    
        return keys
    
    def check_parameter_in_sample_config(self, param_name: str, yaml_keys: Set[str]) -> Tuple[bool, str]:
        """Check if a parameter is present in sample_config.yml."""
        # Direct match
        if param_name in yaml_keys:
            return True, param_name
            
        # Check for partial matches (parameter might be nested)
        matches = [key for key in yaml_keys if param_name in key]
        if matches:
            return True, matches[0]
            
        return False, ""
    
    def generate_table(self) -> None:
        """Generate the analysis table and report."""
        print("Finding internal-only parameters...")
        self.internal_params = self.find_internal_only_parameters()
        
        print("Loading sample_config.yml...")
        self.sample_config_data = self.load_sample_config()
        yaml_keys = self._flatten_yaml_keys(self.sample_config_data)
        
        print(f"Found {len(self.internal_params)} internal-only parameters")
        print(f"Found {len(yaml_keys)} keys in sample_config.yml")
        
        # Enhance parameter data with sample_config presence
        for param in self.internal_params:
            in_sample, location = self.check_parameter_in_sample_config(
                param['param_name'], yaml_keys
            )
            param['in_sample_config'] = 'Yes' if in_sample else 'No'
            param['sample_config_location'] = location if in_sample else ''
        
        # Generate CSV table
        self._generate_csv_table()
        
        # Generate markdown report
        self._generate_markdown_report()
        
        print("\nAnalysis complete!")
        print(f"Generated: internal_only_parameters_table.csv")
        print(f"Generated: internal_only_parameters_report.md")
    
    def _generate_csv_table(self) -> None:
        """Generate CSV table of internal-only parameters."""
        csv_path = self.script_dir / "internal_only_parameters_table.csv"
        
        with open(csv_path, 'w', newline='', encoding='utf-8') as f:
            fieldnames = [
                'param_name', 
                'file_location', 
                'in_sample_config', 
                'sample_config_location',
                'unit'
            ]
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            
            writer.writeheader()
            for param in sorted(self.internal_params, key=lambda x: x['param_name']):
                # Create a copy without description for CSV
                csv_param = {k: v for k, v in param.items() if k != 'description'}
                writer.writerow(csv_param)
    
    def _generate_markdown_report(self) -> None:
        """Generate markdown report with analysis summary."""
        md_path = self.script_dir / "internal_only_parameters_report.md"
        
        # Count statistics
        total_params = len(self.internal_params)
        in_sample = sum(1 for p in self.internal_params if p['in_sample_config'] == 'Yes')
        not_in_sample = total_params - in_sample
        
        # Group by file
        by_file = {}
        for param in self.internal_params:
            file_name = param['file_location'].split(':')[0]
            if file_name not in by_file:
                by_file[file_name] = []
            by_file[file_name].append(param)
        
        with open(md_path, 'w', encoding='utf-8') as f:
            f.write(f"""# Internal-Only Parameters Analysis Report

Generated for GitHub Issue #555: Identify complete list of internal-only parameters

## Summary Statistics

- **Total internal-only parameters**: {total_params}
- **Parameters in sample_config.yml**: {in_sample}
- **Parameters NOT in sample_config.yml**: {not_in_sample}
- **Files analyzed**: {len(by_file)}

## Parameters by File

""")
            
            for file_name, params in sorted(by_file.items()):
                f.write(f"### {file_name} ({len(params)} parameters)\n\n")
                f.write("| Parameter | In sample_config.yml | Unit |\n")
                f.write("|-----------|---------------------|------|\n")
                
                for param in sorted(params, key=lambda x: x['param_name']):
                    f.write(f"| `{param['param_name']}` | {param['in_sample_config']} | {param['unit']} |\n")
                
                f.write("\n")
            
            f.write(f"""## Parameters Present in sample_config.yml

These parameters are marked as internal-only but appear in the sample configuration:

""")
            
            present_params = [p for p in self.internal_params if p['in_sample_config'] == 'Yes']
            if present_params:
                f.write("| Parameter | File | Location in sample_config.yml |\n")
                f.write("|-----------|------|-------------------------------|\n")
                for param in sorted(present_params, key=lambda x: x['param_name']):
                    f.write(f"| `{param['param_name']}` | {param['file_location']} | {param['sample_config_location']} |\n")
            else:
                f.write("*None found*\n")
            
            f.write("\n\n## Parameters NOT in sample_config.yml\n\n")
            f.write("These parameters are internal-only and do not appear in the sample configuration:\n\n")
            
            absent_params = [p for p in self.internal_params if p['in_sample_config'] == 'No']
            if absent_params:
                f.write("| Parameter | File | Unit |\n")
                f.write("|-----------|------|------|\n")
                for param in sorted(absent_params, key=lambda x: x['param_name']):
                    f.write(f"| `{param['param_name']}` | {param['file_location']} | {param['unit']} |\n")
            else:
                f.write("*None found*\n")

            f.write("\n## Recommendations\n\n")
            f.write("1. **Parameters in sample_config.yml**: Review whether these should remain internal-only or be exposed to users\n")
            f.write("2. **Parameters NOT in sample_config.yml**: These are likely true internal state variables that should remain hidden\n")
            f.write("3. **Consider advanced/developer mode**: For parameters needed in benchmark/testing scenarios\n\n")
            f.write("## Usage\n\n")
            f.write("This analysis was generated using:\n")
            f.write("```bash\n")
            f.write("python analyze_internal_only_parameters.py\n")
            f.write("```\n\n")
            f.write(f"Generated on: {self._get_timestamp()}\n")
    
    def _get_timestamp(self) -> str:
        """Get current timestamp for report."""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def main():
    """Main entry point."""
    analyzer = InternalOnlyAnalyzer()
    analyzer.generate_table()


if __name__ == "__main__":
    main()