"""
YAML Annotator for SUEWS Configuration

This module provides functionality to generate annotated YAML files
with inline comments showing validation issues and how to fix them.
"""

import yaml
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import re
from collections import defaultdict
from io import StringIO


class ValidationIssue:
    """Represents a validation issue at a specific location in the config."""
    
    def __init__(self, path: str, param: str, message: str, fix: str, level: str = "WARNING"):
        self.path = path
        self.param = param
        self.message = message
        self.fix = fix
        self.level = level
    
    def __repr__(self):
        return f"ValidationIssue({self.path}, {self.param}, {self.level})"


class YAMLAnnotator:
    """Annotates YAML configuration files with validation feedback."""
    
    def __init__(self):
        self.issues: List[ValidationIssue] = []
        self.issues_by_path: Dict[str, List[ValidationIssue]] = defaultdict(list)
    
    def add_issue(self, path: str, param: str, message: str, fix: str, level: str = "WARNING"):
        """Add a validation issue."""
        issue = ValidationIssue(path, param, message, fix, level)
        self.issues.append(issue)
        # Normalize path for matching
        normalized_path = path.replace("Site ", "sites[").replace("]:", "]")
        self.issues_by_path[normalized_path].append(issue)
    
    def annotate_yaml_string(self, yaml_str: str, original_data: Dict[str, Any]) -> str:
        """
        Annotate a YAML string with validation issues.
        
        Args:
            yaml_str: The original YAML string
            original_data: The parsed YAML data
            
        Returns:
            Annotated YAML string with comments
        """
        lines = yaml_str.split('\n')
        annotated_lines = []
        current_path = []
        indent_stack = [0]
        
        for i, line in enumerate(lines):
            # Skip empty lines and comments
            if not line.strip() or line.strip().startswith('#'):
                annotated_lines.append(line)
                continue
            
            # Calculate indentation
            indent = len(line) - len(line.lstrip())
            
            # Update path based on indentation
            while indent_stack and indent <= indent_stack[-1]:
                indent_stack.pop()
                if current_path:
                    current_path.pop()
            
            # Parse the line to get key
            if ':' in line and not line.strip().startswith('-'):
                key = line.split(':')[0].strip()
                current_path.append(key)
                indent_stack.append(indent)
            elif line.strip().startswith('- '):
                # Handle list items
                if current_path and current_path[-1] == 'sites':
                    # This is a site entry
                    current_path.append('[0]')  # Simplified - would need proper indexing
                    indent_stack.append(indent)
            
            # Add the original line
            annotated_lines.append(line)
            
            # Check if we have issues for this path
            path_str = '.'.join(current_path)
            matching_issues = self._find_issues_for_path(path_str, line)
            
            if matching_issues:
                # Add comment lines for each issue
                for issue in matching_issues:
                    comment_indent = ' ' * (indent + 2)
                    annotated_lines.append(f"{comment_indent}# [WARNING]  {issue.level}: {issue.message}")
                    annotated_lines.append(f"{comment_indent}# [TIP] FIX: {issue.fix}")
                    if issue.param not in line:
                        # Add example fix
                        annotated_lines.append(f"{comment_indent}# EXAMPLE:")
                        annotated_lines.append(f"{comment_indent}{issue.param}: {{value: <INSERT_VALUE>}}")
        
        return '\n'.join(annotated_lines)
    
    def _find_issues_for_path(self, current_path: str, line: str) -> List[ValidationIssue]:
        """Find issues matching the current path."""
        matching_issues = []
        
        # Direct path matching
        for path, issues in self.issues_by_path.items():
            if path in current_path or current_path in path:
                for issue in issues:
                    # Check if this is the right location for the issue
                    if issue.param in line or self._should_add_param_here(current_path, issue):
                        matching_issues.append(issue)
        
        return matching_issues
    
    def _should_add_param_here(self, current_path: str, issue: ValidationIssue) -> bool:
        """Determine if a missing parameter should be added at this location."""
        # This is a simplified check - in practice would need more sophisticated matching
        issue_path_parts = issue.path.lower().split('/')
        current_path_parts = current_path.lower().split('.')
        
        # Check if we're in the right section
        for part in issue_path_parts:
            if part in current_path_parts:
                return True
        
        return False
    
    def generate_annotated_file(
        self, 
        input_path: Path, 
        output_path: Optional[Path] = None,
        validation_report: Optional[Any] = None
    ) -> Path:
        """
        Generate an annotated YAML file with validation feedback.
        
        Args:
            input_path: Path to the original YAML file
            output_path: Path for the annotated file (default: input_path.stem + '_annotated.yml')
            validation_report: Optional validation report to extract issues from
            
        Returns:
            Path to the generated annotated file
        """
        # Read original file
        with open(input_path, 'r') as f:
            yaml_content = f.read()
            
        # Parse YAML
        data = yaml.safe_load(yaml_content)
        
        # If validation report provided, extract issues
        if validation_report:
            self._extract_issues_from_report(validation_report)
        
        # Create a simpler annotation approach
        annotated_content = self._create_annotated_yaml(yaml_content, data)
        
        # Determine output path
        if output_path is None:
            output_path = input_path.parent / f"{input_path.stem}_annotated.yml"
        
        # Write annotated file
        with open(output_path, 'w') as f:
            f.write(annotated_content)
        
        return output_path
    
    def _create_annotated_yaml(self, yaml_content: str, data: Dict[str, Any]) -> str:
        """Create annotated YAML with inline annotations."""
        header = """# ANNOTATED SUEWS CONFIGURATION
# ================================
# This file has been annotated with inline validation feedback.
# Look for:
#   [WARNING]  MISSING: Parameters that need to be added
#   [TIP] ADD: Ready-to-use parameter blocks
#
# To fix:
# 1. Find the [WARNING]  MISSING comments
# 2. Uncomment the suggested ADD blocks
# 3. Adjust values as needed
# 4. Remove the warning comments
# ================================

"""
        
        # Parse YAML to get line numbers for each path
        lines = yaml_content.split('\n')
        
        # Create a mapping of paths to line numbers
        path_to_line = self._build_path_line_mapping(lines)
        
        # Sort issues by line number for insertion
        issues_by_line = self._organize_issues_by_line(path_to_line)
        
        # Insert annotations inline
        annotated_lines = []
        i = 0
        
        while i < len(lines):
            line = lines[i]
            
            # Add the original line
            annotated_lines.append(line)
            
            # Check if we need to insert annotations after this line
            if i in issues_by_line:
                # Get indentation of current line
                indent = len(line) - len(line.lstrip())
                
                # Add annotations for all issues at this location
                for issue in issues_by_line[i]:
                    # Add warning comment
                    annotated_lines.append(' ' * indent + f"# [WARNING]  MISSING: {issue.message}")
                    
                    # Add fix suggestion with proper indentation
                    if issue.param == 'bldgh':
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        annotated_lines.append(' ' * indent + "# bldgh: {value: 20.0}  # building height in meters")
                    
                    elif issue.param == 'faibldg':
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        annotated_lines.append(' ' * indent + "# faibldg: {value: 0.5}  # frontal area index (0.1-0.7)")
                    
                    elif issue.param == 'thermal_layers':
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        annotated_lines.append(' ' * indent + "# thermal_layers:")
                        annotated_lines.append(' ' * indent + "#   dz: {value: [0.2, 0.1, 0.1, 0.5, 1.6]}  # layer thickness (m)")
                        annotated_lines.append(' ' * indent + "#   k: {value: [1.2, 1.1, 1.1, 1.5, 1.6]}  # conductivity (W/m/K)")
                        annotated_lines.append(' ' * indent + "#   rho_cp: {value: [1.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6]}  # heat capacity (J/m3/K)")
                    
                    elif issue.param == 'conductance':
                        annotated_lines.append(' ' * indent + "# [TIP] ADD the following block under 'properties:':")
                        annotated_lines.append(' ' * indent + "# conductance:")
                        annotated_lines.append(' ' * indent + "#   g_max: {value: 3.5}  # maximum surface conductance")
                        annotated_lines.append(' ' * indent + "#   g_k: {value: 200.0}  # solar radiation coefficient")
                        annotated_lines.append(' ' * indent + "#   g_q_base: {value: 0.13}  # VPD coefficient base")
                        annotated_lines.append(' ' * indent + "#   g_q_shape: {value: 0.7}  # VPD coefficient shape")
                        annotated_lines.append(' ' * indent + "#   g_t: {value: 30.0}  # temperature coefficient")
                        annotated_lines.append(' ' * indent + "#   g_sm: {value: 0.05}  # soil moisture coefficient")
                        annotated_lines.append(' ' * indent + "#   kmax: {value: 1200.0}  # maximum solar radiation")
                        annotated_lines.append(' ' * indent + "#   s1: {value: 5.56}  # soil moisture threshold 1")
                        annotated_lines.append(' ' * indent + "#   s2: {value: 0.0}  # soil moisture threshold 2")
                    
                    elif 'g_' in issue.param or issue.param in ['s1', 's2', 'kmax']:
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        annotated_lines.append(' ' * indent + f"# {issue.param}: {{value: <CHECK_DOCS>}}  # {issue.fix}")
                    
                    elif 'co2' in issue.param.lower():
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        if issue.param == 'co2pointsource':
                            annotated_lines.append(' ' * indent + "# co2pointsource: {value: 0.0}  # point source emissions")
                        elif issue.param == 'ef_umolco2perj':
                            annotated_lines.append(' ' * indent + "# ef_umolco2perj: {value: 1.159}  # emission factor")
                        elif issue.param == 'frfossilfuel_heat':
                            annotated_lines.append(' ' * indent + "# frfossilfuel_heat: {value: 0.7}  # fossil fuel fraction for heating")
                        elif issue.param == 'frfossilfuel_nonheat':
                            annotated_lines.append(' ' * indent + "# frfossilfuel_nonheat: {value: 0.7}  # fossil fuel fraction for non-heating")
                        else:
                            annotated_lines.append(' ' * indent + f"# {issue.param}: {{value: <CHECK_DOCS>}}")
                    
                    else:
                        annotated_lines.append(' ' * indent + "# [TIP] ADD:")
                        annotated_lines.append(' ' * indent + f"# {issue.param}: {{value: <TODO>}}  # {issue.fix}")
                    
                    # Add blank line between issues
                    annotated_lines.append("")
            
            i += 1
        
        # Combine header and annotated content
        return header + '\n'.join(annotated_lines)
    
    def _build_path_line_mapping(self, lines: List[str]) -> Dict[str, int]:
        """Build a mapping of YAML paths to line numbers."""
        path_to_line = {}
        current_path = []
        indent_stack = []
        
        for i, line in enumerate(lines):
            if not line.strip() or line.strip().startswith('#'):
                continue
            
            indent = len(line) - len(line.lstrip())
            
            # Manage path based on indentation
            while indent_stack and indent <= indent_stack[-1][1]:
                indent_stack.pop()
                current_path.pop()
            
            if ':' in line and not line.strip().startswith('-'):
                key = line.split(':')[0].strip()
                current_path.append(key)
                indent_stack.append((key, indent))
                
                # Store the path
                path_str = '/'.join(current_path)
                path_to_line[path_str] = i
            elif line.strip().startswith('- '):
                # Handle list items
                if current_path:
                    # Approximate list index
                    list_index = 0  # Simplified - would need proper tracking
                    indexed_path = current_path[:-1] + [f"{current_path[-1]}[{list_index}]"]
                    path_str = '/'.join(indexed_path)
                    path_to_line[path_str] = i
        
        return path_to_line
    
    def _organize_issues_by_line(self, path_to_line: Dict[str, int]) -> Dict[int, List[ValidationIssue]]:
        """Organize issues by the line number where they should be inserted."""
        issues_by_line = defaultdict(list)
        
        for issue in self.issues:
            # Find the best matching line for this issue
            best_line = self._find_best_insertion_line(issue, path_to_line)
            if best_line is not None:
                issues_by_line[best_line].append(issue)
        
        return issues_by_line
    
    def _find_best_insertion_line(self, issue: ValidationIssue, path_to_line: Dict[str, int]) -> Optional[int]:
        """Find the best line to insert an annotation for this issue."""
        # Convert issue path to match our path format
        issue_path = issue.path.replace('sites[', 'sites[')  # Keep as is
        
        # Try to find exact match or parent path
        path_parts = issue_path.split('/')
        
        # For building parameters, insert after 'sfr' line within bldgs
        if 'land_cover/bldgs' in issue_path and issue.param in ['bldgh', 'faibldg']:
            for path, line in path_to_line.items():
                if path.endswith('land_cover/bldgs/sfr'):
                    return line
            # Fallback to bldgs line
            for path, line in path_to_line.items():
                if path.endswith('land_cover/bldgs'):
                    return line
        
        # For thermal layers, insert after the surface type
        if 'thermal_layers' in issue.param:
            surface_type = None
            if '/land_cover/' in issue_path:
                surface_type = issue_path.split('/land_cover/')[-1].split('/')[0]
                for path, line in path_to_line.items():
                    if path.endswith(f'land_cover/{surface_type}'):
                        return line
        
        # For conductance, insert after 'properties:'
        if 'conductance' in issue_path:
            for path, line in path_to_line.items():
                if path.endswith('properties'):
                    return line
        
        # For CO2 params, insert after 'co2:'
        if 'anthropogenic_emissions/co2' in issue_path:
            for path, line in path_to_line.items():
                if path.endswith('anthropogenic_emissions/co2'):
                    return line
        
        return None
    
    def _extract_issues_from_report(self, report):
        """Extract issues from a validation report."""
        # This would extract issues from the validation report
        # Implementation depends on the report format
        pass


def create_example_annotated_yaml():
    """Create an example annotated YAML file to demonstrate the functionality."""
    
    example_yaml = """sites:
  - name: Urban Test Site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      land_cover:
        bldgs:
          sfr: {value: 0.4}
"""
    
    # Create annotator and add issues
    annotator = YAMLAnnotator()
    
    # Add building height issue
    annotator.add_issue(
        path="sites[0]/properties/land_cover/bldgs",
        param="bldgh",
        message="Building height is required (building fraction: 40%)",
        fix="Add building height in meters",
        level="WARNING"
    )
    
    # Add frontal area issue
    annotator.add_issue(
        path="sites[0]/properties/land_cover/bldgs",
        param="faibldg",
        message="Frontal area index recommended for accurate aerodynamics",
        fix="Add frontal area index (typical range: 0.1-0.7)",
        level="WARNING"
    )
    
    # Add thermal layers issue
    annotator.add_issue(
        path="sites[0]/properties/land_cover/bldgs",
        param="thermal_layers",
        message="Thermal layer properties required for heat storage calculations",
        fix="Add thermal_layers with dz, k, and rho_cp arrays",
        level="WARNING"
    )
    
    # Add conductance issue
    annotator.add_issue(
        path="sites[0]/properties",
        param="conductance",
        message="Missing surface conductance parameters for evapotranspiration",
        fix="Add conductance block with g_max, g_k, g_sm, s1, s2 parameters",
        level="WARNING"
    )
    
    # Generate annotated YAML
    annotated = annotator.annotate_yaml_string(example_yaml, yaml.safe_load(example_yaml))
    
    # Add header
    header = """# ANNOTATED SUEWS CONFIGURATION
# ================================
# This file shows validation issues found in your configuration.
# Look for:
#   [WARNING]  WARNING: Issues that should be addressed
#   [TIP] FIX: How to fix each issue
#   EXAMPLE: Sample parameter blocks you can use
# ================================

"""
    
    return header + annotated


if __name__ == "__main__":
    # Generate example
    example = create_example_annotated_yaml()
    print(example)