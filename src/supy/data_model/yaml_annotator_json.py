"""
YAML Annotator using JSON for precise positioning.

This module uses JSON as an intermediate format to ensure
annotations are placed at exactly the right locations.
"""

import yaml
import json
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
from collections import OrderedDict
import re


class ValidationIssue:
    """Represents a validation issue at a specific location."""
    
    def __init__(self, path: str, param: str, message: str, fix: str, level: str = "WARNING"):
        self.path = path
        self.param = param
        self.message = message
        self.fix = fix
        self.level = level
        self.example = self._generate_example()
    
    def _generate_example(self) -> Dict[str, Any]:
        """Generate example value based on parameter type."""
        examples = {
            'bldgh': {'value': 20.0, '_comment': 'building height in meters'},
            'faibldg': {'value': 0.5, '_comment': 'frontal area index (0.1-0.7)'},
            'thermal_layers': {
                'dz': {'value': [0.2, 0.1, 0.1, 0.5, 1.6], '_comment': 'layer thickness (m)'},
                'k': {'value': [1.2, 1.1, 1.1, 1.5, 1.6], '_comment': 'thermal conductivity (W/m/K)'},
                'rho_cp': {'value': [1.2e6, 1.1e6, 1.1e6, 1.5e6, 1.6e6], '_comment': 'heat capacity (J/m3/K)'}
            },
            'g_max': {'value': 3.5, '_comment': 'maximum surface conductance (mm/s)'},
            'g_k': {'value': 200.0, '_comment': 'solar radiation coefficient'},
            'g_q_base': {'value': 0.13, '_comment': 'VPD coefficient base'},
            'g_q_shape': {'value': 0.7, '_comment': 'VPD coefficient shape'},
            'g_t': {'value': 30.0, '_comment': 'temperature coefficient (°C)'},
            'g_sm': {'value': 0.05, '_comment': 'soil moisture coefficient'},
            'kmax': {'value': 1200.0, '_comment': 'maximum solar radiation (W/m2)'},
            's1': {'value': 5.56, '_comment': 'soil moisture threshold 1'},
            's2': {'value': 0.0, '_comment': 'soil moisture threshold 2'},
            'co2pointsource': {'value': 0.0, '_comment': 'point source CO2 emissions'},
            'ef_umolco2perj': {'value': 1.159, '_comment': 'CO2 emission factor (μmol/J)'},
            'frfossilfuel_heat': {'value': 0.7, '_comment': 'fossil fuel fraction for heating'},
            'frfossilfuel_nonheat': {'value': 0.7, '_comment': 'fossil fuel fraction for non-heating'},
        }
        
        return examples.get(self.param, {'value': 'TODO', '_comment': self.fix})


class JsonYamlAnnotator:
    """Annotates YAML using JSON intermediate format for precise positioning."""
    
    def __init__(self):
        self.issues: List[ValidationIssue] = []
        self.issue_index: Dict[str, List[ValidationIssue]] = {}
    
    def add_issue(self, path: str, param: str, message: str, fix: str, level: str = "WARNING"):
        """Add a validation issue."""
        issue = ValidationIssue(path, param, message, fix, level)
        self.issues.append(issue)
        
        # Index by normalized path for easy lookup
        norm_path = self._normalize_path(path)
        if norm_path not in self.issue_index:
            self.issue_index[norm_path] = []
        self.issue_index[norm_path].append(issue)
    
    def _normalize_path(self, path: str) -> str:
        """Normalize path for consistent matching."""
        # Convert sites[0] to sites/0
        path = re.sub(r'\[(\d+)\]', r'/\1', path)
        # Remove leading/trailing slashes
        return path.strip('/')
    
    def generate_annotated_file(self, input_path: Path, output_path: Optional[Path] = None) -> Path:
        """Generate annotated YAML file with validation feedback."""
        # Read original YAML
        with open(input_path, 'r') as f:
            yaml_content = f.read()
        
        # Parse to data structure
        data = yaml.safe_load(yaml_content)
        
        # Convert to JSON for manipulation
        json_data = self._yaml_to_json(data)
        
        # Insert annotations into JSON structure
        annotated_json = self._insert_annotations(json_data)
        
        # Convert back to YAML with custom formatting
        annotated_yaml = self._json_to_annotated_yaml(annotated_json)
        
        # Add header
        header = """# ANNOTATED SUEWS CONFIGURATION
# ================================
# This file has been annotated with validation feedback.
# 
# Look for these markers:
#   [ERROR] MISSING: Required parameters that must be added
#   [TIP] ADD HERE: Ready-to-use parameter blocks
#
# To fix your configuration:
# 1. Search for "[ERROR] MISSING" markers
# 2. Uncomment the suggested parameter blocks
# 3. Adjust values for your specific site
# 4. Remove the comment markers after fixing
# ================================

"""
        
        final_content = header + annotated_yaml
        
        # Write output
        if output_path is None:
            output_path = input_path.parent / f"{input_path.stem}_annotated.yml"
        
        with open(output_path, 'w') as f:
            f.write(final_content)
        
        return output_path
    
    def _yaml_to_json(self, data: Any) -> Any:
        """Convert YAML data to JSON, preserving structure."""
        # Use OrderedDict to maintain order
        if isinstance(data, dict):
            result = OrderedDict()
            for key, value in data.items():
                result[key] = self._yaml_to_json(value)
            return result
        elif isinstance(data, list):
            return [self._yaml_to_json(item) for item in data]
        else:
            return data
    
    def _insert_annotations(self, data: Any, path: str = "") -> Any:
        """Insert annotations into JSON structure."""
        if isinstance(data, OrderedDict):
            result = OrderedDict()
            
            # First, copy existing keys
            for key, value in data.items():
                current_path = f"{path}/{key}" if path else key
                result[key] = self._insert_annotations(value, current_path)
            
            # Then, check if we need to add missing parameters at this level
            norm_path = self._normalize_path(path)
            if norm_path in self.issue_index:
                for issue in self.issue_index[norm_path]:
                    if issue.param not in result:
                        # Add annotation marker
                        annotation_key = f"_MISSING_{issue.param}"
                        result[annotation_key] = {
                            '_type': 'annotation',
                            '_level': issue.level,
                            '_message': issue.message,
                            '_fix': issue.fix,
                            '_param': issue.param,
                            '_example': issue.example
                        }
            
            return result
        
        elif isinstance(data, list):
            result = []
            for i, item in enumerate(data):
                current_path = f"{path}/{i}"
                result.append(self._insert_annotations(item, current_path))
            return result
        
        else:
            return data
    
    def _json_to_annotated_yaml(self, data: Any, indent: int = 0, is_list_item: bool = False) -> str:
        """Convert annotated JSON back to YAML with custom formatting."""
        lines = []
        
        if isinstance(data, (dict, OrderedDict)):
            # Handle list items differently
            first_key = True
            
            for key, value in data.items():
                if key.startswith('_MISSING_'):
                    # This is an annotation
                    ann = value
                    indent_str = "  " * indent
                    lines.append(f"{indent_str}# [ERROR] MISSING: {ann['_message']}")
                    lines.append(f"{indent_str}# [TIP] ADD HERE:")
                    
                    # Format the example
                    example = ann['_example']
                    param = ann['_param']
                    
                    if isinstance(example, dict) and '_comment' not in example:
                        # Complex structure like thermal_layers
                        lines.append(f"{indent_str}# {param}:")
                        for sub_key, sub_value in example.items():
                            if isinstance(sub_value, dict):
                                comment = sub_value.get('_comment', '')
                                value = sub_value.get('value', 'TODO')
                                # Use simple format when no reference info
                                if 'ref' in sub_value and sub_value['ref'] is not None:
                                    lines.append(f"{indent_str}#   {sub_key}: {{value: {self._format_value(value)}, ref: ...}}  # {comment}")
                                else:
                                    lines.append(f"{indent_str}#   {sub_key}: {self._format_value(value)}  # {comment}")
                            else:
                                lines.append(f"{indent_str}#   {sub_key}: {self._format_value(sub_value)}")
                    else:
                        # Simple parameter
                        comment = example.get('_comment', '')
                        value = example.get('value', 'TODO')
                        # Use simple format when no reference info
                        if isinstance(example, dict) and 'ref' in example and example['ref'] is not None:
                            lines.append(f"{indent_str}# {param}: {{value: {self._format_value(value)}, ref: ...}}  # {comment}")
                        else:
                            lines.append(f"{indent_str}# {param}: {self._format_value(value)}  # {comment}")
                    
                    lines.append("")  # Empty line after annotation
                
                elif not key.startswith('_'):
                    # Regular key-value pair
                    if is_list_item and first_key:
                        # First key in a list item
                        indent_str = "  " * (indent - 1) + "- "
                        first_key = False
                    else:
                        indent_str = "  " * indent
                    
                    if isinstance(value, (dict, OrderedDict)):
                        if self._is_simple_dict(value):
                            # Inline simple dicts
                            lines.append(f"{indent_str}{key}: {self._format_inline_dict(value)}")
                        else:
                            # Multi-line complex dicts
                            lines.append(f"{indent_str}{key}:")
                            sub_yaml = self._json_to_annotated_yaml(value, indent + 1)
                            lines.append(sub_yaml)
                    elif isinstance(value, list):
                        if not value:
                            lines.append(f"{indent_str}{key}: []")
                        else:
                            lines.append(f"{indent_str}{key}:")
                            for item in value:
                                sub_yaml = self._json_to_annotated_yaml(item, indent + 1, is_list_item=True)
                                lines.append(sub_yaml)
                    else:
                        lines.append(f"{indent_str}{key}: {self._format_value(value)}")
        
        elif isinstance(data, list):
            for item in data:
                sub_yaml = self._json_to_annotated_yaml(item, indent, is_list_item=True)
                lines.append(sub_yaml)
        
        else:
            # Scalar value in a list
            indent_str = "  " * (indent - 1) + "- "
            lines.append(f"{indent_str}{self._format_value(data)}")
        
        return '\n'.join(filter(None, lines))
    
    def _is_simple_dict(self, d: Dict) -> bool:
        """Check if dict can be formatted inline."""
        if len(d) > 1:
            return False
        for v in d.values():
            if isinstance(v, (dict, list)):
                return False
        return True
    
    def _format_inline_dict(self, d: Dict) -> str:
        """Format simple dict inline."""
        if 'value' in d:
            return f"{{value: {self._format_value(d['value'])}}}"
        items = [f"{k}: {self._format_value(v)}" for k, v in d.items()]
        return "{" + ", ".join(items) + "}"
    
    def _format_list_item(self, item: Dict, base_indent: int) -> str:
        """Format list item."""
        if len(item) == 1 and 'name' in item:
            return f"name: {item['name']}"
        lines = []
        first = True
        for k, v in item.items():
            if first:
                lines.append(f"{k}: {self._format_value(v)}")
                first = False
            else:
                lines.append(f"{' ' * base_indent}{k}: {self._format_value(v)}")
        return '\n'.join(lines)
    
    def _format_value(self, value: Any) -> str:
        """Format a single value for YAML."""
        if isinstance(value, str):
            if any(c in value for c in ':#@|>'):
                return f'"{value}"'
            return value
        elif isinstance(value, bool):
            return 'true' if value else 'false'
        elif isinstance(value, (int, float)):
            return str(value)
        elif isinstance(value, list):
            return "[" + ", ".join(self._format_value(v) for v in value) + "]"
        else:
            return str(value)


# For backward compatibility
YAMLAnnotator = JsonYamlAnnotator