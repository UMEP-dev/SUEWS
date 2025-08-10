#!/usr/bin/env python3
"""
Generate RST documentation for SUEWS Pydantic models by parsing source files.
This approach avoids needing to import the models (and their dependencies).
"""

import ast
import re
from pathlib import Path
from typing import Dict, List, Optional, Tuple

def extract_field_info(node: ast.AnnAssign) -> Optional[Dict]:
    """Extract field information from an annotated assignment."""
    if not isinstance(node.target, ast.Name):
        return None
    
    field_name = node.target.id
    field_info = {
        'name': field_name,
        'type': ast.unparse(node.annotation) if hasattr(ast, 'unparse') else str(node.annotation),
        'default': None,
        'description': '',
        'units': None
    }
    
    # Extract default value
    if node.value:
        if isinstance(node.value, ast.Call):
            # Field() call
            if hasattr(node.value.func, 'id') and node.value.func.id == 'Field':
                for keyword in node.value.keywords:
                    if keyword.arg == 'default':
                        field_info['default'] = ast.unparse(keyword.value) if hasattr(ast, 'unparse') else str(keyword.value)
                    elif keyword.arg == 'description':
                        if isinstance(keyword.value, ast.Constant):
                            field_info['description'] = keyword.value.value
        else:
            # Direct assignment
            field_info['default'] = ast.unparse(node.value) if hasattr(ast, 'unparse') else str(node.value)
    
    # Extract units from description if present
    if field_info['description']:
        units_match = re.search(r'\[([^\]]+)\]', field_info['description'])
        if units_match:
            field_info['units'] = units_match.group(1)
    
    return field_info

def parse_model_file(file_path: Path) -> Dict[str, List[Dict]]:
    """Parse a Python file and extract Pydantic model definitions."""
    with open(file_path, 'r') as f:
        content = f.read()
    
    try:
        tree = ast.parse(content)
    except SyntaxError as e:
        print(f"Error parsing {file_path}: {e}")
        return {}
    
    models = {}
    
    for node in ast.walk(tree):
        if isinstance(node, ast.ClassDef):
            # Check if it's a Pydantic model (inherits from BaseModel)
            is_pydantic = any(
                (isinstance(base, ast.Name) and base.id == 'BaseModel') or
                (isinstance(base, ast.Attribute) and base.attr == 'BaseModel')
                for base in node.bases
            )
            
            if is_pydantic:
                fields = []
                for item in node.body:
                    if isinstance(item, ast.AnnAssign):
                        field_info = extract_field_info(item)
                        if field_info and not field_info['name'].startswith('_'):
                            fields.append(field_info)
                
                if fields:
                    models[node.name] = fields
    
    return models

def generate_model_rst(model_name: str, fields: List[Dict], output_dir: Path):
    """Generate RST file for a model."""
    output_file = output_dir / f"{model_name.lower()}.rst"
    
    # Convert CamelCase to Title Case with spaces
    title = re.sub(r'([a-z])([A-Z])', r'\1 \2', model_name)
    
    rst_content = []
    rst_content.append(f".. _{model_name.lower()}:")
    rst_content.append("")
    rst_content.append(title)
    rst_content.append("=" * len(title))
    rst_content.append("")
    
    if fields:
        rst_content.append("Parameters")
        rst_content.append("----------")
        rst_content.append("")
        
        for field in fields:
            # Field name as heading
            rst_content.append(f"**{field['name']}**")
            
            # Type
            if field['type']:
                rst_content.append(f"  *Type:* ``{field['type']}``")
            
            # Default
            if field['default']:
                rst_content.append(f"  *Default:* ``{field['default']}``")
            
            # Units
            if field['units']:
                rst_content.append(f"  *Units:* {field['units']}")
            
            # Description
            if field['description']:
                # Clean up description
                desc = field['description'].replace(field['units'], '').strip() if field['units'] else field['description']
                desc = re.sub(r'\s*\[[^\]]+\]\s*', ' ', desc).strip()  # Remove units from description
                if desc:
                    rst_content.append(f"  *Description:* {desc}")
            
            rst_content.append("")
    
    # Write the RST file
    with open(output_file, 'w') as f:
        f.write('\n'.join(rst_content))
    
    print(f"  Generated: {output_file.name}")

def main():
    """Main function to generate schema documentation."""
    # Setup paths
    docs_dir = Path(__file__).parent
    project_root = docs_dir.parent
    models_dir = project_root / "src" / "supy" / "data_model"
    output_dir = docs_dir / "source" / "inputs" / "yaml" / "schema"
    
    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"Output directory: {output_dir}")
    
    # Priority models to process
    priority_models = ["ModelPhysics", "ModelControl", "ModelSite", "ModelForcing"]
    
    # Process each Python file in the data_model directory
    all_models = {}
    for py_file in models_dir.glob("*.py"):
        if py_file.name.startswith('_'):
            continue
        
        print(f"\nProcessing: {py_file.name}")
        models = parse_model_file(py_file)
        all_models.update(models)
    
    # Generate RST files for priority models first
    generated = []
    for model_name in priority_models:
        if model_name in all_models:
            generate_model_rst(model_name, all_models[model_name], output_dir)
            generated.append(model_name)
    
    # Generate RST files for remaining models
    for model_name, fields in all_models.items():
        if model_name not in generated:
            generate_model_rst(model_name, fields, output_dir)
            generated.append(model_name)
    
    print(f"\n✅ Generated {len(generated)} model documentation files")
    
    # Create or update the index file
    index_file = output_dir.parent / "index.rst"
    if index_file.exists():
        print(f"ℹ️  Index file exists: {index_file}")
        print("   Please manually update it to include the new schema files")
    
    return 0

if __name__ == "__main__":
    exit(main())