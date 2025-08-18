#!/usr/bin/env python3
"""
Extract Schema without Building SUPY - Fixed Version

This version fixes relative imports in the copied files.
"""

import sys
import json
import shutil
import tempfile
import re
from pathlib import Path
from typing import Dict, Any

def create_minimal_env(temp_dir: Path) -> None:
    """Create a minimal _env.py that doesn't import anything from supy."""
    env_content = '''
# Minimal environment for schema generation
import logging
import sys

# Minimal logger
logger_supy = logging.getLogger("supy")
logger_supy.addHandler(logging.StreamHandler(sys.stdout))
logger_supy.setLevel(logging.WARNING)

# Minimal traversable for resources
class MockTraversable:
    def __init__(self):
        pass
    def joinpath(self, *args):
        return self
    def read_text(self):
        return ""
    def __truediv__(self, other):
        return self

trv_supy_module = MockTraversable()
'''
    (temp_dir / "_env.py").write_text(env_content)
    print("✓ Created minimal _env.py")

def fix_imports_in_file(file_path: Path) -> None:
    """Fix relative imports that go beyond the package."""
    content = file_path.read_text()
    
    # Replace imports from ..._env with imports from _env
    content = re.sub(r'from \.\.\._env import', 'from _env import', content)
    
    # Replace imports from ..schema with imports from data_model.schema
    content = re.sub(r'from \.\.schema import', 'from data_model.schema import', content)
    
    # Replace imports from ..validation with imports from data_model.validation
    content = re.sub(r'from \.\.validation import', 'from data_model.validation import', content)
    
    # Replace imports from ..yaml_processor with imports from data_model.yaml_processor
    content = re.sub(r'from \.\.yaml_processor import', 'from data_model.yaml_processor import', content)
    
    file_path.write_text(content)

def copy_and_fix_data_model(src_dir: Path, temp_dir: Path) -> None:
    """Copy data_model and fix imports."""
    src_data_model = src_dir / "supy" / "data_model"
    dst_data_model = temp_dir / "data_model"
    
    # Copy the entire data_model directory
    shutil.copytree(src_data_model, dst_data_model)
    print(f"✓ Copied data_model to {dst_data_model}")
    
    # Fix imports in all Python files
    for py_file in dst_data_model.rglob("*.py"):
        fix_imports_in_file(py_file)
    print("✓ Fixed relative imports in data_model files")

def generate_schema_standalone() -> Dict[str, Any]:
    """Generate schema in isolated environment."""
    
    # Get paths
    project_root = Path(__file__).parent.parent
    src_dir = project_root / "src"
    
    # Create temporary directory for isolated execution
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        print(f"✓ Created temp directory: {temp_path}")
        
        # Copy and fix data_model
        copy_and_fix_data_model(src_dir, temp_path)
        
        # Create minimal _env.py
        create_minimal_env(temp_path)
        
        # Add temp dir to path
        sys.path.insert(0, str(temp_path))
        
        try:
            # Now import from the isolated data_model
            from data_model.core.config import SUEWSConfig
            from data_model.schema.version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS
            
            print(f"✓ Imported SUEWSConfig from isolated environment")
            print(f"✓ Current schema version: {CURRENT_SCHEMA_VERSION}")
            
            # Generate schema
            schema = SUEWSConfig.model_json_schema()
            
            # Add metadata
            schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
            schema["$id"] = f"https://umep-dev.github.io/SUEWS/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
            schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
            schema["description"] = (
                f"JSON Schema for SUEWS YAML configuration files. "
                f"Schema version {CURRENT_SCHEMA_VERSION}. "
                "See https://suews.readthedocs.io for documentation."
            )
            
            print(f"✓ Generated schema with {len(schema.get('properties', {}))} properties")
            print(f"✓ Schema has {len(schema.get('$defs', {}))} definitions")
            
            # Save to permanent storage
            output_dir = project_root / "schemas" / "suews-config"
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Save versioned schema
            schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
            with open(schema_file, 'w') as f:
                json.dump(schema, f, indent=2)
            print(f"✓ Saved schema to {schema_file}")
            
            # Save as latest
            latest_file = output_dir / "latest.json"
            with open(latest_file, 'w') as f:
                json.dump(schema, f, indent=2)
            print(f"✓ Saved latest.json")
            
            # Import and use registry
            from data_model.schema.registry import SchemaRegistry
            
            registry_path = output_dir / "registry.json"
            registry = SchemaRegistry(registry_path)
            registry.register_version(
                version=CURRENT_SCHEMA_VERSION,
                schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
                description=SCHEMA_VERSIONS.get(CURRENT_SCHEMA_VERSION, "")
            )
            print(f"✓ Updated registry")
            
            # Generate index
            index_content = registry.generate_index_html(
                base_url="https://umep-dev.github.io/SUEWS",
                is_preview=False
            )
            (output_dir / "index.html").write_text(index_content)
            print(f"✓ Generated index.html")
            
            return schema
            
        finally:
            # Clean up sys.path
            if str(temp_path) in sys.path:
                sys.path.remove(str(temp_path))

if __name__ == "__main__":
    try:
        schema = generate_schema_standalone()
        print("\n" + "="*60)
        print("✅ SUCCESS: Schema generated WITHOUT building SUPY!")
        print("="*60)
        print("\nThis proves that:")
        print("1. The data_model is truly independent of compiled code")
        print("2. We can generate schemas in CI without building Fortran")
        print("3. Schema generation can run on any platform")
        print("4. No need for Python 3.13 or specific build tools")
        
        # Show schema stats
        if 'properties' in schema:
            props = list(schema['properties'].keys())[:5]
            print(f"\nSample properties: {', '.join(props)}...")
            print(f"Total properties: {len(schema['properties'])}")
            print(f"Total definitions: {len(schema.get('$defs', {}))}")
            
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)