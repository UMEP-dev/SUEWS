#!/usr/bin/env python3
"""
Minimal Schema Generator - Works without building SUPY

This approach directly modifies sys.modules to prevent loading compiled components.
"""

import sys
import json
from pathlib import Path
from unittest.mock import MagicMock

# Add src to path
src_path = Path(__file__).parent.parent / "src"
sys.path.insert(0, str(src_path))

def mock_compiled_modules():
    """Mock all the compiled modules to prevent ImportError."""
    # Create mock modules for all compiled dependencies
    mock = MagicMock()
    
    # Mock the compiled Fortran driver
    sys.modules['supy._supy_driver'] = mock
    sys.modules['_supy_driver'] = mock
    
    # Mock any other compiled modules that might be imported
    sys.modules['supy._supy_driver_wrapper'] = mock
    
    # Create a minimal mock for _supy_module to prevent its imports
    mock_module = type(sys)('_supy_module')
    mock_module.__dict__.update({
        'dict_var_type_forcing': {},
        'dict_var_type_state': {},
        'gen_df_state': mock,
        'gen_df_forcing': mock,
        'run_supy': mock,
        'scale_var': mock,
        'scale_df': mock,
        'replace_nan': mock,
    })
    sys.modules['supy._supy_module'] = mock_module
    
    # Mock check module
    mock_check = type(sys)('_check')
    mock_check.check_forcing = mock
    mock_check.check_state = mock
    sys.modules['supy._check'] = mock_check
    
    # Mock load module  
    mock_load = type(sys)('_load')
    mock_load.dict_var_type_forcing = {}
    mock_load.set_var_use = mock
    sys.modules['supy._load'] = mock_load
    
    print("✓ Mocked compiled modules")

def generate_schema():
    """Generate schema with mocked dependencies."""
    
    # Mock the compiled modules BEFORE any supy imports
    mock_compiled_modules()
    
    # Now we can safely import the data model
    # Import config directly to avoid main __init__
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS
    from supy.data_model.schema.registry import SchemaRegistry
    
    print(f"✓ Imported data model (schema version: {CURRENT_SCHEMA_VERSION})")
    
    # Generate the schema
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
    
    # Save the schema
    output_dir = Path("schemas/suews-config")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
    with open(schema_file, 'w') as f:
        json.dump(schema, f, indent=2)
    print(f"✓ Saved schema to {schema_file}")
    
    # Save as latest
    latest_file = output_dir / "latest.json"
    with open(latest_file, 'w') as f:
        json.dump(schema, f, indent=2)
    print(f"✓ Saved latest.json")
    
    # Update registry
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

if __name__ == "__main__":
    try:
        schema = generate_schema()
        print("\n✅ SUCCESS: Schema generated without building SUPY!")
        print(f"   This proves we can generate schemas in CI without compilation")
        
        # Show a sample of the schema
        if 'properties' in schema:
            props = list(schema['properties'].keys())[:5]
            print(f"   Sample properties: {', '.join(props)}...")
            
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)