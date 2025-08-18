#!/usr/bin/env python3
"""
Standalone Schema Generator for SUEWS

This script can generate JSON schemas from Pydantic models WITHOUT building SUPY.
It carefully imports only the data model components, avoiding the compiled Fortran parts.
"""

import sys
import json
from pathlib import Path

# Add src to path
src_path = Path(__file__).parent.parent / "src"
sys.path.insert(0, str(src_path))


def generate_schema_without_build():
    """Generate JSON schema without importing compiled SUPY components."""

    # Step 1: Monkey-patch to prevent main supy __init__ from loading
    import supy

    # Replace the problematic imports with dummies
    sys.modules["supy._supy_driver"] = type(sys)("dummy")
    sys.modules["supy._supy_module"] = type(sys)("dummy")

    # Step 2: Now we can safely import the data model
    from supy.data_model.core.config import SUEWSConfig
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION
    from supy.data_model.schema.registry import SchemaRegistry

    print(f"✓ Successfully imported data model components")
    print(f"✓ Current schema version: {CURRENT_SCHEMA_VERSION}")

    # Step 3: Generate the schema
    schema = SUEWSConfig.model_json_schema()

    # Add metadata
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
    schema["$id"] = (
        f"https://umep-dev.github.io/SUEWS/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
    )
    schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
    schema["description"] = (
        f"JSON Schema for SUEWS YAML configuration files. "
        f"Schema version {CURRENT_SCHEMA_VERSION}. "
        "See https://suews.readthedocs.io for documentation."
    )

    print(f"✓ Generated schema with {len(schema.get('properties', {}))} properties")

    # Step 4: Save the schema
    output_dir = Path("schemas/suews-config")
    output_dir.mkdir(parents=True, exist_ok=True)

    schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
    schema_file.write_text(json.dumps(schema, indent=2))
    print(f"✓ Saved schema to {schema_file}")

    # Also save as latest
    latest_file = output_dir / "latest.json"
    latest_file.write_text(json.dumps(schema, indent=2))
    print(f"✓ Saved latest.json")

    # Step 5: Update registry
    registry_path = output_dir / "registry.json"
    registry = SchemaRegistry(registry_path)
    registry.register_version(
        version=CURRENT_SCHEMA_VERSION,
        schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
        description=f"Schema version {CURRENT_SCHEMA_VERSION}",
    )
    print(f"✓ Updated registry")

    # Step 6: Generate index.html
    index_html = registry.generate_index_html(
        base_url="https://umep-dev.github.io/SUEWS", is_preview=False, pr_number=None
    )
    (output_dir / "index.html").write_text(index_html)
    print(f"✓ Generated index.html")

    return schema


if __name__ == "__main__":
    try:
        schema = generate_schema_without_build()
        print("\n✅ Schema generation complete without building SUPY!")
        print(f"   Schema has {len(schema.get('properties', {}))} top-level properties")
        print(f"   Schema has {len(schema.get('definitions', {}))} definitions")
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback

        traceback.print_exc()
        sys.exit(1)
