#!/usr/bin/env python3
"""
Generate JSON Schema from SUEWS Pydantic data model for use in configuration UI.

This script extracts JSON Schema from the SUEWSConfig Pydantic model and places
it in the correct locations for both the documentation static files and the
standalone configuration UI.
"""

import sys
import json
from pathlib import Path


def main():
    """Generate JSON Schema from SUEWSConfig model and distribute to required locations."""

    # Add src directory to Python path to import supy
    docs_dir = Path(__file__).parent
    project_root = docs_dir.parent
    src_dir = project_root / "src"

    if str(src_dir) not in sys.path:
        sys.path.insert(0, str(src_dir))

    try:
        from supy.data_model.core import SUEWSConfig
    except ImportError as e:
        print(
            f"ERROR: Could not import SUEWSConfig. Make sure supy is installed or add src to PYTHONPATH."
        )
        print(f"Import error: {e}")
        sys.exit(1)

    print("Generating JSON Schema from SUEWSConfig Pydantic model...")

    # Generate JSON Schema from the canonical SUEWSConfig model
    schema = SUEWSConfig.model_json_schema()

    # Add metadata to schema
    schema["title"] = "SUEWS Configuration Schema"
    schema["description"] = (
        "JSON Schema for SUEWS (Surface Urban Energy and Water balance Scheme) configuration"
    )
    schema["version"] = "1.0.0"

    # Pretty-print JSON with proper indentation
    schema_json = json.dumps(schema, indent=2, ensure_ascii=False)

    # Define single target location for the schema
    target_path = docs_dir / "source" / "_static" / "suews-config-schema.json"

    try:
        # Ensure directory exists
        target_path.parent.mkdir(parents=True, exist_ok=True)

        # Write schema file
        target_path.write_text(schema_json, encoding="utf-8")
        print(f"✓ Schema written to: {target_path}")

        # Print schema statistics
        print(f"✓ Schema contains {len(schema.get('$defs', {}))} type definitions")
        if "$defs" in schema:
            main_types = list(schema["$defs"].keys())[:5]
            print(f"✓ Main configuration types: {', '.join(main_types)}...")

    except Exception as e:
        print(f"✗ Failed to write schema to {target_path}: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
