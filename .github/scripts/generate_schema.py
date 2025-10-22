#!/usr/bin/env python3
"""
Generate JSON Schema for SUEWS without building.

This script generates JSON schemas from Pydantic models without requiring
SUEWS to be built. It's used by CI/CD workflows for schema deployment.

The script imports data_model directly to avoid triggering supy's __init__.py
which would try to import the compiled Fortran module.

Usage:
    python .github/scripts/generate_schema.py [--preview --pr-number N]
"""

import argparse
import json
import logging
from pathlib import Path
import sys
import traceback
import types
from typing import Any, Optional


def _setup_mock_environment(src_dir: Path) -> None:
    """Set up mock environment to avoid importing the real supy._env."""
    # Create mock _env module
    env_module = types.ModuleType('supy._env')
    env_module.logger_supy = logging.getLogger('supy.data_model')
    if not env_module.logger_supy.handlers:
        handler = logging.StreamHandler()
        handler.setFormatter(logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s"))
        env_module.logger_supy.addHandler(handler)
        env_module.logger_supy.setLevel(logging.WARNING)

    # Mock traversable for resources (not needed for schema generation)
    class MockTraversable:
        def __truediv__(self, other): return Path("/nonexistent")
        @staticmethod
        def exists(): return False
    env_module.trv_supy_module = MockTraversable()

    # Install the mock
    sys.modules['supy'] = types.ModuleType('supy')
    sys.modules['supy'].__path__ = [str(src_dir / 'supy')]
    sys.modules['supy._env'] = env_module


def _add_schema_metadata(schema: dict[str, Any], is_preview: bool, pr_number: Optional[int]) -> None:
    """Add metadata to the schema."""
    base_url = "https://umep-dev.github.io/SUEWS"
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"

    # Import here to avoid issues with mock environment
    from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION  # noqa: PLC0415

    if is_preview and pr_number:
        schema["$id"] = f"{base_url}/preview/pr-{pr_number}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION} (PR #{pr_number} Preview)"
        schema["description"] = (
            f"⚠️ PREVIEW VERSION - PR #{pr_number} - DO NOT USE IN PRODUCTION. "
            f"Schema version {CURRENT_SCHEMA_VERSION}."
        )
    else:
        schema["$id"] = f"{base_url}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
        schema["description"] = (
            f"JSON Schema for SUEWS YAML configuration files. "
            f"Schema version {CURRENT_SCHEMA_VERSION}. "
            "See https://suews.readthedocs.io for documentation."
        )


def generate_schema(
    is_preview: bool = False,
    pr_number: Optional[int] = None
) -> dict[str, Any]:
    """
    Generate schema directly from data_model.

    We modify sys.modules to prevent supy.__init__ from loading,
    allowing us to import data_model directly.

    Args:
        is_preview: Whether this is a PR preview build
        pr_number: PR number if this is a preview

    Returns
    -------
        Generated JSON schema
    """
    # Get project paths
    project_root = Path(__file__).parent.parent.parent
    src_dir = project_root / "src"

    # Add src directory to path
    sys.path.insert(0, str(src_dir))

    # Set up mock environment
    _setup_mock_environment(src_dir)

    try:
        # Now import data_model - it will use our mock _env
        # These imports must happen after mock setup, hence inside the function
        from supy.data_model.core.config import SUEWSConfig  # noqa: PLC0415
        from supy.data_model.schema.registry import SchemaRegistry  # noqa: PLC0415
        from supy.data_model.schema.version import (  # noqa: PLC0415
            CURRENT_SCHEMA_VERSION,
            SCHEMA_VERSIONS,
        )

        print(f"✓ Schema version: {CURRENT_SCHEMA_VERSION}")

        # Generate schema from Pydantic model
        schema = SUEWSConfig.model_json_schema()

        # Add metadata
        _add_schema_metadata(schema, is_preview, pr_number)

        print(f"✓ Generated schema: {len(schema.get('properties', {}))} properties, "
              f"{len(schema.get('$defs', {}))} definitions")

        # Save to permanent storage
        output_dir = project_root / "schemas" / "suews-config"
        output_dir.mkdir(parents=True, exist_ok=True)

        # Add auto-generated headers
        schema_with_headers = {
            "_comment": "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY",
            "_generated_by": "github-actions[bot]",
            "_source": ".github/scripts/generate_schema.py",
            "_schema_version": CURRENT_SCHEMA_VERSION,
            **schema
        }

        # Save versioned schema
        schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
        with open(schema_file, 'w', encoding="utf-8") as f:
            json.dump(schema_with_headers, f, indent=2)
        print(f"✓ Saved: {schema_file}")

        # Save as latest (with headers)
        latest_file = output_dir / "latest.json"
        with open(latest_file, 'w', encoding="utf-8") as f:
            json.dump(schema_with_headers, f, indent=2)

        # Update registry
        registry = SchemaRegistry(output_dir / "registry.json")
        registry.register_version(
            version=CURRENT_SCHEMA_VERSION,
            schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
            description=SCHEMA_VERSIONS.get(CURRENT_SCHEMA_VERSION, "")
        )
        print("✓ Updated registry")

        # Generate index.html
        base_url = "https://umep-dev.github.io/SUEWS"
        index_content = registry.generate_index_html(
            base_url=base_url,
            is_preview=is_preview,
            pr_number=pr_number,
        )
        (output_dir / "index.html").write_text(index_content)
        print("✓ Generated index.html")

        return schema

    finally:
        # Clean up sys.modules and sys.path
        for module_name in ['supy._env', 'supy']:
            if module_name in sys.modules and not hasattr(sys.modules[module_name], '__file__'):
                # Only remove if it's our mock module
                del sys.modules[module_name]
        if str(src_dir) in sys.path:
            sys.path.remove(str(src_dir))


def main():
    """Execute the main entry point for schema generation."""
    parser = argparse.ArgumentParser(
        description="Generate SUEWS JSON Schema without building"
    )
    parser.add_argument(
        "--preview",
        action="store_true",
        help="Generate preview schema for PR"
    )
    parser.add_argument(
        "--pr-number",
        type=int,
        help="PR number for preview builds"
    )

    args = parser.parse_args()

    try:
        generate_schema(
            is_preview=args.preview,
            pr_number=args.pr_number
        )

        print("\n✅ Schema generation complete (no build required)")

    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
