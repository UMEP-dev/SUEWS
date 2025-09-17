#!/usr/bin/env python3
"""
Schema Publishing System for SUEWS YAML Configurations

This module provides tools to export, validate, and publish JSON Schema
representations of the SUEWS configuration structure. This enables:
- External validation tools
- IDE autocomplete and validation
- API documentation
- Schema versioning and evolution tracking
"""

import json
from pathlib import Path
from typing import Dict, Any, Optional
import argparse
import sys
from datetime import datetime

from ..core import SUEWSConfig
from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS


def generate_json_schema(
    version: Optional[str] = None, include_internal: bool = False
) -> Dict[str, Any]:
    """
    Generate JSON Schema from SUEWS Pydantic models.

    Args:
        version: Schema version to generate (default: current)
        include_internal: Include internal-only fields

    Returns:
        JSON Schema dictionary
    """
    # Get the Pydantic schema
    schema = SUEWSConfig.model_json_schema()

    # Add schema metadata
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
    schema["$id"] = (
        f"https://umep-dev.github.io/SUEWS/schema/suews-config/{version or CURRENT_SCHEMA_VERSION}.json"
    )

    # Add versioning information
    schema["version"] = version or CURRENT_SCHEMA_VERSION
    schema["title"] = f"SUEWS Configuration Schema v{version or CURRENT_SCHEMA_VERSION}"
    schema["description"] = (
        f"JSON Schema for SUEWS YAML configuration files. "
        f"Schema version {version or CURRENT_SCHEMA_VERSION}. "
        f"{SCHEMA_VERSIONS.get(version or CURRENT_SCHEMA_VERSION, '')}"
    )

    # Add metadata
    schema["$comment"] = {
        "generated": datetime.utcnow().isoformat() + "Z",
        "generator": "supy.util.schema_publisher",
        "suews_version": _get_suews_version(),
        "schema_version": version or CURRENT_SCHEMA_VERSION,
    }

    # Filter internal fields if requested
    if not include_internal:
        schema = _filter_internal_fields(schema)

    # Add examples
    schema["examples"] = [_get_minimal_example()]

    return schema


def _filter_internal_fields(schema: Dict[str, Any]) -> Dict[str, Any]:
    """
    Remove internal-only fields from schema.

    Args:
        schema: JSON Schema dictionary

    Returns:
        Filtered schema
    """

    def filter_properties(properties: Dict[str, Any]) -> Dict[str, Any]:
        """Recursively filter properties."""
        filtered = {}
        for key, value in properties.items():
            # Skip if marked as internal
            if isinstance(value, dict):
                if value.get("internal_only"):
                    continue
                # Recursively filter nested properties
                if "properties" in value:
                    value = value.copy()
                    value["properties"] = filter_properties(value["properties"])
            filtered[key] = value
        return filtered

    # Filter top-level properties
    if "properties" in schema:
        schema = schema.copy()
        schema["properties"] = filter_properties(schema["properties"])

    # Filter definitions/components
    if "$defs" in schema:
        filtered_defs = {}
        for def_name, def_schema in schema["$defs"].items():
            if "properties" in def_schema:
                def_schema = def_schema.copy()
                def_schema["properties"] = filter_properties(def_schema["properties"])
            filtered_defs[def_name] = def_schema
        schema["$defs"] = filtered_defs

    return schema


def _get_suews_version() -> str:
    """Get current SUEWS version."""
    try:
        from .._version import __version__

        return __version__
    except ImportError:
        return "unknown"


def _get_minimal_example() -> Dict[str, Any]:
    """
    Get a minimal valid configuration example.

    Returns:
        Minimal configuration dictionary
    """
    return {
        "name": "minimal_config",
        "schema_version": CURRENT_SCHEMA_VERSION,
        "description": "Minimal SUEWS configuration example",
        "model": {
            "control": {"tstep": 3600, "forcing_file": "forcing.txt"},
            "physics": {},
        },
        "sites": [
            {
                "name": "example_site",
                "gridiv": 1,
                "properties": {
                    "lat": 51.5,
                    "lng": -0.1,
                    "alt": 10.0,
                    "timezone": 0,
                    "surfacearea": 1000000.0,
                },
            }
        ],
    }


def save_schema(
    output_path: Path,
    version: Optional[str] = None,
    include_internal: bool = False,
    format: str = "json",
) -> None:
    """
    Save JSON Schema to file.

    Args:
        output_path: Path to save schema
        version: Schema version
        include_internal: Include internal fields
        format: Output format ('json' or 'yaml')
    """
    schema = generate_json_schema(version, include_internal)

    output_path.parent.mkdir(parents=True, exist_ok=True)

    if format == "yaml":
        import yaml

        with open(output_path, "w") as f:
            yaml.dump(schema, f, default_flow_style=False, sort_keys=False)
    else:
        with open(output_path, "w") as f:
            json.dump(schema, f, indent=2)

    print(f"Schema saved to {output_path}")


def create_schema_bundle(output_dir: Path, version: Optional[str] = None) -> None:
    """
    Create a complete schema bundle with all formats and documentation.

    Args:
        output_dir: Directory to save schema bundle
        version: Schema version
    """
    version = version or CURRENT_SCHEMA_VERSION
    output_dir = output_dir / f"v{version}"
    output_dir.mkdir(parents=True, exist_ok=True)

    # Generate main schema
    schema = generate_json_schema(version, include_internal=False)

    # Save in multiple formats
    # JSON format
    with open(output_dir / "schema.json", "w") as f:
        json.dump(schema, f, indent=2)

    # YAML format
    import yaml

    with open(output_dir / "schema.yaml", "w") as f:
        yaml.dump(schema, f, default_flow_style=False, sort_keys=False)

    # Minified JSON
    with open(output_dir / "schema.min.json", "w") as f:
        json.dump(schema, f, separators=(",", ":"))

    # Internal schema (with internal fields)
    internal_schema = generate_json_schema(version, include_internal=True)
    with open(output_dir / "schema-internal.json", "w") as f:
        json.dump(internal_schema, f, indent=2)

    # Create README
    readme_content = f"""# SUEWS Configuration Schema v{version}

This directory contains the JSON Schema for SUEWS YAML configuration files.

## Files

- `schema.json` - Main schema in JSON format
- `schema.yaml` - Schema in YAML format for readability
- `schema.min.json` - Minified schema for size-sensitive applications
- `schema-internal.json` - Complete schema including internal fields (for developers)

## Usage

### Validation with Python

```python
import json
import jsonschema
import yaml

# Load schema
with open('schema.json') as f:
    schema = json.load(f)

# Load configuration
with open('my_config.yml') as f:
    config = yaml.safe_load(f)

# Validate
jsonschema.validate(config, schema)
```

### IDE Integration

Many IDEs support JSON Schema for YAML validation:

**VS Code**: Add to your workspace settings:
```json
{{
  "yaml.schemas": {{
    "./schema.json": "*.yml"
  }}
}}
```

**IntelliJ IDEA**: Configure in Settings → Languages & Frameworks → Schemas and DTDs

## Schema Version

- Version: {version}
- Description: {SCHEMA_VERSIONS.get(version, "Current schema version")}
- Generated: {datetime.utcnow().isoformat()}Z

## Online Validation

You can validate your configuration online at:
https://www.jsonschemavalidator.net/

## Support

For issues or questions about the schema, please visit:
https://github.com/UMEP-dev/SUEWS
"""

    with open(output_dir / "README.md", "w") as f:
        f.write(readme_content)

    print(f"Schema bundle created in {output_dir}")
    print(f"Files created:")
    for file in output_dir.glob("*"):
        print(f"  - {file.name}")


def validate_config_against_schema(
    config_path: Path, schema_path: Optional[Path] = None, version: Optional[str] = None
) -> bool:
    """
    Validate a YAML configuration against the schema.

    Args:
        config_path: Path to YAML configuration
        schema_path: Path to schema file (generates if not provided)
        version: Schema version to validate against

    Returns:
        True if valid, False otherwise
    """
    import yaml
    import jsonschema

    # Load configuration
    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    # Load or generate schema
    if schema_path:
        with open(schema_path, "r") as f:
            if schema_path.suffix == ".yaml":
                schema = yaml.safe_load(f)
            else:
                schema = json.load(f)
    else:
        schema = generate_json_schema(version or config.get("schema_version"))

    # Validate
    try:
        jsonschema.validate(config, schema)
        print(
            f"[OK] Configuration is valid against schema v{version or CURRENT_SCHEMA_VERSION}"
        )
        return True
    except jsonschema.ValidationError as e:
        print(f"[ERROR] Validation error: {e.message}")
        print(f"  Path: {' -> '.join(str(p) for p in e.path)}")
        return False
    except jsonschema.SchemaError as e:
        print(f"[ERROR] Schema error: {e.message}")
        return False


def main():
    """Main entry point for schema publisher."""
    parser = argparse.ArgumentParser(
        description="SUEWS Configuration Schema Publisher",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate current schema
  %(prog)s generate schema.json
  
  # Generate specific version
  %(prog)s generate schema.json --version 1.0
  
  # Create complete schema bundle
  %(prog)s bundle ./schemas/
  
  # Validate configuration
  %(prog)s validate my_config.yml
  
  # Include internal fields
  %(prog)s generate schema.json --include-internal
        """,
    )

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Generate command
    gen_parser = subparsers.add_parser("generate", help="Generate JSON Schema")
    gen_parser.add_argument("output", type=Path, help="Output file path")
    gen_parser.add_argument("--version", help="Schema version")
    gen_parser.add_argument(
        "--include-internal", action="store_true", help="Include internal fields"
    )
    gen_parser.add_argument(
        "--format", choices=["json", "yaml"], default="json", help="Output format"
    )

    # Bundle command
    bundle_parser = subparsers.add_parser("bundle", help="Create schema bundle")
    bundle_parser.add_argument("output_dir", type=Path, help="Output directory")
    bundle_parser.add_argument("--version", help="Schema version")

    # Validate command
    val_parser = subparsers.add_parser("validate", help="Validate configuration")
    val_parser.add_argument("config", type=Path, help="Configuration file")
    val_parser.add_argument("--schema", type=Path, help="Schema file")
    val_parser.add_argument("--version", help="Schema version")

    args = parser.parse_args()

    if args.command == "generate":
        save_schema(
            args.output,
            version=args.version,
            include_internal=args.include_internal,
            format=args.format,
        )
    elif args.command == "bundle":
        create_schema_bundle(args.output_dir, version=args.version)
    elif args.command == "validate":
        success = validate_config_against_schema(
            args.config, schema_path=args.schema, version=args.version
        )
        sys.exit(0 if success else 1)
    else:
        parser.print_help()
        sys.exit(1)


if __name__ == "__main__":
    main()
