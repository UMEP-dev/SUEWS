"""
Export JSON Schema for SUEWS YAML configurations.

This module generates JSON Schema files for publication to GitHub Pages.
It runs in CI/CD to create immutable schema versions at release time.
"""

from pathlib import Path
import json
import sys
from typing import Optional

from .version import CURRENT_SCHEMA_VERSION
from ..core import SUEWSConfig


def export_schema(output_dir: Optional[Path] = None) -> None:
    """
    Export JSON Schema to the specified directory.

    Args:
        output_dir: Directory to write schema files (default: public/schema/suews-config)
    """
    # Default output directory for GitHub Pages
    if output_dir is None:
        output_dir = Path("public/schema/suews-config")

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Base URL for GitHub Pages
    BASE_URL = "https://umep-dev.github.io/SUEWS"

    # Generate schema from Pydantic model
    schema = SUEWSConfig.model_json_schema()

    # Add JSON Schema metadata
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
    schema["$id"] = f"{BASE_URL}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"

    # Add versioning and documentation
    schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
    schema["description"] = (
        f"JSON Schema for SUEWS YAML configuration files. "
        f"Schema version {CURRENT_SCHEMA_VERSION}. "
        "See https://suews.readthedocs.io for documentation."
    )

    # Write schema file
    schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
    schema_file.write_text(json.dumps(schema, indent=2))
    print(f"✓ Exported schema v{CURRENT_SCHEMA_VERSION} to {schema_file}")

    # Create .nojekyll file to prevent Jekyll processing
    nojekyll = Path("public") / ".nojekyll"
    nojekyll.parent.mkdir(parents=True, exist_ok=True)
    nojekyll.write_text("")
    print(f"✓ Created {nojekyll}")

    # Create index.html for schema directory listing
    index_html = output_dir / "index.html"
    index_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>SUEWS Schema Versions</title>
    <style>
        body {{ font-family: system-ui, -apple-system, sans-serif; margin: 2em; }}
        h1 {{ color: #333; }}
        .version {{ margin: 1em 0; padding: 1em; background: #f5f5f5; border-radius: 5px; }}
        .current {{ background: #e8f4f8; border: 1px solid #0066cc; }}
        a {{ color: #0066cc; text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
        code {{ background: #f0f0f0; padding: 2px 5px; border-radius: 3px; }}
    </style>
</head>
<body>
    <h1>SUEWS Configuration Schema</h1>
    <p>JSON Schema definitions for SUEWS YAML configuration files.</p>
    
    <div class="version current">
        <h2>Current Version: {CURRENT_SCHEMA_VERSION}</h2>
        <p>
            Schema URL: <code>{BASE_URL}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json</code><br>
            <a href="{CURRENT_SCHEMA_VERSION}.json">View Schema</a> | 
            <a href="https://suews.readthedocs.io">Documentation</a>
        </p>
    </div>
    
    <h2>Usage in YAML</h2>
    <pre><code>schema_version: "{CURRENT_SCHEMA_VERSION}"
$schema: "{BASE_URL}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"</code></pre>
    
    <p>
        <a href="https://github.com/UMEP-dev/SUEWS">GitHub Repository</a> | 
        <a href="https://suews.readthedocs.io/en/latest/inputs/yaml/schema_versioning.html">Schema Documentation</a>
    </p>
</body>
</html>"""
    index_html.write_text(index_content)
    print(f"✓ Created {index_html}")


def main():
    """Main entry point for command-line usage."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Export SUEWS JSON Schema for GitHub Pages publication"
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="Output directory (default: public/schema/suews-config)",
    )
    parser.add_argument(
        "--version",
        action="version",
        version=f"Schema version: {CURRENT_SCHEMA_VERSION}",
    )

    args = parser.parse_args()

    try:
        export_schema(args.output)
        print(f"\n✅ Schema export complete!")
        print(f"   Version: {CURRENT_SCHEMA_VERSION}")
        print(f"   Ready for GitHub Pages deployment")
    except Exception as e:
        print(f"❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
