"""
Export JSON Schema for SUEWS YAML configurations.

This module generates JSON Schema files for publication to GitHub Pages.
It runs in CI/CD to create immutable schema versions at release time.
"""

from pathlib import Path
import json
import sys
from typing import Optional

from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS
from ..core import SUEWSConfig
from .registry import SchemaRegistry


def export_schema(
    output_dir: Optional[Path] = None,
    is_preview: bool = False,
    pr_number: Optional[int] = None,
    export_all_versions: bool = False,
) -> None:
    """
    Export JSON Schema to the specified directory.

    Args:
        output_dir: Directory to write schema files (default: public/schema/suews-config)
                   Can be a string or Path object
        is_preview: Whether this is a PR preview build
        pr_number: PR number if this is a preview build
        export_all_versions: If True, export all known schema versions (for archival)
    """
    # Default output directory for GitHub Pages
    if output_dir is None:
        if is_preview and pr_number:
            output_dir = Path(f"public/preview/pr-{pr_number}/schema/suews-config")
        else:
            output_dir = Path("public/schema/suews-config")
    else:
        # Convert string to Path if needed
        output_dir = Path(output_dir)

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Base URL for GitHub Pages
    BASE_URL = "https://umep-dev.github.io/SUEWS"

    # Generate schema from Pydantic model
    schema = SUEWSConfig.model_json_schema()

    # Add JSON Schema metadata
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"

    # Adjust schema ID for preview builds
    if is_preview and pr_number:
        schema["$id"] = (
            f"{BASE_URL}/preview/pr-{pr_number}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        )
        schema["title"] = (
            f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION} (PR #{pr_number} Preview)"
        )
        schema["description"] = (
            f"⚠️ PREVIEW VERSION - PR #{pr_number} - DO NOT USE IN PRODUCTION. "
            f"JSON Schema for SUEWS YAML configuration files. "
            f"Schema version {CURRENT_SCHEMA_VERSION}. "
            "See https://suews.readthedocs.io for documentation."
        )
    else:
        schema["$id"] = f"{BASE_URL}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
        schema["description"] = (
            f"JSON Schema for SUEWS YAML configuration files. "
            f"Schema version {CURRENT_SCHEMA_VERSION}. "
            "See https://suews.readthedocs.io for documentation."
        )

    # Write schema file
    schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
    schema_file.write_text(json.dumps(schema, indent=2))
    print(f"[OK] Exported schema v{CURRENT_SCHEMA_VERSION} to {schema_file}")

    # Create .nojekyll file to prevent Jekyll processing
    nojekyll = Path("public") / ".nojekyll"
    nojekyll.parent.mkdir(parents=True, exist_ok=True)
    nojekyll.write_text("")
    print(f"[OK] Created {nojekyll}")

    # Create root index.html to prevent 404 errors
    root_index = Path("public") / "index.html"
    root_index.parent.mkdir(parents=True, exist_ok=True)

    # Determine the correct redirect path based on build type
    if is_preview and pr_number:
        redirect_path = f"preview/pr-{pr_number}/schema/suews-config/"
        page_title = f"SUEWS Schema - PR #{pr_number} Preview"
    else:
        redirect_path = "schema/suews-config/"
        page_title = "SUEWS Schema"

    root_index_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>{page_title}</title>
    <meta charset="utf-8">
    <meta http-equiv="refresh" content="0; url={redirect_path}">
    <style>
        body {{ 
            font-family: system-ui, -apple-system, sans-serif; 
            margin: 2em; 
            text-align: center;
            padding-top: 3em;
        }}
        a {{ color: #0066cc; text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
    </style>
</head>
<body>
    <h1>SUEWS Configuration Schema</h1>
    <p>Redirecting to schema documentation...</p>
    <p>If you are not redirected automatically, <a href="{redirect_path}">click here</a>.</p>
</body>
</html>"""
    root_index.write_text(root_index_content)
    print(f"[OK] Created root {root_index} with redirect to {redirect_path}")

    # Initialize or load the schema registry
    registry_path = output_dir / "registry.json"
    registry = SchemaRegistry(registry_path)

    # Register the current version
    registry.register_version(
        version=CURRENT_SCHEMA_VERSION,
        schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
        description=SCHEMA_VERSIONS.get(CURRENT_SCHEMA_VERSION, ""),
    )

    # Create a copy for 'latest' pointing to current version
    latest_file = output_dir / "latest.json"
    latest_file.write_text(schema_file.read_text())
    print(f"[OK] Created latest.json pointing to v{CURRENT_SCHEMA_VERSION}")

    # Generate index.html using the registry
    index_html = output_dir / "index.html"
    index_content = registry.generate_index_html(
        base_url=BASE_URL, is_preview=is_preview, pr_number=pr_number
    )
    index_html.write_text(index_content)
    print(f"[OK] Created {index_html}")


def main():
    """Main entry point for command-line usage."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Export SUEWS JSON Schema for GitHub Pages publication"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory (default: public/schema/suews-config)",
    )
    parser.add_argument(
        "--preview",
        action="store_true",
        help="Mark as preview build (adds warning banner)",
    )
    parser.add_argument(
        "--pr-number",
        type=int,
        help="PR number for preview builds",
    )
    parser.add_argument(
        "--version",
        action="version",
        version=f"Schema version: {CURRENT_SCHEMA_VERSION}",
    )

    args = parser.parse_args()

    try:
        export_schema(
            output_dir=args.output_dir,
            is_preview=args.preview,
            pr_number=args.pr_number,
        )
        print(f"\n[SUCCESS] Schema export complete!")
        print(f"   Version: {CURRENT_SCHEMA_VERSION}")
        if args.preview and args.pr_number:
            print(f"   Type: PR #{args.pr_number} Preview")
            print(
                f"   [WARNING] Preview URL: https://umep-dev.github.io/SUEWS/preview/pr-{args.pr_number}/schema/suews-config/"
            )
        else:
            print(f"   Ready for GitHub Pages deployment")
    except Exception as e:
        print(f"[ERROR] {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
