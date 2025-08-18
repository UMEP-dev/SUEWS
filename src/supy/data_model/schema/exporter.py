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


def export_schema(
    output_dir: Optional[Path] = None,
    is_preview: bool = False,
    pr_number: Optional[int] = None,
) -> None:
    """
    Export JSON Schema to the specified directory.

    Args:
        output_dir: Directory to write schema files (default: public/schema/suews-config)
        is_preview: Whether this is a PR preview build
        pr_number: PR number if this is a preview build
    """
    # Default output directory for GitHub Pages
    if output_dir is None:
        if is_preview and pr_number:
            output_dir = Path(f"public/preview/pr-{pr_number}/schema/suews-config")
        else:
            output_dir = Path("public/schema/suews-config")

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
    print(f"✓ Exported schema v{CURRENT_SCHEMA_VERSION} to {schema_file}")

    # Create .nojekyll file to prevent Jekyll processing
    nojekyll = Path("public") / ".nojekyll"
    nojekyll.parent.mkdir(parents=True, exist_ok=True)
    nojekyll.write_text("")
    print(f"✓ Created {nojekyll}")

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
    print(f"✓ Created root {root_index} with redirect to {redirect_path}")

    # Create index.html for schema directory listing
    index_html = output_dir / "index.html"

    # Add preview warning banner if this is a PR preview
    preview_banner = ""
    if is_preview and pr_number:
        preview_banner = f"""
    <div style="background: #fff3cd; border: 2px solid #ffc107; padding: 1em; margin-bottom: 2em; border-radius: 5px;">
        <h2 style="color: #856404; margin-top: 0;">⚠️ PREVIEW VERSION - PR #{pr_number}</h2>
        <p style="color: #856404; margin-bottom: 0;">
            This is a preview schema from an unmerged pull request. 
            <strong>DO NOT use this schema URL in production configurations.</strong>
            <br>
            <a href="{BASE_URL}/schema/suews-config/" style="color: #0066cc;">View stable schemas →</a>
        </p>
    </div>"""
        schema_url = f"{BASE_URL}/preview/pr-{pr_number}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"
    else:
        schema_url = f"{BASE_URL}/schema/suews-config/{CURRENT_SCHEMA_VERSION}.json"

    index_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>SUEWS Schema {"Preview - PR #" + str(pr_number) if is_preview else "Versions"}</title>
    <style>
        body {{ font-family: system-ui, -apple-system, sans-serif; margin: 2em; }}
        h1 {{ color: #333; }}
        .version {{ margin: 1em 0; padding: 1em; background: #f5f5f5; border-radius: 5px; }}
        .current {{ background: #e8f4f8; border: 1px solid #0066cc; }}
        .preview {{ background: #fff8e8; border: 2px dashed #ff9800; }}
        a {{ color: #0066cc; text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
        code {{ background: #f0f0f0; padding: 2px 5px; border-radius: 3px; }}
    </style>
</head>
<body>
    <h1>SUEWS Configuration Schema{" Preview" if is_preview else ""}</h1>
    {preview_banner}
    <p>JSON Schema definitions for SUEWS YAML configuration files.</p>
    
    <div class="version {"preview" if is_preview else "current"}">
        <h2>{"Preview" if is_preview else "Current"} Version: {CURRENT_SCHEMA_VERSION}</h2>
        <p>
            Schema URL: <code>{schema_url}</code><br>
            <a href="{CURRENT_SCHEMA_VERSION}.json">View Schema</a> | 
            <a href="https://suews.readthedocs.io">Documentation</a>
            {f' | <a href="https://github.com/UMEP-dev/SUEWS/pull/{pr_number}">View PR</a>' if is_preview else ""}
        </p>
    </div>
    
    <h2>Usage in YAML</h2>
    <pre><code>schema_version: "{CURRENT_SCHEMA_VERSION}"
$schema: "{schema_url}"</code></pre>
    
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
        print(f"\n✅ Schema export complete!")
        print(f"   Version: {CURRENT_SCHEMA_VERSION}")
        if args.preview and args.pr_number:
            print(f"   Type: PR #{args.pr_number} Preview")
            print(
                f"   ⚠️  Preview URL: https://umep-dev.github.io/SUEWS/preview/pr-{args.pr_number}/schema/suews-config/"
            )
        else:
            print(f"   Ready for GitHub Pages deployment")
    except Exception as e:
        print(f"❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
