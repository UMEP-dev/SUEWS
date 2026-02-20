"""
Export JSON Schema for SUEWS YAML configurations.

This module generates JSON Schema files for publication to GitHub Pages.
It runs in CI/CD to create immutable schema versions at release time.

Usage:
    # From package (requires SUEWS build):
    python -m supy.data_model.schema.exporter

    # From CI (no build required):
    python .github/scripts/generate_schema.py
"""

import json
import logging
import sys
from pathlib import Path
from typing import Optional

from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS
from ..core import SUEWSConfig
from .registry import SchemaRegistry

logger = logging.getLogger(__name__)

# Canonical base URL for schema hosting
BASE_URL = "https://suews.io"


def _find_project_root() -> Optional[Path]:
    """Find the project root by looking for pyproject.toml or .git.

    Traverses upward from this file's location until a marker is found.

    Returns:
        Path to project root, or None if not found
    """
    current = Path(__file__).resolve().parent
    for parent in [current, *current.parents]:
        if (parent / "pyproject.toml").exists() or (parent / ".git").exists():
            return parent
    return None


def _find_template(output_dir: Path) -> Optional[Path]:
    """Find the HTML template for index generation.

    Checks in order:
    1. Template in output directory (for CI where site/ is copied)
    2. Template in project's site/ directory (for local development)

    Args:
        output_dir: The schema output directory

    Returns:
        Path to template if found, None otherwise
    """
    # Check output directory first (CI copies site/ content here)
    candidate = output_dir / "index.html"
    if candidate.exists():
        return candidate

    # Check project's site/ directory (local development)
    project_root = _find_project_root()
    if project_root:
        candidate = project_root / "site" / "schemas" / "suews-config" / "index.html"
        if candidate.exists():
            return candidate
        logger.debug("Template not found at %s", candidate)

    return None


def export_schema(
    output_dir: Optional[Path] = None,
    is_preview: bool = False,
    pr_number: Optional[int] = None,
    base_url: Optional[str] = None,
    template_path: Optional[Path] = None,
) -> dict:
    """
    Export JSON Schema to the specified directory.

    Args:
        output_dir: Directory to write schema files
                   Default: site/schemas/suews-config (for CI)
                   or public/schema/suews-config (legacy)
        is_preview: Whether this is a PR preview build
        pr_number: PR number if this is a preview build
        base_url: Base URL for schema hosting (default: https://suews.io)
        template_path: Path to HTML template (auto-detected if not provided)

    Returns:
        The generated schema dict
    """
    # Use canonical base URL
    if base_url is None:
        base_url = BASE_URL

    # Default output directory
    if output_dir is None:
        project_root = _find_project_root()
        if project_root:
            output_dir = project_root / "site" / "schemas" / "suews-config"
        else:
            output_dir = Path("public/schema/suews-config")
    else:
        output_dir = Path(output_dir)

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Generate schema from Pydantic model
    schema = SUEWSConfig.model_json_schema()

    # Add JSON Schema metadata
    schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"

    # Adjust schema ID for preview builds
    if is_preview and pr_number:
        schema["$id"] = (
            f"{base_url}/preview/pr-{pr_number}/schemas/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        )
        schema["title"] = (
            f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION} (PR #{pr_number} Preview)"
        )
        schema["description"] = (
            f"PREVIEW VERSION - PR #{pr_number} - DO NOT USE IN PRODUCTION. "
            f"JSON Schema for SUEWS YAML configuration files. "
            f"Schema version {CURRENT_SCHEMA_VERSION}. "
            "See https://docs.suews.io for documentation."
        )
    else:
        schema["$id"] = f"{base_url}/schemas/suews-config/{CURRENT_SCHEMA_VERSION}.json"
        schema["title"] = f"SUEWS Configuration Schema v{CURRENT_SCHEMA_VERSION}"
        schema["description"] = (
            f"JSON Schema for SUEWS YAML configuration files. "
            f"Schema version {CURRENT_SCHEMA_VERSION}. "
            "See https://docs.suews.io for documentation."
        )

    # Add auto-generated headers
    schema_with_headers = {
        "_comment": "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY",
        "_schema_version": CURRENT_SCHEMA_VERSION,
        **schema,
    }

    # Write schema file
    schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
    schema_file.write_text(json.dumps(schema_with_headers, indent=2), encoding="utf-8")
    print(f"[OK] Exported schema v{CURRENT_SCHEMA_VERSION} to {schema_file}")

    # Create a copy for 'latest' pointing to current version
    latest_file = output_dir / "latest.json"
    latest_file.write_text(schema_file.read_text(encoding="utf-8"), encoding="utf-8")
    print(f"[OK] Created latest.json pointing to v{CURRENT_SCHEMA_VERSION}")

    # Initialize or load the schema registry
    registry_path = output_dir / "registry.json"
    registry = SchemaRegistry(registry_path)

    # Register the current version
    registry.register_version(
        version=CURRENT_SCHEMA_VERSION,
        schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
        description=SCHEMA_VERSIONS.get(CURRENT_SCHEMA_VERSION, ""),
    )
    print("[OK] Updated registry")

    # Find template for index.html generation
    if template_path is None:
        template_path = _find_template(output_dir)

    if template_path:
        print(f"[OK] Using template: {template_path}")
    else:
        logger.warning("No template found, using fallback HTML")

    # Generate index.html using the registry
    index_html = output_dir / "index.html"
    index_content = registry.generate_index_html(
        base_url=base_url,
        is_preview=is_preview,
        pr_number=pr_number,
        template_path=template_path,
    )
    index_html.write_text(index_content, encoding="utf-8")
    print(f"[OK] Generated {index_html}")

    return schema


def main():
    """Main entry point for command-line usage."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Export SUEWS JSON Schema for GitHub Pages publication"
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory (default: site/schemas/suews-config)",
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
        "--base-url",
        type=str,
        default=BASE_URL,
        help=f"Base URL for schema hosting (default: {BASE_URL})",
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
            base_url=args.base_url,
        )
        print(f"\n[SUCCESS] Schema export complete!")
        print(f"   Version: {CURRENT_SCHEMA_VERSION}")
        if args.preview and args.pr_number:
            print(f"   Type: PR #{args.pr_number} Preview")
            print(
                f"   Preview URL: {args.base_url}/preview/pr-{args.pr_number}/schemas/suews-config/"
            )
        else:
            print("   Ready for deployment")
    except Exception as e:
        print(f"[ERROR] {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
