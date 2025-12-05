"""
Schema Registry for managing multiple schema versions.

This module provides functionality to manage and preserve multiple
schema versions for SUEWS YAML configurations.
"""

import json
import logging
import re
from pathlib import Path
from typing import Dict, List, Optional

logger = logging.getLogger(__name__)

from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS


class SchemaRegistry:
    """Manages multiple schema versions and their metadata."""

    def __init__(self, registry_path: Path):
        """
        Initialize the schema registry.

        Args:
            registry_path: Path to the registry JSON file
        """
        self.registry_path = registry_path
        self.registry_path.parent.mkdir(parents=True, exist_ok=True)
        self._registry = self._load_registry()

    def _load_registry(self) -> Dict:
        """Load existing registry or create new one."""
        if self.registry_path.exists():
            try:
                return json.loads(self.registry_path.read_text())
            except (json.JSONDecodeError, FileNotFoundError):
                pass

        # Create new registry
        return {
            "versions": {},
            "current": CURRENT_SCHEMA_VERSION,
        }

    def register_version(
        self, version: str, schema_path: str, description: Optional[str] = None
    ):
        """
        Register a new schema version.

        Args:
            version: Schema version string
            schema_path: Relative path to the schema JSON file
            description: Optional description of the version
        """
        if version not in self._registry["versions"]:
            self._registry["versions"][version] = {
                "path": schema_path,
                "description": description or SCHEMA_VERSIONS.get(version, ""),
                "is_current": version == CURRENT_SCHEMA_VERSION,
            }

        # Update current version if needed
        if version == CURRENT_SCHEMA_VERSION:
            self._registry["current"] = version
            # Mark all other versions as not current
            for v in self._registry["versions"]:
                self._registry["versions"][v]["is_current"] = v == version

        self._save_registry()

    def _save_registry(self):
        """Save the registry to disk."""
        self.registry_path.write_text(json.dumps(self._registry, indent=2))

    def get_all_versions(self) -> List[str]:
        """Get list of all registered schema versions."""
        return sorted(
            self._registry["versions"].keys(),
            reverse=True,
            key=lambda v: [int(x) if x.isdigit() else x for x in v.split(".")],
        )

    def get_version_info(self, version: str) -> Optional[Dict]:
        """Get information about a specific version."""
        return self._registry["versions"].get(version)

    def _replace_placeholder(self, html: str, name: str, replacement: str) -> str:
        """Replace a placeholder region in the HTML template.

        Placeholders are marked with <!-- BEGIN:NAME -->...<!-- END:NAME -->
        The markers are preserved so the template remains idempotent.
        """
        # Escape name to handle any regex metacharacters safely
        escaped_name = re.escape(name)
        pattern = rf"<!-- BEGIN:{escaped_name} -->.*?<!-- END:{escaped_name} -->"

        # Check if placeholder exists before replacing
        if not re.search(pattern, html, flags=re.DOTALL):
            logger.warning(
                "Placeholder '%s' not found in template; content will be missing", name
            )
            return html

        # Preserve markers so template can be re-run (idempotent)
        preserved = f"<!-- BEGIN:{name} -->{replacement}<!-- END:{name} -->"
        return re.sub(pattern, lambda m: preserved, html, flags=re.DOTALL)

    def generate_index_html(
        self,
        base_url: str,
        is_preview: bool = False,
        pr_number: Optional[int] = None,
        template_path: Optional[Path] = None,
    ) -> str:
        """
        Generate an index.html listing all schema versions.

        If template_path is provided, reads the HTML template and replaces
        placeholder regions. Otherwise falls back to generating basic HTML.

        Args:
            base_url: Base URL for schema files
            is_preview: Whether this is a PR preview
            pr_number: PR number if preview
            template_path: Path to HTML template with placeholders

        Returns:
            HTML content for the index page
        """
        versions = self.get_all_versions()
        current_version = self._registry.get("current", CURRENT_SCHEMA_VERSION)

        # Build version cards HTML
        version_cards_html = []
        for version in versions:
            info = self.get_version_info(version)
            if not info:
                continue

            is_current = info.get("is_current", False)
            card_class = "version current" if is_current else "version"

            if is_preview and pr_number:
                schema_url = f"{base_url}/preview/pr-{pr_number}/schemas/suews-config/{version}.json"
            else:
                schema_url = f"{base_url}/schemas/suews-config/{version}.json"

            current_label = " (Current)" if is_current else ""
            version_cards_html.append(
                f"""    <div class="{card_class}">
        <h3>Version {version}{current_label}</h3>
        <p>{info.get("description", "")}</p>
        <p>
            <a href="{version}.json">View Schema</a> |
            <a href="latest.json">Latest</a> |
            <code class="url">{schema_url}</code>
        </p>
    </div>"""
            )

        # Build preview banner if needed
        preview_banner = ""
        if is_preview and pr_number:
            preview_banner = f"""<div class="warning-banner" style="background:#fff3cd;border:2px solid #ffc107;padding:1em;margin-bottom:2em;border-radius:5px;">
        <h2 style="color:#856404;margin-top:0;">Preview Version - PR #{pr_number}</h2>
        <p style="color:#856404;margin-bottom:0;">
            This is a preview schema from an unmerged pull request.
            <strong>DO NOT use this schema URL in production configurations.</strong>
            <br>
            <a href="{base_url}/schemas/suews-config/">View stable schemas</a>
        </p>
    </div>"""

        # If template provided, use placeholder replacement
        if template_path and template_path.exists():
            html = template_path.read_text(encoding="utf-8")

            # Replace placeholders
            html = self._replace_placeholder(html, "VERSION_COUNT", str(len(versions)))
            html = self._replace_placeholder(
                html, "VERSION_CARDS", "\n".join(version_cards_html)
            )
            html = self._replace_placeholder(html, "CURRENT_VERSION", current_version)
            html = self._replace_placeholder(html, "BASE_URL", base_url)
            html = self._replace_placeholder(html, "PREVIEW_BANNER", preview_banner)

            return html

        # Fallback: generate basic HTML (for backwards compatibility)
        return self._generate_fallback_html(
            versions, version_cards_html, preview_banner, base_url, current_version
        )

    def _generate_fallback_html(
        self,
        versions: List[str],
        version_cards_html: List[str],
        preview_banner: str,
        base_url: str,
        current_version: str,
    ) -> str:
        """Generate basic HTML when no template is provided."""
        return f"""<!DOCTYPE html>
<html>
<head>
    <title>SUEWS Schema Registry</title>
    <meta charset="utf-8">
    <style>
        body {{ font-family: system-ui, -apple-system, sans-serif; max-width: 1200px; margin: 0 auto; padding: 2em; }}
        .version {{ margin: 1em 0; padding: 1em; background: #f5f5f5; border-radius: 5px; border: 1px solid #ddd; }}
        .current {{ background: #e8f4f8; border: 2px solid #0066cc; }}
        a {{ color: #0066cc; text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
        code {{ background: #f0f0f0; padding: 2px 5px; border-radius: 3px; font-size: 0.9em; }}
        code.url {{ display: inline-block; max-width: 100%; overflow-x: auto; margin-top: 0.5em; }}
        h3 {{ margin-top: 0; }}
        pre {{ background: #f5f5f5; padding: 1em; border-radius: 5px; overflow-x: auto; }}
    </style>
</head>
<body>
    <h1>SUEWS Configuration Schema Registry</h1>
    {preview_banner}
    <p>JSON Schema definitions for SUEWS YAML configuration files.</p>
    <h2>Available Schema Versions ({len(versions)})</h2>
{"".join(version_cards_html)}
    <h2>Usage in YAML Configuration</h2>
    <pre><code># Use specific version:
schema_version: "{current_version}"
$schema: "{base_url}/schemas/suews-config/{current_version}.json"

# Or use latest:
$schema: "{base_url}/schemas/suews-config/latest.json"</code></pre>
    <hr>
    <p><a href="https://github.com/UMEP-dev/SUEWS">GitHub</a> | <a href="registry.json">Registry JSON</a></p>
</body>
</html>"""
