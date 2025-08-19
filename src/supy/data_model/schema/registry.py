"""
Schema Registry for managing multiple schema versions.

This module provides functionality to manage and preserve multiple
schema versions for SUEWS YAML configurations.
"""

import json
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime

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
            "updated": datetime.utcnow().isoformat(),
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
                "added": datetime.utcnow().isoformat(),
                "is_current": version == CURRENT_SCHEMA_VERSION,
            }

        # Update current version if needed
        if version == CURRENT_SCHEMA_VERSION:
            self._registry["current"] = version
            # Mark all other versions as not current
            for v in self._registry["versions"]:
                self._registry["versions"][v]["is_current"] = v == version

        self._registry["updated"] = datetime.utcnow().isoformat()
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

    def generate_index_html(
        self,
        base_url: str,
        is_preview: bool = False,
        pr_number: Optional[int] = None,
    ) -> str:
        """
        Generate an index.html listing all schema versions.

        Args:
            base_url: Base URL for schema files
            is_preview: Whether this is a PR preview
            pr_number: PR number if preview

        Returns:
            HTML content for the index page
        """
        versions = self.get_all_versions()

        # Build version cards
        version_cards = []
        for version in versions:
            info = self.get_version_info(version)
            if not info:
                continue

            is_current = info.get("is_current", False)
            card_class = "current" if is_current else "version"

            if is_preview and pr_number:
                schema_url = f"{base_url}/preview/pr-{pr_number}/schema/suews-config/{version}.json"
            else:
                schema_url = f"{base_url}/schema/suews-config/{version}.json"

            version_cards.append(f"""
    <div class="version {card_class}">
        <h3>Version {version} {"(Current)" if is_current else ""}</h3>
        <p>{info.get("description", "")}</p>
        <p class="meta">Added: {info.get("added", "Unknown")[:10]}</p>
        <p>
            <a href="{version}.json">View Schema</a> |
            <a href="latest.json">Latest</a> |
            <code class="url">{schema_url}</code>
        </p>
    </div>""")

        preview_banner = ""
        if is_preview and pr_number:
            preview_banner = f"""
    <div class="warning-banner">
        <h2>⚠️ PREVIEW VERSION - PR #{pr_number}</h2>
        <p>
            This is a preview schema from an unmerged pull request.
            <strong>DO NOT use this schema URL in production configurations.</strong>
            <br>
            <a href="{base_url}/schema/suews-config/">View stable schemas →</a>
        </p>
    </div>"""

        return f"""<!DOCTYPE html>
<html>
<head>
    <title>SUEWS Schema {"Preview" if is_preview else "Registry"}</title>
    <meta charset="utf-8">
    <style>
        body {{
            font-family: system-ui, -apple-system, sans-serif;
            margin: 2em;
            max-width: 1200px;
            margin: 0 auto;
            padding: 2em;
        }}
        h1 {{ color: #333; }}
        .version {{
            margin: 1em 0;
            padding: 1em;
            background: #f5f5f5;
            border-radius: 5px;
            border: 1px solid #ddd;
        }}
        .current {{
            background: #e8f4f8;
            border: 2px solid #0066cc;
        }}
        .warning-banner {{
            background: #fff3cd;
            border: 2px solid #ffc107;
            padding: 1em;
            margin-bottom: 2em;
            border-radius: 5px;
        }}
        .warning-banner h2 {{
            color: #856404;
            margin-top: 0;
        }}
        .warning-banner p {{
            color: #856404;
            margin-bottom: 0;
        }}
        a {{ color: #0066cc; text-decoration: none; }}
        a:hover {{ text-decoration: underline; }}
        code {{
            background: #f0f0f0;
            padding: 2px 5px;
            border-radius: 3px;
            font-size: 0.9em;
        }}
        code.url {{
            display: inline-block;
            max-width: 100%;
            overflow-x: auto;
            margin-top: 0.5em;
        }}
        h3 {{ margin-top: 0; }}
        .meta {{
            color: #666;
            font-size: 0.9em;
        }}
        pre {{
            background: #f5f5f5;
            padding: 1em;
            border-radius: 5px;
            overflow-x: auto;
        }}
    </style>
</head>
<body>
    <h1>SUEWS Configuration Schema Registry</h1>
    {preview_banner}
    <p>
        JSON Schema definitions for SUEWS YAML configuration files.
        All schema versions are preserved and accessible.
    </p>

    <h2>Available Schema Versions ({len(versions)})</h2>
    {"".join(version_cards)}

    <h2>Usage in YAML Configuration</h2>
    <pre><code># Use specific version:
schema_version: "{self._registry.get("current", CURRENT_SCHEMA_VERSION)}"
$schema: "{base_url}/schema/suews-config/{self._registry.get("current", CURRENT_SCHEMA_VERSION)}.json"

# Or use latest (always points to current version):
$schema: "{base_url}/schema/suews-config/latest.json"

# For older versions (if needed for compatibility):
schema_version: "0.1"
$schema: "{base_url}/schema/suews-config/0.1.json"</code></pre>

    <h2>Version Policy</h2>
    <ul>
        <li><strong>Major version (1.0 → 2.0):</strong> Breaking changes requiring migration</li>
        <li><strong>Minor version (1.0 → 1.1):</strong> Backward compatible additions</li>
        <li>Schema versions are independent of SUEWS release versions</li>
    </ul>

    <hr>
    <p>
        <a href="https://github.com/UMEP-dev/SUEWS">GitHub Repository</a> |
        <a href="https://suews.readthedocs.io/en/latest/inputs/yaml/schema_versioning.html">Documentation</a> |
        <a href="registry.json">Registry JSON</a>
    </p>
    <p class="meta">Last updated: {self._registry.get("updated", "Unknown")[:19]}</p>
</body>
</html>"""
