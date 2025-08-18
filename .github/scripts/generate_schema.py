#!/usr/bin/env python3
"""
Generate JSON Schema for SUEWS without building.

This script generates JSON schemas from Pydantic models without requiring
SUEWS to be built. It's used by CI/CD workflows for schema deployment.

Usage:
    python .github/scripts/generate_schema.py [--preview --pr-number N]
"""

import argparse
import json
import re
import shutil
import sys
import tempfile
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional


def create_minimal_env(temp_dir: Path) -> None:
    """Create minimal _env.py for schema generation."""
    env_content = '''
import logging
import sys

# Minimal logger
logger_supy = logging.getLogger("supy")
logger_supy.addHandler(logging.StreamHandler(sys.stdout))
logger_supy.setLevel(logging.WARNING)

# Mock traversable for resources
class MockTraversable:
    def joinpath(self, *args): return self
    def read_text(self): return ""
    def __truediv__(self, other): return self

trv_supy_module = MockTraversable()
'''
    (temp_dir / "_env.py").write_text(env_content)


def fix_imports_in_file(file_path: Path) -> None:
    """Fix relative imports in isolated data_model."""
    content = file_path.read_text()
    
    # Fix imports that reference parent packages
    replacements = [
        (r'from \.\.\._env import', 'from _env import'),
        (r'from \.\.schema import', 'from data_model.schema import'),
        (r'from \.\.validation import', 'from data_model.validation import'),
        (r'from \.\.yaml_processor import', 'from data_model.yaml_processor import'),
    ]
    
    for pattern, replacement in replacements:
        content = re.sub(pattern, replacement, content)
    
    file_path.write_text(content)


def generate_schema(
    is_preview: bool = False,
    pr_number: Optional[int] = None
) -> Dict[str, Any]:
    """
    Generate schema in isolated environment.
    
    Args:
        is_preview: Whether this is a PR preview build
        pr_number: PR number if this is a preview
        
    Returns:
        Generated JSON schema
    """
    # Get project paths
    project_root = Path(__file__).parent.parent.parent
    src_dir = project_root / "src"
    
    # Create temporary directory for isolated execution
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        
        # Copy data_model to temp location
        src_data_model = src_dir / "supy" / "data_model"
        dst_data_model = temp_path / "data_model"
        shutil.copytree(src_data_model, dst_data_model)
        
        # Fix imports in all Python files
        for py_file in dst_data_model.rglob("*.py"):
            fix_imports_in_file(py_file)
        
        # Create minimal environment
        create_minimal_env(temp_path)
        
        # Add temp dir to path
        sys.path.insert(0, str(temp_path))
        
        try:
            # Import from isolated data_model
            from data_model.core.config import SUEWSConfig
            from data_model.schema.version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS
            from data_model.schema.registry import SchemaRegistry
            
            print(f"✓ Schema version: {CURRENT_SCHEMA_VERSION}")
            
            # Generate schema
            schema = SUEWSConfig.model_json_schema()
            
            # Add metadata
            base_url = "https://umep-dev.github.io/SUEWS"
            schema["$schema"] = "https://json-schema.org/draft/2020-12/schema"
            
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
            
            print(f"✓ Generated schema: {len(schema.get('properties', {}))} properties, "
                  f"{len(schema.get('$defs', {}))} definitions")
            
            # Save to permanent storage
            output_dir = project_root / "schemas" / "suews-config"
            output_dir.mkdir(parents=True, exist_ok=True)
            
            # Add auto-generated headers
            schema_with_headers = {
                "_comment": "AUTO-GENERATED FILE - DO NOT EDIT MANUALLY",
                "_generated_by": "github-actions[bot]",
                "_generated_at": datetime.utcnow().isoformat() + "Z",
                "_source": ".github/scripts/generate_schema.py",
                "_schema_version": CURRENT_SCHEMA_VERSION,
                **schema
            }
            
            # Save versioned schema
            schema_file = output_dir / f"{CURRENT_SCHEMA_VERSION}.json"
            with open(schema_file, 'w') as f:
                json.dump(schema_with_headers, f, indent=2)
            print(f"✓ Saved: {schema_file}")
            
            # Save as latest (with headers)
            latest_file = output_dir / "latest.json"
            with open(latest_file, 'w') as f:
                json.dump(schema_with_headers, f, indent=2)
            
            # Update registry
            registry = SchemaRegistry(output_dir / "registry.json")
            registry.register_version(
                version=CURRENT_SCHEMA_VERSION,
                schema_path=f"{CURRENT_SCHEMA_VERSION}.json",
                description=SCHEMA_VERSIONS.get(CURRENT_SCHEMA_VERSION, "")
            )
            print(f"✓ Updated registry")
            
            # Generate index.html
            index_content = registry.generate_index_html(
                base_url=base_url,
                is_preview=is_preview,
                pr_number=pr_number
            )
            (output_dir / "index.html").write_text(index_content)
            print(f"✓ Generated index.html")
            
            return schema
            
        finally:
            # Clean up sys.path
            if str(temp_path) in sys.path:
                sys.path.remove(str(temp_path))


def main():
    """Main entry point."""
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
        schema = generate_schema(
            is_preview=args.preview,
            pr_number=args.pr_number
        )
        
        print("\n✅ Schema generation complete (no build required)")
        
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()