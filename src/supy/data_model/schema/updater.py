#!/usr/bin/env python3
"""
Utility to update schema version information in YAML configuration files.

This tool helps manage schema versions in SUEWS YAML configurations.
Schema versions track the structure of configurations, not SUEWS model versions.
"""

import yaml
import sys
from pathlib import Path
from typing import Optional, List
import argparse
import re
from .version import CURRENT_SCHEMA_VERSION, SCHEMA_VERSIONS


def increment_schema_version(
    current_version: str, increment_type: str = "minor"
) -> str:
    """
    Increment the schema version number.

    Args:
        current_version: Current version (e.g., "1.0")
        increment_type: "major" or "minor"

    Returns:
        Incremented version string
    """
    match = re.match(r"(\d+)\.(\d+)", current_version)
    if not match:
        return "0.1"

    major = int(match.group(1))
    minor = int(match.group(2))

    if increment_type == "major":
        return f"{major + 1}.0"
    else:  # minor
        return f"{major}.{minor + 1}"


def update_yaml_schema_version(
    file_path: Path, schema_version: Optional[str] = None, auto_current: bool = False
) -> bool:
    """
    Update schema version in a YAML configuration file.

    Args:
        file_path: Path to the YAML file
        schema_version: Schema version to set (uses current if None and auto_current=True)
        auto_current: If True and schema_version is None, use CURRENT_SCHEMA_VERSION

    Returns:
        True if file was updated, False otherwise
    """
    if not file_path.exists():
        print(f"File not found: {file_path}")
        return False

    try:
        with open(file_path, "r") as f:
            config = yaml.safe_load(f)

        if not isinstance(config, dict):
            print(f"Invalid YAML structure in {file_path}")
            return False

        # Determine version to set
        version_to_set = schema_version
        if version_to_set is None and auto_current:
            version_to_set = CURRENT_SCHEMA_VERSION

        if version_to_set is None:
            print(f"No schema version specified for {file_path}")
            return False

        # Check if update needed
        current = config.get("schema_version")
        if current == version_to_set:
            print(f"Schema version already {version_to_set} in {file_path}")
            return False

        # Update schema version
        old_version = config.get("schema_version", "none")
        config["schema_version"] = version_to_set

        # Handle migration from old dual-version system
        if "version" in config or "config_version" in config:
            print(f"Migrating from old dual-version system in {file_path}")
            config.pop("version", None)
            config.pop("config_version", None)

            # Update name if it has version suffix
            if "name" in config and "_v" in config["name"]:
                base_name = config["name"].split("_v")[0]
                config["name"] = base_name

            # Simplify description
            if "description" in config:
                desc = config["description"]
                # Remove old version references
                desc = re.sub(
                    r"Sample config v[\d\.]+ designed for supy version [\d\.]+(?:\.dev\d+)?, ",
                    "",
                    desc,
                )
                desc = re.sub(
                    r"designed for supy version [\d\.]+(?:\.dev\d+)?, ", "", desc
                )
                desc = desc.strip()
                if desc:
                    config["description"] = desc

        # Write back to file
        with open(file_path, "w") as f:
            yaml.dump(
                config, f, default_flow_style=False, sort_keys=False, allow_unicode=True
            )

        print(f"Updated {file_path}: schema_version {old_version} -> {version_to_set}")
        return True

    except yaml.YAMLError as e:
        print(f"Error parsing YAML in {file_path}: {e}")
        return False
    except Exception as e:
        print(f"Error updating {file_path}: {e}")
        return False


def find_yaml_configs(
    root_path: Path, patterns: Optional[List[str]] = None
) -> List[Path]:
    """
    Find YAML configuration files in a directory tree.

    Args:
        root_path: Root directory to search
        patterns: List of glob patterns (default: common patterns)

    Returns:
        List of YAML file paths
    """
    if patterns is None:
        patterns = [
            "**/*.yml",
            "**/*.yaml",
        ]

    files = []
    for pattern in patterns:
        files.extend(root_path.glob(pattern))

    # Filter out non-config files (CI, Docker, etc.)
    excluded_dirs = {
        ".github",
        ".gitlab",
        "ci",
        ".circleci",
        "node_modules",
        ".venv",
        "__pycache__",
    }
    config_files = []
    for f in files:
        if not any(excluded in f.parts for excluded in excluded_dirs):
            # Try to detect if it's a SUEWS config
            try:
                with open(f, "r") as file:
                    content = yaml.safe_load(file)
                    if isinstance(content, dict) and (
                        "model" in content or "sites" in content
                    ):
                        config_files.append(f)
            except:
                pass  # Skip files that can't be parsed

    return config_files


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(
        description="Update schema version in SUEWS YAML configuration files",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Schema Version Policy:
- Schema versions track configuration structure, not SUEWS model versions
- Minor increments (1.0 -> 1.1): Backward compatible changes
- Major increments (1.0 -> 2.0): Breaking changes requiring migration

Examples:
  # Set specific schema version
  %(prog)s config.yml --schema-version 1.0
  
  # Update to current schema version
  %(prog)s config.yml --current
  
  # Update all configs in directory
  %(prog)s --directory ./configs --current
  
  # Migrate from old dual-version system
  %(prog)s old_config.yml --schema-version 1.0
        """,
    )

    # File/directory selection
    parser.add_argument(
        "files", nargs="*", type=Path, help="YAML configuration files to update"
    )
    parser.add_argument(
        "--directory", "-d", type=Path, help="Directory to search for YAML configs"
    )

    # Version options
    parser.add_argument(
        "--schema-version",
        "-s",
        help=f"Schema version to set (current: {CURRENT_SCHEMA_VERSION})",
    )
    parser.add_argument(
        "--current",
        "-c",
        action="store_true",
        help=f"Set to current schema version ({CURRENT_SCHEMA_VERSION})",
    )
    parser.add_argument(
        "--increment",
        choices=["major", "minor"],
        help="Increment existing version (not implemented yet)",
    )

    # Other options
    parser.add_argument(
        "--dry-run",
        "-n",
        action="store_true",
        help="Show what would be done without making changes",
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")

    args = parser.parse_args()

    # Validate arguments
    if not args.files and not args.directory:
        parser.error("Specify files or use --directory")

    if not args.schema_version and not args.current and not args.increment:
        parser.error("Specify --schema-version, --current, or --increment")

    if args.increment:
        parser.error("--increment not yet implemented")

    # Determine version to set
    version_to_set = args.schema_version
    if args.current:
        version_to_set = CURRENT_SCHEMA_VERSION

    # Collect files to update
    files_to_update = list(args.files) if args.files else []

    if args.directory:
        if args.verbose:
            print(f"Searching for YAML configs in {args.directory}")
        found_files = find_yaml_configs(args.directory)
        if args.verbose:
            print(f"Found {len(found_files)} config files")
        files_to_update.extend(found_files)

    if not files_to_update:
        print("No files to update")
        return 1

    # Update files
    updated_count = 0
    for file_path in files_to_update:
        if args.dry_run:
            print(f"Would update: {file_path}")
            updated_count += 1
        else:
            if update_yaml_schema_version(file_path, version_to_set):
                updated_count += 1

    # Summary
    action = "Would update" if args.dry_run else "Updated"
    print(f"\n{action} {updated_count}/{len(files_to_update)} files")

    if updated_count > 0 and not args.dry_run:
        print(f"\nSchema version set to: {version_to_set}")
        if version_to_set in SCHEMA_VERSIONS:
            print(f"Description: {SCHEMA_VERSIONS[version_to_set]}")

    return 0 if updated_count > 0 else 1


if __name__ == "__main__":
    sys.exit(main())
