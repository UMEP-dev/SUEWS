#!/usr/bin/env python3
"""
Utility to update version information in YAML configuration files.
This can be run during release preparation to ensure config files
have the correct version information.
"""

import yaml
import sys
from pathlib import Path
from typing import Optional, Dict, Any
import argparse
import re


def get_current_version() -> str:
    """Get the current supy version from git or package."""
    try:
        # Try to get version from git
        import subprocess

        result = subprocess.run(
            ["python3", "get_ver_git.py"],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent.parent.parent,  # Project root
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except:
        pass

    # Fall back to package version
    try:
        from .._version import __version__

        return __version__
    except ImportError:
        return "unknown"


def increment_config_version(current_version: str) -> str:
    """
    Increment the config version number.
    Format: v{major}.{minor}
    """
    match = re.match(r"v(\d+)\.(\d+)", current_version)
    if match:
        major = int(match.group(1))
        minor = int(match.group(2))
        return f"v{major}.{minor + 1}"
    return "v1.0"


def update_yaml_config(
    file_path: Path,
    model_version: Optional[str] = None,
    config_version: Optional[str] = None,
    update_name: bool = False,
    auto_increment: bool = False,
) -> bool:
    """
    Update version information in a YAML configuration file.

    Args:
        file_path: Path to the YAML file
        model_version: Model version to set (uses current if None)
        config_version: Config schema version to set
        update_name: Whether to update the name field with version info
        auto_increment: Whether to auto-increment the config version

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

        # Get current model version if not specified
        if model_version is None:
            model_version = get_current_version()

        # Handle config version
        if auto_increment and "config_version" in config:
            config_version = increment_config_version(
                config.get("config_version", "v1.0")
            )
        elif config_version is None and "config_version" not in config:
            config_version = "v1.0"

        # Update fields
        changes = []

        if model_version and config.get("version") != model_version:
            config["version"] = model_version
            changes.append(f"version -> {model_version}")

        if config_version and config.get("config_version") != config_version:
            config["config_version"] = config_version
            changes.append(f"config_version -> {config_version}")

        # Update name if requested
        if update_name and "name" in config:
            base_name = (
                config["name"].split("_v")[0]
                if "_v" in config["name"]
                else config["name"]
            )
            new_name = f"{base_name}_{config_version}" if config_version else base_name
            if config["name"] != new_name:
                config["name"] = new_name
                changes.append(f"name -> {new_name}")

        # Update description to include version info
        if "description" in config and model_version:
            # Extract base description without version info
            desc = config["description"]
            # Remove existing version info if present
            desc = re.sub(r"designed for supy version [\d\.]+(?:\.dev\d+)?", "", desc)
            desc = re.sub(r"Sample config v[\d\.]+ ", "", desc)
            desc = re.sub(r"^\s*,?\s*", "", desc)  # Clean up leading comma/spaces

            # Add new version info
            if config_version:
                new_desc = f"Sample config {config_version} designed for supy version {model_version}, {desc}"
            else:
                new_desc = f"Designed for supy version {model_version}, {desc}"

            if config["description"] != new_desc:
                config["description"] = new_desc
                changes.append("description updated")

        if changes:
            # Write back to file
            with open(file_path, "w") as f:
                yaml.dump(
                    config,
                    f,
                    default_flow_style=False,
                    sort_keys=False,
                    allow_unicode=True,
                )

            print(f"Updated {file_path}: {', '.join(changes)}")
            return True
        else:
            print(f"No changes needed for {file_path}")
            return False

    except yaml.YAMLError as e:
        print(f"Error parsing YAML in {file_path}: {e}")
        return False
    except Exception as e:
        print(f"Error updating {file_path}: {e}")
        return False


def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(
        description="Update version information in SUEWS YAML configuration files"
    )
    parser.add_argument(
        "files", nargs="+", type=Path, help="YAML configuration files to update"
    )
    parser.add_argument(
        "--model-version",
        help="Model version to set (uses current git version if not specified)",
    )
    parser.add_argument(
        "--config-version", help="Configuration schema version (e.g., v1.0, v1.1)"
    )
    parser.add_argument(
        "--auto-increment",
        action="store_true",
        help="Automatically increment the config version",
    )
    parser.add_argument(
        "--update-name",
        action="store_true",
        help="Update the name field to include version",
    )
    parser.add_argument(
        "--all-sample-configs",
        action="store_true",
        help="Update all sample config files in the repository",
    )

    args = parser.parse_args()

    files_to_update = []

    if args.all_sample_configs:
        # Find all sample config files
        repo_root = Path(__file__).parent.parent.parent.parent
        patterns = [
            "src/supy/sample_data/sample_config.yml",
            "test/**/benchmark*.yml",
            "docs/source/inputs/yaml/examples/*.yml",
        ]

        for pattern in patterns:
            files_to_update.extend(repo_root.glob(pattern))
    else:
        files_to_update = args.files

    if not files_to_update:
        print("No files to update")
        return 1

    updated_count = 0
    for file_path in files_to_update:
        if update_yaml_config(
            file_path,
            model_version=args.model_version,
            config_version=args.config_version,
            update_name=args.update_name,
            auto_increment=args.auto_increment,
        ):
            updated_count += 1

    print(f"\nSummary: Updated {updated_count}/{len(files_to_update)} files")
    return 0 if updated_count > 0 else 1


if __name__ == "__main__":
    sys.exit(main())
