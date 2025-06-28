#!/usr/bin/env python3
"""Build desktop extension package for SUEWS MCP."""

import json
import os
import shutil
import zipfile
from pathlib import Path
from datetime import datetime


def create_desktop_extension():
    """Create .dxt desktop extension package."""

    # Paths
    build_dir = Path("build/extension")
    output_dir = Path("dist")

    # Clean and create directories
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir(parents=True)
    output_dir.mkdir(exist_ok=True)

    # Copy source files
    print("Copying source files...")
    src_dir = build_dir / "src"
    shutil.copytree("src", src_dir)

    # Copy manifest
    print("Copying manifest...")
    shutil.copy("manifest.json", build_dir / "manifest.json")
    
    # Copy run_server.py
    print("Copying run_server.py...")
    shutil.copy("run_server.py", build_dir / "run_server.py")
    
    # Copy pyproject.toml
    print("Copying pyproject.toml...")
    shutil.copy("pyproject.toml", build_dir / "pyproject.toml")
    
    # Note: SuPy is now a required dependency and will be installed via pip
    print("Note: SuPy is a required dependency (supy==2025.6.2.dev)")
    print("  Users will need to install it separately or via pip when installing the MCP server")

    # Create requirements file
    print("Creating requirements file...")
    requirements = [
        "mcp>=1.3.4",
        "pydantic>=2.0",
        "pyyaml>=6.0",
        "pandas>=2.0",
        "numpy>=1.20",
        "xarray>=2024.0",
        "supy==2025.6.2.dev"
    ]

    with open(build_dir / "requirements.txt", "w") as f:
        f.write("\n".join(requirements))
    
    # Copy setup.py for proper installation
    setup_content = '''from setuptools import setup, find_packages

setup(
    name="suews-mcp",
    version="1.0.0",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    install_requires=[
        "mcp>=1.3.4",
        "pydantic>=2.0",
        "pyyaml>=6.0",
        "pandas>=2.0",
        "numpy>=1.20",
        "xarray>=2024.0",
        "supy==2025.6.2.dev"
    ],
)'''
    
    with open(build_dir / "setup.py", "w") as f:
        f.write(setup_content)

    # Create README for the extension
    print("Creating extension README...")
    readme_content = """# SUEWS MCP Desktop Extension

This extension provides AI-powered assistance for SUEWS urban climate modeling.

## Features
- Configuration validation and suggestions
- Parameter explanations with scientific context
- Template generation for different site types
- Physics method compatibility checking
- Result interpretation and insights

## Dependencies
This extension requires SuPy v2025.6.2.dev to be installed in your Python environment.
Install it using: `pip install supy==2025.6.2.dev`

## Usage
1. Install the extension in Claude Desktop
2. Ensure SuPy is installed in your Python environment
3. Use the provided tools to work with SUEWS configurations and results
4. Ask questions about urban climate modeling

## Support
Visit https://github.com/UMEP-dev/SUEWS for more information.
"""

    with open(build_dir / "README.md", "w") as f:
        f.write(readme_content)

    # Create the .dxt file (ZIP archive)
    extension_name = f"suews-assistant-{datetime.now().strftime('%Y%m%d')}.dxt"
    extension_path = output_dir / extension_name

    print(f"Creating extension package: {extension_name}")

    with zipfile.ZipFile(extension_path, "w", zipfile.ZIP_DEFLATED) as zipf:
        # Add all files from build directory
        for root, dirs, files in os.walk(build_dir):
            for file in files:
                file_path = Path(root) / file
                arcname = file_path.relative_to(build_dir)
                zipf.write(file_path, arcname)

    # Clean up build directory
    shutil.rmtree(build_dir)

    print(f"\n✅ Desktop extension created: {extension_path}")
    print(f"   Size: {extension_path.stat().st_size / 1024:.1f} KB")
    print("\nTo install:")
    print("1. Open Claude Desktop")
    print("2. Go to Extensions")
    print(f"3. Install {extension_name}")

    return extension_path


if __name__ == "__main__":
    create_desktop_extension()
