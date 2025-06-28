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
    
    # Bundle SuPy from the parent worktree
    print("Bundling SuPy...")
    supy_src = Path("../../src/supy")  # Path to SuPy in the worktree
    if supy_src.exists():
        supy_dest = src_dir / "supy"
        shutil.copytree(supy_src, supy_dest)
        print(f"  Copied SuPy from {supy_src}")
    else:
        print(f"  Warning: SuPy not found at {supy_src}")
        # Try alternative path
        supy_src_alt = Path("../../../src/supy")
        if supy_src_alt.exists():
            supy_dest = src_dir / "supy"
            shutil.copytree(supy_src_alt, supy_dest)
            print(f"  Copied SuPy from {supy_src_alt}")
        else:
            print("  Error: Could not find SuPy to bundle")

    # Create requirements file
    print("Creating requirements file...")
    requirements = ["mcp>=0.9.0", "pydantic>=2.0", "pyyaml>=6.0", "pandas>=2.0", "numpy>=1.20"]

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
        "mcp>=0.9.0",
        "pydantic>=2.0", 
        "pyyaml>=6.0",
        "pandas>=2.0",
        "numpy>=1.20"
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

## Usage
1. Install the extension in Claude Desktop
2. Use the provided tools to work with SUEWS configurations and results
3. Ask questions about urban climate modeling

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
