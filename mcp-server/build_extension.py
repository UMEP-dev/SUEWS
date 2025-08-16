#!/usr/bin/env python3
"""Build desktop extension package for SUEWS Parameter Explainer MCP."""

import json
import os
import shutil
import zipfile
from pathlib import Path
from datetime import datetime


def create_desktop_extension():
    """Create .dxt desktop extension package."""

    # Base directory
    base_dir = Path(__file__).parent
    
    # Paths
    build_dir = base_dir / "build" / "extension"
    output_dir = base_dir / "dist"

    # Clean and create directories
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir(parents=True)
    output_dir.mkdir(exist_ok=True)

    # Copy source files
    print("Copying source files...")
    src_dir = build_dir / "src"
    # Create minimal src structure
    (src_dir / "suews_mcp" / "tools").mkdir(parents=True)
    
    # Copy only needed files
    shutil.copy(base_dir / "src/suews_mcp/__init__.py", src_dir / "suews_mcp" / "__init__.py")
    shutil.copy(base_dir / "src/suews_mcp/server.py", src_dir / "suews_mcp" / "server.py")
    shutil.copy(base_dir / "src/suews_mcp/tools/__init__.py", src_dir / "suews_mcp" / "tools" / "__init__.py")
    shutil.copy(base_dir / "src/suews_mcp/tools/parameter_explainer.py", src_dir / "suews_mcp" / "tools" / "parameter_explainer.py")

    # Copy manifest
    print("Copying manifest...")
    shutil.copy(base_dir / "manifest.json", build_dir / "manifest.json")
    
    # Copy run_server.py
    print("Copying run_server.py...")
    shutil.copy(base_dir / "run_server.py", build_dir / "run_server.py")
    
    # Create requirements file with SuPy
    print("Creating requirements file...")
    requirements = [
        "mcp>=0.1.0",
        "pydantic>=2.0",
        "supy==2025.7.6"  # Latest stable version with wheels for all platforms
    ]

    with open(build_dir / "requirements.txt", "w") as f:
        f.write("\n".join(requirements))
    
    # Create setup.py with SuPy
    setup_content = '''from setuptools import setup, find_packages

setup(
    name="suews-assistant",
    version="1.0.0",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    install_requires=[
        "mcp>=0.1.0",
        "pydantic>=2.0",
        "supy==2025.7.6"
    ],
)'''
    
    with open(build_dir / "setup.py", "w") as f:
        f.write(setup_content)

    # Create README for the extension
    print("Creating extension README...")
    readme_content = """# SUEWS Parameter Explainer Desktop Extension

This extension provides instant explanations for SUEWS urban climate model parameters.

## Features
- Detailed parameter descriptions
- Units and typical values
- Scientific context
- Usage examples
- Related parameters

## Usage
1. Install the extension in Claude Desktop
2. Use the `explain_suews_parameter` tool
3. Ask about any SUEWS parameter

## Example
Ask: "Explain the albedo parameter"
Get: Detailed explanation with typical values for different surfaces

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