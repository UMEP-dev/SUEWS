#!/usr/bin/env python3
"""Validate DXT extension against official specifications."""

import json
import sys
import os
import re
import asyncio
from pathlib import Path

# Get the base directory
BASE_DIR = Path(__file__).parent

# Add src to path for testing
sys.path.insert(0, str(BASE_DIR / 'src'))


def validate_manifest(manifest_path=None):
    """Validate manifest against DXT specification."""
    if manifest_path is None:
        manifest_path = BASE_DIR / "manifest.json"
    
    print("Validating manifest.json...")
    
    try:
        with open(manifest_path) as f:
            manifest = json.load(f)
    except json.JSONDecodeError as e:
        print(f"❌ Invalid JSON: {e}")
        return False
    except FileNotFoundError:
        print(f"❌ Manifest not found: {manifest_path}")
        return False
    
    # Required fields according to DXT spec
    required_fields = {
        "dxt_version": str,
        "name": str,
        "version": str,
        "description": str,
        "author": dict,
        "server": dict
    }
    
    valid = True
    
    # Check required fields
    for field, expected_type in required_fields.items():
        if field not in manifest:
            print(f"❌ Missing required field: {field}")
            valid = False
        elif not isinstance(manifest[field], expected_type):
            print(f"❌ Invalid type for {field}: expected {expected_type.__name__}")
            valid = False
        else:
            print(f"✅ {field}: OK")
    
    # Validate specific field formats
    if "version" in manifest:
        if not re.match(r'^\d+\.\d+\.\d+', manifest['version']):
            print("❌ Version must follow semantic versioning (x.y.z)")
            valid = False
    
    if "name" in manifest:
        if not re.match(r'^[a-z0-9-]+$', manifest['name']):
            print("❌ Name must be lowercase alphanumeric with hyphens")
            valid = False
    
    # Validate author
    if "author" in manifest and isinstance(manifest["author"], dict):
        if "name" not in manifest["author"]:
            print("❌ Author must have a name")
            valid = False
    
    # Validate server configuration
    if "server" in manifest and isinstance(manifest["server"], dict):
        if "type" not in manifest["server"]:
            print("❌ Server must have a type")
            valid = False
        elif manifest["server"]["type"] not in ["python", "node", "binary"]:
            print("❌ Server type must be python, node, or binary")
            valid = False
        
        if "entry_point" not in manifest["server"]:
            print("❌ Server must have an entry_point")
            valid = False
        else:
            entry_point_path = BASE_DIR / manifest["server"]["entry_point"]
            if not entry_point_path.exists():
                print(f"❌ Entry point not found: {manifest['server']['entry_point']}")
                valid = False
    
    # Validate tools if present
    if "tools" in manifest:
        if not isinstance(manifest["tools"], list):
            print("❌ Tools must be an array")
            valid = False
        else:
            for i, tool in enumerate(manifest["tools"]):
                if not isinstance(tool, dict):
                    print(f"❌ Tool {i} must be an object")
                    valid = False
                elif "name" not in tool or "description" not in tool:
                    print(f"❌ Tool {i} must have name and description")
                    valid = False
    
    return valid


async def validate_tool_responses():
    """Validate that tools return proper responses."""
    print("\nValidating tool responses...")
    
    try:
        from suews_mcp.tools.parameter_explainer import explain_parameter
    except ImportError as e:
        print(f"❌ Cannot import tool: {e}")
        return False
    
    valid = True
    
    # Test cases
    test_cases = [
        ("albedo", True, "Should return explanation"),
        ("", True, "Should handle empty string gracefully"),
        ("invalid_param_xyz", True, "Should handle unknown parameter"),
        ("a" * 200, True, "Should handle very long input")
    ]
    
    for param, include_examples, description in test_cases:
        try:
            result = await explain_parameter(param, include_examples)
            
            if not isinstance(result, str):
                print(f"❌ {description}: Not a string response")
                valid = False
            elif len(result) == 0:
                print(f"❌ {description}: Empty response")
                valid = False
            else:
                print(f"✅ {description}: OK ({len(result)} chars)")
                
        except Exception as e:
            print(f"❌ {description}: Exception - {e}")
            valid = False
    
    return valid


def validate_directory_structure():
    """Validate expected files exist."""
    print("\nValidating directory structure...")
    
    expected_files = [
        "manifest.json",
        "run_server.py",
        "requirements.txt",
        "src/suews_mcp/__init__.py",
        "src/suews_mcp/server.py",
        "src/suews_mcp/tools/__init__.py",
        "src/suews_mcp/tools/parameter_explainer.py"
    ]
    
    valid = True
    for file in expected_files:
        file_path = BASE_DIR / file
        if file_path.exists():
            print(f"✅ {file}")
        else:
            print(f"❌ Missing: {file}")
            valid = False
    
    return valid


def validate_build_output():
    """Validate the built DXT file if it exists."""
    print("\nValidating build output...")
    
    dist_dir = BASE_DIR / "dist"
    if not dist_dir.exists():
        print("⚠️  No dist directory found")
        return True  # Not a failure, just hasn't been built
    
    dxt_files = list(dist_dir.glob("*.dxt"))
    if not dxt_files:
        print("⚠️  No .dxt files found in dist/")
        return True
    
    latest_dxt = max(dxt_files, key=lambda p: p.stat().st_mtime)
    print(f"Found: {latest_dxt.name}")
    
    # Check file size
    size_kb = latest_dxt.stat().st_size / 1024
    print(f"Size: {size_kb:.1f} KB")
    
    if size_kb < 1:
        print("❌ DXT file seems too small")
        return False
    
    # Try to validate it's a valid ZIP
    import zipfile
    try:
        with zipfile.ZipFile(latest_dxt, 'r') as zf:
            files = zf.namelist()
            if "manifest.json" not in files:
                print("❌ No manifest.json in DXT")
                return False
            print(f"✅ Valid ZIP with {len(files)} files")
    except Exception as e:
        print(f"❌ Invalid ZIP file: {e}")
        return False
    
    return True


def main():
    """Run all validations."""
    print("DXT Extension Validator")
    print("=" * 50)
    
    results = []
    
    # Run validations
    results.append(("Manifest", validate_manifest()))
    results.append(("Directory", validate_directory_structure()))
    results.append(("Build", validate_build_output()))
    
    # Run async validation
    results.append(("Tools", asyncio.run(validate_tool_responses())))
    
    # Summary
    print("\nSummary")
    print("=" * 50)
    
    all_valid = True
    for name, valid in results:
        status = "✅ PASS" if valid else "❌ FAIL"
        print(f"{name:12} {status}")
        if not valid:
            all_valid = False
    
    if all_valid:
        print("\n✅ All validations passed!")
        return 0
    else:
        print("\n❌ Some validations failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())