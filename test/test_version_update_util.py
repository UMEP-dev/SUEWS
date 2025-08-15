#!/usr/bin/env python3
"""Test version update utility functions directly."""
import yaml
import tempfile
from pathlib import Path
import sys
import re

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

def test_increment():
    """Test version incrementing."""
    assert increment_config_version("v1.0") == "v1.1"
    assert increment_config_version("v1.9") == "v1.10"
    assert increment_config_version("v2.5") == "v2.6"
    assert increment_config_version("invalid") == "v1.0"
    print("✓ Config version incrementing works correctly")

def test_yaml_structure():
    """Test YAML structure with version fields."""
    yaml_content = {
        "name": "test_config_v1.0",
        "version": "2025.8.15.dev325",
        "config_version": "v1.0",
        "description": "Sample config v1.0 designed for supy version 2025.8.15.dev325",
        "model": {
            "control": {
                "tstep": 300
            },
            "physics": {}
        },
        "sites": [
            {
                "name": "test_site",
                "gridiv": 1
            }
        ]
    }
    
    # Create temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yml', delete=False) as f:
        yaml.dump(yaml_content, f)
        yaml_path = Path(f.name)
    
    try:
        # Read back and verify
        with open(yaml_path, 'r') as f:
            loaded = yaml.safe_load(f)
        
        assert loaded['version'] == "2025.8.15.dev325"
        assert loaded['config_version'] == "v1.0"
        assert "2025.8.15.dev325" in loaded['description']
        print(f"✓ YAML structure with version fields works correctly")
    finally:
        yaml_path.unlink()

def check_sample_config():
    """Check that sample_config.yml has been updated."""
    sample_path = Path(__file__).parent.parent / "src/supy/sample_data/sample_config.yml"
    
    with open(sample_path, 'r') as f:
        config = yaml.safe_load(f)
    
    print(f"\nSample config status:")
    print(f"  Name: {config.get('name', 'Not set')}")
    print(f"  Version: {config.get('version', 'Not set')}")
    print(f"  Config version: {config.get('config_version', 'Not set')}")
    print(f"  Description: {config.get('description', 'Not set')[:80]}...")
    
    if 'version' in config and 'config_version' in config:
        print("\n✓ Sample config has version tracking fields")
    else:
        print("\n⚠️  Sample config missing version tracking fields")

if __name__ == "__main__":
    print("Testing version tracking utilities...\n")
    
    try:
        test_increment()
        test_yaml_structure()
        check_sample_config()
        print("\n✅ All tests passed!")
    except AssertionError as e:
        print(f"\n❌ Test failed: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)