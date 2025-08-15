"""Simple test for version tracking functionality without full supy imports."""

import yaml
import tempfile
from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


def test_sample_config_has_version():
    """Test that sample_config.yml has version fields."""
    sample_path = (
        Path(__file__).parent.parent / "src/supy/sample_data/sample_config.yml"
    )

    with open(sample_path, "r") as f:
        config = yaml.safe_load(f)

    assert "version" in config, "sample_config.yml should have 'version' field"
    assert "config_version" in config, (
        "sample_config.yml should have 'config_version' field"
    )
    assert config["version"] == "2025.8.15.dev325"
    assert config["config_version"] == "v1.0"
    print(f"✓ sample_config.yml has version: {config['version']}")
    print(f"✓ sample_config.yml has config_version: {config['config_version']}")


def test_update_config_version_utility():
    """Test the update_config_version utility functions."""
    from supy.util.update_config_version import increment_config_version

    # Test version incrementing
    assert increment_config_version("v1.0") == "v1.1"
    assert increment_config_version("v1.9") == "v1.10"
    assert increment_config_version("v2.5") == "v2.6"
    print("✓ Config version incrementing works correctly")


def test_yaml_update():
    """Test updating a YAML file with version info."""
    from supy.util.update_config_version import update_yaml_config

    # Create a temporary YAML file
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml_content = {
            "name": "test_config",
            "description": "Test configuration",
            "model": {},
            "sites": [],
        }
        yaml.dump(yaml_content, f)
        yaml_path = Path(f.name)

    try:
        # Update the config with version info
        result = update_yaml_config(
            yaml_path,
            model_version="2025.8.15.dev325",
            config_version="v1.0",
            update_name=False,
        )

        assert result is True

        # Read back and verify
        with open(yaml_path, "r") as f:
            updated = yaml.safe_load(f)

        assert updated["version"] == "2025.8.15.dev325"
        assert updated["config_version"] == "v1.0"
        print(
            f"✓ YAML update successful: version={updated['version']}, config_version={updated['config_version']}"
        )
    finally:
        yaml_path.unlink()


if __name__ == "__main__":
    print("Testing version tracking functionality...\n")

    try:
        test_sample_config_has_version()
        print()
        test_update_config_version_utility()
        print()
        test_yaml_update()
        print("\n✅ All tests passed!")
    except AssertionError as e:
        print(f"\n❌ Test failed: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        sys.exit(1)
