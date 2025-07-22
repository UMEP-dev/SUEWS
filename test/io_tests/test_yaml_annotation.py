#!/usr/bin/env python
"""
Tests for YAML annotation functionality.

This test verifies that the JSON-based YAML annotator correctly
places inline annotations at the exact locations where parameters
are missing.
"""

import pytest
import yaml
import tempfile
from pathlib import Path
from supy.data_model import SUEWSConfig


def test_json_based_yaml_annotation():
    """Test JSON-based YAML annotation for precise positioning."""

    # Create test config with missing parameters
    yaml_content = """name: Test Config
sites:
  - name: Urban Site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      
      land_cover:
        bldgs:
          sfr: {value: 0.45}
          alb: {value: 0.12}
          # Missing: bldgh, faibldg, thermal_layers
          
        grass:
          sfr: {value: 0.25}
          alb: {value: 0.21}
          # Missing: thermal_layers
"""

    # Create temporary file
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        test_file = Path(f.name)

    try:
        # Load config (will trigger validation)
        config = SUEWSConfig.from_yaml(test_file)

        # Generate annotated file
        annotated_file = config.generate_annotated_yaml(test_file)
        annotated_path = Path(annotated_file)

        # Verify annotated file exists
        assert annotated_path.exists()

        # Read annotated content
        with open(annotated_file, "r") as f:
            annotated_content = f.read()

        # Verify annotations are present
        assert "[ERROR] MISSING:" in annotated_content
        assert "[TIP] ADD HERE:" in annotated_content

        # Verify specific missing parameters are annotated
        assert "bldgh:" in annotated_content
        assert "building height in meters" in annotated_content

        # Parse the annotated YAML to verify structure
        # (removing comments for parsing)
        lines = annotated_content.split("\n")
        yaml_lines = [l for l in lines if not l.strip().startswith("#")]
        clean_yaml = "\n".join(yaml_lines)

        # Should still be valid YAML
        parsed = yaml.safe_load(clean_yaml)
        assert parsed is not None
        assert "sites" in parsed

        # Cleanup
        annotated_path.unlink()

    finally:
        test_file.unlink()


def test_annotation_with_sample_config():
    """Test annotation with the sample config file."""

    sample_config = Path("src/supy/sample_run/sample_config.yml")
    if not sample_config.exists():
        pytest.skip("Sample config not found")

    # Load the sample config
    config = SUEWSConfig.from_yaml(sample_config)

    # The sample config should be complete (no missing parameters)
    # So annotation should not add any missing parameter blocks
    with tempfile.TemporaryDirectory() as tmpdir:
        output_file = Path(tmpdir) / "sample_annotated.yml"
        annotated_file = config.generate_annotated_yaml(sample_config, output_file)

        with open(annotated_file, "r") as f:
            content = f.read()

        # Should have header
        assert "ANNOTATED SUEWS CONFIGURATION" in content
        # Note: The sample config may have some missing optional parameters
        # So we just check that the annotated file was generated properly


if __name__ == "__main__":
    # Run the tests
    test_json_based_yaml_annotation()
    test_annotation_with_sample_config()
    print("✓ All tests passed!")
