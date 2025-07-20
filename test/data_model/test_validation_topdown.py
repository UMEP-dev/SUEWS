"""
Tests for top-down parameter validation approach.

This module tests the new validation system where:
1. Validation happens at SUEWSConfig level (not components)
2. Clear validation summaries are provided to users
3. Annotated YAML files help fix missing parameters
4. No spurious warnings during normal operations
"""

import pytest
import warnings
import tempfile
from pathlib import Path
from supy.data_model import SUEWSConfig
from supy.data_model.validation_utils import check_missing_params
from supy.data_model.type import RefValue


class TestTopDownValidation:
    """Test the new top-down validation approach."""

    def test_no_warnings_during_import(self):
        """Verify importing doesn't generate spurious warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            import importlib
            import supy.data_model

            importlib.reload(supy.data_model)

        assert len(w) == 0, f"Import generated {len(w)} warnings"

    def test_no_warnings_during_component_creation(self):
        """Verify component creation doesn't generate warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # Create various components that used to warn
            from supy.data_model.human_activity import CO2Params
            from supy.data_model.site import Conductance
            from supy.data_model.surface import BldgsProperties

            co2 = CO2Params()
            cond = Conductance()
            bldgs = BldgsProperties(sfr=RefValue(0.3))

        # Should have no warnings at component level
        assert len(w) == 0, f"Component creation generated {len(w)} warnings"

    def test_validation_at_config_level(self):
        """Test that validation happens at config level with clear summary."""
        config_yaml = """
sites:
  - site_id: test_site
    properties:
      land_cover:
        bldgs:
          sfr: {value: 0.45}
          # Missing critical params
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(config_yaml)
            yaml_path = Path(f.name)

        try:
            # Capture validation output
            import logging
            import io

            log_capture = io.StringIO()
            handler = logging.StreamHandler(log_capture)
            handler.setLevel(logging.WARNING)
            logger = logging.getLogger("SuPy")
            logger.addHandler(handler)

            # Load config
            config = SUEWSConfig.from_yaml(yaml_path)

            # Check validation summary was generated
            log_output = log_capture.getvalue()
            assert "VALIDATION SUMMARY" in log_output
            assert "Missing building parameters" in log_output
            assert "will shortly be generated" in log_output

            logger.removeHandler(handler)

        finally:
            yaml_path.unlink()

    def test_annotated_yaml_generation(self):
        """Test annotated YAML shows missing parameters clearly."""
        config_yaml = """
sites:
  - site_id: test_site  
    properties:
      land_cover:
        bldgs:
          sfr: {value: 0.3}
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(config_yaml)
            yaml_path = Path(f.name)

        try:
            config = SUEWSConfig.from_yaml(yaml_path)
            annotated_path = config.generate_annotated_yaml(yaml_path)

            # Check annotated file exists and contains annotations
            annotated_path = Path(annotated_path)  # Convert string to Path
            assert annotated_path.exists()
            content = annotated_path.read_text()

            # Should have missing parameter annotations
            assert "[ERROR] MISSING:" in content
            assert "[TIP] ADD HERE:" in content
            assert "bldgh:" in content  # Missing building height
            assert "faibldg:" in content  # Missing frontal area

            # Check simplified syntax (no {value: ...} wrapper)
            assert "bldgh: 20.0" in content or "bldgh: " in content
            assert "bldgh: {value:" not in content

            annotated_path.unlink()

        finally:
            yaml_path.unlink()

    def test_no_validation_with_complete_config(self):
        """Test no warnings when all required parameters are provided."""
        # This would be a complex test - simplified for now
        # Key point: when all params provided, validation summary should be minimal
        pass


class TestValidationUtils:
    """Test validation utility functions still work correctly."""

    def test_check_missing_params_with_refvalue(self):
        """Test RefValue handling in validation utils."""

        class TestObj:
            good = RefValue(value=10.0)
            bad = RefValue(value=None)
            also_bad = None

        params = {"good": "Good param", "bad": "Bad param", "also_bad": "Also bad"}

        missing = check_missing_params(params, TestObj(), "test", "test")
        assert len(missing) == 2
        assert "bad (Bad param)" in missing
        assert "also_bad (Also bad)" in missing
        assert "good" not in str(missing)

    def test_refvalue_zero_not_missing(self):
        """Test that RefValue(0.0) is not considered missing."""

        class TestObj:
            zero = RefValue(value=0.0)
            false_val = RefValue(value=False)

        params = {"zero": "Zero", "false_val": "False"}
        missing = check_missing_params(params, TestObj(), "test", "test")
        assert len(missing) == 0


# Optional: Integration test
def test_full_validation_workflow():
    """Test complete validation workflow from YAML to annotated output."""
    yaml_content = """
name: Test Config
sites:
  - site_id: site1
    properties:
      land_cover:
        paved:
          sfr: {value: 0.5}
        bldgs:
          sfr: {value: 0.3}
        grass:
          sfr: {value: 0.2}
"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        yaml_path = Path(f.name)

    try:
        # Load config (triggers validation)
        config = SUEWSConfig.from_yaml(yaml_path)

        # Generate annotated YAML
        annotated = config.generate_annotated_yaml(yaml_path)

        # Verify it was created
        annotated = Path(annotated)  # Convert string to Path
        assert annotated.exists()
        assert "annotated" in str(annotated)

        # Clean up
        annotated.unlink()

    finally:
        yaml_path.unlink()
