"""Test suite for verifying no spurious validation warnings."""

import warnings

from supy.data_model import SUEWSConfig


class TestNoSpuriousWarnings:
    """Test that spurious warnings are eliminated in the new validation approach."""

    def test_import_no_warnings(self):
        """Test that importing SUEWSConfig doesn't generate warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            # Force reimport to test import-time behavior
            import importlib

            import supy.data_model

            importlib.reload(supy.data_model)

        # Check no warnings were generated
        assert len(w) == 0, f"Import generated {len(w)} unexpected warnings"

    def test_component_creation_no_warnings(self):
        """Test that creating components doesn't generate validation warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # Import and create various components
            from supy.data_model.core.human_activity import CO2Params
            from supy.data_model.core.site import Conductance
            from supy.data_model.core.surface import BldgsProperties, ThermalLayers
            from supy.data_model.core.type import RefValue

            # Create components with missing parameters
            co2 = CO2Params()
            cond = Conductance()
            bldgs = BldgsProperties(sfr=RefValue(0.3))
            thermal = ThermalLayers()

            # Create components with complete parameters
            co2_complete = CO2Params(
                co2pointsource=RefValue(0.0),
                ef_umolco2perj=RefValue(1.159),
                frfossilfuel_heat=RefValue(0.7),
                frfossilfuel_nonheat=RefValue(0.7),
            )

        # No warnings should be generated at component level
        assert len(w) == 0, f"Component creation generated {len(w)} warnings"

    def test_minimal_config_creation_no_warnings(self):
        """Test creating a minimal SUEWSConfig doesn't generate spurious warnings."""
        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")

            # Create minimal config
            config = SUEWSConfig(
                sites=[
                    {
                        "site_id": "test",
                        "properties": {
                            "lat": {"value": 51.5},
                            "lng": {"value": -0.1},
                            "alt": {"value": 10.0},
                            "timezone": {"value": 0},
                        },
                    }
                ]
            )

        # Should not have spurious warnings during creation
        # (Validation warnings only appear when explicitly run)
        spurious_warnings = [
            warn
            for warn in w
            if "lambda" in str(warn.filename)
            or "default_factory" in str(warn.message)
            or warn.lineno == -1  # Internal pydantic warnings
        ]

        assert len(spurious_warnings) == 0, (
            f"Found {len(spurious_warnings)} spurious warnings"
        )

    def test_yaml_loading_no_spurious_warnings(self):
        """Test that loading from YAML only shows intentional validation warnings."""
        import io
        import logging
        from pathlib import Path
        import tempfile

        yaml_content = """
sites:
  - site_id: test_site
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
      alt: {value: 10.0}
      timezone: {value: 0}
"""

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            f.write(yaml_content)
            yaml_path = Path(f.name)

        try:
            # Capture all warnings
            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")

                # Also capture logging output
                log_capture = io.StringIO()
                handler = logging.StreamHandler(log_capture)
                handler.setLevel(logging.WARNING)
                logger = logging.getLogger("SuPy")
                # Ensure logger is properly configured
                original_level = logger.level
                logger.setLevel(logging.WARNING)
                original_handlers = logger.handlers.copy()
                logger.handlers.clear()
                logger.addHandler(handler)

                try:
                    config = SUEWSConfig.from_yaml(yaml_path)
                    log_output = log_capture.getvalue()
                finally:
                    logger.removeHandler(handler)
                    logger.handlers = original_handlers
                    logger.setLevel(original_level)

            # Check warnings are appropriate
            # Should have no Python warnings
            assert len(w) == 0, f"Got {len(w)} Python warnings"

            # Check that if validation summary was generated, it was in log not warnings
            # (The minimal config might not trigger validation warnings, which is fine)
            if "VALIDATION SUMMARY" in log_output:
                # Good - validation went to log, not Python warnings
                pass
            else:
                # No validation warnings for minimal config is also acceptable
                pass

        finally:
            yaml_path.unlink()
