"""
End-to-end integration tests for the complete wizard flow.
"""

import pytest
import tempfile
from pathlib import Path
import yaml
from unittest.mock import patch, MagicMock

from supy.cli.wizard.engine import WizardEngine
from supy.cli.wizard.validators.pydantic_integration import PydanticValidator


class TestCompleteWizardFlow:
    """Test the complete wizard flow with all steps"""

    def test_all_steps_present(self):
        """Test that all wizard steps are initialized"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "test_config.yaml"
            engine = WizardEngine(str(output_path))

            # Check all steps are present
            assert (
                len(engine.steps) == 6
            )  # Basic, Forcing, Surface, Initial, Advanced, Output

            # Check step names
            step_names = [step.name for step in engine.steps]
            assert "Basic Configuration" in step_names
            assert "Forcing Data" in step_names
            assert "Surface Parameters" in step_names
            assert "Initial Conditions" in step_names
            assert "Advanced Options" in step_names
            assert "Output Configuration" in step_names

    def test_complete_configuration_structure(self):
        """Test that a complete configuration has all required sections"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "test_config.yaml"
            engine = WizardEngine(str(output_path))

            # Create a complete configuration
            complete_config = {
                "site": {
                    "name": "TestCity",
                    "latitude": 51.5074,
                    "longitude": -0.1278,
                    "altitude": 15.0,
                    "timezone": 0,
                },
                "simulation": {
                    "timestep": 300,
                    "start_date": "2024-01-01",
                    "end_date": "2024-12-31",
                },
                "forcing": {
                    "file_path": "/data/forcing_2024.txt",
                    "source": "file",
                },
                "surface": {
                    "fractions": {
                        "paved": 0.40,
                        "buildings": 0.30,
                        "evergreen_tree": 0.05,
                        "deciduous_tree": 0.10,
                        "grass": 0.10,
                        "bare_soil": 0.03,
                        "water": 0.02,
                    },
                    "heights": {"building_mean": 12.0, "tree_mean": 8.0},
                },
                "initial_conditions": {
                    "air_temperature": 10.0,
                    "relative_humidity": 70.0,
                    "soil_moisture": {
                        "paved": 0.2,
                        "bldgs": 0.0,
                        "evergreen": 0.3,
                        "deciduous": 0.3,
                        "grass": 0.35,
                        "baresoil": 0.25,
                        "water": 1.0,
                    },
                    "surface_temperature": {
                        "paved": 10.0,
                        "bldgs": 10.0,
                        "evergreen": 9.0,
                        "deciduous": 9.0,
                        "grass": 9.5,
                        "baresoil": 9.5,
                        "water": 8.0,
                    },
                    "snow_water_equivalent": 0.0,
                    "snow_albedo": 0.0,
                    "snow_density": 0.0,
                },
                "advanced_options": {
                    "netradiationmethod": 3,
                    "emissionsmethod": 2,
                    "storageheatmethod": 1,
                    "ohmincqf": 1,
                    "stabilitymethod": 2,
                    "roughlenheatmethod": 2,
                    "roughlenmommethod": 2,
                    "smdmethod": 0,
                    "waterusemethod": 0,
                    "snowuse": 0,
                    "rslmethod": 0,
                    "rsllevel": 0,
                    "gsmodel": 2,
                    "faimethod": 0,
                    "stebbsmethod": 0,
                },
                "output": {
                    "format": "parquet",
                    "freq": 3600,
                    "groups": None,  # Not needed for parquet
                },
            }

            # Set the configuration
            engine.session.configuration = complete_config

            # Test conversion to SUEWS format
            structured = engine._structure_config(complete_config)

            # Verify basic structure
            assert "name" in structured
            assert "model" in structured
            assert "sites" in structured

            # Verify model sections
            assert "control" in structured["model"]
            assert "physics" in structured["model"]

            # Verify control section
            control = structured["model"]["control"]
            assert control["tstep"] == 300
            assert "output_file" in control

            # Verify output configuration
            output_config = control["output_file"]
            assert output_config["format"] == "parquet"
            assert output_config["freq"] == 3600

            # Verify sites
            assert len(structured["sites"]) == 1
            site = structured["sites"][0]
            assert site["name"] == "TestCity"
            assert site["properties"]["lat"]["value"] == 51.5074
            assert site["properties"]["lng"]["value"] == -0.1278

    def test_output_configuration_validation(self):
        """Test output configuration validation"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "test_config.yaml"
            engine = WizardEngine(str(output_path))

            # Find the output step
            output_step = None
            for step in engine.steps:
                if step.name == "Output Configuration":
                    output_step = step
                    break

            assert output_step is not None

            # Test valid output configuration
            valid_config = {"format": "parquet", "freq": 3600}
            assert output_step.validate(valid_config) is True

            # Test invalid frequency (not positive)
            invalid_freq = {"format": "txt", "freq": -1}
            assert output_step.validate(invalid_freq) is False

            # Test frequency not multiple of timestep
            engine.session.configuration["simulation"] = {"timestep": 300}
            invalid_multiple = {"format": "txt", "freq": 1000}  # Not multiple of 300
            assert output_step.validate(invalid_multiple) is False

            # Test valid text format with groups
            valid_txt = {
                "format": "txt",
                "freq": 600,
                "groups": ["SUEWS", "DailyState"],
            }
            assert output_step.validate(valid_txt) is True

            # Test text format without groups (should add default)
            txt_no_groups = {"format": "txt", "freq": 900}
            assert output_step.validate(txt_no_groups) is True
            assert txt_no_groups["groups"] == ["SUEWS"]  # Default added

    def test_wizard_templates(self):
        """Test that wizard templates load correctly"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Test urban template
            urban_path = Path(tmpdir) / "urban_config.yaml"
            urban_engine = WizardEngine(str(urban_path), template="urban")

            # Check template loaded
            assert "surface" in urban_engine.session.configuration
            assert "fractions" in urban_engine.session.configuration["surface"]

            # Check urban characteristics
            fractions = urban_engine.session.configuration["surface"]["fractions"]
            assert fractions["buildings"] == 0.35  # High building fraction for urban
            assert fractions["paved"] == 0.45  # High paved fraction

            # Test suburban template
            suburban_path = Path(tmpdir) / "suburban_config.yaml"
            suburban_engine = WizardEngine(str(suburban_path), template="suburban")

            sub_fractions = suburban_engine.session.configuration["surface"][
                "fractions"
            ]
            assert sub_fractions["buildings"] == 0.20  # Lower buildings for suburban
            assert sub_fractions["grass"] == 0.25  # More grass

            # Test rural template
            rural_path = Path(tmpdir) / "rural_config.yaml"
            rural_engine = WizardEngine(str(rural_path), template="rural")

            rural_fractions = rural_engine.session.configuration["surface"]["fractions"]
            assert rural_fractions["buildings"] == 0.05  # Very low buildings
            assert rural_fractions["grass"] == 0.40  # High grass

    @patch("rich.prompt.IntPrompt.ask")
    @patch("rich.prompt.FloatPrompt.ask")
    @patch("rich.prompt.Confirm.ask")
    def test_output_step_interaction(self, mock_confirm, mock_float, mock_int):
        """Test the output configuration step user interaction"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "test_config.yaml"
            engine = WizardEngine(str(output_path))

            # Set up simulation config first
            engine.session.configuration["simulation"] = {"timestep": 300}

            # Find output step
            output_step = None
            for step in engine.steps:
                if step.name == "Output Configuration":
                    output_step = step
                    break

            # Mock user inputs for parquet format
            mock_int.side_effect = [
                2,  # Choose parquet format
                3600,  # Output frequency
            ]

            # Run the step
            result = output_step.collect_input()

            # Verify results
            assert result["format"] == "parquet"
            assert result["freq"] == 3600
            assert result.get("groups") is None  # Parquet doesn't need groups

            # Test text format with groups
            mock_int.side_effect = [
                1,  # Choose text format
                1800,  # Output frequency
                2,  # Standard preset (SUEWS, DailyState, SEB)
            ]

            result = output_step.collect_input()

            assert result["format"] == "txt"
            assert result["freq"] == 1800
            assert "SUEWS" in result["groups"]
            assert "DailyState" in result["groups"]
            assert "SEB" in result["groups"]

    def test_complete_config_to_yaml(self):
        """Test saving a complete configuration to YAML"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = Path(tmpdir) / "complete_config.yaml"
            engine = WizardEngine(str(output_path))

            # Create complete configuration
            engine.session.configuration = {
                "site": {
                    "name": "London",
                    "latitude": 51.5074,
                    "longitude": -0.1278,
                    "altitude": 15.0,
                    "timezone": 0,
                },
                "simulation": {
                    "timestep": 300,
                    "start_date": "2024-01-01",
                    "end_date": "2024-01-31",
                },
                "forcing": {"file_path": "forcing.txt", "source": "file"},
                "surface": {
                    "fractions": {
                        "paved": 0.45,
                        "buildings": 0.35,
                        "evergreen_tree": 0.05,
                        "deciduous_tree": 0.05,
                        "grass": 0.08,
                        "bare_soil": 0.01,
                        "water": 0.01,
                    },
                    "heights": {"building_mean": 15.0, "tree_mean": 10.0},
                },
                "initial_conditions": {
                    "air_temperature": 5.0,
                    "relative_humidity": 80.0,
                    "soil_moisture": {
                        "paved": 0.2,
                        "bldgs": 0.0,
                        "evergreen": 0.3,
                        "deciduous": 0.3,
                        "grass": 0.35,
                        "baresoil": 0.25,
                        "water": 1.0,
                    },
                    "surface_temperature": {
                        "paved": 5.0,
                        "bldgs": 5.0,
                        "evergreen": 4.0,
                        "deciduous": 4.0,
                        "grass": 4.5,
                        "baresoil": 4.5,
                        "water": 3.0,
                    },
                    "snow_water_equivalent": 0.0,
                    "snow_albedo": 0.0,
                    "snow_density": 0.0,
                },
                "advanced_options": {
                    "netradiationmethod": 3,
                    "emissionsmethod": 2,
                    "storageheatmethod": 1,
                    "ohmincqf": 1,
                    "stabilitymethod": 2,
                    "roughlenheatmethod": 2,
                    "roughlenmommethod": 2,
                    "smdmethod": 0,
                    "waterusemethod": 0,
                    "snowuse": 0,
                    "rslmethod": 0,
                    "rsllevel": 0,
                    "gsmodel": 2,
                    "faimethod": 0,
                    "stebbsmethod": 0,
                },
                "output": {
                    "format": "txt",
                    "freq": 3600,
                    "groups": ["SUEWS", "DailyState", "SEB"],
                },
            }

            # Convert to SUEWS format
            structured = engine._structure_config(engine.session.configuration)

            # Save to file
            engine.save_config(str(output_path))

            # Load and verify
            assert output_path.exists()
            with open(output_path, "r") as f:
                saved_config = yaml.safe_load(f)

            # Verify key sections exist
            assert "name" in saved_config
            assert "model" in saved_config
            assert "sites" in saved_config

            # Verify output configuration was properly saved
            assert "output_file" in saved_config["model"]["control"]
            output_cfg = saved_config["model"]["control"]["output_file"]
            assert output_cfg["format"] == "txt"
            assert output_cfg["freq"] == 3600
            assert "SUEWS" in output_cfg["groups"]

    def test_pydantic_validation_with_output(self):
        """Test that Pydantic validation works with output configuration"""
        validator = PydanticValidator()

        # Create complete wizard config including output
        wizard_config = {
            "site": {
                "name": "TestSite",
                "latitude": 51.5,
                "longitude": -0.1,
                "altitude": 10,
                "timezone": 0,
            },
            "simulation": {
                "timestep": 300,
                "start_date": "2024-01-01",
                "end_date": "2024-12-31",
            },
            "forcing": {"file_path": "forcing.txt"},
            "surface": {"fractions": {"paved": 0.5, "buildings": 0.3, "grass": 0.2}},
            "initial_conditions": {
                "air_temperature": 10.0,
                "relative_humidity": 70.0,
                "soil_moisture": {
                    "paved": 0.2,
                    "bldgs": 0.0,
                    "grass": 0.35,
                    "evergreen": 0.3,
                    "deciduous": 0.3,
                    "baresoil": 0.25,
                    "water": 1.0,
                },
                "surface_temperature": {
                    "paved": 10.0,
                    "bldgs": 10.0,
                    "grass": 9.5,
                    "evergreen": 9.0,
                    "deciduous": 9.0,
                    "baresoil": 9.5,
                    "water": 8.0,
                },
                "snow_water_equivalent": 0.0,
                "snow_albedo": 0.0,
                "snow_density": 0.0,
            },
            "advanced_options": {
                "netradiationmethod": 3,
                "storageheatmethod": 1,
                "gsmodel": 2,
            },
            "output": {"format": "parquet", "freq": 1800},
        }

        # Convert to SUEWS format
        structured = validator._structure_config(wizard_config)

        # Verify output configuration is included
        assert "output_file" in structured["model"]["control"]
        assert structured["model"]["control"]["output_file"]["format"] == "parquet"
        assert structured["model"]["control"]["output_file"]["freq"] == 1800


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
