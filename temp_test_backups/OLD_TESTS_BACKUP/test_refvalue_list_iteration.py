"""Test RefValue iteration functionality and forcing_file with RefValue[List[str]]"""

import pytest
from supy.data_model.type import RefValue
from supy.data_model.model import ModelControl
from supy.data_model.core import SUEWSConfig
from supy.suews_sim import SUEWSSimulation
import yaml


class TestRefValueIteration:
    """Test the __iter__ method added to RefValue for list iteration"""

    def test_refvalue_iteration_with_list(self):
        """Test that RefValue can be iterated when containing a list"""
        test_list = ["file1.txt", "file2.txt", "file3.txt"]
        ref_value = RefValue(test_list)

        # Test iteration
        result = list(ref_value)
        assert result == test_list

        # Test that each iteration yields expected items
        for i, item in enumerate(ref_value):
            assert item == test_list[i]

    def test_refvalue_iteration_with_string(self):
        """Test that RefValue iteration works with strings (character iteration)"""
        test_string = "test"
        ref_value = RefValue(test_string)

        # Test iteration over string characters
        result = list(ref_value)
        assert result == ["t", "e", "s", "t"]

    def test_refvalue_iteration_with_non_iterable(self):
        """Test that RefValue iteration raises TypeError for non-iterable values"""
        ref_value = RefValue(42)

        with pytest.raises(TypeError):
            list(ref_value)


class TestForcingFileRefValue:
    """Test forcing_file with RefValue functionality"""

    def test_model_control_forcing_file_with_refvalue_list(self):
        """Test ModelControl accepts RefValue containing a list for forcing_file"""
        yaml_format = {"value": ["forcing_2020.txt", "forcing_2021.txt"]}
        expected_list = ["forcing_2020.txt", "forcing_2021.txt"]

        control = ModelControl(forcing_file=yaml_format)

        # Check that the forcing_file is stored as RefValue
        assert isinstance(control.forcing_file, RefValue)
        assert control.forcing_file.value == expected_list

        # Check that we can iterate over it
        result = list(control.forcing_file)
        assert result == expected_list

    def test_model_control_forcing_file_with_refvalue_string(self):
        """Test ModelControl works with RefValue containing a single string"""
        yaml_format = {"value": "forcing.txt"}

        control = ModelControl(forcing_file=yaml_format)

        assert isinstance(control.forcing_file, RefValue)
        assert control.forcing_file.value == "forcing.txt"


class TestSUEWSSimulationRefValue:
    """Critical tests for SUEWSSimulation with RefValue forcing_file"""

    def test_update_forcing_with_refvalue_list_critical(self):
        """CRITICAL: Test update_forcing() can load forcing data using RefValue list"""
        try:
            from importlib.resources import files
        except ImportError:
            from importlib_resources import files

        # Get sample forcing file from installed package
        supy_resources = files("supy")
        sample_forcing = supy_resources / "sample_data" / "Kc_2012_data_60.txt"
        sample_config_resource = supy_resources / "sample_data" / "sample_config.yml"

        # Load sample config
        original_data = yaml.safe_load(sample_config_resource.read_text())

        # Create RefValue list with the sample forcing file
        forcing_list = [str(sample_forcing)]
        ref_forcing = RefValue(forcing_list)

        # Modify config to use RefValue list
        original_data["model"]["control"]["forcing_file"] = {"value": forcing_list}

        # Create simulation
        config = SUEWSConfig(**original_data)
        simulation = SUEWSSimulation(config)

        # CRITICAL TEST: Call update_forcing with RefValue list
        simulation.update_forcing(ref_forcing)

        # Verify that forcing data was loaded
        assert simulation._df_forcing is not None
        assert len(simulation._df_forcing) > 0

    def test_update_forcing_with_refvalue_string_critical(self):
        """CRITICAL: Test update_forcing() can load forcing data using RefValue string"""
        try:
            from importlib.resources import files
        except ImportError:
            from importlib_resources import files

        # Get sample forcing file from installed package
        supy_resources = files("supy")
        sample_forcing = supy_resources / "sample_data" / "Kc_2012_data_60.txt"
        sample_config_resource = supy_resources / "sample_data" / "sample_config.yml"

        # Load sample config
        original_data = yaml.safe_load(sample_config_resource.read_text())

        # Create RefValue string with the sample forcing file
        ref_forcing = RefValue(str(sample_forcing))

        # Modify config to use RefValue string
        original_data["model"]["control"]["forcing_file"] = {
            "value": str(sample_forcing)
        }

        # Create simulation
        config = SUEWSConfig(**original_data)
        simulation = SUEWSSimulation(config)

        # CRITICAL TEST: Call update_forcing with RefValue string
        simulation.update_forcing(ref_forcing)

        # Verify that forcing data was loaded
        assert simulation._df_forcing is not None
        assert len(simulation._df_forcing) > 0
