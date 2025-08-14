"""
Consolidated tests for SUEWS data model core functionality.

This file combines:
- Core SUEWSConfig tests from test_data_model.py
- RefValue functionality from test_flexible_refvalue_*.py and test_refvalue_list_iteration.py
- Essential type system tests
"""

# Import section will be combined from all relevant files
import unittest
import pandas as pd
import supy as sp
from pathlib import Path
import numpy as np
import pytest
from supy.data_model import (
    SUEWSConfig,
    Model,
    Site,
    SiteProperties,
    InitialStates,
    RefValue,
)
from supy.data_model.model import ModelControl


class TestSUEWSConfig(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures."""
        self.path_sample_config = (
            Path(sp.__file__).parent / "sample_data" / "sample_config.yml"
        )
        self.config = SUEWSConfig.from_yaml(self.path_sample_config)

    def test_config_conversion_cycle(self):
        """Test if SUEWS configuration can be correctly converted between YAML and DataFrame formats."""
        print("\n========================================")
        print("Testing YAML-DataFrame-YAML conversion cycle for SUEWS configuration...")

        # Convert to DataFrame
        df_state = self.config.to_df_state()

        # Convert back to config object
        config_reconst = SUEWSConfig.from_df_state(df_state)

        # Compare the two configs
        # We'll compare a few key attributes as a basic test
        self.assertEqual(self.config.name, config_reconst.name)
        self.assertEqual(self.config.description, config_reconst.description)
        self.assertEqual(
            self.config.model.control.tstep, config_reconst.model.control.tstep
        )
        self.assertEqual(
            self.config.model.physics.netradiationmethod.value,
            config_reconst.model.physics.netradiationmethod.value,
        )
        self.assertEqual(
            self.config.sites[0].properties.lat.value,
            config_reconst.sites[0].properties.lat.value,
        )

        # Test if DataFrame conversion preserves structure
        df_state_2 = config_reconst.to_df_state()

        pd.testing.assert_frame_equal(df_state, df_state_2, check_dtype=False)

    def test_df_state_conversion_cycle(self):
        """Test conversion cycle starting from a DataFrame state."""
        print("\n========================================")
        print(
            "Testing DataFrame-YAML-DataFrame conversion cycle for SUEWS configuration..."
        )
        # TODO: Fix loopholes for bad sample data

        # Load initial DataFrame state
        df_state_init = sp.load_sample_data()[0]
        df_state_init2 = df_state_init.copy()

        # Fix sample data to pass validation
        for i in range(1, 7):
            if df_state_init2[("soilstore_surf", f"({i},)")].values[0] < 10:
                df_state_init2[("soilstore_surf", f"({i},)")] = 10

        # Create config object from DataFrame
        config_from_df = SUEWSConfig.from_df_state(df_state_init2)

        # Convert back to DataFrame
        df_state_reconst = config_from_df.to_df_state()

        # Reset ohm_coef 7th surface parameter as not used or changed
        for x in range(4):
            for y in range(3):
                df_state_reconst[("ohm_coef", f"(7, {x}, {y})")] = df_state_init[
                    ("ohm_coef", f"(7, {x}, {y})")
                ]

        df_state_reconst[("ohm_threshsw", f"(7,)")] = df_state_init[
            ("ohm_threshsw", f"(7,)")
        ]
        df_state_reconst[("ohm_threshwd", f"(7,)")] = df_state_init[
            ("ohm_threshwd", f"(7,)")
        ]

        for i in range(1, 7):
            if df_state_init[("soilstore_surf", f"({i},)")].values[0] < 10:
                df_state_reconst[("soilstore_surf", f"({i},)")] = 0

        # Compare the initial and reconstructed DataFrame states
        pd.testing.assert_frame_equal(
            df_state_init, df_state_reconst, check_dtype=False, check_like=True
        )

    def test_model_physics_validation(self):
        """Test model physics validation rules."""
        # Test storageheatmethod and ohmincqf validation in SUEWSConfig
        with self.assertRaises(ValueError):
            SUEWSConfig(
                sites=[{}],
                model={
                    "physics": {
                        "storageheatmethod": {"value": 1},
                        "ohmincqf": {"value": 1},
                    }
                },
            )

        with self.assertRaises(ValueError):
            SUEWSConfig(sites=[{}], model={"physics": {"snowuse": {"value": 1}}})

    def test_site_properties(self):
        """Test site properties data model."""
        # Test latitude bounds
        with self.assertRaises(ValueError):
            properties = SiteProperties(lat=RefValue(91.0))  # Invalid latitude

        with self.assertRaises(ValueError):
            properties = SiteProperties(lat=RefValue(-91.0))  # Invalid latitude

        # Test valid latitude
        properties = SiteProperties(lat=RefValue(51.5))  # London's latitude
        self.assertEqual(properties.lat.value, 51.5)

    def test_initial_states(self):
        """Test initial states data model."""
        states = InitialStates()

        # Test snow albedo bounds
        with self.assertRaises(ValueError):
            states.snowalb = RefValue(1.5)  # Invalid albedo > 1
            InitialStates.model_validate(states.model_dump())

        with self.assertRaises(ValueError):
            states.snowalb = RefValue(-0.1)  # Invalid albedo < 0
            InitialStates.model_validate(states.model_dump())

        # Test valid snow albedo
        states.snowalb = RefValue(0.8)
        validated_states = InitialStates.model_validate(states.model_dump())
        self.assertEqual(validated_states.snowalb.value, 0.8)

    def test_multi_site_config(self):
        """Test configuration with multiple sites."""
        config = SUEWSConfig(
            name="Multi-site test",
            description="Test configuration with multiple sites",
            sites=[
                Site(gridiv=0),
                Site(gridiv=1),
                Site(gridiv=2),
            ],
        )

        # Convert to DataFrame
        df_state = config.to_df_state()

        # Check if all sites are present
        self.assertEqual(len(df_state.index), 3)
        self.assertTrue(all(idx in df_state.index for idx in [0, 1, 2]))

        # Convert back and check if sites are preserved
        config_reconst = SUEWSConfig.from_df_state(df_state)
        self.assertEqual(len(config_reconst.sites), 3)
        self.assertEqual([site.gridiv for site in config_reconst.sites], [0, 1, 2])

    def test_wrap_numeric(self):
        """Test that wrap creates a RefValue from a plain numeric."""
        rv = RefValue.wrap(5.0)
        self.assertIsInstance(rv, RefValue)
        self.assertEqual(rv.value, 5.0)

    def test_wrap_existing_refvalue(self):
        """Test that wrap leaves an existing RefValue unchanged."""
        original = RefValue(10.0)
        wrapped = RefValue.wrap(original)
        self.assertIs(wrapped, original)

    def test_equality_between_refvalues(self):
        """Test equality and inequality between RefValues."""
        a = RefValue(5.0)
        b = RefValue(5.0)
        c = RefValue(10.0)
        self.assertEqual(a, b)
        self.assertNotEqual(a, c)

    def test_equality_with_plain_value(self):
        """Test equality comparison between RefValue and plain value."""
        a = RefValue(5.0)
        self.assertEqual(a, 5.0)
        self.assertNotEqual(a, 10.0)

    def test_comparison_with_none_returns_true(self):
        """Test that comparison with None always returns True."""
        a = RefValue(None)
        b = RefValue(5.0)

        # Less than
        self.assertTrue(a < b)
        self.assertTrue(b < a)

        # Less than or equal
        self.assertTrue(a <= b)
        self.assertTrue(b <= a)

        # Greater than
        self.assertTrue(a > b)
        self.assertTrue(b > a)

        # Greater than or equal
        self.assertTrue(a >= b)
        self.assertTrue(b >= a)


if __name__ == "__main__":
    unittest.main()


# From test_flexible_refvalue_clean.py
#!/usr/bin/env python
"""Test FlexibleRefValue by programmatically removing 'value' keys from sample config"""

import sys
from pathlib import Path
import yaml
import copy

try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files


def remove_value_keys(data):
    """
    Recursively remove 'value' keys from config data where appropriate.
    If a dict has only a 'value' key, replace the dict with the value itself.
    """
    if isinstance(data, dict):
        # Check if this dict is a simple {value: X} structure
        if len(data) == 1 and "value" in data:
            return data["value"]

        # Otherwise, process each key-value pair
        result = {}
        for key, val in data.items():
            result[key] = remove_value_keys(val)
        return result
    elif isinstance(data, list):
        return [remove_value_keys(item) for item in data]
    else:
        return data


def test_flexible_refvalue_with_cleaning():
    """Test FlexibleRefValue by cleaning the sample config"""

    print("Testing FlexibleRefValue by removing 'value' keys from sample config")
    print("=" * 70)

    # Load original sample config using proper package resource loading
    print("\n1. Loading original sample config from supy package resources...")

    # Get the supy package resource traversable
    supy_resources = files("supy")
    sample_config_resource = supy_resources / "sample_data" / "sample_config.yml"

    # Load the config data
    original_data = yaml.safe_load(sample_config_resource.read_text())

    # Create a cleaned version
    cleaned_data = copy.deepcopy(original_data)
    cleaned_data = remove_value_keys(cleaned_data)

    print("\n2. Examples of cleaning:")
    # Show some examples of what was cleaned
    examples = [
        (
            "model.control.tstep",
            original_data["model"]["control"]["tstep"],
            cleaned_data["model"]["control"]["tstep"],
        ),
        (
            "sites[0].properties.lat",
            original_data["sites"][0]["properties"]["lat"],
            cleaned_data["sites"][0]["properties"]["lat"],
        ),
        (
            "sites[0].properties.lumps.raincover",
            original_data["sites"][0]["properties"]["lumps"]["raincover"],
            cleaned_data["sites"][0]["properties"]["lumps"]["raincover"],
        ),
    ]

    for path, orig, clean in examples:
        print(f"\n   {path}:")
        print(f"   Original: {orig}")
        print(f"   Cleaned:  {clean}")

    # Test loading both versions
    print("\n3. Testing SUEWSConfig loading:")

    print("\n   a) Loading original config with value keys...")
    try:
        config_original = SUEWSConfig(**original_data)
        print("      ✓ Original config loaded successfully")
    except Exception as e:
        print(f"      ✗ Failed: {e}")
        assert False, f"Failed to load original config: {e}"

    print("\n   b) Loading cleaned config without value keys...")
    try:
        config_cleaned = SUEWSConfig(**cleaned_data)
        print("      ✓ Cleaned config loaded successfully")
    except Exception as e:
        print(f"      ✗ Failed: {e}")
        assert False, f"Failed to load original config: {e}"

    # Compare key values
    print("\n4. Comparing values between original and cleaned configs:")

    test_values = [
        ("tstep", lambda c: c.model.control.tstep),
        ("forcing_file", lambda c: c.model.control.forcing_file),
        ("lat", lambda c: c.sites[0].properties.lat),
        ("emissionsmethod", lambda c: c.model.physics.emissionsmethod),
        ("raincover", lambda c: c.sites[0].properties.lumps.raincover),
        ("g_max", lambda c: c.sites[0].properties.conductance.g_max),
    ]

    all_match = True
    for name, getter in test_values:
        try:
            val_orig = getter(config_original)
            val_clean = getter(config_cleaned)

            # Extract values if they're RefValue objects
            if isinstance(val_orig, RefValue):
                val_orig = val_orig.value
            if isinstance(val_clean, RefValue):
                val_clean = val_clean.value

            match = val_orig == val_clean
            symbol = "✓" if match else "✗"
            print(f"   {symbol} {name}: {val_orig} == {val_clean}")

            if not match:
                all_match = False
        except Exception as e:
            print(f"   ✗ {name}: Error - {e}")
            all_match = False

    # Test DataFrame conversion
    print("\n5. Testing DataFrame conversion:")
    try:
        df_orig = config_original.to_df_state()
        df_clean = config_cleaned.to_df_state()

        print(f"   Original DataFrame shape: {df_orig.shape}")
        print(f"   Cleaned DataFrame shape:  {df_clean.shape}")

        if df_orig.shape == df_clean.shape:
            print("   ✓ DataFrame shapes match")
        else:
            print("   ✗ DataFrame shapes differ")
            all_match = False

    except Exception as e:
        print(f"   ✗ DataFrame conversion failed: {e}")
        all_match = False

    # Count how many value keys were removed
    print("\n6. Cleaning statistics:")
    value_count = count_value_keys(original_data)
    print(f"   Total 'value' keys removed: {value_count}")

    print("\n" + "=" * 70)

    assert all_match, "Some tests failed"


def count_value_keys(data, count=0):
    """Count the number of 'value' keys that would be removed"""
    if isinstance(data, dict):
        if len(data) == 1 and "value" in data:
            return count + 1
        for val in data.values():
            count = count_value_keys(val, count)
    elif isinstance(data, list):
        for item in data:
            count = count_value_keys(item, count)
    return count


# From test_flexible_refvalue_syntax.py
"""Test that tstep and diagnose fields accept both direct values and RefValue syntax."""

import tempfile


def test_tstep_direct_value():
    """Test tstep with direct integer value."""
    control = ModelControl(tstep=600)
    assert control.tstep == 600


def test_tstep_refvalue():
    """Test tstep with RefValue syntax."""
    control = ModelControl(
        tstep=RefValue(value=1800, ref={"desc": "30 minute timestep"})
    )
    # Check the value is accessible
    assert control.tstep.value == 1800
    assert control.tstep.ref.desc == "30 minute timestep"


def test_diagnose_direct_value():
    """Test diagnose with direct integer value."""
    control = ModelControl(diagnose=1)
    assert control.diagnose == 1


def test_diagnose_refvalue():
    """Test diagnose with RefValue syntax."""
    control = ModelControl(
        diagnose=RefValue(value=2, ref={"desc": "Detailed diagnostics"})
    )
    assert control.diagnose.value == 2
    assert control.diagnose.ref.desc == "Detailed diagnostics"


def test_yaml_loading_both_syntaxes():
    """Test loading YAML with both direct and RefValue syntax."""
    yaml_content = """
name: "Test Config"
sites:
  - name: "Test Site"
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
model:
  control:
    # Direct values
    tstep: 300
    diagnose: 0
"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        yaml_path = Path(f.name)

    try:
        config = SUEWSConfig.from_yaml(str(yaml_path))
        assert config.model.control.tstep == 300
        assert config.model.control.diagnose == 0
    finally:
        yaml_path.unlink()


def test_yaml_loading_refvalue_syntax():
    """Test loading YAML with RefValue syntax for tstep and diagnose."""
    yaml_content = """
name: "Test Config"
sites:
  - name: "Test Site"
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
model:
  control:
    # RefValue syntax
    tstep: 
      value: 3600
      ref:
        desc: "Hourly timestep for long simulations"
    diagnose:
      value: 2
      ref:
        desc: "Full diagnostics enabled"
"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        yaml_path = Path(f.name)

    try:
        config = SUEWSConfig.from_yaml(str(yaml_path))
        # Check values are accessible
        tstep = config.model.control.tstep
        assert tstep.value == 3600
        assert tstep.ref.desc == "Hourly timestep for long simulations"

        diagnose = config.model.control.diagnose
        assert diagnose.value == 2
        assert diagnose.ref.desc == "Full diagnostics enabled"
    finally:
        yaml_path.unlink()


def test_mixed_syntax_in_yaml():
    """Test YAML with mixed syntax - some fields direct, some RefValue."""
    yaml_content = """
name: "Test Config"
sites:
  - name: "Test Site"
    gridiv: 1
    properties:
      lat: {value: 51.5}  # RefValue syntax
      lng: {value: -0.1}  # RefValue syntax
model:
  control:
    tstep: 900          # Direct value
    diagnose:           # RefValue syntax
      value: 1
      ref:
        desc: "Basic diagnostics"
"""

    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        f.write(yaml_content)
        yaml_path = Path(f.name)

    try:
        config = SUEWSConfig.from_yaml(str(yaml_path))
        # Direct value
        assert config.model.control.tstep == 900
        # RefValue
        assert config.model.control.diagnose.value == 1
        assert config.model.control.diagnose.ref.desc == "Basic diagnostics"
    finally:
        yaml_path.unlink()


# From test_refvalue_list_iteration.py
"""Test RefValue iteration functionality and forcing_file with RefValue[List[str]]"""

from supy.suews_sim import SUEWSSimulation


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
