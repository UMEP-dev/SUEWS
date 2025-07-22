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

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from supy.data_model.core import SUEWSConfig
from supy.data_model.type import RefValue


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
    sample_config_resource = supy_resources / "sample_run" / "sample_config.yml"

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
