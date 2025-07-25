"""Test timezone enum support for issue #552"""

import pytest
from pathlib import Path
import tempfile
import yaml

import supy as sp
from supy.data_model.timezone_enum import TimezoneOffset


def test_timezone_enum_support():
    """Test that timezone accepts valid timezone offsets and converts them to enum"""

    # Create a test config with float timezone value
    config_dict = {
        "forcing": {"file": "test/benchmark1/forcing/Kc1_2011_data_5.txt"},
        "sites": [
            {
                "name": "Test Site",
                "properties": {
                    "lat": 28.6139,  # Delhi latitude
                    "lng": 77.2090,  # Delhi longitude
                    "timezone": {"value": 5.5},  # India Standard Time (GMT+5:30)
                    "surfacearea": 1000000,
                    "z": 10,
                    "z0m_in": 0.1,
                    "zdm_in": 2,
                },
            }
        ],
    }

    # Write config to temporary file
    with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
        yaml.dump(config_dict, f)
        temp_config_path = f.name

    try:
        # This should not raise ValidationError
        config = sp.data_model.init_config_from_yaml(temp_config_path)

        # Verify timezone value is correctly loaded as enum
        site = config.sites[0]
        timezone_value = (
            site.properties.timezone.value
            if hasattr(site.properties.timezone, "value")
            else site.properties.timezone
        )
        assert isinstance(timezone_value, TimezoneOffset), (
            f"Expected TimezoneOffset enum, got {type(timezone_value)}"
        )
        assert timezone_value == TimezoneOffset.UTC_PLUS_5_30, (
            f"Expected UTC_PLUS_5_30, got {timezone_value}"
        )
        assert timezone_value.value == 5.5, (
            f"Expected value 5.5, got {timezone_value.value}"
        )

    finally:
        # Clean up
        Path(temp_config_path).unlink(missing_ok=True)


def test_timezone_boundary_values():
    """Test timezone boundary values"""

    test_values = [
        -12.0,  # Baker Island Time
        -11.0,  # Niue Time
        -9.5,  # Marquesas Islands Time
        0.0,  # UTC
        3.5,  # Iran Standard Time
        5.5,  # India Standard Time
        5.75,  # Nepal Time
        6.5,  # Myanmar Time
        8.75,  # Australian Central Western Time (unofficial)
        9.5,  # Australian Central Standard Time
        10.5,  # Lord Howe Standard Time
        12.0,  # New Zealand Standard Time
        12.75,  # Chatham Standard Time
        14.0,  # Line Islands Time
    ]

    for tz_value in test_values:
        config_dict = {
            "forcing": {"file": "test/benchmark1/forcing/Kc1_2011_data_5.txt"},
            "sites": [
                {
                    "name": f"Test Site TZ {tz_value}",
                    "properties": {
                        "lat": 0,
                        "lng": 0,
                        "timezone": {"value": tz_value},
                        "surfacearea": 1000000,
                        "z": 10,
                        "z0m_in": 0.1,
                        "zdm_in": 2,
                    },
                }
            ],
        }

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml.dump(config_dict, f)
            temp_config_path = f.name

        try:
            # This should not raise ValidationError
            config = sp.data_model.init_config_from_yaml(temp_config_path)

            # Verify timezone value
            site = config.sites[0]
            timezone_value_loaded = (
                site.properties.timezone.value
                if hasattr(site.properties.timezone, "value")
                else site.properties.timezone
            )
            assert isinstance(timezone_value_loaded, TimezoneOffset), (
                f"Expected TimezoneOffset enum, got {type(timezone_value_loaded)}"
            )
            assert timezone_value_loaded.value == tz_value, (
                f"Expected timezone {tz_value}, got {timezone_value_loaded.value}"
            )

        finally:
            Path(temp_config_path).unlink(missing_ok=True)


def test_timezone_validation_errors():
    """Test that invalid timezone values still raise errors"""

    invalid_values = [
        -13.0,
        15.0,
        -12.1,
        12.1,
        5.25,
        7.75,
        -0.5,
        -11.5,
        13.5,
    ]  # Not standard timezone offsets

    for tz_value in invalid_values:
        config_dict = {
            "forcing": {"file": "test/benchmark1/forcing/Kc1_2011_data_5.txt"},
            "sites": [
                {
                    "name": "Test Site",
                    "properties": {
                        "lat": 0,
                        "lng": 0,
                        "timezone": {"value": tz_value},
                        "surfacearea": 1000000,
                        "z": 10,
                        "z0m_in": 0.1,
                        "zdm_in": 2,
                    },
                }
            ],
        }

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yml", delete=False) as f:
            yaml.dump(config_dict, f)
            temp_config_path = f.name

        try:
            # This should raise ValidationError
            with pytest.raises(
                Exception
            ):  # Could be ValidationError or other pydantic error
                sp.data_model.init_config_from_yaml(temp_config_path)

        finally:
            Path(temp_config_path).unlink(missing_ok=True)


if __name__ == "__main__":
    test_timezone_enum_support()
    test_timezone_boundary_values()
    test_timezone_validation_errors()
    print("All timezone tests passed!")
