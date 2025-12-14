"""Test output configuration functionality"""

import pytest
from supy.data_model.core.model import (
    ModelControl,
    OutputConfig,
    OutputFormat,
    Model,
    ModelPhysics,
)
from supy.data_model.core.config import SUEWSConfig
from pathlib import Path
import yaml


def test_output_config_creation():
    """Test creating OutputConfig objects"""
    # Default config
    config = OutputConfig()
    assert config.format == OutputFormat.TXT
    assert config.freq is None
    assert config.groups is None
    assert config.path is None

    # Custom config
    config = OutputConfig(
        format=OutputFormat.PARQUET, freq=1800, groups=["SUEWS", "DailyState", "ESTM"]
    )
    assert config.format == OutputFormat.PARQUET
    assert config.freq == 1800
    assert config.groups == ["SUEWS", "DailyState", "ESTM"]
    assert config.path is None

    # Custom config with path
    config = OutputConfig(format=OutputFormat.TXT, freq=3600, path="./output_dir")
    assert config.format == OutputFormat.TXT
    assert config.freq == 3600
    assert config.path == "./output_dir"


def test_output_config_validation():
    """Test OutputConfig validation"""
    # Valid groups
    config = OutputConfig(groups=["SUEWS", "DailyState"])
    assert config.groups == ["SUEWS", "DailyState"]

    # Invalid groups should raise error
    with pytest.raises(ValueError, match="Invalid output groups"):
        OutputConfig(groups=["SUEWS", "InvalidGroup"])


def test_model_control_with_output_config():
    """Test ModelControl with OutputConfig"""
    # String (backward compatibility)
    control = ModelControl(output_file="output.txt")
    assert control.output_file == "output.txt"

    # OutputConfig object
    output_config = OutputConfig(format=OutputFormat.PARQUET, freq=3600)
    control = ModelControl(output_file=output_config)
    assert isinstance(control.output_file, OutputConfig)
    assert control.output_file.format == OutputFormat.PARQUET


def test_frequency_validation():
    """Test output frequency validation against timestep"""
    # Valid: freq is multiple of tstep - should work with SUEWSConfig
    config = SUEWSConfig(
        sites=[{}], model={"control": {"tstep": 300, "output_file": {"freq": 3600}}}
    )
    assert config.model.control.tstep == 300
    assert config.model.control.output_file.freq == 3600

    # Invalid: freq not multiple of tstep - should raise ValueError in SUEWSConfig
    with pytest.raises(ValueError, match="must be a multiple of timestep"):
        SUEWSConfig(
            sites=[{}],
            model={
                "control": {
                    "tstep": 300,
                    "output_file": {
                        "freq": 1000  # Not multiple of 300
                    },
                }
            },
        )


def test_yaml_output_config():
    """Test loading output config from YAML"""
    yaml_content = """
model:
  control:
    tstep: 300
    output_file:
      format: parquet
      freq: 1800
      groups:
        - SUEWS
        - DailyState
        - ESTM
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    config = SUEWSConfig(**config_dict)

    # Pydantic automatically converts the dict to OutputConfig
    assert isinstance(config.model.control.output_file, OutputConfig)
    assert config.model.control.output_file.format == OutputFormat.PARQUET
    assert config.model.control.output_file.freq == 1800
    assert config.model.control.output_file.groups == ["SUEWS", "DailyState", "ESTM"]
    assert config.model.control.output_file.path is None


def test_yaml_output_config_with_path():
    """Test loading output config with path from YAML"""
    yaml_content = """
model:
  control:
    tstep: 300
    output_file:
      format: txt
      freq: 3600
      path: "./Output"
      groups:
        - SUEWS
        - DailyState
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    config = SUEWSConfig(**config_dict)

    # Pydantic automatically converts the dict to OutputConfig
    assert isinstance(config.model.control.output_file, OutputConfig)
    assert config.model.control.output_file.format == OutputFormat.TXT
    assert config.model.control.output_file.freq == 3600
    assert config.model.control.output_file.path == "./Output"
    assert config.model.control.output_file.groups == ["SUEWS", "DailyState"]


def test_yaml_backward_compatibility():
    """Test YAML with string output_file for backward compatibility"""
    yaml_content = """
model:
  control:
    output_file: "output.txt"
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    config = SUEWSConfig(**config_dict)

    assert config.model.control.output_file == "output.txt"


def test_deprecation_warning():
    """Test deprecation warning for non-default string output_file"""
    import warnings

    # Test that non-default string triggers warning in SUEWSConfig
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        config = SUEWSConfig(
            sites=[{}], model={"control": {"output_file": "custom_output.txt"}}
        )
        # Filter for deprecation warnings only (ignore validation warnings)
        deprecation_warnings = [
            warning for warning in w if issubclass(warning.category, DeprecationWarning)
        ]
        assert len(deprecation_warnings) == 1
        assert "deprecated" in str(deprecation_warnings[0].message)

    # Test that default string doesn't trigger warning
    with warnings.catch_warnings(record=True) as w:
        warnings.simplefilter("always")
        config = SUEWSConfig(
            sites=[{}],
            model={
                "control": {
                    "output_file": "output.txt"  # default value
                }
            },
        )
        # Filter for deprecation warnings only (ignore validation warnings)
        deprecation_warnings = [
            warning for warning in w if issubclass(warning.category, DeprecationWarning)
        ]
        assert (
            len(deprecation_warnings) == 0
        )  # No deprecation warning for default value
