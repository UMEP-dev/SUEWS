"""Test output configuration functionality."""

import pytest
from supy.data_model.core.model import (
    ModelControl,
    OutputControl,
    OutputFormat,
    Model,
    ModelPhysics,
)
from supy.data_model.core.config import SUEWSConfig
from pathlib import Path
import yaml

pytestmark = pytest.mark.api


def test_output_control_creation():
    """Test creating OutputControl objects."""
    # Default config
    config = OutputControl()
    assert config.format == OutputFormat.TXT
    assert config.freq is None
    assert config.groups is None
    assert config.dir is None

    # Custom config
    config = OutputControl(
        format=OutputFormat.PARQUET, freq=1800, groups=["SUEWS", "DailyState", "ESTM"]
    )
    assert config.format == OutputFormat.PARQUET
    assert config.freq == 1800
    assert config.groups == ["SUEWS", "DailyState", "ESTM"]
    assert config.dir is None

    # Custom config with directory
    config = OutputControl(format=OutputFormat.TXT, freq=3600, dir="./output_dir")
    assert config.format == OutputFormat.TXT
    assert config.freq == 3600
    assert config.dir == "./output_dir"


def test_output_control_validation():
    """Test OutputControl validation."""
    # Valid groups
    config = OutputControl(groups=["SUEWS", "DailyState"])
    assert config.groups == ["SUEWS", "DailyState"]

    # Invalid groups should raise error
    with pytest.raises(ValueError, match="Invalid output groups"):
        OutputControl(groups=["SUEWS", "InvalidGroup"])


def test_modelcontrol_accepts_new_output_subobject():
    """The new ``output:`` sub-object replaces ``output_file:``."""
    mc = ModelControl(output={"format": "parquet", "freq": 3600, "dir": "./out"})
    assert mc.output.format == OutputFormat.PARQUET
    assert mc.output.freq == 3600
    assert mc.output.dir == "./out"
    assert not hasattr(mc, "output_file")


def test_modelcontrol_legacy_output_file_dict_coerced():
    """Old ``output_file:`` dict form is coerced to ``output:`` in-memory."""
    import warnings

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        mc = ModelControl(
            output_file={
                "format": "txt",
                "freq": 1800,
                "groups": ["SUEWS", "DailyState"],
                "path": "./legacy_out",
            }
        )
    assert mc.output.format == OutputFormat.TXT
    assert mc.output.freq == 1800
    assert mc.output.groups == ["SUEWS", "DailyState"]
    assert mc.output.dir == "./legacy_out"
    assert any(
        "output_file" in str(w.message).lower() for w in caught
    ), "expected DeprecationWarning mentioning output_file"


def test_modelcontrol_legacy_output_file_string_dropped():
    """Old ``output_file: 'output.txt'`` string form is dropped with a warning."""
    import warnings

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        mc = ModelControl(output_file="output.txt")
    # Default OutputControl is installed.
    assert mc.output.format == OutputFormat.TXT
    assert mc.output.dir is None
    assert any(
        "output_file" in str(w.message).lower()
        and "ignored" in str(w.message).lower()
        for w in caught
    ), "expected DeprecationWarning that the string form is ignored"


def test_modelcontrol_both_keys_output_wins():
    """If both keys are present, ``output:`` wins; ``output_file:`` is dropped."""
    import warnings

    with warnings.catch_warnings(record=True):
        warnings.simplefilter("always")
        mc = ModelControl(
            output={"format": "parquet", "freq": 3600, "dir": "./new"},
            output_file={"format": "txt", "path": "./old"},
        )
    assert mc.output.format == OutputFormat.PARQUET
    assert mc.output.dir == "./new"


def test_frequency_validation():
    """Test output frequency validation against timestep."""
    # Valid: freq is multiple of tstep.
    config = SUEWSConfig(
        sites=[{}], model={"control": {"tstep": 300, "output": {"freq": 3600}}}
    )
    assert config.model.control.tstep == 300
    assert config.model.control.output.freq == 3600

    # Invalid: freq not multiple of tstep.
    with pytest.raises(ValueError, match="must be a multiple of timestep"):
        SUEWSConfig(
            sites=[{}],
            model={
                "control": {
                    "tstep": 300,
                    "output": {"freq": 1000},  # not a multiple of 300
                }
            },
        )


def test_yaml_output_config():
    """Test loading output config from YAML."""
    yaml_content = """
model:
  control:
    tstep: 300
    output:
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

    assert isinstance(config.model.control.output, OutputControl)
    assert config.model.control.output.format == OutputFormat.PARQUET
    assert config.model.control.output.freq == 1800
    assert config.model.control.output.groups == ["SUEWS", "DailyState", "ESTM"]
    assert config.model.control.output.dir is None


def test_yaml_output_config_with_dir():
    """Test loading output config with ``dir:`` from YAML."""
    yaml_content = """
model:
  control:
    tstep: 300
    output:
      format: txt
      freq: 3600
      dir: "./Output"
      groups:
        - SUEWS
        - DailyState
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    config = SUEWSConfig(**config_dict)

    assert isinstance(config.model.control.output, OutputControl)
    assert config.model.control.output.format == OutputFormat.TXT
    assert config.model.control.output.freq == 3600
    assert config.model.control.output.dir == "./Output"
    assert config.model.control.output.groups == ["SUEWS", "DailyState"]


def test_yaml_legacy_output_file_dict_still_loads():
    """Pre-migration YAML using ``output_file:`` (dict) still loads via the in-memory coercion."""
    import warnings

    yaml_content = """
model:
  control:
    output_file:
      format: parquet
      freq: 3600
      path: "./legacy_out"
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        config = SUEWSConfig(sites=[{}], **config_dict)

    assert isinstance(config.model.control.output, OutputControl)
    assert config.model.control.output.format == OutputFormat.PARQUET
    assert config.model.control.output.freq == 3600
    assert config.model.control.output.dir == "./legacy_out"
    assert any(
        issubclass(w.category, DeprecationWarning)
        and "output_file" in str(w.message).lower()
        for w in caught
    )


def test_yaml_legacy_output_file_string_emits_warning():
    """Pre-migration YAML using the string form of ``output_file`` emits a deprecation warning and falls back to defaults."""
    import warnings

    yaml_content = """
model:
  control:
    output_file: "custom_output.txt"
"""

    from supy.data_model.core import SUEWSConfig

    config_dict = yaml.safe_load(yaml_content)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        config = SUEWSConfig(sites=[{}], **config_dict)

    assert isinstance(config.model.control.output, OutputControl)
    assert config.model.control.output.format == OutputFormat.TXT
    deprecation_warnings = [
        w
        for w in caught
        if issubclass(w.category, DeprecationWarning)
        and "output_file" in str(w.message).lower()
    ]
    assert deprecation_warnings, "expected a DeprecationWarning for the legacy string form"
