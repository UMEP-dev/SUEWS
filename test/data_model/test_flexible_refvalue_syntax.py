"""Test that tstep and diagnose fields accept both direct values and RefValue syntax."""

import pytest
from pathlib import Path
import tempfile
import yaml

from supy.data_model.core import SUEWSConfig
from supy.data_model.model import ModelControl
from supy.data_model.type import RefValue


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
