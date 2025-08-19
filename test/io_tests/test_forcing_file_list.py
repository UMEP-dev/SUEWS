"""Test that forcing_file parameter accepts both string and list of strings"""

import pytest
from supy.data_model.core.model import ModelControl


def test_forcing_file_single_string():
    """Test that forcing_file accepts a single string"""
    control = ModelControl(forcing_file="forcing_2020.txt")
    assert control.forcing_file == "forcing_2020.txt"


def test_forcing_file_list_of_strings():
    """Test that forcing_file accepts a list of strings"""
    files = ["forcing_2020.txt", "forcing_2021.txt", "forcing_2022.txt"]
    control = ModelControl(forcing_file=files)
    assert control.forcing_file == files
    assert isinstance(control.forcing_file, list)
    assert len(control.forcing_file) == 3


def test_forcing_file_with_ref_value():
    """Test that forcing_file works with RefValue wrapper"""
    from supy.data_model.core.type import RefValue

    # Single file with RefValue
    control = ModelControl(forcing_file=RefValue("forcing_with_ref.txt"))
    assert hasattr(control.forcing_file, "value")
    assert control.forcing_file.value == "forcing_with_ref.txt"


def test_yaml_loading_with_list():
    """Test loading a YAML config with forcing file list"""
    import yaml
    from supy.data_model.core import SUEWSConfig

    yaml_content = """
model:
  control:
    forcing_file:
      - "forcing_2020.txt" 
      - "forcing_2021.txt"
      - "forcing_2022.txt"
sites:
  - name: "TestSite"
    latitude: 51.5
    longitude: -0.1
"""

    config_dict = yaml.safe_load(yaml_content)
    config = SUEWSConfig(**config_dict)

    assert isinstance(config.model.control.forcing_file, list)
    assert len(config.model.control.forcing_file) == 3
    assert config.model.control.forcing_file[0] == "forcing_2020.txt"
