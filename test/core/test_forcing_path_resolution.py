"""Test forcing path resolution (issue #573) - simplified version."""

import os
import tempfile
from pathlib import Path
import pytest
from contextlib import contextmanager

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

from supy.suews_sim import SUEWSSimulation


@contextmanager
def temp_config_setup(config_content, forcing_location="next_to_config"):
    """Helper to set up temporary config and forcing files."""
    sample_forcing = files("supy").joinpath("sample_data/Kc_2012_data_60.txt")

    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        config_dir = tmpdir / "config"
        config_dir.mkdir(parents=True)

        # Write config
        config_path = config_dir / "config.yml"
        config_path.write_text(config_content)

        # Place forcing file based on test needs
        if forcing_location == "next_to_config":
            forcing_path = config_dir / "Kc_2012_data_60.txt"
        elif forcing_location == "parent":
            forcing_path = tmpdir / "forcing.txt"
        elif forcing_location == "subdir":
            data_dir = config_dir / "data"
            data_dir.mkdir()
            forcing_path = data_dir / "forcing_0.txt"
            # Create second file for list tests
            (data_dir / "forcing_1.txt").write_bytes(sample_forcing.read_bytes())
        else:
            forcing_path = tmpdir / forcing_location

        if forcing_location != "missing":
            forcing_path.write_bytes(sample_forcing.read_bytes())

        # Change to tmpdir and yield paths
        original_cwd = os.getcwd()
        try:
            os.chdir(tmpdir)
            yield config_path, forcing_path
        finally:
            os.chdir(original_cwd)


def get_base_config(forcing_file="Kc_2012_data_60.txt"):
    """Get minimal test config."""
    return f"""
name: test
model:
  control:
    forcing_file: {forcing_file}
sites:
  - gridiv: 1
    properties:
      lat: {{value: 51.5}}
      lng: {{value: -0.1}}
"""


def test_relative_path_next_to_config():
    """Forcing file next to config should be found."""
    with temp_config_setup(get_base_config()) as (config_path, _):
        sim = SUEWSSimulation(str(config_path))
        assert sim._df_forcing is not None


def test_parent_directory_reference():
    """Test '../' paths work correctly."""
    config = get_base_config("../forcing.txt")
    with temp_config_setup(config, "parent") as (config_path, _):
        sim = SUEWSSimulation(str(config_path))
        assert sim._df_forcing is not None


def test_forcing_file_list():
    """Test list of forcing files."""
    config = """
name: test
model:
  control:
    forcing_file: [data/forcing_0.txt, data/forcing_1.txt]
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
"""
    with temp_config_setup(config, "subdir") as (config_path, _):
        sim = SUEWSSimulation(str(config_path))
        assert sim._df_forcing is not None
        assert len(sim._df_forcing) > 200000  # Two files concatenated


def test_absolute_path():
    """Absolute paths should work from anywhere."""
    sample_forcing = files("supy").joinpath("sample_data/Kc_2012_data_60.txt")

    with tempfile.TemporaryDirectory() as tmpdir:
        # Create forcing at absolute path
        forcing_path = Path(tmpdir) / "forcing.txt"
        forcing_path.write_bytes(sample_forcing.read_bytes())

        # Config with absolute path
        config = get_base_config(str(forcing_path))

        with temp_config_setup(config, "missing") as (config_path, _):
            # Even though we're in a different dir, absolute path works
            sim = SUEWSSimulation(str(config_path))
            assert sim._df_forcing is not None


def test_missing_forcing_warns():
    """Missing forcing should warn but not crash."""
    config = get_base_config("nonexistent.txt")
    with temp_config_setup(config, "missing") as (config_path, _):
        with pytest.warns(UserWarning, match="Could not load forcing"):
            sim = SUEWSSimulation(str(config_path))
        assert sim._df_forcing is None
