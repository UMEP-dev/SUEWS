"""Test forcing path resolution (issue #573) - simplified version."""

import os
import tempfile
from pathlib import Path
import pandas as pd
import pytest
from contextlib import contextmanager

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

from supy.suews_sim import SUEWSSimulation

pytestmark = pytest.mark.api


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
        config_path.write_text(config_content, encoding="utf-8")

        # Place forcing file based on test needs
        if forcing_location == "next_to_config":
            forcing_path = config_dir / "Kc_2012_data_60.txt"
        elif forcing_location == "parent":
            forcing_path = tmpdir / "forcing.txt"
        elif forcing_location == "subdir":
            data_dir = config_dir / "data"
            data_dir.mkdir()
            forcing_path = data_dir / "forcing_0.txt"
            # Second file for list tests: the second half of the sample year,
            # so the two files are distinct and non-overlapping (#1747).
            lines = sample_forcing.read_text(encoding="utf-8").splitlines(keepends=True)
            header, body = lines[0], lines[1:]
            half = len(body) // 2
            (data_dir / "forcing_1.txt").write_text(
                header + "".join(body[half:]), encoding="utf-8"
            )
            forcing_path.write_text(header + "".join(body[:half]), encoding="utf-8")
            # Change to tmpdir and yield paths
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                yield config_path, forcing_path
            finally:
                os.chdir(original_cwd)
            return
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
    forcing:
      file: {forcing_file}
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
    forcing:
      file: [data/forcing_0.txt, data/forcing_1.txt]
sites:
  - gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
"""
    with temp_config_setup(config, "subdir") as (config_path, _):
        sim = SUEWSSimulation(str(config_path))
        assert sim._df_forcing is not None
        # The two files hold the two halves of the sample year (#1747): the
        # merge must span both, once each, on a unique 5-min index.
        df = sim._df_forcing
        assert df.index.is_unique
        assert df.index[0] == pd.Timestamp("2012-01-01 00:05")
        assert df.index[-1] == pd.Timestamp("2013-01-01 00:00")
        assert len(df) == 366 * 24 * 12


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
