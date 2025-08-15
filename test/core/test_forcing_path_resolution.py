"""Test forcing path resolution for issue #573.

Ensures forcing paths in config files are resolved relative to the config file location,
not the current working directory.
"""

import os
import tempfile
from pathlib import Path
import pytest

try:
    from importlib.resources import files
except ImportError:
    from importlib_resources import files

from supy.suews_sim import SUEWSSimulation


class TestForcingPathResolution:
    """Test that forcing paths are resolved correctly relative to config file."""

    def test_relative_forcing_path_next_to_config(self):
        """Test loading forcing file that's next to the config file."""
        # Get sample data
        sample_data_dir = files("supy").joinpath("sample_data")
        sample_config = sample_data_dir / "sample_config.yml"
        sample_forcing = sample_data_dir / "Kc_2012_data_60.txt"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create config directory
            config_dir = tmpdir / "config"
            config_dir.mkdir()

            # Copy config and forcing to same directory
            config_path = config_dir / "test_config.yml"
            config_path.write_text(sample_config.read_text())

            forcing_path = config_dir / "Kc_2012_data_60.txt"
            forcing_path.write_bytes(sample_forcing.read_bytes())

            # Change to different directory
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)  # Not in config dir

                # Load config - should find forcing next to config
                sim = SUEWSSimulation(str(config_path))

                assert sim._df_forcing is not None
                assert len(sim._df_forcing) > 0

            finally:
                os.chdir(original_cwd)

    def test_relative_forcing_path_with_parent_reference(self):
        """Test loading forcing with '../' relative path in config."""
        sample_data_dir = files("supy").joinpath("sample_data")
        sample_config = sample_data_dir / "sample_config.yml"
        sample_forcing = sample_data_dir / "Kc_2012_data_60.txt"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create nested structure
            config_subdir = tmpdir / "config" / "subdir"
            config_subdir.mkdir(parents=True)

            # Modify config to use ../forcing.txt
            config_content = sample_config.read_text()
            config_content = config_content.replace(
                "Kc_2012_data_60.txt", "../forcing.txt"
            )
            config_path = config_subdir / "test_config.yml"
            config_path.write_text(config_content)

            # Put forcing in parent directory
            forcing_path = tmpdir / "config" / "forcing.txt"
            forcing_path.write_bytes(sample_forcing.read_bytes())

            # Run from different directory
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)

                sim = SUEWSSimulation(str(config_path))

                assert sim._df_forcing is not None
                assert len(sim._df_forcing) > 0

            finally:
                os.chdir(original_cwd)

    def test_forcing_list_with_relative_paths(self):
        """Test config with list of relative forcing paths."""
        sample_data_dir = files("supy").joinpath("sample_data")
        sample_forcing = sample_data_dir / "Kc_2012_data_60.txt"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create structure
            config_dir = tmpdir / "config"
            config_dir.mkdir()
            forcing_dir = config_dir / "data"
            forcing_dir.mkdir()

            # Create forcing files
            for i in range(2):
                forcing_path = forcing_dir / f"forcing_{i}.txt"
                forcing_path.write_bytes(sample_forcing.read_bytes())

            # Create config with list
            config_content = """
name: test config
model:
  control:
    forcing_file:
      - data/forcing_0.txt
      - data/forcing_1.txt
sites:
  - name: test site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
"""
            config_path = config_dir / "config.yml"
            config_path.write_text(config_content)

            # Run from different directory
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)

                sim = SUEWSSimulation(str(config_path))

                assert sim._df_forcing is not None
                # Should have concatenated both files
                assert len(sim._df_forcing) > 200000

            finally:
                os.chdir(original_cwd)

    def test_absolute_forcing_path(self):
        """Test that absolute paths still work correctly."""
        sample_data_dir = files("supy").joinpath("sample_data")
        sample_forcing = sample_data_dir / "Kc_2012_data_60.txt"

        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create forcing file
            forcing_path = tmpdir / "forcing.txt"
            forcing_path.write_bytes(sample_forcing.read_bytes())

            # Create config with absolute path
            config_content = f"""
name: test config
model:
  control:
    forcing_file: {str(forcing_path)}
sites:
  - name: test site
    gridiv: 1
    properties:
      lat: {{value: 51.5}}
      lng: {{value: -0.1}}
"""
            config_dir = tmpdir / "config"
            config_dir.mkdir()
            config_path = config_dir / "config.yml"
            config_path.write_text(config_content)

            # Run from completely different directory
            original_cwd = os.getcwd()
            try:
                os.chdir("/tmp")

                sim = SUEWSSimulation(str(config_path))

                assert sim._df_forcing is not None
                assert len(sim._df_forcing) > 0

            finally:
                os.chdir(original_cwd)

    def test_forcing_not_found_raises_error(self):
        """Test that missing forcing file still raises appropriate error."""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            # Create config referencing non-existent forcing
            config_content = """
name: test config
model:
  control:
    forcing_file: nonexistent.txt
sites:
  - name: test site
    gridiv: 1
    properties:
      lat: {value: 51.5}
      lng: {value: -0.1}
"""
            config_path = tmpdir / "config.yml"
            config_path.write_text(config_content)

            # Should warn but not fail on init
            with pytest.warns(UserWarning, match="Could not load forcing from config"):
                sim = SUEWSSimulation(str(config_path))

            # Forcing should not be loaded
            assert sim._df_forcing is None
