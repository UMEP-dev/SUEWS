"""
Round-trip tests for SUEWSConfig.to_yaml() output.

Verifies both lossless round-tripping and clean-export behaviour.
"""

import tempfile
from pathlib import Path

import pytest
import yaml
from supy._env import trv_supy_module
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.validation.pipeline.phase_a import find_extra_parameters


@pytest.fixture
def standard_data():
    """Load the standard sample_config.yml as reference."""
    path = trv_supy_module / "sample_data" / "sample_config.yml"
    with path.open() as f:
        return yaml.safe_load(f)


@pytest.fixture
def sample_config_path():
    """Return the packaged sample configuration path."""
    return trv_supy_module / "sample_data" / "sample_config.yml"


@pytest.fixture
def sample_config(sample_config_path):
    """Load the packaged sample configuration as a model instance."""
    return SUEWSConfig.from_yaml(str(sample_config_path))


@pytest.mark.cfg
class TestToYamlRoundTrip:
    """Verify to_yaml() preserves state by default and supports clean exports."""

    def test_clean_export_has_no_extra_parameters(self, sample_config, standard_data):
        """Clean export should not add parameters absent from sample_config (#1288)."""
        # ARRANGE
        with tempfile.NamedTemporaryFile(
            suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path, include_internal=False)

        with open(tmp_path) as f:
            user_data = yaml.safe_load(f)

        extra = find_extra_parameters(user_data, standard_data)

        # ASSERT
        assert extra == [], (
            f"to_yaml() produced extra parameters not in sample_config.yml: {extra}"
        )

        # Cleanup
        Path(tmp_path).unlink(missing_ok=True)

    def test_no_private_fields_in_output(self, sample_config):
        """to_yaml() should not include _yaml_path or _auto_generate_annotated."""
        # ARRANGE
        with tempfile.NamedTemporaryFile(
            suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path)

        with open(tmp_path) as f:
            user_data = yaml.safe_load(f)

        # ASSERT
        assert "_yaml_path" not in user_data
        assert "_auto_generate_annotated" not in user_data

        Path(tmp_path).unlink(missing_ok=True)

    def test_clean_export_excludes_internal_fields(self, sample_config):
        """to_yaml(include_internal=False) should exclude internal_only fields."""
        # ARRANGE
        with tempfile.NamedTemporaryFile(
            suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path, include_internal=False)

        with open(tmp_path) as f:
            user_data = yaml.safe_load(f)

        # ASSERT - sample_config always includes at least one site
        init_states = user_data["sites"][0]["initial_states"]
        for field in (
            "dqndt",
            "dqnsdt",
            "dt_since_start",
            "lenday_id",
            "qn_av",
            "qn_s_av",
            "tair_av",
            "snowfallcum",
            "l_mod",
            "ustar",
            "ra_h",
            "rb",
            "rs",
            "hdd_id",
            "qn_surfs",
            "dqndt_surf",
        ):
            assert field not in init_states, (
                f"Internal field '{field}' should not appear in clean export output"
            )

        # Check model.control internal fields
        control = user_data["model"]["control"]
        assert "diagnose" not in control
        assert "kdownzen" not in control

        Path(tmp_path).unlink(missing_ok=True)

    def test_default_round_trip_preserves_internal_fields(self, sample_config):
        """Default to_yaml() should preserve internal state on round-trip."""
        # ARRANGE
        sample_config.model.control.diagnose = 2
        sample_config.model.control.kdownzen = 1
        sample_config.sites[0].initial_states.dqndt = 12.5
        sample_config.sites[0].initial_states.hdd_id.hdd_daily = 7.0
        sample_config.sites[0].initial_states.qn_surfs = [1.0] * 7

        with tempfile.NamedTemporaryFile(
            suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path)
        reloaded = SUEWSConfig.from_yaml(tmp_path)

        # ASSERT
        assert reloaded.model.control.diagnose == 2
        assert reloaded.model.control.kdownzen == 1
        assert reloaded.sites[0].initial_states.dqndt == 12.5
        assert reloaded.sites[0].initial_states.hdd_id.hdd_daily == 7.0
        assert reloaded.sites[0].initial_states.qn_surfs == [1.0] * 7

        Path(tmp_path).unlink(missing_ok=True)
