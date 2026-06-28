"""
Round-trip tests for SUEWSConfig.to_yaml() output.

Verifies both lossless round-tripping and clean-export behaviour.
"""

import tempfile
import warnings
from pathlib import Path

import pytest
import yaml
from supy._env import trv_supy_module
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.type import RefValue
from supy.data_model.validation.pipeline.phase_a import find_extra_parameters

pytestmark = pytest.mark.api


@pytest.fixture
def standard_data():
    """Load the standard sample_config.yml as reference."""
    path = trv_supy_module / "sample_data" / "sample_config.yml"
    with path.open(encoding="utf-8") as f:
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
            encoding="utf-8", suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path, include_internal=False)

        with open(tmp_path, encoding="utf-8") as f:
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
            encoding="utf-8", suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path)

        with open(tmp_path, encoding="utf-8") as f:
            user_data = yaml.safe_load(f)

        # ASSERT
        assert "_yaml_path" not in user_data
        assert "_auto_generate_annotated" not in user_data

        Path(tmp_path).unlink(missing_ok=True)

    def test_clean_export_excludes_internal_fields(self, sample_config):
        """to_yaml(include_internal=False) should exclude internal_only fields."""
        # ARRANGE
        with tempfile.NamedTemporaryFile(
            encoding="utf-8", suffix=".yml", delete=False, mode="w"
        ) as tmp:
            tmp_path = tmp.name

        # ACT
        sample_config.to_yaml(tmp_path, include_internal=False)

        with open(tmp_path, encoding="utf-8") as f:
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
            encoding="utf-8", suffix=".yml", delete=False, mode="w"
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

    def test_refvalue_none_co2_fields_round_trip(self, sample_config, tmp_path):
        """Unused CO2 RefValue fields should not serialise as empty mappings."""
        # ARRANGE
        co2 = sample_config.sites[0].properties.anthropogenic_emissions.co2
        fields = [
            "co2pointsource",
            "ef_umolco2perj",
            "enef_v_jkm",
            "trafficunits",
        ]
        for field in fields:
            setattr(co2, field, RefValue(None))

        path_out = tmp_path / "roundtrip.yml"

        # ACT
        sample_config.to_yaml(path_out)
        saved = yaml.safe_load(path_out.read_text(encoding="utf-8"))
        reloaded = SUEWSConfig.from_yaml(path_out)

        # ASSERT
        saved_co2 = saved["sites"][0]["properties"]["anthropogenic_emissions"]["co2"]
        reloaded_co2 = reloaded.sites[0].properties.anthropogenic_emissions.co2
        for field in fields:
            assert saved_co2[field] == {"value": None}
            assert getattr(reloaded_co2, field).value is None

    def test_refvalue_none_land_cover_co2_fields_round_trip(
        self, sample_config, tmp_path
    ):
        """CO2-related vegetated land-cover fields should share the same contract."""
        # ARRANGE
        land_cover = sample_config.sites[0].properties.land_cover
        surface_types = ["evetr", "dectr", "grass"]
        for surface_type in surface_types:
            surface = getattr(land_cover, surface_type)
            surface.alpha_enh_bioco2 = RefValue(None)

        path_out = tmp_path / "land-cover-roundtrip.yml"

        # ACT
        sample_config.to_yaml(path_out)
        saved = yaml.safe_load(path_out.read_text(encoding="utf-8"))
        reloaded = SUEWSConfig.from_yaml(path_out)

        # ASSERT
        saved_land_cover = saved["sites"][0]["properties"]["land_cover"]
        reloaded_land_cover = reloaded.sites[0].properties.land_cover
        for surface_type in surface_types:
            assert saved_land_cover[surface_type]["alpha_enh_bioco2"] == {
                "value": None
            }
            assert (
                getattr(reloaded_land_cover, surface_type).alpha_enh_bioco2.value
                is None
            )


@pytest.mark.cfg
class TestRefValueSerialisationNoWarnings:
    """gh#1569: serialising a config must not flood serializer warnings.

    ``FlexibleRefValue(T)`` is ``Union[RefValue[T], T]``. Pydantic's union
    serializer only routes a value cleanly to the ``RefValue[T]`` branch when the
    instance carries the matching generic parameter. A bare, unparametrised
    ``RefValue`` (from ``RefValue(value)``) matches neither branch and triggers a
    ``PydanticSerializationUnexpectedValue`` warning per field on dump -- hundreds
    for a config rebuilt from ``df_state``. ``RefValue.__new__`` now
    auto-parametrises bare construction (``RefValue(x)`` -> ``RefValue[type(x)]``)
    so the warnings never arise; nothing is suppressed.
    """

    @staticmethod
    def _serialization_warnings(func):
        """Return the serializer warnings emitted by calling ``func``."""
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            func()
        return [
            w
            for w in caught
            if "PydanticSerializationUnexpectedValue" in str(w.message)
            or "serialized value may not be as expected" in str(w.message)
        ]

    def test_bare_construction_is_parametrised(self):
        """``RefValue(x)`` yields ``RefValue[type(x)]`` so unions serialise cleanly."""
        assert type(RefValue(0.4)).__name__ == "RefValue[float]"
        assert type(RefValue(3)).__name__ == "RefValue[int]"
        # value is preserved unchanged
        assert RefValue(0.4).value == pytest.approx(0.4)
        assert RefValue(3).value == 3

    def test_none_construction_stays_bare(self):
        """``RefValue(None)`` has no inferable type, so it stays the bare generic."""
        rv = RefValue(None)
        assert type(rv).__name__ == "RefValue"
        assert rv.value is None

    def test_parametrised_construction_unchanged(self):
        """Explicit ``RefValue[T](x)`` is left exactly as-is."""
        rv = RefValue[float](1.0)
        assert type(rv).__name__ == "RefValue[float]"
        assert rv.value == pytest.approx(1.0)

    @pytest.fixture
    def df_state_config(self, sample_config):
        """A config reconstructed from a ``df_state`` round-trip.

        ``from_df_state`` rebuilds ``RefValue`` fields via bare ``RefValue(value)``
        constructions -- the path that triggered the warning flood before the
        auto-parametrisation fix.
        """
        return SUEWSConfig.from_df_state(sample_config.to_df_state())

    def test_model_dump_emits_no_serialization_warnings(self, df_state_config):
        warns = self._serialization_warnings(
            lambda: df_state_config.model_dump(mode="json")
        )
        assert warns == [], (
            f"model_dump() emitted {len(warns)} serializer warning(s); "
            "expected none (gh#1569)"
        )

    def test_model_dump_json_emits_no_serialization_warnings(self, df_state_config):
        warns = self._serialization_warnings(df_state_config.model_dump_json)
        assert warns == [], (
            f"model_dump_json() emitted {len(warns)} serializer warning(s); "
            "expected none (gh#1569)"
        )

    def test_to_yaml_emits_no_serialization_warnings(self, df_state_config, tmp_path):
        out_path = tmp_path / "gh1569.yml"
        warns = self._serialization_warnings(
            lambda: df_state_config.to_yaml(str(out_path))
        )
        assert warns == [], (
            f"to_yaml() emitted {len(warns)} serializer warning(s); "
            "expected none (gh#1569)"
        )

    def test_df_state_roundtrip_preserves_values(self, df_state_config, tmp_path):
        """The clean dump must still round-trip the data losslessly."""
        # ARRANGE
        out_path = tmp_path / "gh1569-roundtrip.yml"
        before = df_state_config.sites[0].properties.land_cover.paved.sfr.value

        # ACT
        df_state_config.to_yaml(str(out_path))
        reloaded = SUEWSConfig.from_yaml(str(out_path))

        # ASSERT
        after = reloaded.sites[0].properties.land_cover.paved.sfr.value
        assert after == pytest.approx(before)
