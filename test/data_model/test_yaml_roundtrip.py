"""SUEWSConfig YAML I/O round-trips and error handling.

One file for the from_yaml/to_yaml surface:

* ``from_yaml`` error handling (formerly ``test_from_yaml_errors.py``):
  schema-drift hints added in #1303 (TypeError wrapping, extra_forbidden
  hints, detected-vs-current version naming) and the gh#1530 dict-input
  hardening follow-ups (safe YAML loading, loud unknown keys, validated
  assignment).

* ``to_yaml`` round-trips (formerly ``test_to_yaml_roundtrip.py``):
  lossless round-tripping, clean exports, and the gh#1569 guarantee that
  serialising a config emits no Pydantic serializer warnings.
"""

import tempfile
import warnings
from pathlib import Path

import pytest
import yaml

from supy._env import trv_supy_module
from supy.data_model.core.config import SUEWSConfig
from supy.data_model.core.type import RefValue
from supy.data_model.configuration import CURRENT_SCHEMA_VERSION
from supy.data_model.validation.pipeline.phase_a import find_extra_parameters

pytestmark = pytest.mark.api


# ===========================================================================
# from_yaml error handling -- formerly test_from_yaml_errors.py
# ===========================================================================

@pytest.fixture
def sample_config_dict() -> dict:
    """Load the packaged sample configuration as a mutable dict."""
    path = trv_supy_module / "sample_data" / "sample_config.yml"
    with path.open(encoding="utf-8") as f:
        return yaml.safe_load(f)


def _write_yaml(tmp_path: Path, data: dict) -> Path:
    """Serialise `data` to a YAML file under `tmp_path` and return the path."""
    target = tmp_path / "drifted.yml"
    with target.open("w", encoding="utf-8") as f:
        yaml.safe_dump(data, f, sort_keys=False)
    return target


@pytest.mark.cfg
class TestFromYamlDriftHints:
    """Parse failures surface actionable schema-drift hints."""

    def test_valid_yaml_still_parses(self, sample_config_dict, tmp_path):
        """Positive control: the packaged sample still loads cleanly."""
        # ARRANGE
        path = _write_yaml(tmp_path, sample_config_dict)

        # ACT
        config = SUEWSConfig.from_yaml(str(path))

        # ASSERT
        assert config.name == "sample_config"

    def test_extra_forbidden_gets_drift_hint(self, sample_config_dict, tmp_path):
        """A stray field on a nested `extra=forbid` model triggers the hint.

        `SUEWSConfig` itself uses `extra="allow"`, but nested models
        (e.g. `SiteProperties`) forbid extras, so we trigger the drift smell
        there.
        """
        # ARRANGE
        drifted = dict(sample_config_dict)
        drifted["sites"][0]["properties"]["removed_field_from_old_release"] = 42
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "suews-convert -i <old.yml>" in message
        assert f"Current schema version:  {CURRENT_SCHEMA_VERSION}" in message
        assert "Detected schema version:" in message

    def test_typeerror_path_gets_drift_hint(self, sample_config_dict, tmp_path):
        """A raw TypeError from validation is wrapped with the drift hint."""
        # ARRANGE: force a TypeError through from_yaml. We monkeypatch the
        # config-construction step to mimic what union dispatch does when
        # drifted YAML reaches a model with a kwarg-strict __init__.
        path = _write_yaml(tmp_path, sample_config_dict)

        original_init = SUEWSConfig.__init__

        def raise_type_error(self, **_):
            raise TypeError(
                "RefValue.__init__() got an unexpected keyword argument 'working_day'"
            )

        SUEWSConfig.__init__ = raise_type_error
        try:
            with pytest.raises(ValueError) as excinfo:
                SUEWSConfig.from_yaml(str(path))
        finally:
            SUEWSConfig.__init__ = original_init

        # ASSERT
        message = str(excinfo.value)
        assert "suspected schema drift" in message
        assert "suews-convert -i <old.yml>" in message
        assert "RefValue.__init__()" in message
        assert f"Current schema version:  {CURRENT_SCHEMA_VERSION}" in message

    def test_drift_hint_names_detected_schema_version(
        self, sample_config_dict, tmp_path
    ):
        """When the YAML declares schema_version, it shows up as 'Detected'."""
        # ARRANGE
        drifted = dict(sample_config_dict)
        drifted["schema_version"] = "2025.12"
        drifted["sites"][0]["properties"]["from_an_old_release"] = True
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "Detected schema version: 2025.12" in message

    def test_unsigned_yaml_hint_requests_from_ver(self, sample_config_dict, tmp_path):
        """Unsigned YAMLs must not be told to run the bare suews-convert command.

        The CLI rejects unsigned YAMLs unless `-f/--from` is supplied, so
        the hint must advertise that flag and must not claim a spurious
        "Detected schema version: <current>" that was only there because
        from_yaml stamped CURRENT_SCHEMA_VERSION as a default.
        """
        # ARRANGE - strip the signature to mimic a pre-schema-version release
        drifted = dict(sample_config_dict)
        drifted.pop("schema_version", None)
        drifted["sites"][0]["properties"]["from_an_old_release"] = True
        path = _write_yaml(tmp_path, drifted)

        # ACT / ASSERT
        with pytest.raises(ValueError) as excinfo:
            SUEWSConfig.from_yaml(str(path))

        message = str(excinfo.value)
        assert "No schema_version field in YAML" in message
        assert "-f <release-tag>" in message
        assert f"Detected schema version: {CURRENT_SCHEMA_VERSION}" not in message


class TestDictInputHardening:
    """gh#1530 follow-ups: safe YAML loading and loud unknown-key handling."""

    def test_from_yaml_rejects_python_object_tags(self, sample_config_dict, tmp_path):
        """from_yaml must not construct arbitrary Python objects from tags.

        yaml.FullLoader resolves ``!!python/name:os.system`` to the live
        function; safe loading must reject the tag at parse time instead.
        """
        tagged = dict(sample_config_dict)
        tagged["description"] = "PLACEHOLDER_FOR_TAG"
        path = _write_yaml(tmp_path, tagged)
        text = path.read_text(encoding="utf-8")
        path.write_text(
            text.replace("PLACEHOLDER_FOR_TAG", "!!python/name:os.system"),
            encoding="utf-8",
        )

        with pytest.raises(yaml.YAMLError):
            SUEWSConfig.from_yaml(str(path))

    def test_unknown_top_level_key_raises(self, sample_config_dict):
        """Unknown top-level keys must raise, not be silently retained."""
        drifted = dict(sample_config_dict)
        drifted["stray_key"] = 1

        with pytest.raises(ValueError, match="stray_key"):
            SUEWSConfig.from_dict(drifted)

    def test_internal_bookkeeping_keys_accepted(self, sample_config_dict, tmp_path):
        """The private _yaml_* keys must keep working through from_yaml."""
        path = _write_yaml(tmp_path, sample_config_dict)
        config = SUEWSConfig.from_yaml(str(path))
        assert getattr(config, "_yaml_path", None) == str(path)

    def test_assignment_is_validated(self, sample_config_dict):
        """Direct attribute assignment must be validated, not stored raw."""
        config = SUEWSConfig.from_dict(sample_config_dict)

        with pytest.raises(ValueError):
            config.model.physics.net_radiation = "not_a_method"

        with pytest.raises(ValueError):
            config.sites = "not a list"

        # Valid assignments still coerce and succeed
        config.model.control.tstep = 600
        tstep = config.model.control.tstep
        assert int(tstep.value if hasattr(tstep, "value") else tstep) == 600

    def test_from_dict_runs_completeness_checks_like_yaml(
        self, sample_config_dict, tmp_path
    ):
        """Dict input must hit the same site-completeness checks as YAML.

        gh#1530 review follow-up: the completeness validator was gated on
        ``_yaml_path``, so the in-memory dict path silently skipped checks
        that the same data loaded from a file would fail.
        """
        from copy import deepcopy

        broken = deepcopy(sample_config_dict)
        broken["sites"][0]["properties"]["land_cover"]["grass"].pop("lai")

        path = _write_yaml(tmp_path, broken)
        with pytest.raises(ValueError):
            SUEWSConfig.from_yaml(str(path))

        with pytest.raises(ValueError):
            SUEWSConfig.from_dict(broken)

# ===========================================================================
# to_yaml round-trips -- formerly test_to_yaml_roundtrip.py
# ===========================================================================

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
            "emission_co2_point_source",
            "emission_factor_co2_fuel",
            "emission_factor_energy_vehicle",
            "type_traffic_rate",
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
