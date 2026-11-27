"""Contracts for deprecated ``strict=False`` loader calls and AnthropogenicHeat defaults.

Covers the remaining #2111 data-model contracts:
1. ``SUEWSConfig.from_yaml(..., strict=False)`` and ``SUEWSConfig.from_dict(..., strict=False)``
   emit a caller-attributed ``DeprecationWarning`` stating that ``strict=False`` is
   deprecated and ignored while strict Pydantic validation continues to apply; omitting
   ``strict`` or passing ``strict=True`` emits no deprecation warning.
2. ``AnthropogenicHeat`` compatibility placeholder defaults (``popdensnighttime=10.0``
   and ``DayProfile(working_day=1.0, holiday=0.0)``) remain non-breaking for existing
   configurations and fixtures, are documented explicitly in schema metadata and
   ``docs/source/inputs/tables/schema.json``, and emit a caller-attributed
   ``UserWarning`` when omitted under active modelled emissions
   (``model.physics.emissions != 0``).
"""

from __future__ import annotations

import copy
from importlib.resources import files
import json
from pathlib import Path
import warnings

import pytest
import yaml

from supy.data_model.core import AnthropogenicHeat, DayProfile, SUEWSConfig

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-API", "C-VALID", "C-SCHEMA"),
]

PROJECT_ROOT = Path(__file__).resolve().parents[2]
COMMITTED_SCHEMA_PATH = (
    PROJECT_ROOT / "docs" / "source" / "inputs" / "tables" / "schema.json"
)


@pytest.fixture(scope="module")
def sample_dict() -> dict:
    """Return a clean parsed copy of the packaged ``sample_config.yml``."""
    text = (
        files("supy")
        .joinpath("sample_data/sample_config.yml")
        .read_text(encoding="utf-8")
    )
    return yaml.safe_load(text)


def _write_yaml(data: dict, tmp_path: Path, name: str = "config.yml") -> Path:
    """Write *data* to a temporary YAML file and return its path."""
    path = tmp_path / name
    path.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    return path


def _heat_block(data: dict, site_idx: int = 0) -> dict:
    """Return the mutable raw ``anthropogenic_emissions.heat`` mapping."""
    return data["sites"][site_idx]["properties"]["anthropogenic_emissions"]["heat"]


# =============================================================================
# F-06: strict=False deprecation contract on from_yaml / from_dict
# =============================================================================


def test_from_yaml_strict_false_warns_once_and_attributes_to_caller(
    sample_dict: dict, tmp_path: Path
) -> None:
    """``SUEWSConfig.from_yaml(..., strict=False)`` emits one caller-level DeprecationWarning."""
    # ARRANGE
    yaml_path = _write_yaml(sample_dict, tmp_path)

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_yaml(str(yaml_path), strict=False)

    # ASSERT
    assert isinstance(cfg, SUEWSConfig)
    dep_warnings = [w for w in caught if issubclass(w.category, DeprecationWarning)]
    assert len(dep_warnings) == 1
    msg = str(dep_warnings[0].message)
    assert "SUEWSConfig.from_yaml" in msg
    assert "strict=False" in msg
    assert "deprecated" in msg
    assert "ignored" in msg
    assert "use_conditional_validation=False" in msg
    assert Path(dep_warnings[0].filename).resolve() == Path(__file__).resolve()


def test_from_dict_strict_false_warns_once_and_attributes_to_caller(
    sample_dict: dict,
) -> None:
    """``SUEWSConfig.from_dict(..., strict=False)`` emits one caller-level DeprecationWarning."""
    # ARRANGE / ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_dict(copy.deepcopy(sample_dict), strict=False)

    # ASSERT
    assert isinstance(cfg, SUEWSConfig)
    dep_warnings = [w for w in caught if issubclass(w.category, DeprecationWarning)]
    assert len(dep_warnings) == 1
    msg = str(dep_warnings[0].message)
    assert "SUEWSConfig.from_dict" in msg
    assert "strict=False" in msg
    assert "deprecated" in msg
    assert "ignored" in msg
    assert Path(dep_warnings[0].filename).resolve() == Path(__file__).resolve()


@pytest.mark.parametrize("via_yaml", [False, True])
def test_strict_false_still_enforces_validation_on_invalid_input(
    sample_dict: dict, tmp_path: Path, via_yaml: bool
) -> None:
    """``strict=False`` warns AND still raises on invalid configuration input."""
    # ARRANGE: introduce an unknown top-level key rejected by strict validation
    bad = copy.deepcopy(sample_dict)
    bad["unexpected_top_level_key"] = 123

    # ACT / ASSERT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(ValueError, match="unexpected_top_level_key"):
            if via_yaml:
                yaml_path = _write_yaml(bad, tmp_path, "invalid.yml")
                SUEWSConfig.from_yaml(str(yaml_path), strict=False)
            else:
                SUEWSConfig.from_dict(bad, strict=False)

    dep_warnings = [w for w in caught if issubclass(w.category, DeprecationWarning)]
    assert len(dep_warnings) == 1
    assert "strict=False" in str(dep_warnings[0].message)
    assert Path(dep_warnings[0].filename).resolve() == Path(__file__).resolve()


@pytest.mark.parametrize("via_yaml", [False, True])
def test_strict_false_warns_even_when_conditional_validation_disabled(
    sample_dict: dict, tmp_path: Path, via_yaml: bool
) -> None:
    """``strict=False`` still emits DeprecationWarning when ``use_conditional_validation=False``."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        if via_yaml:
            yaml_path = _write_yaml(data, tmp_path, "unchecked.yml")
            cfg = SUEWSConfig.from_yaml(
                str(yaml_path), use_conditional_validation=False, strict=False
            )
        else:
            cfg = SUEWSConfig.from_dict(
                data, use_conditional_validation=False, strict=False
            )

    # ASSERT
    assert isinstance(cfg, SUEWSConfig)
    dep_warnings = [w for w in caught if issubclass(w.category, DeprecationWarning)]
    assert len(dep_warnings) == 1
    assert "strict=False" in str(dep_warnings[0].message)
    assert Path(dep_warnings[0].filename).resolve() == Path(__file__).resolve()


@pytest.mark.parametrize("strict_kwargs", [{}, {"strict": True}])
@pytest.mark.parametrize("via_yaml", [False, True])
def test_strict_true_or_omitted_emits_no_warnings(
    sample_dict: dict, tmp_path: Path, via_yaml: bool, strict_kwargs: dict
) -> None:
    """Default or ``strict=True`` loading on a complete config emits zero warnings."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        if via_yaml:
            yaml_path = _write_yaml(data, tmp_path, "clean.yml")
            cfg = SUEWSConfig.from_yaml(str(yaml_path), **strict_kwargs)
        else:
            cfg = SUEWSConfig.from_dict(data, **strict_kwargs)

    # ASSERT
    assert isinstance(cfg, SUEWSConfig)
    assert caught == []


# =============================================================================
# AnthropogenicHeat placeholder defaults & DayProfile contract
# =============================================================================


def test_programmatic_defaults_preserved_without_warnings() -> None:
    """Compatibility default values remain unchanged and silent on programmatic construction."""
    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        profile = DayProfile()
        heat = AnthropogenicHeat()

    # ASSERT
    assert caught == []
    assert profile.working_day == pytest.approx(1.0)
    assert profile.holiday == pytest.approx(0.0)
    assert heat.popdensnighttime == pytest.approx(10.0)
    assert heat.popdensdaytime.working_day == pytest.approx(1.0)
    assert heat.popdensdaytime.holiday == pytest.approx(0.0)


def test_observed_emissions_method_omitted_heat_emits_no_warning(
    sample_dict: dict,
) -> None:
    """When ``model.physics.emissions == 0`` (OBSERVED), omitting heat fields emits zero warnings."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["emissions"] = {"value": 0}
    del data["sites"][0]["properties"]["anthropogenic_emissions"]["heat"]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_dict(data)

    # ASSERT
    assert caught == []
    heat = cfg.sites[0].properties.anthropogenic_emissions.heat
    assert heat.popdensnighttime == pytest.approx(10.0)
    assert heat.popdensdaytime.working_day == pytest.approx(1.0)
    assert heat.popdensdaytime.holiday == pytest.approx(0.0)


@pytest.mark.parametrize("via_yaml", [False, True])
def test_active_j11_omitted_popdensnighttime_warns_and_keeps_default(
    sample_dict: dict, tmp_path: Path, via_yaml: bool
) -> None:
    """Omitting ``popdensnighttime`` under active J11 warns clearly while preserving default 10.0."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    del _heat_block(data)["popdensnighttime"]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        if via_yaml:
            yaml_path = _write_yaml(data, tmp_path, "missing_popdens.yml")
            cfg = SUEWSConfig.from_yaml(str(yaml_path))
        else:
            cfg = SUEWSConfig.from_dict(data)

    # ASSERT
    user_warnings = [w for w in caught if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    msg = str(user_warnings[0].message)
    assert "popdensnighttime (default 10.0 ha-1)" in msg
    assert "sites[0]" in msg
    assert Path(user_warnings[0].filename).resolve() == Path(__file__).resolve()
    assert cfg.sites[
        0
    ].properties.anthropogenic_emissions.heat.popdensnighttime == pytest.approx(10.0)


def test_active_j11_omitted_dayprofile_fields_warn_and_keep_defaults(
    sample_dict: dict,
) -> None:
    """Omitting active J11 ``DayProfile`` fields warns and preserves ``(1.0, 0.0)`` defaults."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    del _heat_block(data)["popdensdaytime"]
    del _heat_block(data)["qf_a"]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_dict(data)

    # ASSERT
    user_warnings = [w for w in caught if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    msg = str(user_warnings[0].message)
    assert "popdensdaytime (DayProfile default working_day=1.0, holiday=0.0)" in msg
    assert "qf_a (DayProfile default working_day=1.0, holiday=0.0)" in msg
    heat = cfg.sites[0].properties.anthropogenic_emissions.heat
    assert heat.popdensdaytime.working_day == pytest.approx(1.0)
    assert heat.popdensdaytime.holiday == pytest.approx(0.0)
    assert heat.qf_a.working_day == pytest.approx(1.0)
    assert heat.qf_a.holiday == pytest.approx(0.0)


def test_active_j11_partial_dayprofile_missing_holiday_warns(
    sample_dict: dict,
) -> None:
    """Omitting ``holiday`` inside an active DayProfile warns about the asymmetric 0.0 fallback."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    _heat_block(data)["baset_heating"] = {"working_day": 18.2}

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        cfg = SUEWSConfig.from_dict(data)

    # ASSERT
    user_warnings = [w for w in caught if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    msg = str(user_warnings[0].message)
    assert "baset_heating.holiday (DayProfile default 0.0)" in msg
    assert "baset_heating.working_day" not in msg
    assert cfg.sites[
        0
    ].properties.anthropogenic_emissions.heat.baset_heating.holiday == pytest.approx(
        0.0
    )


def test_active_l11_warns_only_for_l11_active_dayprofiles(
    sample_dict: dict,
) -> None:
    """L11 (method 1) ignores omitted J11-only fields but warns on omitted L11 fields."""
    # ARRANGE: omit J11-only fields (qf_a, qf_b, qf_c, baset_cooling) under L11 -> zero warnings
    data_inactive_omitted = copy.deepcopy(sample_dict)
    data_inactive_omitted["model"]["physics"]["emissions"] = {"value": 1}
    for field in ("qf_a", "qf_b", "qf_c", "baset_cooling", "ah_slope_cooling"):
        _heat_block(data_inactive_omitted).pop(field, None)

    # ACT
    with warnings.catch_warnings(record=True) as caught_clean:
        warnings.simplefilter("always")
        SUEWSConfig.from_dict(data_inactive_omitted)

    # ASSERT
    assert caught_clean == []

    # ARRANGE: now omit L11-active ah_min -> warns specifically about ah_min
    data_l11_omitted = copy.deepcopy(data_inactive_omitted)
    del _heat_block(data_l11_omitted)["ah_min"]

    # ACT
    with warnings.catch_warnings(record=True) as caught_l11:
        warnings.simplefilter("always")
        SUEWSConfig.from_dict(data_l11_omitted)

    # ASSERT
    user_warnings = [w for w in caught_l11 if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    assert "ah_min (DayProfile default working_day=1.0, holiday=0.0)" in str(
        user_warnings[0].message
    )


def test_active_l11_updated_warns_on_cooling_slope_and_base_temp(
    sample_dict: dict,
) -> None:
    """L11_UPDATED (method 3) warns when ``ah_slope_cooling`` or ``baset_cooling`` is omitted."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    data["model"]["physics"]["emissions"] = {"value": 3}
    del _heat_block(data)["ah_slope_cooling"]
    del _heat_block(data)["baset_cooling"]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        SUEWSConfig.from_dict(data)

    # ASSERT
    user_warnings = [w for w in caught if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    msg = str(user_warnings[0].message)
    assert "ah_slope_cooling (DayProfile default working_day=1.0, holiday=0.0)" in msg
    assert "baset_cooling (DayProfile default working_day=1.0, holiday=0.0)" in msg


def test_multisite_warning_identifies_only_omitting_site(
    sample_dict: dict,
) -> None:
    """Multi-site configs identify only the site index whose active defaults were omitted."""
    # ARRANGE
    data = copy.deepcopy(sample_dict)
    site0 = copy.deepcopy(data["sites"][0])
    site1 = copy.deepcopy(data["sites"][0])
    site1["name"] = "SparseSite"
    site1["gridiv"] = 2
    del site1["properties"]["anthropogenic_emissions"]["heat"]["popdensnighttime"]
    data["sites"] = [site0, site1]

    # ACT
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        SUEWSConfig.from_dict(data)

    # ASSERT
    user_warnings = [w for w in caught if issubclass(w.category, UserWarning)]
    assert len(user_warnings) == 1
    msg = str(user_warnings[0].message)
    assert "sites[1] (SparseSite)" in msg
    assert "sites[0]" not in msg


def test_schema_metadata_and_committed_schema_document_placeholder_defaults() -> None:
    """Live Pydantic schema and committed ``schema.json`` both document placeholder defaults."""
    # ARRANGE
    live_defs = SUEWSConfig.model_json_schema()["$defs"]
    committed_defs = json.loads(COMMITTED_SCHEMA_PATH.read_text(encoding="utf-8"))[
        "$defs"
    ]

    # ACT / ASSERT: AnthropogenicHeat fields
    for defs in (live_defs, committed_defs):
        heat_props = defs["AnthropogenicHeat"]["properties"]
        pop_night = heat_props["popdensnighttime"]
        assert pop_night["default"] == pytest.approx(10.0)
        assert "placeholder" in pop_night["description"].lower()
        assert "10.0" in pop_night["default_description"]
        assert "204.58" in pop_night["default_description"]
        assert "204.58" in pop_night["range_description"]

        pop_day = heat_props["popdensdaytime"]
        assert "placeholder" in pop_day["description"].lower()
        assert "working_day=1.0, holiday=0.0" in pop_day["default_description"]

        for day_field in (
            "qf0_beu",
            "qf_a",
            "qf_b",
            "qf_c",
            "baset_cooling",
            "baset_heating",
            "ah_min",
            "ah_slope_cooling",
            "ah_slope_heating",
        ):
            assert (
                "working_day=1.0, holiday=0.0"
                in heat_props[day_field]["default_description"]
            ), f"Missing DayProfile placeholder note on {day_field}"

        day_profile_props = defs["DayProfile"]["properties"]
        assert day_profile_props["working_day"]["default"] == pytest.approx(1.0)
        assert day_profile_props["holiday"]["default"] == pytest.approx(0.0)
        assert "placeholder" in day_profile_props["working_day"]["description"].lower()
        assert "placeholder" in day_profile_props["holiday"]["description"].lower()
