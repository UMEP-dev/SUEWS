"""Wave 4 tests: pure scientific patch planners and schema-driven Phase A.

Verifies:
- Phase B scientific suggestion planning performs zero ``deepcopy`` calls and
  never mutates its input mapping.
- Phase B ``apply`` mode executes all planned patches in a single pass while
  preserving bare scalar vs ``RefValue`` representation and ``ref`` metadata.
- Phase A derives missing and extra parameters directly from ``SUEWSConfig``
  without reading ``sample_config.yml``.
- Phase A reports omitted optional fields without injecting speculative
  ``null`` placeholders into ``updated_<name>.yml``, while keeping critical
  missing physics placeholders intact.
- ``_NLAYER_DEPENDENT_ARRAY_RE`` covers bare arrays as well as ``RefValue``
  wrappers without over-matching fixed-length nested sub-arrays.
"""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
from pathlib import Path

import pytest
import yaml

from supy.data_model.core._yaml_io import safe_dump_yaml, safe_load_yaml
from supy.data_model.core.physics_families import flatten_physics_in_config
from supy.data_model.validation.pipeline import phase_a, phase_b
from supy.data_model.validation.pipeline.document import ConfigDocument
from supy.data_model.validation.pipeline.patches import (
    plan_land_cover_fraction_patches,
    plan_model_dependent_nullification_patches,
    plan_rcmethod_patches,
    plan_setpointmethod_patches,
    plan_stebbsmethod_wwr_patches,
)

pytestmark = [
    pytest.mark.api,
    pytest.mark.cfg,
    pytest.mark.contract("C-VALID", "C-ERROR"),
]


def _load_sample_dict() -> dict:
    text = (files("supy") / "sample_data" / "sample_config.yml").read_text(
        encoding="utf-8"
    )
    return safe_load_yaml(text)


def test_suggest_mode_performs_zero_deepcopy_and_does_not_mutate_input(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Planning Phase B suggestions avoids deep-copying and leaves input untouched."""
    data = _load_sample_dict()
    flatten_physics_in_config(data)
    # Trigger land-cover drift (within tolerance) and disabled-STEBBS nullification.
    data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]["value"] = 0.430002
    data["sites"][0]["properties"]["stebbs"]["temperature_air_indoor_initial"] = {
        "value": 21.0,
        "ref": {"DOI": "10.1234/example"},
    }
    snapshot = deepcopy(data)

    deepcopy_calls = 0
    real_deepcopy = deepcopy

    def _forbid_deepcopy(obj, *args, **kwargs):
        nonlocal deepcopy_calls
        deepcopy_calls += 1
        return real_deepcopy(obj, *args, **kwargs)

    monkeypatch.setattr(phase_b, "deepcopy", _forbid_deepcopy)
    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 12.5
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda lat, lng: 14.0
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: 11.2
    )

    suggestions = phase_b.collect_science_suggestions(data, "2012-01-01", 2012)

    assert deepcopy_calls == 0
    assert data == snapshot
    assert len(suggestions) > 0


def test_apply_mode_single_pass_preserves_bare_refvalue_and_ref_metadata(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Applying planned patches preserves bare scalars and RefValue ``ref`` keys."""
    data = _load_sample_dict()
    flatten_physics_in_config(data)
    # Bare sfr on paved with tiny drift; RefValue+ref on bldgs (max sfr).
    lc = data["sites"][0]["properties"]["land_cover"]
    lc["paved"]["sfr"] = 0.430001
    lc["bldgs"]["sfr"] = {"value": 0.38, "ref": {"DOI": "10.1000/bldgs"}}
    # Bare temperature_ground on paved initial_states; RefValue+ref on bldgs.
    init = data["sites"][0]["initial_states"]
    init["paved"]["tsfc"] = 5.0
    init["bldgs"]["tsfc"] = {"value": 5.0, "ref": {"DOI": "10.1000/tsfc"}}

    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 14.2
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda lat, lng: None
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: None
    )

    updated, adjustments = phase_b.apply_science_suggestions(
        deepcopy(data), [], "2012-01-01", 2012
    )

    assert len(adjustments) > 0
    assert updated["sites"][0]["initial_states"]["paved"]["tsfc"] == pytest.approx(14.2)
    assert isinstance(updated["sites"][0]["initial_states"]["paved"]["tsfc"], float)
    assert updated["sites"][0]["initial_states"]["bldgs"]["tsfc"] == {
        "value": 14.2,
        "ref": {"DOI": "10.1000/tsfc"},
    }
    # Paved (max sfr) stayed bare float after drift adjustment.
    assert isinstance(
        updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"], float
    )
    assert updated["sites"][0]["properties"]["land_cover"]["bldgs"]["sfr"] == {
        "value": 0.38,
        "ref": {"DOI": "10.1000/bldgs"},
    }


def test_plan_patches_are_pure_and_apply_via_config_document() -> None:
    """Pure patch planners leave input untouched and apply cleanly via ConfigDocument."""
    cfg = {
        "model": {
            "physics": {
                "stebbs": {
                    "enabled": True,
                    "parameters": "default",
                    "capacitance": 0,
                    "setpoint": 0,
                },
                "emissions": 2,
            }
        },
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": 0.600001},
                        "bldgs": {"sfr": {"value": 0.4, "ref": {"ID": "b"}}},
                    },
                    "building_archetype": {
                        "fraction_heat_capacity_wall_external": {
                            "value": 0.25,
                            "ref": {"ID": "w"},
                        },
                        "fraction_heat_capacity_roof_external": 0.3,
                        "profile_setpoint_temperature_heating_air": {
                            "working_day": {"1": 20.0},
                            "holiday": {"1": 18.0},
                        },
                    },
                },
            }
        ],
    }
    snapshot = deepcopy(cfg)

    plans = [
        plan_land_cover_fraction_patches(cfg),
        plan_rcmethod_patches(cfg),
        plan_setpointmethod_patches(cfg),
        plan_stebbsmethod_wwr_patches(cfg),
        plan_model_dependent_nullification_patches(cfg),
    ]
    assert cfg == snapshot

    all_patches = [p for plan in plans for p in plan.patches]
    assert all_patches

    doc = ConfigDocument.from_dict(cfg, normalise=False).apply_patches(all_patches)
    arch = doc.data["sites"][0]["properties"]["building_archetype"]
    assert arch["fraction_heat_capacity_wall_external"] == {
        "value": 0.5,
        "ref": {"ID": "w"},
    }
    assert arch["fraction_heat_capacity_roof_external"] == 0.5
    assert arch["profile_setpoint_temperature_heating_air"] == {
        "working_day": {"1": None},
        "holiday": {"1": None},
    }
    assert doc.data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] == pytest.approx(
        0.6
    )


def test_schema_driven_phase_a_needs_no_sample_config_and_does_not_pad_optional_fields(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """Phase A uses SUEWSConfig schema, reports omitted optionals, and skips null padding."""
    data = _load_sample_dict()
    del data["model"]["control"]["start_time"]
    del data["sites"][0]["properties"]["conductance"]["g_max"]
    data["sites"][0]["properties"]["bogus_extra_field"] = 99

    user_file = tmp_path / "user.yml"
    updated_file = tmp_path / "updated.yml"
    report_file = tmp_path / "report.txt"
    user_file.write_text(
        safe_dump_yaml(data, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    def _forbid_standard_load(*args, **kwargs):
        raise AssertionError("Phase A must not load sample_config.yml")

    monkeypatch.setattr(phase_a, "_load_normalised_standard_data", _forbid_standard_load)

    report = phase_a.annotate_missing_parameters(
        user_file=str(user_file),
        uptodate_file=str(updated_file),
        report_file=str(report_file),
        forcing="off",
        detect_nlayer=True,
    )

    assert not report.has_errors
    missing_paths = {
        i.yaml_path for i in report.issues if i.code == "A.MISSING_PARAM"
    }
    extra_paths = {i.yaml_path for i in report.issues if i.code == "A.EXTRA_PARAM"}
    assert "model.control.start_time" in missing_paths
    assert "sites[0].properties.conductance.g_max" in missing_paths
    assert "sites[0].properties.bogus_extra_field" in extra_paths

    updated = yaml.safe_load(updated_file.read_text(encoding="utf-8"))
    assert "start_time" not in updated["model"]["control"]
    assert "g_max" not in updated["sites"][0]["properties"]["conductance"]


def test_schema_driven_phase_a_pads_only_critical_missing_physics(
    tmp_path: Path,
) -> None:
    """Critical missing physics still gets null placeholders; optionals do not."""
    data = _load_sample_dict()
    del data["model"]["physics"]["net_radiation"]
    del data["model"]["control"]["start_time"]

    user_file = tmp_path / "crit.yml"
    updated_file = tmp_path / "updated_crit.yml"
    report_file = tmp_path / "report_crit.txt"
    user_file.write_text(
        safe_dump_yaml(data, default_flow_style=False, sort_keys=False),
        encoding="utf-8",
    )

    report = phase_a.annotate_missing_parameters(
        user_file=str(user_file),
        uptodate_file=str(updated_file),
        report_file=str(report_file),
        forcing="off",
        detect_nlayer=True,
    )

    assert report.has_errors
    assert any(
        i.code == "A.MISSING_PARAM.CRITICAL"
        and i.yaml_path == "model.physics.net_radiation"
        for i in report.issues
    )
    updated = yaml.safe_load(updated_file.read_text(encoding="utf-8"))
    assert updated["model"]["physics"]["net_radiation"] == {"value": None}
    assert "start_time" not in updated["model"]["control"]


def test_schema_finders_clean_on_sample_and_bare_nlayer_regex() -> None:
    """Sample config yields zero schema diffs; nlayer regex matches bare and wrapped arrays."""
    sample = _load_sample_dict()
    assert phase_a.find_missing_parameters_from_schema(sample) == []
    assert phase_a.find_extra_parameters_from_schema(sample) == []

    regex = phase_a._NLAYER_DEPENDENT_ARRAY_RE
    assert regex.match("sites[0].properties.vertical_layers.height")
    assert regex.match("sites[0].properties.vertical_layers.height.value")
    assert regex.match("sites[2].properties.vertical_layers.veg_frac")
    assert regex.match("sites[1].properties.vertical_layers.roofs")
    assert regex.match("sites[1].initial_states.walls")
    assert not regex.match(
        "sites[0].properties.vertical_layers.roofs[0].thermal_layers.dz"
    )


def test_get_allowed_nested_sections_uses_schema_graph() -> None:
    """Cached schema graph returns sorted extra-allowing nested sections without sharing mutable state."""
    first = phase_a.get_allowed_nested_sections_in_properties()
    second = phase_a.get_allowed_nested_sections_in_properties()
    assert first == second == sorted(first)
    assert {"stebbs", "irrigation", "snow", "spartacus", "lumps", "building_archetype"} <= set(
        first
    )
    first.append("mutated_caller_copy")
    assert "mutated_caller_copy" not in phase_a.get_allowed_nested_sections_in_properties()
