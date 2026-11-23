"""Regression tests for bare-scalar vs RefValue handling in validation/pipeline (gh#2297)."""

from __future__ import annotations

from types import MappingProxyType

import pytest

from supy.data_model.validation.core.yaml_helpers import (
    get_ref_or_bare,
    set_ref_or_bare,
)
from supy.data_model.validation.pipeline.orchestrator import detect_pydantic_defaults
from supy.data_model.validation.pipeline.phase_b import (
    _check_surface_parameters as phase_b_check_surface_parameters,
    adjust_land_cover_fractions,
    adjust_model_option_rcmethod,
    adjust_model_option_setpointmethod,
    adjust_model_option_stebbsmethod,
    adjust_seasonal_parameters,
    adjust_surface_temperatures,
    update_temperature_parameters,
)
from supy.data_model.validation.pipeline.phase_b_rules.other_rules import (
    _check_surface_parameters as rules_check_surface_parameters,
    validate_land_cover_consistency,
)
from supy.data_model.validation.pipeline.phase_b_rules.rules_core import (
    ValidationContext,
)
from supy.data_model.validation.pipeline.phase_b_rules.stebbs_rules import (
    check_archetype_radiation_properties,
)

pytestmark = [pytest.mark.api, pytest.mark.contract("C-VALID")]


def test_get_and_set_ref_or_bare_helpers():
    """Shared helpers read and write both bare values and RefValue mappings cleanly."""
    assert get_ref_or_bare({"sfr": 0.4}, "sfr") == 0.4
    assert (
        get_ref_or_bare({"sfr": {"value": 0.4, "ref": {"desc": "GIS"}}}, "sfr") == 0.4
    )
    assert get_ref_or_bare({"sfr": {}}, "sfr", 0.0) == 0.0
    assert get_ref_or_bare({"sfr": {"ref": {"desc": "only"}}}, "sfr", 0.0) == 0.0
    assert get_ref_or_bare({"sfr": None}, "sfr", 0.0) is None
    assert get_ref_or_bare({"sfr": {"value": None}}, "sfr", 0.0) is None
    assert get_ref_or_bare({}, "sfr", 0.0) == 0.0
    assert get_ref_or_bare(None, "sfr", 0.0) == 0.0

    container = {
        "bare": 0.4,
        "refval": {"value": 0.4, "ref": {"desc": "GIS"}},
        "ro": MappingProxyType({"value": 0.4, "ref": {"desc": "readonly"}}),
    }
    set_ref_or_bare(container, "bare", 0.45)
    assert container["bare"] == 0.45
    assert isinstance(container["bare"], float)

    set_ref_or_bare(container, "refval", 0.45)
    assert container["refval"] == {"value": 0.45, "ref": {"desc": "GIS"}}

    set_ref_or_bare(container, "ro", 0.45)
    assert container["ro"] == {"value": 0.45, "ref": {"desc": "readonly"}}

    set_ref_or_bare(container, "missing", 0.45)
    assert container["missing"] == {"value": 0.45}


@pytest.mark.parametrize(
    ("paved_sfr", "bldgs_sfr", "expected_paved"),
    [
        (0.59995, 0.4, 0.6),
        (0.60005, 0.4, 0.6),
    ],
    ids=["sum_below_1", "sum_above_1"],
)
def test_adjust_land_cover_fractions_bare_and_ref_preserved(
    paved_sfr, bldgs_sfr, expected_paved
):
    """adjust_land_cover_fractions handles bare floats and preserves RefValue ref metadata."""
    bare_yaml = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": paved_sfr},
                        "bldgs": {"sfr": bldgs_sfr},
                    }
                },
            }
        ]
    }
    updated_bare, adj_bare = adjust_land_cover_fractions(bare_yaml)
    paved_out = updated_bare["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]
    assert isinstance(paved_out, float)
    assert paved_out == pytest.approx(expected_paved)
    assert len(adj_bare) == 1
    assert adj_bare[0].parameter == "paved.sfr"

    ref_meta = {"desc": "survey", "ID": "LC-1"}
    ref_yaml = {
        "sites": [
            {
                "gridiv": {"value": 1},
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": {"value": paved_sfr, "ref": dict(ref_meta)}},
                        "bldgs": {"sfr": bldgs_sfr},
                    }
                },
            }
        ]
    }
    updated_ref, adj_ref = adjust_land_cover_fractions(ref_yaml)
    paved_ref_out = updated_ref["sites"][0]["properties"]["land_cover"]["paved"]["sfr"]
    assert isinstance(paved_ref_out, dict)
    assert paved_ref_out["value"] == pytest.approx(expected_paved)
    assert paved_ref_out["ref"] == ref_meta
    assert len(adj_ref) == 1


def test_validate_land_cover_consistency_bare_and_ref():
    """validate_land_cover_consistency supports bare float and RefValue sfr entries."""
    ok_ctx = ValidationContext(
        yaml_data={
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.6},
                            "bldgs": {"sfr": {"value": 0.4, "ref": {"desc": "GIS"}}},
                        }
                    },
                }
            ]
        }
    )
    ok_results = validate_land_cover_consistency(ok_ctx)
    assert any(r.status == "PASS" for r in ok_results)
    assert not any(r.status == "ERROR" for r in ok_results)

    bad_ctx = ValidationContext(
        yaml_data={
            "sites": [
                {
                    "gridiv": 1,
                    "properties": {
                        "land_cover": {
                            "paved": {"sfr": 0.5},
                            "bldgs": {"sfr": 0.3},
                        }
                    },
                }
            ]
        }
    )
    bad_results = validate_land_cover_consistency(bad_ctx)
    assert any(
        r.status == "ERROR" and r.parameter == "paved.sfr" and "0.8000" in r.message
        for r in bad_results
    )


@pytest.mark.parametrize(
    "checker",
    [phase_b_check_surface_parameters, rules_check_surface_parameters],
    ids=["phase_b", "other_rules"],
)
def test_check_surface_parameters_handles_bare_and_ref_nulls(checker):
    """Both _check_surface_parameters implementations catch bare and RefValue nulls."""
    props = {
        "sfr": 0.5,
        "alb": None,
        "emis": {"value": None},
        "thermal_layers": {"dz": [0.2, None], "k": {"value": [1.0, ""]}},
        "valid_bare": 0.15,
        "valid_ref": {"value": 0.95},
    }
    missing = set(checker(props, "paved"))
    assert missing == {"alb", "emis", "thermal_layers.dz", "thermal_layers.k"}


def test_temperature_and_stebbs_updates_bare_and_ref(monkeypatch):
    """update_temperature_parameters and adjust_surface_temperatures support bare and RefValue."""
    bare_elem = {"temperature": [5.0] * 5, "tsfc": 5.0, "tin": 5.0}
    updated_elem, params = update_temperature_parameters(bare_elem, 12.5)
    assert params == ["temperature", "tsfc", "tin"]
    assert updated_elem == {"temperature": [12.5] * 5, "tsfc": 12.5, "tin": 12.5}

    ref_elem = {
        "temperature": {"value": [5.0] * 5, "ref": {"desc": "t"}},
        "tsfc": {"value": 5.0, "ref": {"desc": "s"}},
        "tin": {"value": 5.0, "ref": {"desc": "i"}},
    }
    updated_ref, ref_params = update_temperature_parameters(ref_elem, 12.5)
    assert ref_params == ["temperature", "tsfc", "tin"]
    assert updated_ref["temperature"] == {"value": [12.5] * 5, "ref": {"desc": "t"}}
    assert updated_ref["tsfc"] == {"value": 12.5, "ref": {"desc": "s"}}
    assert updated_ref["tin"] == {"value": 12.5, "ref": {"desc": "i"}}

    monkeypatch.setattr(
        "supy.data_model.validation.pipeline.phase_b.get_mean_monthly_air_temperature",
        lambda lat, lng, month: 14.0,
    )
    monkeypatch.setattr(
        "supy.data_model.validation.pipeline.phase_b.get_monthly_air_temperature_diffmax",
        lambda lat, lng: 8.5,
    )
    monkeypatch.setattr(
        "supy.data_model.validation.pipeline.phase_b.get_mean_annual_air_temperature",
        lambda lat, lng: 11.2,
    )

    yaml_data = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "lat": 51.5,
                    "lng": -0.1,
                    "stebbs": {
                        "temperature_air_outdoor_initial": 5.0,
                        "temperature_air_indoor_initial": {
                            "value": 5.0,
                            "ref": {"desc": "indoor"},
                        },
                        "temperature_air_month_mean_diffmax": 2.0,
                        "temperature_air_annual_mean": 4.0,
                    },
                },
                "initial_states": {
                    "paved": {"temperature": [5.0] * 5, "tsfc": 5.0, "tin": 5.0},
                },
            }
        ]
    }
    updated, adjustments = adjust_surface_temperatures(yaml_data, "2020-07-01")
    stebbs_out = updated["sites"][0]["properties"]["stebbs"]
    assert stebbs_out["temperature_air_outdoor_initial"] == 14.0
    assert stebbs_out["temperature_air_indoor_initial"] == {
        "value": 14.0,
        "ref": {"desc": "indoor"},
    }
    assert stebbs_out["temperature_air_month_mean_diffmax"] == 8.5
    assert stebbs_out["temperature_air_annual_mean"] == 11.2
    assert updated["sites"][0]["initial_states"]["paved"] == {
        "temperature": [14.0] * 5,
        "tsfc": 14.0,
        "tin": 14.0,
    }
    assert len(adjustments) == 5


def test_adjust_seasonal_parameters_bare_and_ref_preserved(monkeypatch):
    """adjust_seasonal_parameters handles bare sfr, lai_id, alb_id, DLS, and timezone."""

    class _FakeDLSCheck:
        def __init__(self, lat, lng, year):
            pass

        def compute_dst_transitions(self):
            return 88, 300, 0

    monkeypatch.setattr(
        "supy.data_model.validation.pipeline.phase_b.DLSCheck", _FakeDLSCheck
    )

    bare_yaml = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "lat": 51.5,
                    "lng": -0.1,
                    "timezone": 2,
                    "anthropogenic_emissions": {"startdls": 80, "enddls": 290},
                    "land_cover": {
                        "dectr": {
                            "sfr": 0.25,
                            "lai": {"lai_min": 1.0, "lai_max": 5.0},
                            "alb_min": 0.12,
                            "alb_max": 0.18,
                        },
                        "grass": {
                            "sfr": 0.20,
                            "alb_min": 0.18,
                            "alb_max": 0.24,
                        },
                        "evetr": {
                            "sfr": None,
                            "alb_min": 0.10,
                            "alb_max": 0.14,
                        },
                    },
                },
                "initial_states": {
                    "dectr": {"lai_id": 2.0, "alb_id": 0.12},
                    "grass": {"alb_id": 0.24},
                },
            }
        ]
    }
    updated_bare, _ = adjust_seasonal_parameters(bare_yaml, "2020-07-01", 2020)
    props_b = updated_bare["sites"][0]["properties"]
    init_b = updated_bare["sites"][0]["initial_states"]
    assert props_b["timezone"] == 0
    assert isinstance(props_b["timezone"], int)
    assert props_b["anthropogenic_emissions"]["startdls"] == 88
    assert props_b["anthropogenic_emissions"]["enddls"] == 300
    assert init_b["dectr"]["lai_id"] == 5.0
    assert init_b["dectr"]["alb_id"] == 0.18
    assert init_b["grass"]["alb_id"] == 0.18

    ref_yaml = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.1},
                    "timezone": {"value": 2, "ref": {"desc": "tz"}},
                    "anthropogenic_emissions": {
                        "startdls": {"value": 80, "ref": {"desc": "s"}},
                        "enddls": {"value": 290, "ref": {"desc": "e"}},
                    },
                    "land_cover": {
                        "dectr": {
                            "sfr": {"value": 0.25, "ref": {"desc": "sfr"}},
                            "lai": {"lai_min": 1.0, "lai_max": 5.0},
                            "alb_min": 0.12,
                            "alb_max": 0.18,
                        }
                    },
                },
                "initial_states": {
                    "dectr": {
                        "lai_id": {"value": 2.0, "ref": {"desc": "lai"}},
                        "alb_id": {"value": 0.12, "ref": {"desc": "alb"}},
                    }
                },
            }
        ]
    }
    updated_ref, _ = adjust_seasonal_parameters(ref_yaml, "2020-07-01", 2020)
    props_r = updated_ref["sites"][0]["properties"]
    init_r = updated_ref["sites"][0]["initial_states"]
    assert props_r["timezone"] == {"value": 0, "ref": {"desc": "tz"}}
    assert props_r["anthropogenic_emissions"]["startdls"] == {
        "value": 88,
        "ref": {"desc": "s"},
    }
    assert props_r["anthropogenic_emissions"]["enddls"] == {
        "value": 300,
        "ref": {"desc": "e"},
    }
    assert init_r["dectr"]["lai_id"] == {"value": 5.0, "ref": {"desc": "lai"}}
    assert init_r["dectr"]["alb_id"] == {"value": 0.18, "ref": {"desc": "alb"}}


def test_stebbs_and_building_archetype_option_adjustments_bare_and_ref():
    """rcmethod, setpointmethod, and stebbsmethod adjust bare scalars and preserve RefValue ref."""
    rc_yaml = {
        "model": {"physics": {"stebbs": {"capacitance": 0}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "building_archetype": {
                        "fraction_heat_capacity_roof_external": 0.3,
                        "fraction_heat_capacity_wall_external": {
                            "value": 0.7,
                            "ref": {"desc": "wall"},
                        },
                    }
                },
            }
        ],
    }
    rc_out, rc_adj = adjust_model_option_rcmethod(rc_yaml)
    ba_rc = rc_out["sites"][0]["properties"]["building_archetype"]
    assert ba_rc["fraction_heat_capacity_roof_external"] == 0.5
    assert ba_rc["fraction_heat_capacity_wall_external"] == {
        "value": 0.5,
        "ref": {"desc": "wall"},
    }
    assert len(rc_adj) == 2

    sp_yaml = {
        "model": {"physics": {"stebbs": {"setpoint": 2}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "building_archetype": {
                        "setpoint_temperature_heating_air": 20.0,
                        "setpoint_temperature_cooling_air": {
                            "value": 25.0,
                            "ref": {"desc": "cool"},
                        },
                    }
                },
            }
        ],
    }
    sp_out, sp_adj = adjust_model_option_setpointmethod(sp_yaml)
    ba_sp = sp_out["sites"][0]["properties"]["building_archetype"]
    assert ba_sp["setpoint_temperature_heating_air"] is None
    assert ba_sp["setpoint_temperature_cooling_air"] == {
        "value": None,
        "ref": {"desc": "cool"},
    }
    assert len(sp_adj) == 2

    wwr0_yaml = {
        "model": {"physics": {"stebbs": {"enabled": True}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "stebbs": {
                        "convection_coefficient_window_internal": 3.0,
                        "convection_coefficient_window_external": {
                            "value": 10.0,
                            "ref": {"desc": "win"},
                        },
                    },
                    "building_archetype": {
                        "ratio_window_to_wall": 0.0,
                        "thickness_window": 0.02,
                        "emissivity_window_external": {
                            "value": 0.85,
                            "ref": {"desc": "ew"},
                        },
                    },
                },
            }
        ],
    }
    wwr0_out, wwr0_adj = adjust_model_option_stebbsmethod(wwr0_yaml)
    st0 = wwr0_out["sites"][0]["properties"]["stebbs"]
    ba0 = wwr0_out["sites"][0]["properties"]["building_archetype"]
    assert st0["convection_coefficient_window_internal"] is None
    assert st0["convection_coefficient_window_external"] == {
        "value": None,
        "ref": {"desc": "win"},
    }
    assert ba0["thickness_window"] is None
    assert ba0["emissivity_window_external"] == {"value": None, "ref": {"desc": "ew"}}
    assert len(wwr0_adj) == 4

    wwr1_yaml = {
        "model": {"physics": {"stebbs": {"enabled": True}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "stebbs": {
                        "convection_coefficient_wall_external": 12.0,
                        "convection_coefficient_wall_internal": {
                            "value": 4.0,
                            "ref": {"desc": "wall"},
                        },
                    },
                    "building_archetype": {
                        "ratio_window_to_wall": 1.0,
                        "thickness_wall": 0.3,
                        "emissivity_wall_external": {
                            "value": 0.9,
                            "ref": {"desc": "ewall"},
                        },
                    },
                },
            }
        ],
    }
    wwr1_out, wwr1_adj = adjust_model_option_stebbsmethod(wwr1_yaml)
    st1 = wwr1_out["sites"][0]["properties"]["stebbs"]
    ba1 = wwr1_out["sites"][0]["properties"]["building_archetype"]
    assert st1["convection_coefficient_wall_external"] is None
    assert st1["convection_coefficient_wall_internal"] == {
        "value": None,
        "ref": {"desc": "wall"},
    }
    assert ba1["thickness_wall"] is None
    assert ba1["emissivity_wall_external"] == {"value": None, "ref": {"desc": "ewall"}}
    assert len(wwr1_adj) == 4


def test_orchestrator_detect_pydantic_defaults_accepts_bare_scalars():
    """Bare user scalars must not be falsely flagged as 'found null' when Pydantic wraps them."""
    original = {"sfr": 0.4, "alb": None}
    processed = {"sfr": {"value": 0.4}, "alb": {"value": 0.15}}
    critical, defaults = detect_pydantic_defaults(original, processed)
    assert critical == []
    assert [d["field_name"] for d in defaults] == ["alb"]
    assert defaults[0]["status"] == "found null"


def test_stebbs_radiation_sum_handles_null_refvalue():
    """RefValue dicts with None payloads return None cleanly instead of raising TypeError."""
    result = check_archetype_radiation_properties(
        {
            "wall_reflectivity": {"value": None},
            "wall_absorptivity": {"value": 0.5},
            "wall_transmissivity": {"value": 0.5},
        },
        "wall",
    )
    assert result is None
