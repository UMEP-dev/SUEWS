"""Regression tests for bare-scalar vs RefValue handling in validation/pipeline (gh#2297)."""

from __future__ import annotations

from copy import deepcopy
from importlib.resources import files
from types import MappingProxyType

import pytest
import yaml

from supy.data_model.core.field_renames import normalise_yaml_renames
from supy.data_model.validation.core.yaml_helpers import (
    _nullify_biogenic_in_props,
    collect_nullified_paths,
    get_ref_or_bare,
    nullify_co2_block_recursive,
    set_ref_or_bare,
)
from supy.data_model.validation.pipeline import phase_b
from supy.data_model.validation.pipeline.orchestrator import detect_pydantic_defaults
from supy.data_model.validation.pipeline.phase_a import (
    annotate_missing_parameters,
    create_null_roof_wall_template,
    find_extra_parameters,
    find_missing_parameters,
    validate_nlayer_dimensions,
)
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
from supy.data_model.validation.pipeline.phase_b_rules import RulesRegistry
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
        # STEBBS on: with it off the fractions stay nullified (see
        # test_rcmethod_keeps_fractions_nullified_when_stebbs_off).
        "model": {"physics": {"stebbs": {"enabled": True, "capacitance": 0}}},
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
            "reflectivity_wall_external": {"value": None},
            "absorptivity_wall_external": {"value": 0.5},
            "transmissivity_wall_external": {"value": 0.5},
        },
        "wall",
    )
    assert result is None


# ---------------------------------------------------------------------------
# Follow-up: rules on frozen data, remaining RefValue/bare sites,
# canonical archetype names, crash isolation, multi-site Phase A, ordering.
# ---------------------------------------------------------------------------


def _run_rule(rule_id, yaml_data):
    """Run one registered Phase B rule on deep-frozen data, as the pipeline does."""
    context = ValidationContext(yaml_data=yaml_data)
    return RulesRegistry(context=context)[rule_id](context)


def _single_surface_site(surface_props, surface="paved"):
    return {
        "sites": [
            {
                "gridiv": 1,
                "properties": {"land_cover": {surface: surface_props}},
            }
        ]
    }


@pytest.mark.parametrize(
    "dz",
    [[0.2, None], {"value": [0.2, None], "ref": {"desc": "layers"}}],
    ids=["bare_list", "refvalue_list"],
)
def test_land_cover_rule_flags_null_in_frozen_list(dz):
    """A null inside a list is found after ValidationContext turns lists into tuples."""
    data = _single_surface_site({"sfr": 1.0, "thermal_layers": {"dz": dz}})

    results = _run_rule("land_cover", data)

    errors = [r for r in results if r.status == "ERROR"]
    assert [r.parameter for r in errors] == ["paved.thermal_layers.dz"]


def test_land_cover_rule_warns_zero_sfr_bare_and_ref_only_params():
    """Zero-fraction surfaces list bare and ref-only parameters, not only RefValue ones."""
    data = {
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "land_cover": {
                        "paved": {"sfr": 1.0},
                        "bldgs": {
                            "sfr": 0.0,
                            "bldgh": 10.0,
                            "faibldg": {"ref": {"desc": "placeholder"}},
                            "thermal_layers": {"dz": [0.1, 0.2]},
                        },
                    }
                },
            }
        ]
    }

    results = _run_rule("land_cover", data)

    warnings = [r for r in results if r.parameter == "land_cover.bldgs"]
    assert len(warnings) == 1
    suggestion = warnings[0].suggested_value
    for name in ("bldgh", "faibldg", "thermal_layers.dz"):
        assert name in suggestion
    assert "faibldg.ref" not in suggestion


def test_physics_params_rule_flags_empty_frozen_mapping():
    """An empty physics mapping is caught although it is frozen to a MappingProxyType."""
    physics_rule = RulesRegistry.rules["physics_params"]
    base = {"model": {"physics": {"net_radiation": 3}}}
    empty = {"model": {"physics": {"net_radiation": {}}}}

    base_errors = {
        r.parameter
        for r in physics_rule(ValidationContext(yaml_data=base))
        if r.status == "ERROR"
    }
    empty_errors = {
        r.parameter
        for r in physics_rule(ValidationContext(yaml_data=empty))
        if r.status == "ERROR"
    }

    assert "model.physics.net_radiation" not in base_errors
    assert "model.physics.net_radiation" in empty_errors


def test_same_emissivity_rule_accepts_single_frozen_wall_mapping():
    """A single wall/roof mapping (not a list) is read after freezing."""
    data = {
        "model": {
            "physics": {
                "stebbs": {"same_emissivity_wall": 0, "same_emissivity_roof": 0}
            }
        },
        "sites": [
            {
                "name": "s1",
                "properties": {
                    "vertical_layers": {
                        "walls": {"emis": 0.91},
                        "roofs": {"emis": {"value": 0.93}},
                    }
                },
            }
        ],
    }

    results = _run_rule("same_emissivity", data)

    messages = {r.parameter: r.message for r in results}
    assert "found values: [0.91]" in messages["same_emissivity_wall"]
    assert "found values: [0.93]" in messages["same_emissivity_roof"]


def _stebbs_on_archetype_yaml(archetype):
    return normalise_yaml_renames(
        {
            "model": {
                "physics": {
                    "stebbs": {
                        "enabled": True,
                        "same_albedo_wall": 0,
                        "same_albedo_roof": 1,
                    }
                }
            },
            "sites": [
                {
                    "gridiv": 1,
                    "name": "s1",
                    "properties": {"building_archetype": archetype},
                }
            ],
        }
    )


@pytest.mark.parametrize(
    ("archetype", "expected"),
    [
        (
            # Legacy spellings, renamed before the rules run as in Phase B.
            {
                "wall_reflectivity": 0.1,
                "wall_absorptivity": {"value": 0.3, "ref": {"desc": "a"}},
                "wall_transmissivity": 0.6,
            },
            "PASS",
        ),
        (
            {
                "reflectivity_wall_external": 0.2,
                "absorptivity_wall_external": 0.3,
                "transmissivity_wall_external": 0.4,
            },
            "ERROR",
        ),
        (
            {
                "reflectivity_wall_external": "high",
                "absorptivity_wall_external": 0.3,
                "transmissivity_wall_external": 0.4,
            },
            "ERROR",
        ),
    ],
    ids=["legacy_names_float_sum", "canonical_bad_sum", "non_numeric"],
)
def test_archetype_radiation_rule_reads_canonical_names(archetype, expected):
    """The facet radiation rule reads *_external names; 0.1 + 0.3 + 0.6 counts as 1."""
    data = _stebbs_on_archetype_yaml(archetype)

    results = _run_rule("archetype_properties", data)

    wall = [r for r in results if "reflectivity_wall_external" in r.parameter]
    assert [r.status for r in wall] == [expected]


def test_same_albedo_rule_reports_canonical_reflectivity():
    """same_albedo_wall == 0 reports the canonical archetype reflectivity value."""
    data = _stebbs_on_archetype_yaml({"wall_reflectivity": 0.35})

    results = _run_rule("same_albedo", data)

    assert len(results) == 1
    assert "reflectivity_wall_external (found value: 0.35)" in results[0].message


def test_rules_registry_isolates_a_crashing_rule(monkeypatch):
    """A rule that raises becomes one ERROR naming it; the other rules still run."""
    def _boom(context):
        raise KeyError("broken")

    monkeypatch.setitem(RulesRegistry.rules, "boom", _boom)
    data = _single_surface_site({"sfr": 1.0})
    registry = RulesRegistry(
        context=ValidationContext(yaml_data=data),
        rule_order=("boom", "land_cover"),
    )

    results = registry.run_validation()

    crashed = [r for r in results if r.code == "VALIDATOR.RULE_FAILED.BOOM"]
    assert len(crashed) == 1
    assert crashed[0].status == "ERROR"
    assert "rule 'boom'" in crashed[0].message
    assert "KeyError" in crashed[0].message
    assert any(r.parameter == "land_cover_validation" for r in results)


def test_adjustment_pipeline_isolates_a_crashing_step(monkeypatch):
    """With an error sink a raising step is reported and rolled back; others run."""
    def _boom(yaml_data):
        yaml_data["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] = -1
        raise ValueError("broken step")

    _boom.__name__ = "adjust_land_cover_fractions"
    monkeypatch.setattr(phase_b, "adjust_land_cover_fractions", _boom)
    data = {
        "model": {"physics": {"stebbs": {"enabled": True, "capacitance": 0}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "land_cover": {"paved": {"sfr": 1.0}},
                    "building_archetype": {
                        "fraction_heat_capacity_roof_external": 0.2,
                        "fraction_heat_capacity_wall_external": 0.2,
                    },
                },
            }
        ],
    }

    with pytest.raises(ValueError, match="broken step"):
        phase_b.run_scientific_adjustment_pipeline(data, "2020-01-01", 2020)

    errors = []
    updated, adjustments = phase_b.run_scientific_adjustment_pipeline(
        data, "2020-01-01", 2020, errors=errors
    )

    assert [e.code for e in errors] == [
        "VALIDATOR.RULE_FAILED.ADJUST_LAND_COVER_FRACTIONS"
    ]
    assert updated["sites"][0]["properties"]["land_cover"]["paved"]["sfr"] == pytest.approx(1.0)
    assert {a.parameter for a in adjustments} >= {
        "building_archetype.fraction_heat_capacity_roof_external"
    }


def _stebbs_off_yaml():
    return {
        "model": {"physics": {"stebbs": {"enabled": False, "capacitance": 0}}},
        "sites": [
            {
                "gridiv": 1,
                "properties": {
                    "lat": 51.5,
                    "lng": -0.1,
                    "land_cover": {"paved": {"sfr": 1.0}},
                    "stebbs": {
                        "temperature_air_outdoor_initial": 5.0,
                        "temperature_air_indoor_initial": {
                            "value": 5.0,
                            "ref": {"desc": "indoor"},
                        },
                        "temperature_air_annual_mean": 4.0,
                    },
                    "building_archetype": {
                        "fraction_heat_capacity_roof_external": 0.3,
                        "fraction_heat_capacity_wall_external": {
                            "value": 0.7,
                            "ref": {"desc": "wall"},
                        },
                        "absorptivity_wall_external": {"ref": {"desc": "ref only"}},
                    },
                },
            }
        ],
    }


def test_stebbs_off_temperatures_not_suggested_then_nullified(monkeypatch):
    """With STEBBS off its temperatures are left to the nullifier, not set from CRU."""
    monkeypatch.setattr(
        phase_b, "get_mean_monthly_air_temperature", lambda lat, lng, month: 14.0
    )
    monkeypatch.setattr(
        phase_b, "get_monthly_air_temperature_diffmax", lambda lat, lng: 8.5
    )
    monkeypatch.setattr(
        phase_b, "get_mean_annual_air_temperature", lambda lat, lng: 11.2
    )

    updated, adjustments = phase_b.run_scientific_adjustment_pipeline(
        _stebbs_off_yaml(), "2020-07-01", 2020
    )

    parameters = [a.parameter for a in adjustments]
    assert not [p for p in parameters if p.startswith("stebbs.")]
    stebbs = updated["sites"][0]["properties"]["stebbs"]
    assert stebbs["temperature_air_outdoor_initial"] is None
    assert stebbs["temperature_air_indoor_initial"] == {
        "value": None,
        "ref": {"desc": "indoor"},
    }


def test_rcmethod_keeps_fractions_nullified_when_stebbs_off():
    """capacitance == 0 must not restore archetype fractions nullified for STEBBS off."""
    updated, adjustments = phase_b.run_scientific_adjustment_pipeline(
        _stebbs_off_yaml(), "2020-01-01", 2020
    )

    archetype = updated["sites"][0]["properties"]["building_archetype"]
    assert archetype["fraction_heat_capacity_roof_external"] is None
    assert archetype["fraction_heat_capacity_wall_external"] == {
        "value": None,
        "ref": {"desc": "wall"},
    }
    # The ref-only placeholder keeps its metadata (no descent into "ref").
    assert archetype["absorptivity_wall_external"] == {"ref": {"desc": "ref only"}}
    assert not [a for a in adjustments if "capacitance == 0" in str(a.reason)]


def test_co2_nullifier_keeps_ref_only_metadata_and_reports_lists():
    """The CO2 nullifier skips ref-only placeholders; nulled lists are reported."""
    block = {
        "co2_point_source": {"ref": {"desc": "placeholder", "DOI": "x"}},
        "ef_umolco2perj": {"value": [1.0, 2.0], "ref": {"desc": "ef"}},
        "enef_v_jkm": 3.0,
    }
    before = deepcopy(block)

    nullify_co2_block_recursive(block)

    assert block["co2_point_source"] == {"ref": {"desc": "placeholder", "DOI": "x"}}
    assert block["ef_umolco2perj"] == {"value": [None, None], "ref": {"desc": "ef"}}
    assert block["enef_v_jkm"] is None
    assert collect_nullified_paths(before, block) == ["ef_umolco2perj", "enef_v_jkm"]


def test_biogenic_nullifier_bare_ref_and_already_null():
    """Biogenic CO2 nullification handles bare values and skips value-less entries."""
    props = {
        "land_cover": {
            "dectr": {
                "alpha_bioco2": 0.5,
                "beta_bioco2": {"value": [1.0, 2.0], "ref": {"desc": "b"}},
                "resp_a": {"ref": {"desc": "ref only"}},
                "resp_b": {"value": None},
            }
        }
    }

    assert _nullify_biogenic_in_props(props) is True
    dectr = props["land_cover"]["dectr"]
    assert dectr["alpha_bioco2"] is None
    assert dectr["beta_bioco2"] == {"value": [None, None], "ref": {"desc": "b"}}
    assert dectr["resp_a"] == {"ref": {"desc": "ref only"}}
    assert dectr["resp_b"] == {"value": None}

    already_null = {"land_cover": {"grass": {"resp_a": None, "resp_b": {"value": None}}}}
    assert _nullify_biogenic_in_props(already_null) is False


def test_null_roof_wall_template_keeps_bare_list_lengths():
    """Padding templates keep per-layer list lengths for bare and RefValue forms."""
    template = create_null_roof_wall_template(
        {
            "temperature": [15.0, 15.0, 15.0, 15.0, 15.0],
            "tsfc": 15.0,
            "alb": {"value": 0.2},
            "thermal_layers": {"dz": [0.2, 0.1, 0.1], "k": {"value": [1.0, 1.0, 1.0]}},
        }
    )

    assert template == {
        "temperature": [None] * 5,
        "tsfc": None,
        "alb": {"value": None},
        "thermal_layers": {"dz": [None] * 3, "k": {"value": [None] * 3}},
    }


def test_phase_a_checks_every_site_against_the_template():
    """Missing and extra parameters are found on sites beyond the first."""
    # pipe_capacity is not null-rejected, so its absence is reportable.
    standard = {"sites": [{"gridiv": 1, "properties": {"pipe_capacity": 100.0}}]}
    user = {
        "sites": [
            {"gridiv": 1, "properties": {"pipe_capacity": 100.0}},
            {"gridiv": 2, "properties": {"not_a_field": 1}},
        ]
    }

    missing = find_missing_parameters(user, standard, _normalised=True)
    extra = find_extra_parameters(user, standard, _normalised=True)

    assert [path for path, _value, _physics in missing] == [
        "sites[1].properties.pipe_capacity"
    ]
    assert extra == ["sites[1].properties.not_a_field"]


def test_nlayer_dimensions_follow_each_site():
    """Detected nlayer is applied per site, not taken from the first site for all."""
    def _site(nlayer):
        return {
            "properties": {
                "vertical_layers": {
                    "nlayer": nlayer,
                    "height": [float(i) for i in range(nlayer + 1)],
                    "building_frac": [0.4] * nlayer,
                }
            }
        }

    user = {"sites": [_site(3), _site(2)]}

    _, per_site_errors = validate_nlayer_dimensions(deepcopy(user), 3, per_site=True)
    _, global_errors = validate_nlayer_dimensions(deepcopy(user), 3)

    assert per_site_errors == []
    assert {path for path, *_ in global_errors} == {
        "sites[1].properties.vertical_layers.height",
        "sites[1].properties.vertical_layers.building_frac",
    }


def test_phase_a_file_run_reports_and_inserts_under_later_site(tmp_path):
    """Through the Phase A entry point, a second site is diffed and padded on its own terms."""
    standard = files("supy").joinpath("sample_data", "sample_config.yml")
    data = yaml.safe_load(standard.read_text(encoding="utf-8"))
    second = deepcopy(data["sites"][0])
    second["gridiv"] = 2
    second["name"] = "second"
    second["properties"]["conductance"].pop("g_max")
    second["properties"]["not_a_field"] = {"value": 1}
    # The second site has two layers, with arrays sized to match.
    layers = second["properties"]["vertical_layers"]
    layers["nlayer"] = {"value": 2}
    layers["height"]["value"] = layers["height"]["value"][:3]
    for name in ("veg_frac", "veg_scale", "veg_ext", "building_frac", "building_scale"):
        layers[name]["value"] = layers[name]["value"][:2]
    for name in ("roofs", "walls"):
        layers[name] = layers[name][:2]
        second["initial_states"][name] = second["initial_states"][name][:2]
    data["sites"].append(second)
    user_file = tmp_path / "user.yml"
    user_file.write_text(yaml.safe_dump(data, sort_keys=False), encoding="utf-8")
    updated_file = tmp_path / "updated.yml"
    report_file = tmp_path / "report.txt"

    annotate_missing_parameters(
        str(user_file),
        str(standard),
        str(updated_file),
        str(report_file),
        forcing="off",
        detect_nlayer=True,
    )

    updated = yaml.safe_load(updated_file.read_text(encoding="utf-8"))
    report = report_file.read_text(encoding="utf-8")
    assert updated["sites"][0]["properties"]["conductance"]["g_max"] == {"value": 3.5}
    assert updated["sites"][1]["properties"]["conductance"]["g_max"] == {"value": None}
    assert "sites[1].properties.conductance.g_max" in report
    assert "sites[1].properties.not_a_field" in report
    assert "vertical_layers" not in report
    assert len(updated["sites"][1]["properties"]["vertical_layers"]["roofs"]) == 2
