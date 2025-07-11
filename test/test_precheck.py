import pytest
import copy
import logging
from supy._env import logger_supy
from copy import deepcopy
from datetime import datetime
from supy.data_model.precheck import (
    precheck_model_physics_params,
    precheck_start_end_date,
    precheck_site_season_adjustments,
    precheck_model_options_constraints,
    precheck_replace_empty_strings_with_none,
    precheck_land_cover_fractions,
    precheck_nullify_zero_sfr_params,
    precheck_nonzero_sfr_requires_nonnull_params,
    precheck_model_option_rules,
    collect_yaml_differences,
    precheck_update_surface_temperature, 
    get_monthly_avg_temp,
    precheck_warn_zero_sfr_params,
    SeasonCheck,
)


def test_precheck_start_end_date_valid_from_yaml():
    yaml_input = {
        "model": {"control": {"start_time": "2011-01-01", "end_time": "2013-12-31"}}
    }

    updated_data, model_year, start_date, end_date = precheck_start_end_date(yaml_input)

    assert updated_data == yaml_input
    assert start_date == "2011-01-01"
    assert end_date == "2013-12-31"
    assert model_year == 2011


def test_model_physics_check_passes():
    yaml_input = {
        "model": {
            "physics": {
                k: {"value": 1 if "use" not in k else 0}
                for k in [
                    "netradiationmethod",
                    "emissionsmethod",
                    "storageheatmethod",
                    "ohmincqf",
                    "roughlenmommethod",
                    "roughlenheatmethod",
                    "stabilitymethod",
                    "smdmethod",
                    "waterusemethod",
                    "rslmethod",
                    "faimethod",
                    "rsllevel",
                    "snowuse",
                    "stebbsmethod",
                ]
            }
        }
    }
    result = precheck_model_physics_params(yaml_input)
    assert isinstance(result, dict)


def test_model_physics_missing_key_raises():
    yaml_input = {"model": {"physics": {"rslmethod": {"value": 2}}}}
    with pytest.raises(ValueError, match=r"Missing required params"):
        precheck_model_physics_params(yaml_input)


def test_model_physics_empty_value_raises():
    yaml_input = {
        "model": {
            "physics": {
                "rslmethod": {"value": 2},
                "stabilitymethod": {"value": None},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "storageheatmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            }
        }
    }
    with pytest.raises(ValueError, match=r"Empty or null values for"):
        precheck_model_physics_params(yaml_input)


def test_rslmethod_stability_constraint_fails():
    yaml_input = {
        "model": {
            "control": {"start_time": "2025-01-01", "end_time": "2025-12-31"},
            "physics": {
                "rslmethod": {"value": 2},
                "stabilitymethod": {"value": 1},
                "storageheatmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [{}],
    }

    with pytest.raises(ValueError, match=r"If rslmethod == 2.*must be 3"):
        precheck_model_options_constraints(yaml_input)


def test_model_physics_not_touched_by_empty_string_cleanup():
    yaml_input = {
        "model": {
            "control": {
                "start_time": "2025-01-01",
                "end_time": "2025-12-31",
            },
            "physics": {
                "rslmethod": {"value": ""},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 3},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [{"gridiv": 1, "properties": {"lat": {"value": 51.5}}}],
    }

    data = precheck_replace_empty_strings_with_none(yaml_input)
    with pytest.raises(ValueError, match=r"Empty or null values for"):
        precheck_model_physics_params(data)


def test_empty_string_becomes_none():
    yaml_input = {
        "model": {
            "control": {
                "start_time": "2025-01-01",
                "end_time": "2025-12-31",
            },
            "physics": {
                "rslmethod": {"value": 1},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [
            {
                "site_name": "",
                "properties": {
                    "lat": {"value": ""},
                    "lng": {"value": -0.12},
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}},
                        "paved": {"sfr": {"value": 0.5}},
                    },
                },
            }
        ],
    }

    result = precheck_replace_empty_strings_with_none(yaml_input)

    assert result["sites"][0]["site_name"] is None
    assert result["sites"][0]["properties"]["lat"]["value"] is None


def test_empty_string_in_list_of_floats():
    yaml_input = {
        "model": {
            "control": {
                "start_time": "2025-01-01",
                "end_time": "2025-12-31",
            },
            "physics": {
                "rslmethod": {"value": 1},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [
            {
                "properties": {
                    "thermal_layers": {"dz": {"value": [0.2, "", 0.1]}},
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}},
                        "paved": {"sfr": {"value": 0.5}},
                    },
                }
            }
        ],
    }

    result = precheck_replace_empty_strings_with_none(yaml_input)

    assert result["sites"][0]["properties"]["thermal_layers"]["dz"]["value"][1] is None


def test_empty_string_in_nested_dict():
    yaml_input = {
        "model": {
            "control": {
                "start_time": "2025-01-01",
                "end_time": "2025-12-31",
            },
            "physics": {
                "rslmethod": {"value": 1},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [
            {
                "properties": {
                    "ohm_coef": {
                        "summer_dry": {
                            "a1": {"value": ""},
                            "a2": {"value": 0.3},
                        }
                    },
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}},
                        "paved": {"sfr": {"value": 0.5}},
                    },
                }
            }
        ],
    }

    result = precheck_replace_empty_strings_with_none(yaml_input)

    assert (
        result["sites"][0]["properties"]["ohm_coef"]["summer_dry"]["a1"]["value"]
        is None
    )


def test_empty_string_in_surface_type_dict():
    yaml_input = {
        "model": {
            "control": {
                "start_time": "2025-01-01",
                "end_time": "2025-12-31",
            },
            "physics": {
                "rslmethod": {"value": 1},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [
            {
                "properties": {
                    "waterdist": {
                        "to_grass": {"value": ""},
                        "to_runoff": {"value": 0.9},
                    },
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}},
                        "paved": {"sfr": {"value": 0.5}},
                    },
                }
            }
        ],
    }

    result = precheck_replace_empty_strings_with_none(yaml_input)

    assert result["sites"][0]["properties"]["waterdist"]["to_grass"]["value"] is None


def test_season_check_sets_snowalb_to_none():
    yaml_input = {
        "sites": [
            {
                "properties": {"lat": {"value": 5.0}},
                "initial_states": {"snowalb": {"value": 0.3}},
            }
        ],
    }
    result = precheck_site_season_adjustments(
        deepcopy(yaml_input), "2025-06-01", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["snowalb"]["value"] is None


def test_site_in_winter_does_not_touch_snowalb():
    data = {
        "sites": [
            {
                "properties": {"lat": {"value": 51.5}},
                "initial_states": {"snowalb": {"value": 0.3}},
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(data), "2025-01-15", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["snowalb"]["value"] == 0.3


def test_site_equatorial_sets_snowalb_none():
    data = {
        "sites": [
            {
                "properties": {"lat": {"value": 0.0}},
                "initial_states": {"snowalb": {"value": 0.3}},
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(data), "2025-06-01", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["snowalb"]["value"] is None


def test_season_check_equatorial():
    sc = SeasonCheck(start_date="2025-06-01", lat=0)
    assert sc.get_season() == "equatorial"


def test_season_check_tropical():
    sc = SeasonCheck(start_date="2025-06-01", lat=15.0)
    assert sc.get_season() == "tropical"


def test_season_check_northern_summer():
    sc = SeasonCheck(start_date="2025-07-01", lat=51.5)
    assert sc.get_season() == "summer"


def test_season_check_northern_winter():
    sc = SeasonCheck(start_date="2025-01-15", lat=51.5)
    assert sc.get_season() == "winter"


def test_season_check_southern_summer():
    sc = SeasonCheck(start_date="2025-01-15", lat=-30.0)
    assert sc.get_season() == "summer"


def test_season_check_invalid_date():
    with pytest.raises(ValueError, match=r"start_date must be in YYYY-MM-DD format"):
        SeasonCheck(start_date="bad-date", lat=51.5).get_season()


def test_lai_id_set_in_summer():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "land_cover": {
                        "dectr": {
                            "sfr": {"value": 0.2},
                            "lai": {
                                "laimin": {"value": 1.0},
                                "laimax": {"value": 5.0},
                            },
                        }
                    },
                },
                "initial_states": {"dectr": {}},
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(yaml_input), "2025-07-01", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] == 5.0


def test_lai_id_set_in_winter():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "land_cover": {
                        "dectr": {
                            "sfr": {"value": 0.2},
                            "lai": {
                                "laimin": {"value": 1.0},
                                "laimax": {"value": 5.0},
                            },
                        }
                    },
                },
                "initial_states": {"dectr": {}},
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(yaml_input), "2025-01-15", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] == 1.0


def test_lai_id_set_in_fall():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "land_cover": {
                        "dectr": {
                            "sfr": {"value": 0.2},
                            "lai": {
                                "laimin": {"value": 1.0},
                                "laimax": {"value": 5.0},
                            },
                        }
                    },
                },
                "initial_states": {"dectr": {}},
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(yaml_input), "2025-10-01", model_year=2025
    )
    assert (
        result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] == 3.0
    )  # (1.0 + 5.0) / 2


def test_lai_id_nullified_if_no_dectr_surface():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "land_cover": {
                        "dectr": {
                            "sfr": {"value": 0.0},
                            "lai": {
                                "laimin": {"value": 1.0},
                                "laimax": {"value": 5.0},
                            },
                        }
                    },
                },
                "initial_states": {
                    "dectr": {
                        "lai_id": {"value": 999.0}  # Dummy old value to be nullified
                    }
                },
            }
        ]
    }
    result = precheck_site_season_adjustments(
        deepcopy(yaml_input), "2025-07-01", model_year=2025
    )
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] is None


def test_precheck_dls_assignment():
    data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.12},
                    "anthropogenic_emissions": {},
                },
                "initial_states": {},
            }
        ]
    }

    updated = precheck_site_season_adjustments(
        deepcopy(data), start_date="2025-03-01", model_year=2025
    )
    emissions = updated["sites"][0]["properties"]["anthropogenic_emissions"]
    props = updated["sites"][0]["properties"]

    assert "startdls" in emissions
    assert "enddls" in emissions
    assert "timezone" in props
    assert emissions["startdls"]["value"] is not None
    assert emissions["enddls"]["value"] is not None
    assert props["timezone"]["value"] == 0  # London standard time offset (UTC+0)


def test_precheck_dls_for_unknown_location_graceful():
    data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 0.0},
                    "lng": {"value": 0.0},
                    "anthropogenic_emissions": {},
                },
                "initial_states": {},
            }
        ]
    }

    result = precheck_site_season_adjustments(
        deepcopy(data), "2025-03-01", model_year=2025
    )
    props = result["sites"][0]["properties"]

    assert "timezone" in props
    assert props["timezone"]["value"] == 0  # 'Etc/GMT' offset


def test_precheck_dls_overwrites_existing_values():
    data = {
        "sites": [
            {
                "properties": {
                    "lat": {"value": 51.5},
                    "lng": {"value": -0.12},
                    "anthropogenic_emissions": {
                        "startdls": {"value": 999},
                        "enddls": {"value": 999},
                    },
                },
                "initial_states": {},
            }
        ]
    }

    updated = precheck_site_season_adjustments(
        deepcopy(data), start_date="2025-03-01", model_year=2025
    )
    emissions = updated["sites"][0]["properties"]["anthropogenic_emissions"]

    assert emissions["startdls"]["value"] != 999
    assert emissions["enddls"]["value"] != 999
    assert isinstance(emissions["startdls"]["value"], int)
    assert isinstance(emissions["enddls"]["value"], int)


def test_land_cover_exact_sum():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "dectr": {"sfr": {"value": 0.4}},
                        "grass": {"sfr": {"value": 0.6}},
                    }
                }
            }
        ]
    }
    result = precheck_land_cover_fractions(deepcopy(data))
    total = sum(
        v.get("sfr", {}).get("value", 0)
        for v in result["sites"][0]["properties"]["land_cover"].values()
    )
    assert abs(total - 1.0) < 1e-6


def test_land_cover_low_sum_autofix():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "dectr": {"sfr": {"value": 0.49995}},
                        "grass": {"sfr": {"value": 0.49995}},
                    }
                }
            }
        ]
    }
    result = precheck_land_cover_fractions(deepcopy(data))
    total = sum(
        v.get("sfr", {}).get("value", 0)
        for v in result["sites"][0]["properties"]["land_cover"].values()
    )
    assert abs(total - 1.0) < 1e-6


def test_land_cover_high_sum_autofix():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "dectr": {"sfr": {"value": 0.50005}},
                        "grass": {"sfr": {"value": 0.50005}},
                    }
                }
            }
        ]
    }
    result = precheck_land_cover_fractions(deepcopy(data))
    total = sum(
        v.get("sfr", {}).get("value", 0)
        for v in result["sites"][0]["properties"]["land_cover"].values()
    )
    assert abs(total - 1.0) < 1e-6


def test_land_cover_invalid_sum_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "dectr": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.3}},
                    }
                }
            }
        ]
    }
    with pytest.raises(ValueError, match="Invalid land_cover sfr sum"):
        precheck_land_cover_fractions(deepcopy(data))


def test_land_cover_invalid_structure_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "dectr": {"sfr": {"value": 0.7}},
                        # Missing 'sfr' in grass → should trigger Pydantic error
                        "grass": {},
                    }
                }
            }
        ]
    }
    with pytest.raises(ValueError, match="Invalid land_cover"):
        precheck_land_cover_fractions(deepcopy(data))

def test_precheck_nullify_zero_sfr_params_nullifies_correctly():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.0},
                            "alb": {"value": 0.12},
                            "irrfrac": {"value": 0.0},
                        },
                        "paved": {"sfr": {"value": 0.5}, "alb": {"value": 0.1}},
                    },
                    "faibldg": {"value": 1.2},
                    "waterdist": {"to_bldgs": {"value": 0.3}},
                }
            }
        ]
    }

    result = precheck_nullify_zero_sfr_params(deepcopy(yaml_input))
    site_props = result["sites"][0]["properties"]

    # All params under bldgs except sfr should now be None
    assert site_props["land_cover"]["bldgs"]["sfr"]["value"] == 0.0
    assert site_props["land_cover"]["bldgs"]["alb"]["value"] is None
    assert site_props["land_cover"]["bldgs"]["irrfrac"]["value"] is None

    # Params under paved should remain untouched
    assert site_props["land_cover"]["paved"]["alb"]["value"] == 0.1

    # faibldg should remain untouched (it's outside land_cover.bldgs)
    assert site_props["faibldg"]["value"] == 1.2


def test_precheck_nullify_zero_sfr_params_does_nothing_if_all_nonzero():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}, "alb": {"value": 0.12}},
                        "paved": {"sfr": {"value": 0.5}, "alb": {"value": 0.1}},
                    }
                }
            }
        ]
    }

    result = precheck_nullify_zero_sfr_params(deepcopy(yaml_input))
    site_props = result["sites"][0]["properties"]

    # Everything should remain untouched
    assert site_props["land_cover"]["bldgs"]["alb"]["value"] == 0.12
    assert site_props["land_cover"]["paved"]["alb"]["value"] == 0.1


def test_precheck_nullify_zero_sfr_params_handles_lists():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.0},
                            "thermal_layers": {
                                "dz": {"value": [0.2, 0.1, 0.1]},
                                "k": {"value": [1.2, 1.1, 1.1]},
                                "cp": {"value": [1200000.0, 1100000.0, 1100000.0]},
                            },
                            "alb": {"value": 0.12},
                        },
                        "paved": {
                            "sfr": {"value": 0.5},
                            "alb": {"value": 0.1},
                        },
                    }
                }
            }
        ]
    }

    result = precheck_nullify_zero_sfr_params(deepcopy(yaml_input))
    bldgs = result["sites"][0]["properties"]["land_cover"]["bldgs"]

    # Check that all non-sfr parameters in bldgs are now None or list of None
    assert bldgs["alb"]["value"] is None
    assert bldgs["thermal_layers"]["dz"]["value"] == [None, None, None]
    assert bldgs["thermal_layers"]["k"]["value"] == [None, None, None]
    assert bldgs["thermal_layers"]["cp"]["value"] == [None, None, None]

    # Check that paved remains untouched
    paved = result["sites"][0]["properties"]["land_cover"]["paved"]
    assert paved["alb"]["value"] == 0.1


def test_nonzero_sfr_with_valid_params_passes():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.5},
                            "alb": {"value": 0.12},
                            "ohm_coef": {"summer_dry": {"a1": {"value": 0.5}}},
                        }
                    }
                }
            }
        ]
    }
    # Should not raise
    result = precheck_nonzero_sfr_requires_nonnull_params(deepcopy(data))
    assert isinstance(result, dict)


def test_nonzero_sfr_missing_param_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.5},
                            "alb": {"value": None},  # Should trigger error
                        }
                    }
                }
            }
        ]
    }
    with pytest.raises(ValueError, match=r"bldgs\.alb.*must be set"):
        precheck_nonzero_sfr_requires_nonnull_params(deepcopy(data))


def test_nonzero_sfr_empty_string_param_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.5},
                            "alb": {"value": ""},  # Should trigger error
                        }
                    }
                }
            }
        ]
    }
    with pytest.raises(ValueError, match=r"bldgs\.alb.*must be set"):
        precheck_nonzero_sfr_requires_nonnull_params(deepcopy(data))


def test_nonzero_sfr_nested_param_null_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.5},
                            "ohm_coef": {
                                "summer_dry": {
                                    "a1": {"value": None}  # Should trigger error
                                }
                            },
                        }
                    }
                }
            }
        ]
    }
    with pytest.raises(
        ValueError, match=r"bldgs\.ohm_coef\.summer_dry\.a1.*must be set"
    ):
        precheck_nonzero_sfr_requires_nonnull_params(deepcopy(data))


def test_nonzero_sfr_with_list_containing_none_raises():
    data = {
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {
                            "sfr": {"value": 0.5},
                            "thermal_layers": {
                                "dz": {
                                    "value": [None, None, None]
                                }  # Should trigger error
                            },
                        }
                    }
                }
            }
        ]
    }
    with pytest.raises(ValueError, match=r"bldgs\.thermal_layers\.dz.*must be set"):
        precheck_nonzero_sfr_requires_nonnull_params(deepcopy(data))


def build_base_yaml():
    """Minimal valid YAML input for physics and sites blocks."""
    return {
        "model": {
            "physics": {
                "rslmethod": {"value": 1},
                "stabilitymethod": {"value": 3},
                "storageheatmethod": {"value": 1},
                "stebbsmethod": {"value": 1},
                "netradiationmethod": {"value": 1},
                "emissionsmethod": {"value": 1},
                "ohmincqf": {"value": 1},
                "roughlenmommethod": {"value": 1},
                "roughlenheatmethod": {"value": 1},
                "smdmethod": {"value": 1},
                "waterusemethod": {"value": 1},
                "faimethod": {"value": 1},
                "rsllevel": {"value": 1},
                "snowuse": {"value": 0},
            }
        },
        "sites": [
            {
                "properties": {
                    "land_cover": {
                        "bldgs": {"sfr": {"value": 0.5}, "faibldg": {"value": 1.2}},
                        "paved": {"sfr": {"value": 0.5}},
                    },
                    "vertical_layers": {
                        "walls": [
                            {
                                "thermal_layers": {
                                    "dz": {"value": [0.2]},
                                    "k": {"value": [1.2]},
                                    "cp": {"value": [1000000.0]},
                                }
                            }
                        ]
                    },
                    "lambda_c": {"value": 3.0},
                    "stebbs": {
                        "WallInternalConvectionCoefficient": {"value": 5.0},
                        "WindowExternalConvectionCoefficient": {"value": 30.0},
                    },
                }
            }
        ]
    }

def test_rslmethod2_requires_faibldg():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["rslmethod"]["value"] = 2
    yaml_input["sites"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"] = None

    with pytest.raises(ValueError, match=r"faibldg.*must be set"):

        precheck_model_option_rules(deepcopy(yaml_input))

def test_rslmethod2_with_faibldg_passes():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["rslmethod"]["value"] = 2
    yaml_input["sites"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"] = 1.5

    result = precheck_model_option_rules(deepcopy(yaml_input))
    assert result["sites"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"] == 1.5

def test_storageheatmethod6_requires_wall_layers():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["storageheatmethod"]["value"] = 6
    yaml_input["sites"][0]["properties"]["vertical_layers"]["walls"] = []

    with pytest.raises(ValueError, match=r"Missing vertical_layers\.walls.*storageheatmethod == 6"):
        precheck_model_option_rules(deepcopy(yaml_input))

def test_storageheatmethod6_requires_thermal_layers_params():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["storageheatmethod"]["value"] = 6
    wall = yaml_input["sites"][0]["properties"]["vertical_layers"]["walls"][0]
    wall["thermal_layers"]["dz"]["value"] = []
    
    with pytest.raises(ValueError, match=r"thermal_layers\.dz.*storageheatmethod == 6"):
        precheck_model_option_rules(deepcopy(yaml_input))

def test_storageheatmethod6_requires_lambda_c():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["storageheatmethod"]["value"] = 6
    yaml_input["sites"][0]["properties"]["lambda_c"]["value"] = None

    with pytest.raises(ValueError, match=r"lambda_c.*storageheatmethod == 6"):
        precheck_model_option_rules(deepcopy(yaml_input))

def test_storageheatmethod6_valid_passes():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["storageheatmethod"]["value"] = 6

    result = precheck_model_option_rules(deepcopy(yaml_input))
    assert result["sites"][0]["properties"]["lambda_c"]["value"] == 3.0

def test_stebbsmethod0_nullifies_stebbs():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["stebbsmethod"]["value"] = 0
    yaml_input["sites"][0]["properties"]["stebbs"]["WallInternalConvectionCoefficient"]["value"] = 5.0

    result = precheck_model_option_rules(deepcopy(yaml_input))
    stebbs = result["sites"][0]["properties"]["stebbs"]
    assert stebbs["WallInternalConvectionCoefficient"]["value"] is None
    assert stebbs["WindowExternalConvectionCoefficient"]["value"] is None

def test_combined_diag2_stebbs0_storage6():
    yaml_input = build_base_yaml()
    yaml_input["model"]["physics"]["rslmethod"]["value"] = 2
    yaml_input["model"]["physics"]["stebbsmethod"]["value"] = 0
    yaml_input["model"]["physics"]["storageheatmethod"]["value"] = 6
    yaml_input["sites"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"] = 1.5

    result = precheck_model_option_rules(deepcopy(yaml_input))

    # Check stebbs nullified
    stebbs = result["sites"][0]["properties"]["stebbs"]
    assert all(v["value"] is None for v in stebbs.values())

    # Check faibldg still set
    assert result["sites"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"] == 1.5

    # Check wall layers untouched (still present)
    walls = result["sites"][0]["properties"]["vertical_layers"]["walls"]
    assert walls[0]["thermal_layers"]["dz"]["value"] == [0.2]


def test_collect_yaml_differences_simple():
    original = {
        "sites": [
            {
                "properties": {
                    "snowalb": {"value": 0.3},
                    "lat": {"value": 51.5},
                },
                "initial_states": {
                    "dectr": {"lai_id": {"value": 2.0}}
                }
            }
        ]
    }

    updated = {
        "sites": [
            {
                "properties": {
                    "snowalb": {"value": None},  # Was 0.3 → now null
                    "lat": {"value": 51.5},      # No change
                },
                "initial_states": {
                    "dectr": {"lai_id": {"value": 5.0}}  # Changed from 2.0 → 5.0
                }
            }
        ]
    }

    diffs = collect_yaml_differences(original, updated)

    # Must detect 2 diffs: snowalb and lai_id
    assert len(diffs) == 2

    expected_params = {d["parameter"] for d in diffs}
    assert "snowalb" in expected_params
    assert "lai_id" in expected_params

    for d in diffs:
        if d["parameter"] == "snowalb":
            assert d["old_value"] == 0.3
            assert d["new_value"] is None
            assert d["site"] == 0
        if d["parameter"] == "lai_id":
            assert d["old_value"] == 2.0
            assert d["new_value"] == 5.0
            assert d["site"] == 0

def build_minimal_yaml_for_surface_temp():
    return {
        "model": {
            "control": {
                "start_time": "2011-07-01",  # July → month = 7
                "end_time": "2011-12-31"
            }
        },
        "sites": [
            {
                "properties": {
                    "lat": {"value": 45.0},  # midlatitudes
                    "lng": {"value": 10.0}
                },
                "initial_states": {
                    surf: {
                        "temperature": {"value": [0, 0, 0, 0, 0]},
                        "tsfc": {"value": 0},
                        "tin": {"value": 0}
                    } for surf in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]
                }
            }
        ]
    }

def test_precheck_update_surface_temperature():
    data = build_minimal_yaml_for_surface_temp()
    start_date = data["model"]["control"]["start_time"]
    month = datetime.strptime(start_date, "%Y-%m-%d").month
    lat = data["sites"][0]["properties"]["lat"]["value"]

    expected_temp = get_monthly_avg_temp(lat, month)

    updated = precheck_update_surface_temperature(deepcopy(data), start_date=start_date)

    for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]:
        temp_array = updated["sites"][0]["initial_states"][surface]["temperature"]["value"]
        tsfc = updated["sites"][0]["initial_states"][surface]["tsfc"]["value"]
        tin = updated["sites"][0]["initial_states"][surface]["tin"]["value"]

        assert temp_array == [expected_temp] * 5, f"Mismatch in temperature array for {surface}"
        assert tsfc == expected_temp, f"Mismatch in tsfc for {surface}"
        assert tin == expected_temp, f"Mismatch in tin for {surface}"

def test_precheck_update_surface_temperature_missing_lat():
    data = build_minimal_yaml_for_surface_temp()
    data["sites"][0]["properties"]["lat"] = None  # Simulate missing lat

    start_date = data["model"]["control"]["start_time"]

    # Should not raise, but skip update
    updated = precheck_update_surface_temperature(deepcopy(data), start_date=start_date)

    for surface in ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]:
        temp_array = updated["sites"][0]["initial_states"][surface]["temperature"]["value"]
        assert temp_array == [0, 0, 0, 0, 0], f"Temperature should stay unchanged for {surface} when lat is missing."
