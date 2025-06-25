import pytest
from copy import deepcopy
from supy.data_model.core import (
    run_precheck,
    precheck_model_physics_params,
    precheck_start_end_date,
    precheck_site_season_adjustments,
    SeasonCheck,
)


def test_precheck_start_end_date_valid():
    data = {}
    updated_data, model_year, start_date, end_date = precheck_start_end_date(data)
    assert updated_data == data
    assert start_date == "2011-01-22"
    assert end_date == "2011-02-22"
    assert model_year == 2011


def test_model_physics_check_passes():
    yaml_input = {
        "model": {
            "physics": {
                k: {"value": 1 if "use" not in k else 0}
                for k in [
                    "netradiationmethod", "emissionsmethod", "storageheatmethod", "ohmincqf",
                    "roughlenmommethod", "roughlenheatmethod", "stabilitymethod", "smdmethod",
                    "waterusemethod", "diagmethod", "faimethod", "localclimatemethod",
                    "snowuse", "stebbsmethod"
                ]
            }
        }
    }
    result = precheck_model_physics_params(yaml_input)
    assert isinstance(result, dict)


def test_model_physics_missing_key_raises():
    yaml_input = {
        "model": {
            "physics": {
                "diagmethod": {"value": 2}
            }
        }
    }
    with pytest.raises(ValueError, match=r"Missing required params"):
        precheck_model_physics_params(yaml_input)


def test_model_physics_empty_value_raises():
    yaml_input = {
        "model": {
            "physics": {
                "diagmethod": {"value": 2},
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
                "localclimatemethod": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            }
        }
    }
    with pytest.raises(ValueError, match=r"Empty or null values for"):
        precheck_model_physics_params(yaml_input)


def test_diagmethod_stability_constraint_fails():
    yaml_input = {
        "model": {
            "physics": {
                "diagmethod": {"value": 2},
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
                "localclimatemethod": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [{}],
    }

    with pytest.raises(ValueError, match=r"If diagmethod == 2.*stabilitymethod.*3"):
        run_precheck(deepcopy(yaml_input))


def test_model_physics_not_touched_by_empty_string_cleanup():
    yaml_input = {
        "model": {
            "physics": {
                "diagmethod": {"value": ""},
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
                "localclimatemethod": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
            "control": {"forcing_file": {"value": "dummy.txt"}}
        },
        "sites": [{"gridiv": 1, "properties": {"lat": {"value": 51.5}}}],
    }

    with pytest.raises(ValueError, match=r"Empty or null values for"):
        run_precheck(deepcopy(yaml_input))


def test_empty_string_becomes_none():
    yaml_input = {
        "sites": [
            {
                "site_name": "",
                "properties": {
                    "lat": {"value": ""},
                    "lng": {"value": -0.12}
                }
            }
        ],
    }
    result = run_precheck(deepcopy(yaml_input))
    assert result["sites"][0]["site_name"] is None
    assert result["sites"][0]["properties"]["lat"]["value"] is None


def test_empty_string_in_list_of_floats():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "thermal_layers": {
                        "dz": {"value": [0.2, "", 0.1]}
                    }
                }
            }
        ],
    }
    result = run_precheck(deepcopy(yaml_input))
    assert result["sites"][0]["properties"]["thermal_layers"]["dz"]["value"][1] is None


def test_empty_string_in_nested_dict():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "ohm_coef": {
                        "summer_dry": {
                            "a1": {"value": ""},
                            "a2": {"value": 0.3},
                        }
                    }
                }
            }
        ],
    }
    result = run_precheck(deepcopy(yaml_input))
    assert result["sites"][0]["properties"]["ohm_coef"]["summer_dry"]["a1"]["value"] is None


def test_empty_string_in_surface_type_dict():
    yaml_input = {
        "sites": [
            {
                "properties": {
                    "waterdist": {
                        "to_grass": {"value": ""},
                        "to_runoff": {"value": 0.9}
                    }
                }
            }
        ],
    }
    result = run_precheck(deepcopy(yaml_input))
    assert result["sites"][0]["properties"]["waterdist"]["to_grass"]["value"] is None


def test_season_check_sets_snowalb_to_none():
    yaml_input = {
        "sites": [
            {
                "properties": {"lat": {"value": 5.0}},
                "initial_states": {
                    "snowalb": {"value": 0.3}
                }
            }
        ],
    }
    result = precheck_site_season_adjustments(deepcopy(yaml_input), "2025-06-01")
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
    result = precheck_site_season_adjustments(deepcopy(data), "2025-01-15")
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
    result = precheck_site_season_adjustments(deepcopy(data), "2025-06-01")
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
                            }
                        }
                    }
                },
                "initial_states": {
                    "dectr": {}
                }
            }
        ]
    }
    result = precheck_site_season_adjustments(deepcopy(yaml_input), "2025-07-01")
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
                            }
                        }
                    }
                },
                "initial_states": {
                    "dectr": {}
                }
            }
        ]
    }
    result = precheck_site_season_adjustments(deepcopy(yaml_input), "2025-01-15")
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
                            }
                        }
                    }
                },
                "initial_states": {
                    "dectr": {}
                }
            }
        ]
    }
    result = precheck_site_season_adjustments(deepcopy(yaml_input), "2025-10-01")
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] == 3.0  # (1.0 + 5.0) / 2


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
                            }
                        }
                    }
                },
                "initial_states": {
                    "dectr": {
                        "lai_id": {"value": 999.0}  # Dummy old value to be nullified
                    }
                }
            }
        ]
    }
    result = precheck_site_season_adjustments(deepcopy(yaml_input), "2025-07-01")
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] is None
