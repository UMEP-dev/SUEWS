import pytest
from copy import deepcopy
from supy.data_model import SUEWSConfig
from supy.data_model.core import precheck_start_end_date
from supy.data_model.core import precheck_model_physics_params

import pytest
from copy import deepcopy
from supy.data_model.core import (
    run_precheck,
    precheck_model_physics_params,
    precheck_start_end_date,
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
    with pytest.raises(ValueError, match="Missing required parameters"):
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
    with pytest.raises(ValueError, match="Parameters with empty string or null values"):
        precheck_model_physics_params(yaml_input)

def test_diagmethod_stability_constraint_passes():
    yaml_input = {
        "name": "test",
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
            "physics": {
                "diagmethod": {"value": 2},
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
                "localclimatemethod": {"value": 1},
                "snowuse": {"value": 0},
                "stebbsmethod": {"value": 0},
            },
        },
        "sites": [{}],
    }

    cleaned = run_precheck(deepcopy(yaml_input))
    assert cleaned["model"]["physics"]["diagmethod"]["value"] == 2

def test_diagmethod_stability_constraint_fails():
    yaml_input = {
        "name": "test",
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
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

    with pytest.raises(ValueError, match=r"If diagmethod == 2, then stabilitymethod must be 3"):
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

    with pytest.raises(ValueError, match="Parameters with empty string or null values"):
        run_precheck(deepcopy(yaml_input))

def test_empty_string_becomes_none():
    yaml_input = {
        "name": "test",
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
            "physics": {
                "diagmethod": {"value": 2},
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
        },
        "sites": [
            {
                "gridiv": 1,
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
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
            "physics": {
                "diagmethod": {"value": 2},
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
        },
        "sites": [
            {
                "gridiv": 1,
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
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
            "physics": {
                "diagmethod": {"value": 2},
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
        },
        "sites": [
            {
                "gridiv": 1,
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
        "model": {
            "control": {"forcing_file": {"value": "dummy.txt"}},
            "physics": {
                "diagmethod": {"value": 2},
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
        },
        "sites": [
            {
                "gridiv": 1,
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
