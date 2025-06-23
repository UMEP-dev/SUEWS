"""
Unit tests for run_precheck function logic.

These tests verify:
1. snowalb gets nullified in summer
2. LAI is set correctly if dectr > 0
3. LAI is nullified if dectr == 0
4. Logic error is raised for invalid stability setting
5. Missing physics keys raise error
"""

import pytest
from copy import deepcopy
from supy.data_model.core import run_precheck

minimal_yaml = {
    "model": {
        "control": {
            "start_time": "2020-07-01",
            "end_time": "2020-12-01",
        },
        "physics": {
            "netradiationmethod": {"value": 1},
            "emissionsmethod": {"value": 1},
            "storageheatmethod": {"value": 1},
            "ohmincqf": {"value": 1},
            "roughlenmommethod": {"value": 1},
            "roughlenheatmethod": {"value": 1},
            "stabilitymethod": {"value": 3},
            "smdmethod": {"value": 1},
            "waterusemethod": {"value": 1},
            "diagmethod": {"value": 2},
            "faimethod": {"value": 1},
            "localclimatemethod": {"value": 1},
            "snowuse": {"value": 0},
            "stebbsmethod": {"value": 0},
        },
    },
    "sites": [
        {
            "properties": {
                "lat": {"value": 45.0},
                "lng": {"value": 0.0},
                "land_cover": {
                    "paved":  {"sfr": {"value": 0.1}},
                    "bldgs":  {"sfr": {"value": 0.1}},
                    "evetr":  {"sfr": {"value": 0.15}},
                    "dectr":  {
                        "sfr": {"value": 0.2},
                        "lai": {
                            "laimin": {"value": 1.0},
                            "laimax": {"value": 3.0}
                        }
                    },
                    "grass":  {"sfr": {"value": 0.15}},
                    "bsoil":  {"sfr": {"value": 0.15}},
                    "water":  {"sfr": {"value": 0.15}},
                },
            },
            "initial_states": {
                "snowalb": {"value": 0.5},
                "dectr": {"lai_id": {"value": None}}
            },
        }
    ],
}

def test_run_precheck_success():
    data = deepcopy(minimal_yaml)
    result = run_precheck(data)
    assert result["sites"][0]["initial_states"]["snowalb"]["value"] is None
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] == 3.0

def test_missing_physics_key():
    data = deepcopy(minimal_yaml)
    del data["model"]["physics"]["diagmethod"]
    with pytest.raises(ValueError, match="Missing required parameters"):
        run_precheck(data)

def test_invalid_stability_logic():
    data = deepcopy(minimal_yaml)
    data["model"]["physics"]["stabilitymethod"]["value"] = 1
    with pytest.raises(ValueError, match="diagmethod == 2 requires stabilitymethod == 3"):
        run_precheck(data)

def test_lai_nullified_when_sfr_zero():
    data = deepcopy(minimal_yaml)
    data["sites"][0]["properties"]["land_cover"]["dectr"]["sfr"]["value"] = 0
    data["sites"][0]["properties"]["land_cover"]["bldgs"]["sfr"]["value"] += 0.2
    result = run_precheck(data)
    assert result["sites"][0]["initial_states"]["dectr"]["lai_id"]["value"] is None

if __name__ == "__main__":
    test_run_precheck_success()
    test_missing_physics_key()
    test_invalid_stability_logic()
    test_lai_nullified_when_sfr_zero()
    print("🎉 All run_precheck tests passed successfully!")
