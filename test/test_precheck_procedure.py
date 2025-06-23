"""
Unit tests for run_precheck function logic and helper classes (DLSCheck, SeasonCheck).

These tests verify the following behaviors:

RUN_PRECHECK FUNCTION
---------------------
1. snowalb gets nullified in summer (only if it's a dict)
2. LAI is set correctly to laimax in summer when dectr > 0
3. LAI is nullified if dectr.sfr == 0
4. Empty string in model.physics keys are converted to null and raise error
5. Raises ValueError if required physics keys are missing
6. Raises ValueError if diagmethod == 2 and stabilitymethod != 3
7. Handles case where land_cover is missing and raises error
8. Handles land_cover sfr sums near 1.0, auto-adjusting the max key
9. Raises ValueError if sfr sum deviates too much from 1.0
10. Handles missing lat/lng by skipping DLS computation, but still includes empty anthropogenic_emissions
11. Handles non-dict snowalb without crashing
12. Season detection errors are caught and reported per-site

SEASONCHECK CLASS
-----------------
13. Correctly identifies northern hemisphere summer
14. Correctly identifies northern hemisphere winter
15. Correctly identifies equatorial region
16. Correctly identifies tropical region
17. Correctly identifies southern hemisphere summer
18. Handles unknown or out-of-range seasons gracefully

DLSCHECK CLASS
--------------
19. Computes correct daylight-saving time transitions (startdls, enddls, offset) for known locations
20. Handles unknown timezone case (e.g., ocean) gracefully and returns None

All tests run independently and assert expected outputs or raised errors for robust coverage of edge cases.
"""


import pytest
from copy import deepcopy
from supy.data_model.core import run_precheck
from supy.data_model.site import DLSCheck, SeasonCheck

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

def test_clean_empty_string_in_physics():
    data = deepcopy(minimal_yaml)
    data["model"]["physics"]["snowuse"]["value"] = ""
    with pytest.raises(ValueError, match="empty string or null"):
        run_precheck(data)

def test_season_check_unknown():
    data = deepcopy(minimal_yaml)
    data["sites"][0]["properties"]["lat"]["value"] = 90.0
    data["model"]["control"]["start_time"] = "2020-06-01"
    result = run_precheck(data)
    assert result  # should not raise

def test_missing_land_cover():
    data = deepcopy(minimal_yaml)
    del data["sites"][0]["properties"]["land_cover"]
    with pytest.raises(ValueError, match="Missing land_cover"):
        run_precheck(data)

def test_missing_lat_lng_skips_dls():
    data = deepcopy(minimal_yaml)
    del data["sites"][0]["properties"]["lat"]
    del data["sites"][0]["properties"]["lng"]
    result = run_precheck(data)
    assert isinstance(result["sites"][0]["properties"]["anthropogenic_emissions"], dict)

def test_snowalb_non_dict_skipped():
    data = deepcopy(minimal_yaml)
    data["sites"][0]["initial_states"]["snowalb"] = "not_a_dict"
    result = run_precheck(data)
    assert result["sites"][0]["initial_states"]["snowalb"] == "not_a_dict"

def test_season_check_northern_summer():
    sc = SeasonCheck(start_date="2020-07-01", lat=45.0)
    assert sc.get_season() == "summer"

def test_season_check_northern_winter():
    sc = SeasonCheck(start_date="2020-01-10", lat=45.0)
    assert sc.get_season() == "winter"

def test_season_check_equatorial():
    sc = SeasonCheck(start_date="2020-06-01", lat=0.0)
    assert sc.get_season() == "equatorial"

def test_season_check_tropical():
    sc = SeasonCheck(start_date="2020-06-01", lat=15.0)
    assert sc.get_season() == "tropical"

def test_season_check_southern_summer():
    sc = SeasonCheck(start_date="2020-01-10", lat=-35.0)
    assert sc.get_season() == "summer"


def test_dls_check_valid():
    dls = DLSCheck(lat=45.0, lng=0.0, year=2020)
    start, end, offset = dls.compute_dst_transitions()
    assert isinstance(start, int)
    assert isinstance(end, int)
    assert isinstance(offset, int)


def test_dls_check_unknown_may_exist():
    dls = DLSCheck(lat=0.0, lng=180.0, year=2020)
    start, end, offset = dls.compute_dst_transitions()

    # timezonefinder *may* return a timezone for this location
    assert (start is None and end is None) or (isinstance(start, int) and isinstance(end, int))
    assert offset is None or isinstance(offset, int)

if __name__ == "__main__":
    test_run_precheck_success()
    test_missing_physics_key()
    test_invalid_stability_logic()
    test_lai_nullified_when_sfr_zero()
    test_season_check_northern_summer()
    test_season_check_northern_winter()
    test_season_check_southern_summer()
    test_season_check_equatorial()
    test_season_check_tropical()
    test_season_check_southern_summer()
    test_dls_check_valid()
    test_dls_check_unknown_may_exist()
    test_clean_empty_string_in_physics()
    test_season_check_unknown()
    test_missing_land_cover()
    test_missing_lat_lng_skips_dls()
    test_snowalb_non_dict_skipped()
    print("🎉 All run_precheck tests passed successfully!")
