# precheck.py

from .site import SeasonCheck, DLSCheck, LandCover
from pydantic import ValidationError

def run_precheck(data):
    print("\nStarting precheck procedure...\n")

    control = data.get("model", {}).get("control", {})
    start_date = control.get("start_time")
    end_date = control.get("end_time")

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError("Missing or invalid 'start_time' in model.control — must be in 'YYYY-MM-DD' format.")
    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError("Missing or invalid 'end_time' in model.control — must be in 'YYYY-MM-DD' format.")
    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError("Could not extract model year from 'start_time'. Ensure it is in 'YYYY-MM-DD' format.")

    model_data = data.get("model", {})
    physics = model_data.get("physics", {})

    required_keys = [
        "netradiationmethod", "emissionsmethod", "storageheatmethod", "ohmincqf",
        "roughlenmommethod", "roughlenheatmethod", "stabilitymethod", "smdmethod",
        "waterusemethod", "diagmethod", "faimethod", "localclimatemethod",
        "snowuse", "stebbsmethod"
    ]

    missing_keys = [k for k in required_keys if k not in physics]
    if missing_keys:
        raise ValueError(f"[model.physics] Missing required parameters: {missing_keys}")

    empty_keys = [k for k in required_keys if physics.get(k, {}).get("value") in ("", None)]
    if empty_keys:
        raise ValueError(f"[model.physics] Parameters with empty string or null values: {empty_keys}")

    if physics["diagmethod"]["value"] == 2 and physics["stabilitymethod"]["value"] != 3:
        raise ValueError("Invalid model logic: diagmethod == 2 requires stabilitymethod == 3")

    def clean_empty_strings(d):
        for k, v in d.items():
            if isinstance(v, dict):
                clean_empty_strings(v)
            elif v == "":
                d[k] = None
    clean_empty_strings(data)

    for i, site in enumerate(data.get("sites", [])):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        lat = props.get("lat", {}).get("value")
        try:
            if lat is not None:
                season = SeasonCheck(start_date=start_date, end_date=end_date, lat=lat).get_season()
                print(f"[site #{i}] Season detected: {season}")
                if season in ("summer", "tropical", "equatorial") and "snowalb" in initial_states:
                    if isinstance(initial_states["snowalb"], dict):
                        initial_states["snowalb"]["value"] = None
                        print(f"[site #{i}] Set snowalb to None")
        except Exception as e:
            raise ValueError(f"[site #{i}] SeasonCheck failed: {e}")

        dectr = props.get("land_cover", {}).get("dectr", {})
        sfr = dectr.get("sfr", {}).get("value", 0)
        if sfr > 0:
            lai = dectr.get("lai", {})
            laimin = lai.get("laimin", {}).get("value")
            laimax = lai.get("laimax", {}).get("value")
            lai_val = None
            if laimin is not None and laimax is not None:
                if season == "summer":
                    lai_val = laimax
                elif season == "winter":
                    lai_val = laimin
                elif season in ("spring", "fall"):
                    lai_val = (laimax + laimin) / 2
            if "dectr" in initial_states:
                initial_states["dectr"]["lai_id"] = {"value": lai_val}
        else:
            if "dectr" in initial_states:
                initial_states["dectr"]["lai_id"] = {"value": None}
                print(f"[site #{i}] Nullified lai_id")

        lng = props.get("lng", {}).get("value")
        emissions = props.get("anthropogenic_emissions", {})
        if lat is not None and lng is not None:
            try:
                dls = DLSCheck(lat=lat, lng=lng, year=model_year)
                start_dls, end_dls, tz_name = dls.compute_dst_transitions()
                if start_dls and end_dls:
                    emissions.setdefault("startdls", {})["value"] = start_dls
                    emissions.setdefault("enddls", {})["value"] = end_dls
                    print(f"[site #{i}] DLS: start={start_dls}, end={end_dls}")
                if tz_name:
                    props.setdefault("timezone", {})["value"] = tz_name
                    print(f"[site #{i}] Timezone set to {tz_name}")
            except Exception as e:
                raise ValueError(f"[site #{i}] DLSCheck failed: {e}")
        props["anthropogenic_emissions"] = emissions
        site["properties"] = props

        land_cover = props.get("land_cover")
        if not land_cover:
            raise ValueError(f"[site #{i}] Missing land_cover")
        sfr_sum = sum(
            v.get("sfr", {}).get("value", 0)
            for v in land_cover.values()
            if isinstance(v, dict)
        )
        if 0.9999 <= sfr_sum < 1.0:
            max_key = max(land_cover, key=lambda k: land_cover[k]["sfr"]["value"])
            land_cover[max_key]["sfr"]["value"] += 1.0 - sfr_sum
        elif 1.0 < sfr_sum <= 1.0001:
            max_key = max(land_cover, key=lambda k: land_cover[k]["sfr"]["value"])
            land_cover[max_key]["sfr"]["value"] -= sfr_sum - 1.0
        elif abs(sfr_sum - 1.0) > 0.0001:
            raise ValueError(f"[site #{i}] Invalid land_cover sfr sum: {sfr_sum:.4f}")
        try:
            LandCover(**land_cover)
        except ValidationError as e:
            raise ValueError(f"[site #{i}] Invalid land_cover: {e}")

    print("\nPrecheck complete.\n")
    return data
