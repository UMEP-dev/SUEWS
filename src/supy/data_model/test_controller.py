import sys
import yaml
import os
import copy
from pydantic import ValidationError
from supy.data_model.core import SUEWSConfig

#yaml_path = "../../../test/benchmark1/benchmark1b.yml"
yaml_path = "../sample_run/sample_config.yml"

# --- Load YAML as raw dict ---
try:
    with open(yaml_path, "r") as f:
        cfg = yaml.safe_load(f)
except FileNotFoundError:
    print(f"File not found: {yaml_path}")
    sys.exit(1)
except yaml.YAMLError as e:
    print(f"YAML parse error: {e}")
    sys.exit(1)

updated_cfg = copy.deepcopy(cfg)

# --- CHECK that model.physics methods are NOT null or missing ---
required_methods = [
    "netradiationmethod", "emissionsmethod", "storageheatmethod", "ohmincqf",
    "roughlenmommethod", "roughlenheatmethod", "stabilitymethod", "smdmethod",
    "waterusemethod", "diagmethod", "faimethod", "localclimatemethod",
    "snowuse", "stebbsmethod"
]

print("CHECKING -- Model options.")
physics = updated_cfg.get("model", {}).get("physics", {})
errors = []

for method in required_methods:
    if method not in physics:
        errors.append(f"Missing method at {method}")
    elif "value" not in physics[method] or physics[method]["value"] is None:
        errors.append(f"Missing {method} option or null")

if errors:
    print("WARNING -- These model options are missing or null:")
    for e in errors:
        print("  -", e)
    sys.exit(2)
else:
    print("CHECKING DONE -- All required model options are present.")

def insert_nulls(data, path=""):
    """
    Recursively walk dicts *and* lists, replacing any "" or None 
    (outside model.physics) with explicit None, and printing where.
    """
    updated = False

    if isinstance(data, dict):
        for key, val in data.items():
            current_path = f"{path}.{key}" if path else key

            if val == "" or (val is None and not current_path.startswith("model.physics.")):
                parts = current_path.split(".")
                if parts[-1] == "value" and len(parts) >= 2:
                    param_name = parts[-2]
                else:
                    param_name = parts[-1]
                print(f"REPLACING -- missing value (empty string) for parameter '{param_name}' at: {current_path}")
                data[key] = None
                updated = True

            elif insert_nulls(val, current_path):
                updated = True

    elif isinstance(data, list):
        for idx, item in enumerate(data):
            current_path = f"{path}[{idx}]"

            if item == "" or (item is None and not current_path.startswith("model.physics.")):
                parts = current_path.replace("]", "").replace("[", ".").split(".")
                if "value" in parts:
                    i = parts.index("value")
                    param_name = parts[i-1]
                else:
                    param_name = parts[-1]
                print(f"REPLACING -- missing list‐element for parameter '{param_name}' at: {current_path}")
                data[idx] = None
                updated = True

            elif insert_nulls(item, current_path):
                updated = True

    return updated

print("\nSCANNING YAML -- Looking for missing parameters values and replacing empty strings with null values...")
insert_nulls(updated_cfg)


# --- SAVE updated YAML ---
yaml_basename = os.path.basename(yaml_path)
yaml_dirname = os.path.dirname(yaml_path)
new_filename = f"updated_{yaml_basename}"
new_path = os.path.join(yaml_dirname, new_filename)

with open(new_path, "w") as f:
    yaml.dump(updated_cfg, f, sort_keys=False)

print(f"\nUPDATING YAML -- New yaml with null values saved to: {new_path}")

# --- SURFACE FRACTIONS CONTROLLER ---
print("CHECKING -- Surface fractions.")
# grab your three surface fractions
bldgs_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["bldgs"]["sfr"]["value"]
evetr_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["evetr"]["sfr"]["value"]
dectr_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["dectr"]["sfr"]["value"]

# define for each group which companion params to handle
groups = {
    "bldgs": {"sfr": bldgs_sfr_val, "params": ["faibldg", "bldgh"]},
    "evetr": {"sfr": evetr_sfr_val, "params": ["faievetree", "evetreeh"]},
    "dectr": {"sfr": dectr_sfr_val, "params": ["faidectree", "dectreeh", "pormin_dec", "pormax_dec"]},
}

missing_sf = []

for grp, info in groups.items():
    sfr = info["sfr"]
    base = updated_cfg["site"][0]["properties"]["land_cover"][grp]

    for companion in info["params"]:
        # ensure the node exists
        try:
            node = base[companion]
        except KeyError:
            missing_sf.append(f"site[0].properties.land_cover.{grp}.{companion}")
            continue

        # CASE A: sfr is None → set companion.value = None
        if sfr is None:
            node["value"] = None

        # CASE B: sfr == 0 → set companion.value = 0
        elif sfr == 0:
            node["value"] = 0

        # CASE C: sfr > 0 → enforce companion.value exists & non‐null
        else:
            # if no “value” key or it’s None, record missing
            if "value" not in node or node["value"] is None:
                missing_sf.append(f"site[0].properties.land_cover.{grp}.{companion}.value")

if missing_sf:
    print("ERROR — the following surface‐fraction parameters are missing or null:")
    for fld in missing_sf:
        print("  -", fld)
    sys.exit(5)
else:
    print("All surface‐fraction dependent parameters are checked.")


# --- DIAGMETHOD CONTROLLER ---
print("CHECKING -- DiagMethod options.")
diag_val = updated_cfg["model"]["physics"]["diagmethod"]["value"]
stability_val = updated_cfg["model"]["physics"]["stabilitymethod"]["value"]

if diag_val == 0:
    print("diagmethod is 0 (OFF).")
elif diag_val == 2:
    print("diagmethod is 2 (ON). Checking related parameters...")
    if stability_val != 3:
        print("WARNING -- For diagmethod==2, stabilitymethod==3 should be selected.")
        sys.exit(4)

    diag2_fields = [
        "site[0].properties.surfacearea",
        "site[0].properties.land_cover.bldgs.sfr",
        "site[0].properties.land_cover.bldgs.bldgh",
        "site[0].properties.land_cover.bldgs.faibldg",
        "site[0].properties.land_cover.paved.sfr",
        "site[0].properties.land_cover.evetr.sfr",
        "site[0].properties.land_cover.evetr.evetreeh",
        "site[0].properties.land_cover.evetr.faievetree",
        "site[0].properties.land_cover.dectr.sfr",
        "site[0].properties.land_cover.dectr.pormin_dec",
        "site[0].properties.land_cover.dectr.pormax_dec",
        "site[0].properties.land_cover.dectr.dectreeh",
        "site[0].properties.land_cover.dectr.faidectree",
        "site[0].properties.n_buildings",
        "site[0].initial_states.dectr.porosity_id"
    ]

    missing_diag2 = []
    for field in diag2_fields:
        parts = field.replace("]", "").replace("[", ".").split(".")
        ref = updated_cfg
        try:
            for part in parts:
                if part.isdigit():
                    ref = ref[int(part)]
                else:
                    ref = ref[part]
            if isinstance(ref, dict) and "value" in ref:
                if ref["value"] is None:
                    missing_diag2.append(field + ".value")
            elif ref is None:
                missing_diag2.append(field)
        except (KeyError, IndexError, TypeError):
            missing_diag2.append(field)

    if missing_diag2:
        print("WARNING -- The following required fields for diagmethod==2 are missing or null:")
        for m in missing_diag2:
            parts = m.replace("]", "").replace("[", ".").split(".")
            param_name = parts[-2] if parts[-1] == "value" else parts[-1]
            print(f"  - Parameter '{param_name}' is missing (at {m})")
        sys.exit(5)
    else:
        print("All diagmethod==2 related parameters are present.")
    
    print("Checking consistency among parameters...")

    missing_consistency = []

    #If bldg sfr > 0, faibldg and bldgh must be present
    bldgs_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["bldgs"]["sfr"]["value"]
    if bldgs_sfr_val > 0:
        for param in ("faibldg", "bldgh"):
            field = f"site[0].properties.land_cover.bldgs.{param}"
            try:
                val = updated_cfg["site"][0]["properties"]["land_cover"]["bldgs"][param]["value"]
            except KeyError:
                missing_consistency.append(field)
            else:
                if val is None:
                    missing_consistency.append(field + ".value")

    #If evetr sfr > 0, evetreeh and faievetree must be present
    evetr_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["evetr"]["sfr"]["value"]
    if evetr_sfr_val > 0:
        for param in ("evetreeh", "faievetree"):
            field = f"site[0].properties.land_cover.evetr.{param}"
            try:
                val = updated_cfg["site"][0]["properties"]["land_cover"]["evetr"][param]["value"]
            except KeyError:
                missing_consistency.append(field)
            else:
                if val is None:
                    missing_consistency.append(field + ".value")

    #If dectre sfr > 0, check dectr parameters too
    dectr_sfr_val = updated_cfg["site"][0]["properties"]["land_cover"]["dectr"]["sfr"]["value"]
    if dectr_sfr_val > 0:
        for param in ("pormin_dec", "pormax_dec", "dectreeh", "faidectree"):
            field = f"site[0].properties.land_cover.dectr.{param}"
            try:
                val = updated_cfg["site"][0]["properties"]["land_cover"]["dectr"][param]["value"]
            except KeyError:
                missing_consistency.append(field)
            else:
                if val is None:
                    missing_consistency.append(field + ".value")

    if missing_consistency:
        print("WARNING -- consistency check failed, missing or null fields:")
        for m in missing_consistency:
            # derive friendly param name
            parts = m.replace("]", "").replace("[", ".").split(".")
            if parts[-1] == "value":
                name = parts[-2]
            else:
                name = parts[-1]
            print(f"  - Parameter '{name}' missing at {m}")
        sys.exit(6)
    else:
        print("Consistency checks for diagmethod == 2 passed.")

else:
    print(f"diagmethod = {diag_val}: validation will proceed.")

# --- STORAGEHEATMETHOD CONTROLLER ---
print("CHECKING -- StorageHeatMethod options.")
storage_val = updated_cfg["model"]["physics"]["storageheatmethod"]["value"]

if storage_val == 6:
    print("storageheatmethod is 6. Checking related parameters...")

    missing_sh = []

    for field in ["site[0].properties.lambda_c.value"]:
        parts = field.replace("]", "").replace("[", ".").split(".")
        ref = updated_cfg
        try:
            for part in parts:
                if part.isdigit():
                    ref = ref[int(part)]
                else:
                    ref = ref[part]
            if isinstance(ref, dict):
                val = ref.get("value", None)
                if val is None or (isinstance(val, list) and not val):
                    missing_sh.append(field)
            else:
                if ref is None or (isinstance(ref, (list, str)) and not ref):
                    missing_sh.append(field)
        except (KeyError, IndexError, TypeError):
            missing_sh.append(field)

    try:
        walls = updated_cfg["site"][0]["properties"]["vertical_layers"]["walls"]
    except (KeyError, TypeError):
        missing_sh.append("site[0].properties.vertical_layers.walls")
    else:
        for idx, wall in enumerate(walls):
            base = f"site[0].properties.vertical_layers.walls[{idx}].thermal_layers"
            try:
                layers = wall["thermal_layers"]
            except (KeyError, TypeError):
                missing_sh.append(base)
                continue

            for prop in ("dz", "k", "cp"):
                leaf = f"{base}.{prop}.value[0]"
                try:
                    vals = layers[prop]["value"]
                    if not isinstance(vals, list) or not vals or vals[0] is None:
                        missing_sh.append(leaf)
                except (KeyError, TypeError):
                    missing_sh.append(leaf)

    if missing_sh:
        print("WARNING -- The following required fields for storageheatmethod==6 are missing or null:")
        for m in missing_sh:
            name = m.split(".")[-2] if m.endswith(".value") else m.split(".")[-1]
            print(f"  - Parameter '{name}' is missing at {m}")
        sys.exit(6)
    else:
        print("All storageheatmethod==6 related parameters are present.")

# --- Validate config using SUEWSConfig ---

print("\n Starting Pydantic Validation")
try:
    config = SUEWSConfig.from_yaml(new_path)
    print("Validation passed.")
    df = config.to_df_state()
    print("DataFrame created.")
    # print(df.head())
except ValidationError as e:
    print("Validation failed.")
    print(e)
    sys.exit(3)
