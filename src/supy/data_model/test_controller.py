import sys
import yaml
import os
import copy
from pydantic import ValidationError
from supy.data_model.core import SUEWSConfig

yaml_path = "../../../test/benchmark1/benchmark1b.yml"

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

# --- CRITICAL model.physics methods that must NOT be null or missing ---
required_methods = [
    "netradiationmethod",
    "emissionsmethod",
    "storageheatmethod",
    "ohmincqf",
    "roughlenmommethod",
    "roughlenheatmethod",
    "stabilitymethod",
    "smdmethod",
    "waterusemethod",
    "diagmethod",
    "faimethod",
    "localclimatemethod",
    "snowuse",
    "stebbsmethod"
]

print("CHECKING model options")
physics = updated_cfg.get("model", {}).get("physics", {})
errors = []

for method in required_methods:
    if method not in physics:
        errors.append(f"Missing method at {method}")
    elif "value" not in physics[method] or physics[method]["value"] is None:
        errors.append(f"Missing {method} option or null")

if errors:
    print("Critical model options are missing or null:")
    for e in errors:
        print("  -", e)
    sys.exit(2)
else:
    print("All required model options are present.")

# --- SCAN YAML and replace empty strings / None with null (except for model.physics) ---
def insert_nulls(data, path=""):
    updated = False
    if isinstance(data, dict):
        for key, val in data.items():
            current_path = f"{path}.{key}" if path else key
            if val == "" or (val is None and not current_path.startswith("model.physics.")):
                print(f"Replacing missing parameters value with null at: {current_path}")
                data[key] = None
                updated = True
            else:
                if insert_nulls(val, current_path):
                    updated = True
    elif isinstance(data, list):
        for idx, item in enumerate(data):
            current_path = f"{path}[{idx}]"
            if insert_nulls(item, current_path):
                updated = True
    return updated

print("\nSCANNING YAML for missing parameters values...")
insert_nulls(updated_cfg)

# --- SAVE updated YAML ---
yaml_basename = os.path.basename(yaml_path)
yaml_dirname = os.path.dirname(yaml_path)
new_filename = f"updated_{yaml_basename}"
new_path = os.path.join(yaml_dirname, new_filename)

with open(new_path, "w") as f:
    yaml.dump(updated_cfg, f, sort_keys=False)

print(f"\nUpdated YAML saved to: {new_path}")

# --- Check diagmethod value and act accordingly ---
diag_val = updated_cfg["model"]["physics"]["diagmethod"]["value"]

if diag_val == 0:
    print("diagmethod is 0 (OFF). Proceeding to validation.")
elif diag_val == 2:
    print("diagmethod is 2 (ON).")
    try:
        faibldg_value = updated_cfg["site"][0]["properties"]["land_cover"]["bldgs"]["faibldg"]["value"]
        if faibldg_value is None:
            print("diagmethod is ON (2) but 'faibldg.value' is null.")
            print("Please provide a valid 'faibldg.value' before proceeding.")
            sys.exit(4)
        else:
            print("'faibldg.value' is set. Proceeding with validation.")
    except KeyError:
        print("diagmethod is ON (2) but 'faibldg.value' is missing in YAML.")
        print("Please ensure 'faibldg.value' is defined before proceeding.")
        sys.exit(4)
else:
    print(f"diagmethod = {diag_val}: validation will proceed.")

# --- Validate config using SUEWSConfig ---
try:
    config = SUEWSConfig.from_yaml(new_path)
    print("Validation passed.")
    df = config.to_df_state()
    print("DataFrame created.")
    #print(df.head())
except ValidationError as e:
    print("Validation failed.")
    print(e)
    sys.exit(3)
