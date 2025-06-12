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

# --- CRITICAL model.physics methods that must NOT be null or missing ---
required_methods = [
    "netradiationmethod", "emissionsmethod", "storageheatmethod", "ohmincqf",
    "roughlenmommethod", "roughlenheatmethod", "stabilitymethod", "smdmethod",
    "waterusemethod", "diagmethod", "faimethod", "localclimatemethod",
    "snowuse", "stebbsmethod"
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

def insert_nulls(data, path=""):
    """
    Recursively walk dicts *and* lists, replacing any "" or None 
    (outside model.physics) with explicit None, and printing where.
    """
    updated = False

    if isinstance(data, dict):
        for key, val in data.items():
            current_path = f"{path}.{key}" if path else key

            # if this leaf is "" or None (and not under model.physics), replace it
            if val == "" or (val is None and not current_path.startswith("model.physics.")):
                parts = current_path.split(".")
                # derive a friendly param name
                if parts[-1] == "value" and len(parts) >= 2:
                    param_name = parts[-2]
                else:
                    param_name = parts[-1]
                print(f"REPLACING -- missing value (empty string) for parameter '{param_name}' at: {current_path}")
                data[key] = None
                updated = True

            # otherwise, recurse deeper
            elif insert_nulls(val, current_path):
                updated = True

    elif isinstance(data, list):
        for idx, item in enumerate(data):
            current_path = f"{path}[{idx}]"

            # catch list‐element leaves
            if item == "" or (item is None and not current_path.startswith("model.physics.")):
                # if the path ends in ".value[<n>]" we use param_name = the part before '.value'
                parts = current_path.replace("]", "").replace("[", ".").split(".")
                if "value" in parts:
                    i = parts.index("value")
                    param_name = parts[i-1]
                else:
                    param_name = parts[-1]
                print(f"REPLACING -- missing list‐element for parameter '{param_name}' at: {current_path}")
                data[idx] = None
                updated = True

            # if it’s a nested container, recurse
            elif insert_nulls(item, current_path):
                updated = True

    return updated

# … later in your script …
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

# --- DIAGMETHOD CONTROLLER ---
diag_val = updated_cfg["model"]["physics"]["diagmethod"]["value"]

if diag_val == 0:
    print("diagmethod is 0 (OFF). Proceeding to validation.")
elif diag_val == 2:
    print("diagmethod is 2. Checking related parameters...")

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
            # If final value is a dict with 'value', check that
            if isinstance(ref, dict) and "value" in ref:
                if ref["value"] is None:
                    missing_diag2.append(field + ".value")
            elif ref is None:
                missing_diag2.append(field)
        except (KeyError, IndexError, TypeError):
            missing_diag2.append(field)

    if missing_diag2:
        print("The following required fields for diagmethod==2 are missing or null:")
        for m in missing_diag2:
            parts = m.replace("]", "").replace("[", ".").split(".")
            param_name = parts[-2] if parts[-1] == "value" else parts[-1]
            print(f"  - Parameter '{param_name}' is missing (at {m})")
        sys.exit(4)
    else:
        print("All diagmethod==2 related parameters are present. Proceeding.")

else:
    print(f"diagmethod = {diag_val}: validation will proceed.")

# --- STORAGEHEATMETHOD CONTROLLER ---

storage_val = updated_cfg["model"]["physics"]["storageheatmethod"]["value"]

if storage_val == 6:
    print("storageheatmethod is 6. Checking related parameters…")

    missing_sh = []

    # 1) top-level lambda_c under site[0].properties
    for field in ["site[0].properties.lambda_c.value"]:
        parts = field.replace("]", "").replace("[", ".").split(".")
        ref = updated_cfg
        try:
            for part in parts:
                if part.isdigit():
                    ref = ref[int(part)]
                else:
                    ref = ref[part]
            # ref is either dict-with-value or a leaf
            if isinstance(ref, dict):
                val = ref.get("value", None)
                if val is None or (isinstance(val, list) and not val):
                    missing_sh.append(field)
            else:
                if ref is None or (isinstance(ref, (list, str)) and not ref):
                    missing_sh.append(field)
        except (KeyError, IndexError, TypeError):
            missing_sh.append(field)

    # 2) each wall under vertical_layers.walls
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
        print("The following required fields for storageheatmethod==6 are missing or null:")
        for m in missing_sh:
            name = m.split(".")[-2] if m.endswith(".value") else m.split(".")[-1]
            print(f"  - Parameter '{name}' is missing at {m}")
        sys.exit(5)
    else:
        print("All storageheatmethod==6 related parameters are present. Proceeding.")

# --- Validate config using SUEWSConfig ---
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
