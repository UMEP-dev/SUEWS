
import yaml
import sys
from pprint import pprint


try:
    from supy.data_model.core.site import StebbsProperties
    from supy.data_model.core.profile import HourlyProfile, DayProfile, WeeklyProfile
except Exception as e:
    print("Import error - adjust import path. Exception:", e)
    sys.exit(1)

YAML_FILE = "test_profiles.yml"
GRID_ID = 1

AR_CH_FIELDS = ["ApplianceProfile"]
DF_BASE_NAMES = ["applianceprofile"]


def load_yaml(path):
    with open(path, "r") as f:
        return yaml.safe_load(f)


def get_archetype_properties(site_doc):

    props = site_doc.get("properties", {})
    archetype = props.get("stebbs")
    if not archetype:
        raise RuntimeError("No building_archetype found in site properties")
    return archetype


def try_reconstruct_profiles(df, grid_id, base_name):
    """Try to reconstruct profile objects from df_state for the given base_name."""
    print("\nLooking for profile columns for:", base_name)
    cols = [c for c in df.columns if c[0] == base_name]
    print("Found columns:", cols)

    try:
        hp = HourlyProfile.from_df_state(df, grid_id, base_name)
        print(f"\nReconstructed HourlyProfile for '{base_name}':")
        try:
            vals = list(hp.values)
            print(f"  values (len {len(vals)}): {vals[:8]} ...")
        except Exception:
            pprint(hp.model_dump() if hasattr(hp, "model_dump") else hp.__dict__)
        return
    except Exception as e:
        print("  HourlyProfile.from_df_state failed ->", repr(e))

    try:
        dp = DayProfile.from_df_state(df, grid_id, base_name)
        print(f"\nReconstructed DayProfile for '{base_name}':")
        pprint(dp.model_dump() if hasattr(dp, "model_dump") else dp.__dict__)
        return
    except Exception as e:
        print("  DayProfile.from_df_state failed ->", repr(e))

    try:
        wp = WeeklyProfile.from_df_state(df, grid_id, base_name)
        print(f"\nReconstructed WeeklyProfile for '{base_name}':")
        pprint(wp.model_dump() if hasattr(wp, "model_dump") else wp.__dict__)
        return
    except Exception as e:
        print("  WeeklyProfile.from_df_state failed ->", repr(e))

    print(f"Could not reconstruct any profile type for '{base_name}'")


def main():
    doc = load_yaml(YAML_FILE)
    print("YAML loaded keys:", list(doc.keys()))

    site0 = doc.get("sites", [])[0] if doc.get("sites") else None
    if site0 is None:
        raise RuntimeError("No sites in YAML doc")

    archetype_dict = get_archetype_properties(site0)

    try:
        archetype = StebbsProperties(**archetype_dict)
    except Exception as e:
        print("Validation/Construction error for Stebbs:", e)
        raise

    print("\nConstructed Stebbs (partial dump):")
    try:
        pprint(archetype.model_dump())
    except Exception:
        pprint(vars(archetype))

    for field_name in AR_CH_FIELDS:
        value = getattr(archetype, field_name, None)
        print(f"\nFound Stebbs.{field_name} attribute:", value is not None)

    try:
        df = archetype.to_df_state(GRID_ID)
    except Exception as e:
        print("StebbsProperties.to_df_state failed:", e)
        raise

    print("\nGenerated df_state columns:")
    print(df.columns.tolist())

    for base_name in DF_BASE_NAMES:
        try_reconstruct_profiles(df, GRID_ID, base_name)
        scalar_col = (base_name, "0")
        if scalar_col in df.columns:
            print(f"\nScalar {base_name} column present:", df.loc[GRID_ID, scalar_col])


if __name__ == "__main__":
    main()