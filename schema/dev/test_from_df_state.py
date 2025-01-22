import pandas as pd
import numpy as np
from def_config_suews import *
from typing import Dict, List, Set, Tuple
import sys
import os
from def_config_suews import (
    BuildingLayer,
    SurfaceType,
    # ... other existing imports
)


def load_df_state() -> pd.DataFrame:
    """Load the reference DataFrame from df_state.pkl"""
    # Try different possible locations
    possible_paths = [
        "df_state.pkl",
        "schema/dev/df_state.pkl",
        "../df_state.pkl",
        "./df_state.pkl",
    ]

    for path in possible_paths:
        try:
            if os.path.exists(path):
                print(f"Loading DataFrame from {path}")
                df = pd.read_pickle(path)
                return df
        except Exception as e:
            continue

    print("Error: df_state.pkl not found in any of these locations:", possible_paths)
    sys.exit(1)


def get_all_classes_with_from_df_state() -> List[type]:
    """Get all classes that have from_df_state method"""
    classes = []
    for name in dir(sys.modules[__name__]):
        obj = getattr(sys.modules[__name__], name)
        if isinstance(obj, type) and hasattr(obj, "from_df_state"):
            classes.append(obj)
    return classes


def test_df_state_to_class(cls: type, ref_df: pd.DataFrame):
    """Test from_df_state implementation for a class"""
    print(f"\nTesting {cls.__name__} - {cls}")

    if cls.__name__ not in [
        # "DayProfile",
        # "WeeklyProfile",
        # "HourlyProfile",
        # "CO2Params",
        # "AnthropogenicHeat",
        # "AnthropogenicEmissions",
        # "ThermalLayers",
        # "BuildingLayer",
        # "VerticalLayers",
        # "WaterUse",
        # "LUMPSParams",
        # "SPARTACUSParams",
        # "Conductance",
        # "OHMCoefficients",
        # "OHM_Coefficient_season_wetness",
        # "SurfaceInitialState",
        # "VegetatedSurfaceInitialState",
        # "DeciduousTreeSurfaceInitialState",
        # "InitialStates",
        # "ModelPhysics",
        # "IrrigationParams",
        # "SnowParams",
        # # "BsoilProperties",
        # "SnowAlb",
        # "SurfaceProperties",
        # "WaterDistribution",
        "ArchetypeProperties",
        "StebbsProperties",
    ]:
        print(f"Skipping {cls.__name__} for now...")
        return

    try:
        # import yaml
        # with open('./config-suews.yml', 'r') as file:
        #     yaml_config = yaml.safe_load(file)
        # instance = cls(**yaml_config[0])
        instance = cls()
    except Exception as e:
        print(f"Error creating instance of {cls.__name__}: {str(e)}")
        raise e

    # Call from_df_state
    try:
        grid_id = 1  # Use a test grid_id
        if isinstance(instance, BuildingLayer):
            class_instance = instance.from_df_state(ref_df, grid_id, 0, "roof")
        elif isinstance(instance, LAIParams):
            class_instance = instance.from_df_state(ref_df, grid_id, 3)
        elif isinstance(instance, LAIPowerCoefficients):
            class_instance = instance.from_df_state(ref_df, grid_id, 1)
        elif isinstance(
            instance, (OHMCoefficients, StorageDrainParams, WaterDistribution)
        ):
            class_instance = instance.from_df_state(ref_df, grid_id, 0)
        elif isinstance(instance, (OHM_Coefficient_season_wetness)):
            class_instance = instance.from_df_state(ref_df, grid_id, 0, 1)
        elif isinstance(instance, WaterUse):
            class_instance = instance.from_df_state(ref_df, 0, grid_id)
        elif isinstance(instance, SurfaceInitialState):
            class_instance = instance.from_df_state(ref_df, grid_id, 0)
        elif isinstance(instance, SurfaceProperties):
            class_instance = instance.from_df_state(ref_df, grid_id, 0)
        elif isinstance(instance, DayProfile):
            class_instance = instance.from_df_state(
                ref_df, grid_id, param_name="ah_min"
            )
        elif isinstance(instance, HourlyProfile):
            class_instance = instance.from_df_state(
                ref_df, grid_id, param_name="ahprof_24hr"
            )
        elif isinstance(instance, WeeklyProfile):
            class_instance = instance.from_df_state(
                ref_df, grid_id, param_name="daywatper"
            )
        elif isinstance(instance, ThermalLayers):
            class_instance = instance.from_df_state(
                ref_df, grid_id, 0, surf_type="roof"
            )
        else:
            class_instance = instance.from_df_state(ref_df, grid_id)
    except Exception as e:
        print(f"Error creating instance of {cls.__name__} from DataFrame: {str(e)}")
        raise e

    # Compare the created instance to the expected instance
    class_values = class_instance.__dict__
    expected_values = instance.__dict__

    for key, value in expected_values.items():
        if key not in class_values:
            print(f"Error: {cls.__name__} missing attribute {key}")
            raise ValueError(f"{cls.__name__} missing attribute {key}")
    print(
        f"{cls.__name__} successfully created from DataFrame and keys match expected instance"
    )


def test_suews_config_roundtrip():
    """Test SUEWSConfig to_df_state and from_df_state roundtrip conversion"""
    try:
        print("\nTesting SUEWSConfig roundtrip conversion")

        # Create a SUEWSConfig instance with multiple sites
        config = SUEWSConfig(
            name="test config",
            description="test configuration for roundtrip testing",
            site=[Site(), Site()],  # Create two sites
        )

        # Convert to DataFrame state
        print("Converting to DataFrame state...")
        df_state = config.to_df_state()
        print(
            f"DataFrame has {len(df_state.columns)} columns and {len(df_state.index)} rows"
        )

        # Save DataFrame state for inspection if needed
        df_state.to_pickle("../df_state_test.pkl")
        print("Saved DataFrame state to ../df_state_test.pkl")

        # Convert back to SUEWSConfig
        print("Converting back to SUEWSConfig...")
        config_reconstructed = SUEWSConfig.from_df_state(df_state)

        # Compare the configurations
        print("\nComparing configurations:")
        print(f"Original sites: {len(config.site)}")
        print(f"Reconstructed sites: {len(config_reconstructed.site)}")

        if len(config.site) != len(config_reconstructed.site):
            print("ERROR: Number of sites does not match!")
            return False

        # Compare model physics parameters for each site
        for i, (orig_site, recon_site) in enumerate(
            zip(config.site, config_reconstructed.site)
        ):
            print(f"\nSite {i}:")

            # Compare properties
            orig_properties = orig_site.properties
            recon_properties = recon_site.properties
            print("Properties:")

            # Get all attributes excluding magic methods and callables
            attrs = [
                attr
                for attr in dir(orig_properties)
                if not attr.startswith(("_", "model_"))
                and not callable(getattr(orig_properties, attr))
            ]

            differences = []
            for attr in attrs:
                orig_val = getattr(orig_properties, attr)
                recon_val = getattr(recon_properties, attr)

                # Handle numpy arrays and other sequence types
                if isinstance(orig_val, (np.ndarray, list, tuple)):
                    are_equal = np.array_equal(np.array(orig_val), np.array(recon_val))
                else:
                    are_equal = orig_val == recon_val

                if not are_equal:
                    differences.append(
                        {"attr": attr, "original": orig_val, "reconstructed": recon_val}
                    )
                    print(f"  {attr}:")
                    print(f"    Original: {orig_val}")
                    print(f"    Reconstructed: {recon_val}")

            if differences:
                print(f"\nFound {len(differences)} differences in properties")
                return False
            else:
                print("All properties match exactly")

        print("\nRoundtrip conversion completed successfully")
        # Save the reconstructed configuration to a YAML file
        with open("schema/dev/reconstructed_config.yml", "w") as file:
            yaml.dump(config_reconstructed.model_dump(exclude_none=True), file, sort_keys=False, allow_unicode=True)
        print("Saved reconstructed configuration to ../reconstructed_config.yml")
        return True

    except Exception as e:
        print(f"Error during test: {str(e)}")
        import traceback
        traceback.print_exc()
        return False


def main():
    # Load initial df_state file
    ref_df = load_df_state()
    print(f"Initial df_state has {len(ref_df.columns)} columns")

    # Get all classes with to_df_state
    classes = get_all_classes_with_from_df_state()

    print(f"Found {len(classes)} classes with to_df_state method:")
    for cls in classes:
        print(f"  - {cls.__name__}")

    # # Test each class
    # for cls in classes:
    #     test_df_state_to_class(cls, ref_df)

    # Run the roundtrip test
    print("\nRunning SUEWSConfig roundtrip test...")
    success = test_suews_config_roundtrip()
    if not success:
        print("\nSUEWSConfig roundtrip test failed!")
        sys.exit(1)
    print("\nSUEWSConfig roundtrip test passed!")


if __name__ == "__main__":
    main()
