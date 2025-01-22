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


def load_reference_df() -> pd.DataFrame:
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


def get_all_classes_with_to_df_state() -> List[type]:
    """Get all classes that have to_df_state method"""
    classes = []
    for name in dir(sys.modules[__name__]):
        obj = getattr(sys.modules[__name__], name)
        if isinstance(obj, type) and hasattr(obj, "to_df_state"):
            classes.append(obj)
    return classes


def compare_df_columns(
    class_df: pd.DataFrame, ref_df: pd.DataFrame, class_name: str
) -> Tuple[Set[str], Set[str]]:
    """Compare columns between class DataFrame and reference DataFrame

    Returns:
        Tuple[Set[str], Set[str]]: (missing_columns, extra_columns)
    """
    class_cols = set(class_df.columns)
    ref_cols = set(ref_df.columns)

    # missing_cols = ref_cols - class_cols
    extra_cols = class_cols - ref_cols

    return extra_cols


def test_class_to_df_state(cls: type, ref_df: pd.DataFrame):
    """Test to_df_state implementation for a class"""
    print(f"\nTesting {cls.__name__} - {cls}")

    if cls.__name__ in [
        "LandCover",
        "DayProfile",
        "HourlyProfile",
        "InitialStates",
        "ModelControl",
        "ModelPhysics",
        "NonVegetatedSurfaceProperties",
        "VegetatedSurfaceProperties",
        # "Site",
        # "SUEWSConfig",
        "SurfaceInitialState",
        "SurfaceProperties",
        "ThermalLayers",
        "VegInitialState",
        "WaterDistribution",
        "WeeklyProfile",
    ]:
        print(f"Skipping {cls.__name__} for now...")
        return

    # Create an instance with default values
    try:
        # import yaml
        # with open('./config-suews.yml', 'r') as file:
        #     yaml_config = yaml.safe_load(file)
        # instance = cls(**yaml_config[0])
        instance = cls()
    except Exception as e:
        print(f"Error creating instance of {cls.__name__}: {str(e)}")
        raise e

    # Call to_df_state
    try:
        grid_id = 1  # Use a test grid_id
        if isinstance(instance, BuildingLayer):
            class_df = instance.to_df_state(grid_id, 0, "roof")
        elif isinstance(instance, LAIParams):
            class_df = instance.to_df_state(grid_id, 3)
        elif isinstance(instance, LAIPowerCoefficients):
            class_df = instance.to_df_state(grid_id, 1)
        elif isinstance(
            instance, (OHMCoefficients, StorageDrainParams, WaterDistribution)
        ):
            class_df = instance.to_df_state(grid_id, 0)
        elif isinstance(instance, (OHM_Coefficient_season_wetness)):
            class_df = instance.to_df_state(grid_id, 0, 1)
        elif isinstance(instance, WaterUse):
            class_df = instance.to_df_state(0, grid_id)
        elif isinstance(instance, SUEWSConfig):
            class_df = instance.to_df_state()
        else:
            class_df = instance.to_df_state(grid_id)
    except Exception as e:
        print(f"Error creating instance of {cls.__name__}: {str(e)}")
        raise e

    # Compare columns
    extra_cols = compare_df_columns(class_df, ref_df, cls.__name__)


    if extra_cols:
        print(f"Extra columns in {cls.__name__}:")
        for col in sorted(extra_cols):
            print(f"  - {col}")
        raise ValueError(f"{len(extra_cols)} extra columns in {cls.__name__}")

    if not extra_cols:
        print(f"{cls.__name__} has NO extra columns")

    if cls.__name__ == "SUEWSConfig":
        # For SUEWSConfig, also check for missing columns
        missing_cols = set(ref_df.columns) - set(class_df.columns)
        if missing_cols:
            print(f"\nMissing columns in {cls.__name__}:")
            for col in sorted(missing_cols):
                print(f"  - {col}")
            raise ValueError(f"{len(missing_cols)} missing columns in {cls.__name__}")
        else:
            print(f"{cls.__name__} has NO missing columns")

def main():
    # Load reference DataFrame
    ref_df = load_reference_df()
    print(f"Reference DataFrame has {len(ref_df.columns)} columns")

    # Get all classes with to_df_state
    classes = get_all_classes_with_to_df_state()

    print(f"Found {len(classes)} classes with to_df_state method:")
    for cls in classes:
        print(f"  - {cls.__name__}")

    # Test each class
    for cls in classes:
        test_class_to_df_state(cls, ref_df)


if __name__ == "__main__":
    main()
