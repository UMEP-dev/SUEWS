from typing import Dict, List, Optional, Union, Literal, Tuple, Type, Generic, TypeVar
from pydantic import (
    ConfigDict,
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
)
import numpy as np
import pandas as pd
import yaml
import ast
import supy as sp

from .model import Model
from .site import Site, SiteProperties, InitialStates
import os

def precheck_printing(data: dict) -> dict:
    print("Running basic precheck...")
    return data

def precheck_start_end_date(data: dict) -> Tuple[dict, int, str, str]:
    start_date = "2011-01-22"
    end_date = "2011-02-22"

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError("Missing or invalid 'start_time' in model.control — must be in 'YYYY-MM-DD' format.")
    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError("Missing or invalid 'end_time' in model.control — must be in 'YYYY-MM-DD' format.")

    try:
        model_year = int(start_date.split("-")[0])
    except Exception:
        raise ValueError("Could not extract model year from 'start_time'. Ensure it is in 'YYYY-MM-DD' format.")

    return data, model_year, start_date, end_date

def precheck_model_physics_params(data: dict) -> dict:
    model_data = data.get("model", {})
    physics = model_data.get("physics", {})

    if not physics:
        print("Skipping physics param check — physics is empty.")
        return data

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

    print(f"All required model.physics parameters present and non-empty.")
    return data

def precheck_model_options_constraints(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})

    diag = physics.get("diagmethod", {}).get("value")
    stability = physics.get("stabilitymethod", {}).get("value")

    if diag == 2 and stability != 3:
        raise ValueError(
            "[model.physics] If diagmethod == 2, then stabilitymethod must be 3."
        )

    print("diagmethod-stabilitymethod constraint passed.")
    return data

def precheck_replace_empty_strings_with_none(data: dict) -> dict:
    model_keys_to_ignore = {"control", "physics"}

    def recurse(obj, path=()):
        if isinstance(obj, dict):
            new_dict = {}
            for k, v in obj.items():
                new_path = path + (k,)
                if (
                    v == ""
                    and not (len(new_path) >= 2 and new_path[0] == "model" and new_path[1] in model_keys_to_ignore)
                ):
                    new_dict[k] = None
                else:
                    new_dict[k] = recurse(v, new_path)
            return new_dict
        elif isinstance(obj, list):
            return [None if item == "" else recurse(item, path) for item in obj]
        else:
            return obj

    cleaned = recurse(data)
    print("Empty strings replaced with null (None), except for model.control and model.physics.")
    return cleaned

def run_precheck(data: dict) -> dict:
    print("\nStarting precheck procedure...")

    data = precheck_replace_empty_strings_with_none(data)
    data = precheck_printing(data)
    data, model_year, start_date, end_date = precheck_start_end_date(data)
    print(f"Start date: {start_date}; End date: {end_date}")
    print(f"Extracted model year: {model_year}")
    data = precheck_model_physics_params(data)
    data = precheck_model_options_constraints(data)

    print("Precheck complete.\n")
    return data


class SUEWSConfig(BaseModel):
    name: str = Field(
        default="sample config", description="Name of the SUEWS configuration"
    )
    description: str = Field(
        default="this is a sample config for testing purposes ONLY - values are not realistic",
        description="Description of this SUEWS configuration",
    )
    model: Model = Field(
        default_factory=Model,
        description="Model control and physics parameters",
    )
    sites: List[Site] = Field(
        default=[Site()],
        description="List of sites to simulate",
        min_length=1,
    )

    model_config = ConfigDict(extra="allow")

    # Sort the filtered columns numerically
    @staticmethod
    def sort_key(col):
        try:
            return (col[0], ast.literal_eval(col[1]))
        except ValueError:
            return (col[0], col[1])
        
    @model_validator(mode="before")
    @classmethod
    def preprocess(cls, data: dict) -> dict:
        return run_precheck(data)

    @classmethod
    def from_yaml(cls, path: str) -> "SUEWSConfig":
        """Initialize SUEWSConfig from YAML file.

        Args:
            path (str): Path to YAML configuration file

        Returns:
            SUEWSConfig: Instance of SUEWSConfig initialized from YAML
        """
        with open(path, "r") as file:
            config = yaml.load(file, Loader=yaml.FullLoader)
        return cls(**config)

    def create_multi_index_columns(self, columns_file: str) -> pd.MultiIndex:
        """Create MultiIndex from df_state_columns.txt"""
        with open(columns_file, "r") as f:
            lines = f.readlines()

        tuples = []
        for line in lines:
            col_name, indices = line.strip().split(",", 1)
            str_indices = f"{indices}" if indices != "0" else "0"
            tuples.append((col_name, str_indices))

        return pd.MultiIndex.from_tuples(tuples)

    def to_df_state(self) -> pd.DataFrame:
        """Convert config to DataFrame state format"""
        list_df_site = []
        for i in range(len(self.sites)):
            grid_id = self.sites[i].gridiv
            df_site = self.sites[i].to_df_state(grid_id)
            df_model = self.model.to_df_state(grid_id)
            df_site = pd.concat([df_site, df_model], axis=1)
            list_df_site.append(df_site)

        df = pd.concat(list_df_site, axis=0)
        # remove duplicate columns
        df = df.loc[:, ~df.columns.duplicated()]

        # # Fix level=1 columns sorted alphabetically not numerically (i.e. 10 < 2)
        # # Filter columns based on level=0 criteria
        # level_0_counts = df.columns.get_level_values(0).value_counts()
        # columns_to_sort = [col for col in df.columns if level_0_counts[col[0]] >= 10]

        # # Sort the filtered columns numericallyí
        # def sort_key(col):
        #     try:
        #         return (col[0], ast.literal_eval(col[1]))
        #     except ValueError:
        #         return (col[0], col[1])

        # sorted_columns = sorted(columns_to_sort, key=sort_key)

        # # Combine the sorted columns with the remaining columns
        # remaining_columns = [col for col in df.columns if col not in columns_to_sort]
        # final_columns = remaining_columns + sorted_columns

        # # Reindex the DataFrame using the final column order
        # df = df.reindex(columns=pd.MultiIndex.from_tuples(final_columns))

        # # set index name
        # df.index.set_names("grid", inplace=True)

        # Custom sorting function for level=1 columns
        def parse_level_1(value):
            """Parse level=1 column values into sortable tuples."""
            if value.startswith("(") and value.endswith(")"):
                # Remove parentheses and split by comma
                parts = value[1:-1].split(",")
                # Convert to integers, ignoring empty strings
                return tuple(int(part) for part in parts if part)
            try:
                # Try converting to an integer for single values like "x"
                return (int(value),)
            except ValueError:
                # Fallback for non-numeric values
                return (value,)

        # Extract MultiIndex levels as a list of tuples
        columns = list(df.columns)

        # Sort the columns using the custom function
        sorted_columns = sorted(columns, key=lambda col: (col[0], parse_level_1(col[1])))

        # Re-create the MultiIndex with the sorted columns
        sorted_multi_index = pd.MultiIndex.from_tuples(sorted_columns)

        # Reindex the DataFrame with the sorted MultiIndex to preserve values
        df = df.reindex(columns=sorted_multi_index)

        # set column names
        df.columns.set_names(["var", "ind_dim"], inplace=True)
        df.index.name = "grid"

        return df

    @classmethod
    def from_df_state(cls, df: pd.DataFrame) -> "SUEWSConfig":
        """Create config from DataFrame state format.

        Args:
            df (pd.DataFrame): DataFrame containing SUEWS configuration state.

        Returns:
            SUEWSConfig: Instance of SUEWSConfig reconstructed from DataFrame.
        """
        # Initialize with default values
        config = cls()

        # Get grid IDs from DataFrame index
        grid_ids = df.index.tolist()

        # Create list of sites
        sites = []
        for grid_id in grid_ids:
            # Create site instance
            site = Site(gridiv=grid_id)

            # Set site properties
            site_properties = SiteProperties.from_df_state(df, grid_id)
            site.properties = site_properties

            # Set initial states
            initial_states = InitialStates.from_df_state(df, grid_id)
            site.initial_states = initial_states

            sites.append(site)

        # Update config with reconstructed data
        config.sites = sites

        # Reconstruct model
        config.model = Model.from_df_state(df, grid_ids[0])


        return config

    def to_yaml(self, path: str = "./config-suews.yml"):
        """Convert config to YAML format"""
        with open(path, "w") as file:
            yaml.dump(
                self.model_dump(exclude_none=True),
                file,
                sort_keys=False,
                allow_unicode=True,
            )


def init_config_from_yaml(path: str = "./config-suews.yml") -> SUEWSConfig:
    """Initialize SUEWSConfig from YAML file"""
    with open(path, "r") as file:
        config = yaml.load(file, Loader=yaml.FullLoader)
    return SUEWSConfig(**config)
