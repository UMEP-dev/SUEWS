from typing import Dict, List, Optional, Union, Literal, Tuple, Type, Generic, TypeVar, Any
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

from datetime import datetime

class SeasonCheck(BaseModel):
    start_date: str  # Expected format: YYYY-MM-DD
    lat: float

    def get_season(self) -> str:
        try:
            start = datetime.strptime(self.start_date, "%Y-%m-%d").timetuple().tm_yday
        except ValueError:
            raise ValueError("start_date must be in YYYY-MM-DD format")

        abs_lat = abs(self.lat)

        if abs_lat <= 10:
            return "equatorial"
        if 10 < abs_lat < 23.5:
            return "tropical"

        if self.lat >= 0:  # Northern Hemisphere
            if 150 < start < 250:
                return "summer"
            elif 60 < start <= 150:
                return "spring"
            elif 250 <= start < 335:
                return "fall"
            else:
                return "winter"
        else:  # Southern Hemisphere
            if 150 < start < 250:
                return "winter"
            elif 60 < start <= 150:
                return "fall"
            elif 250 <= start < 335:
                return "spring"
            else:
                return "summer"


def precheck_printing(data: dict) -> dict:
    print("Running basic precheck...")
    return data

def precheck_start_end_date(data: dict) -> Tuple[dict, int, str, str]:
    start_date = "2011-01-22"  # Placeholder: Replace with real YAML read
    end_date = "2011-02-22"

    if not isinstance(start_date, str) or "-" not in start_date:
        raise ValueError("Invalid 'start_time' — must be YYYY-MM-DD")
    if not isinstance(end_date, str) or "-" not in end_date:
        raise ValueError("Invalid 'end_time' — must be YYYY-MM-DD")

    model_year = int(start_date.split("-")[0])
    return data, model_year, start_date, end_date

def precheck_model_physics_params(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})

    if not physics:
        print("Skipping physics param check — physics is empty.")
        return data

    required = [
        "netradiationmethod", "emissionsmethod", "storageheatmethod", "ohmincqf",
        "roughlenmommethod", "roughlenheatmethod", "stabilitymethod", "smdmethod",
        "waterusemethod", "diagmethod", "faimethod", "localclimatemethod",
        "snowuse", "stebbsmethod"
    ]

    missing = [k for k in required if k not in physics]
    if missing:
        raise ValueError(f"[model.physics] Missing required params: {missing}")

    empty = [k for k in required if physics.get(k, {}).get("value") in ("", None)]
    if empty:
        raise ValueError(f"[model.physics] Empty or null values for: {empty}")

    print("All model.physics required params present and non-empty.")
    return data

def precheck_model_options_constraints(data: dict) -> dict:
    physics = data.get("model", {}).get("physics", {})

    diag = physics.get("diagmethod", {}).get("value")
    stability = physics.get("stabilitymethod", {}).get("value")

    if diag == 2 and stability != 3:
        raise ValueError("[model.physics] If diagmethod == 2, stabilitymethod must be 3.")

    print("diagmethod-stabilitymethod constraint passed.")
    return data

def precheck_replace_empty_strings_with_none(data: dict) -> dict:
    ignore_keys = {"control", "physics"}

    def recurse(obj: Any, path=()):
        if isinstance(obj, dict):
            new = {}
            for k, v in obj.items():
                sub_path = path + (k,)
                if (
                    v == ""
                    and not (len(sub_path) >= 2 and sub_path[0] == "model" and sub_path[1] in ignore_keys)
                ):
                    new[k] = None
                else:
                    new[k] = recurse(v, sub_path)
            return new
        elif isinstance(obj, list):
            return [None if item == "" else recurse(item, path) for item in obj]
        else:
            return obj

    cleaned = recurse(data)
    print("Empty strings replaced with None (except model.control and model.physics).")
    return cleaned


def precheck_site_season_adjustments(data: dict, start_date: str) -> dict:
    cleaned_sites = []

    for i, site in enumerate(data.get("sites", [])):
        if isinstance(site, BaseModel):
            site = site.model_dump(mode="python")

        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})

        # --------------------
        # 1. Determine season
        # --------------------
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        season = None

        try:
            if lat is not None:
                season = SeasonCheck(start_date=start_date, lat=lat).get_season()
                print(f"[site #{i}] Season detected: {season}")

                # If equatorial / tropical / summer → nullify snowalb
                if season in ("summer", "tropical", "equatorial") and "snowalb" in initial_states:
                    if isinstance(initial_states["snowalb"], dict):
                        initial_states["snowalb"]["value"] = None
                        print(f"[site #{i}] Set snowalb to None")
        except Exception as e:
            raise ValueError(f"[site #{i}] SeasonCheck failed: {e}")

        # --------------------------------------
        # 2. Seasonal adjustment for DecTrees LAI
        # --------------------------------------
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
                    print(f"[site #{i}] Set lai_id to {lai_val} for season {season}")
        else:
            if "dectr" in initial_states:
                initial_states["dectr"]["lai_id"] = {"value": None}
                print(f"[site #{i}] Nullified lai_id (no dectr surface)")

        cleaned_sites.append(site)

    data["sites"] = cleaned_sites
    return data



def run_precheck(data: dict) -> dict:
    if isinstance(data, BaseModel):
        data = data.model_dump(mode="python")

    # Also flatten any BaseModel in sites
    if "sites" in data and isinstance(data["sites"], list):
        flat_sites = []
        for site in data["sites"]:
            if isinstance(site, BaseModel):
                flat_sites.append(site.model_dump(mode="python"))
            else:
                flat_sites.append(site)
        data["sites"] = flat_sites

    data = precheck_printing(data)
    data, model_year, start_date, end_date = precheck_start_end_date(data)
    print(f"Start date: {start_date}, end date: {end_date}, year: {model_year}")

    data = precheck_model_physics_params(data)
    data = precheck_model_options_constraints(data)
    data = precheck_replace_empty_strings_with_none(data)
    data = precheck_site_season_adjustments(data, start_date)

    print("Precheck complete.")
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
        
    # @model_validator(mode="before")
    # @classmethod
    # def preprocess(cls, data: dict) -> dict:
    #     return run_precheck(data)

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
