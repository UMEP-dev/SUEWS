from typing import Dict, List, Optional, Union, Literal, Tuple, Type, Generic, TypeVar
from pydantic import (
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
    ValidationError
)
import numpy as np
import pandas as pd
import yaml
import ast
import supy as sp

from .model import Model
from .site import Site, SiteProperties, InitialStates, LandCover
from .site import SeasonCheck, DLSCheck

try:
    from ..validation import enhanced_from_yaml_validation, enhanced_to_df_state_validation
    _validation_available = True
except ImportError:
    try:
        from .validation_controller import validate_suews_config_conditional
        def enhanced_from_yaml_validation(config_data, strict=True):
            result = validate_suews_config_conditional(config_data, strict=False, verbose=True)
            if result['errors'] and strict:
                error_msg = f"SUEWS Configuration Validation Failed: {len(result['errors'])} errors\n"
                error_msg += "\n".join(f"  - {err}" for err in result['errors'])
                raise ValueError(error_msg)
            return result
        
        def enhanced_to_df_state_validation(config_data, strict=False):
            result = validate_suews_config_conditional(config_data, strict=False, verbose=False)
            if result['errors'] and strict:
                error_msg = f"Configuration validation found {len(result['errors'])} issues\n"
                error_msg += "\n".join(f"  - {err}" for err in result['errors'])
                raise ValueError(error_msg)
            return result
        
        _validation_available = True
    except ImportError:
        _validation_available = False
        enhanced_from_yaml_validation = None
        enhanced_to_df_state_validation = None
from .type import SurfaceType
import os
import warnings

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
        season = None

        try:
            if lat is not None:
                season = SeasonCheck(start_date=start_date, lat=lat).get_season()
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
        #emissions = props.get("anthropogenic_emissions", {})
        emissions = props.setdefault("anthropogenic_emissions", {})

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
        default_factory=lambda: [],
        description="List of sites to simulate",
        min_items=1,
    )

    class Config:
        extra = "allow"

    # Sort the filtered columns numerically
    @staticmethod
    def sort_key(col):
        try:
            return (col[0], ast.literal_eval(col[1]))
        except ValueError:
            return (col[0], col[1])

    @model_validator(mode="before")
    @classmethod
    def precheck(cls, data):
        return run_precheck(data)

    @model_validator(mode="after")
    def update_initial_states(self):
        from .._load import load_SUEWS_Forcing_met_df_yaml
        forcing = load_SUEWS_Forcing_met_df_yaml(self.model.control.forcing_file.value)
        
        # Cut the forcing data to model period
        cut_forcing = forcing.loc[self.model.control.start_time: self.model.control.end_time]
        
        # Check for missing forcing data
        missing_data = any(cut_forcing.isna().any())
        if missing_data:
            raise ValueError("Forcing data contains missing values.")

        # Check initial meteorology (for initial_states)
        first_day_forcing = cut_forcing.loc[self.model.control.start_time]
        first_day_min_temp = first_day_forcing.iloc[0]["Tair"]
        first_day_precip = first_day_forcing.iloc[0]["rain"] # Could check previous day if available

        # Use min temp for surface temperature states
        for site in self.sites:
            for surf_type in SurfaceType:
                surface = getattr(site.initial_states, surf_type)
                surface.temperature.value = [first_day_min_temp]*5
                surface.tsfc = first_day_min_temp
                surface.tin = first_day_min_temp

        # Use precip to determine wetness state
        for site in self.sites:
            for surf_type in SurfaceType:
                surface_is = getattr(site.initial_states, surf_type)
                surface_props =getattr(site.properties.land_cover, surf_type)
                if first_day_precip:
                    surface_is.state = surface_props.statelimit
                    surface_is.soilstore = surface_props.soilstorecap
                    if first_day_min_temp < 4:
                        surface_is.snowpack = surface_props.snowpacklimit
                        surface_is.snowfrac = 0.5 # Can these sum to greater than 1?
                        surface_is.icefrac = 0.5 # Can these sum to greater than 1?
                        surface_is.snowwater = 1 # TODO: What is the limit to this?
                        surface_is.snowdens = surface_props.snowdensmax
                else:
                    surface_is.state = 0
        return self

    @classmethod
    def from_yaml(cls, path: str, use_conditional_validation: bool = True, strict: bool = True) -> "SUEWSConfig":
        """Initialize SUEWSConfig from YAML file with conditional validation.

        Args:
            path (str): Path to YAML configuration file
            use_conditional_validation (bool): Whether to use conditional validation
            strict (bool): If True, raise errors on validation failure

        Returns:
            SUEWSConfig: Instance of SUEWSConfig initialized from YAML
        """
        with open(path, "r") as file:
            config_data = yaml.load(file, Loader=yaml.FullLoader)
        
        
        if use_conditional_validation and _validation_available: #_validation_available is always FALSE -- need to fix this
            # Step 1: Pre-validation with enhanced validation
            try:
                enhanced_from_yaml_validation(config_data, strict=strict)
            except ValueError:
                if strict:
                    raise
                # Continue with warnings already issued
            
            # Step 2: Create config with conditional validation applied
            try:
                return cls(**config_data)
            except Exception as e:
                if strict:
                    raise ValueError(f"Failed to create SUEWSConfig after conditional validation: {e}")
                else:
                    warnings.warn(f"Config creation warning: {e}")
                    # Try with model_construct to bypass strict validation
                    return cls.model_construct(**config_data)
        elif use_conditional_validation and not _validation_available:
            warnings.warn("Conditional validation requested but not available. Using standard validation.")
            # Fall back to original behavior
            return cls(**config_data)
        else:
            # Original behavior - validate everything
            print ("Entering SUEWSConfig pydantic validator...")
            return cls(**config_data)

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

    def to_df_state(self, use_conditional_validation: bool = True, strict: bool = False) -> pd.DataFrame:
        """Convert config to DataFrame state format with optional conditional validation.
        
        Args:
            use_conditional_validation (bool): Whether to run conditional validation before conversion
            strict (bool): If True, fail on validation errors; if False, warn and continue
            
        Returns:
            pd.DataFrame: DataFrame containing SUEWS configuration state
        """
        if use_conditional_validation and _validation_available:
            # Pre-validate configuration before conversion
            config_data = self.model_dump()
            try:
                enhanced_to_df_state_validation(config_data, strict=strict)
            except ValueError:
                if strict:
                    raise
                # Continue with warnings already issued
        elif use_conditional_validation and not _validation_available:
            warnings.warn("Conditional validation requested but not available.")
        
        # Proceed with DataFrame conversion
        try:
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
        except Exception as e:
            if use_conditional_validation and not strict:
                warnings.warn(f"Error during to_df_state conversion: {e}. This may be due to invalid parameters for disabled methods.")
                raise
            else:
                raise

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
