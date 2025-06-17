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

from .site import DLSValidator
from .model import Model
from .site import Site, SiteProperties, InitialStates, LandCover
from .site import SeasonCheck

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
import os
import warnings


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
    site: List[Site] = Field(
        default=[Site()],
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
        sites_data = data.get("site", [])
        if not isinstance(sites_data, list):
            raise TypeError("Expected 'site' to be a list.")

        for i, site in enumerate(sites_data):
            props = site.get("properties", {})

            # --- Season Check  ---
            try:
                lat = props.get("lat", {}).get("value")
                start_date = "2014-06-20"  # hardcoded for now
                end_date = "2014-08-20" # hardcoded for now
                if lat is not None:
                    season_check = SeasonCheck(start_date=start_date, end_date=end_date, lat=lat)
                    season_check.validate_season()
                else:
                    print(f"⚠️ [site #{i}] Skipping season check: missing lat")
            except Exception as e:
                raise ValueError(f"[site #{i}] SeasonCheck failed: {e}")

            # --- DLS Validator ---
            try:
                lng = props.get("lng", {}).get("value")
                emissions = props.get("anthropogenic_emissions", {})
                startdls = emissions.get("startdls", {}).get("value")
                enddls = emissions.get("enddls", {}).get("value")

                if lat is None or lng is None:
                    raise ValueError("Missing lat or lng for DLS check")

                dlsvalidator = DLSValidator(
                    lat=lat,
                    lng=lng,
                    startdls=startdls,
                    enddls=enddls,
                    year=2014  # hardcoded for now
                )
                dlsvalidator.validate_dls()
            except Exception as e:
                raise ValueError(f"[site #{i}] DLS validation failed: {e}")

            # --- Land Cover Validator ---
            landcover_data = props.get("land_cover") or props.get("landcover")
            if landcover_data is None:
                raise ValueError(f"[site #{i}] Missing 'land_cover' section in site properties")
            if not isinstance(landcover_data, dict):
                raise TypeError(f"[site #{i}] 'land_cover' must be a dict, got {type(landcover_data).__name__}")
            try:
                print(f"🧪 Validating land_cover for site #{i}...")
                LandCover(**landcover_data)
            except ValidationError as e:
                raise ValueError(f"[site #{i}] Invalid land_cover: {e}")

        return data


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
        
        if use_conditional_validation and _validation_available:
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
            for i in range(len(self.site)):
                grid_id = self.site[i].gridiv
                df_site = self.site[i].to_df_state(grid_id)
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
        config.site = sites

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
