from typing import (
    Dict,
    List,
    Optional,
    Union,
    Literal,
    Tuple,
    Type,
    Generic,
    TypeVar,
    Any,
)
from pydantic import (
    ConfigDict,
    BaseModel,
    Field,
    model_validator,
    field_validator,
    PrivateAttr,
    conlist,
    ValidationError,
)
import numpy as np
import pandas as pd
import yaml
import ast

import csv
import os
from copy import deepcopy
from pathlib import Path
import warnings

from .model import Model
from .site import Site, SiteProperties, InitialStates, LandCover
from .type import SurfaceType

from datetime import datetime
from timezonefinder import TimezoneFinder
import pytz

from .._env import logger_supy
from .yaml_annotator_json import JsonYamlAnnotator as YAMLAnnotator

try:
    from ..validation import (
        enhanced_from_yaml_validation,
        enhanced_to_df_state_validation,
    )

    _validation_available = True
except ImportError:
    try:
        from .validation_controller import validate_suews_config_conditional

        def enhanced_from_yaml_validation(config_data, strict=True):
            result = validate_suews_config_conditional(
                config_data, strict=False, verbose=True
            )
            if result.errors and strict:
                error_msg = f"SUEWS Configuration Validation Failed: {len(result.errors)} errors\n"
                error_msg += "\n".join(f"  - {err}" for err in result.errors)
                raise ValueError(error_msg)
            return result

        def enhanced_to_df_state_validation(config_data, strict=False):
            result = validate_suews_config_conditional(
                config_data, strict=False, verbose=False
            )
            if result.errors and strict:
                error_msg = (
                    f"Configuration validation found {len(result.errors)} issues\n"
                )
                error_msg += "\n".join(f"  - {err}" for err in result.errors)
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
        default="sample config",
        description="Name of the SUEWS configuration",
        json_schema_extra={"display_name": "Configuration Name"},
    )
    description: str = Field(
        default="this is a sample config for testing purposes ONLY - values are not realistic",
        description="Description of this SUEWS configuration",
        json_schema_extra={"display_name": "Configuration Description"},
    )
    model: Model = Field(
        default_factory=Model,
        description="Model control and physics parameters",
        json_schema_extra={"display_name": "Model Parameters"},
    )
    sites: List[Site] = Field(
        default_factory=list,
        description="List of sites to simulate",
        min_length=1,
        json_schema_extra={"display_name": "Sites"},
    )

    model_config = ConfigDict(extra="allow")

    # Sort the filtered columns numerically
    @staticmethod
    def sort_key(col):
        try:
            return (col[0], ast.literal_eval(col[1]))
        except ValueError:
            return (col[0], col[1])

    @model_validator(mode="after")
    def validate_parameter_completeness(self) -> "SUEWSConfig":
        """
        Validate all parameters after complete construction.
        This runs AFTER all values are populated from YAML.
        """
        # Track validation issues for summary
        self._validation_summary = {
            "total_warnings": 0,
            "sites_with_issues": [],
            "issue_types": set(),
            "yaml_path": None,
        }

        for i, site in enumerate(self.sites):
            self._validate_site_parameters(site, site_index=i)

        # Show summary if there are warnings
        if self._validation_summary["total_warnings"] > 0:
            self._show_validation_summary()

        return self

    def _show_validation_summary(self) -> None:
        """Show a concise summary of validation issues."""
        # Check if we have a yaml path stored
        yaml_path = getattr(self, "_yaml_path", None)

        if yaml_path:
            # When loaded from YAML, we know the source file
            annotated_filename = Path(yaml_path).stem + "_annotated.yml"
            fix_instructions = (
                f"To see detailed fixes for each parameter: please refer to inline guidance "
                f"in '{annotated_filename}' that will shortly be generated"
            )
        else:
            fix_instructions = (
                f"To see detailed fixes for each parameter:\n"
                f"   1. Save your configuration to a YAML file\n"
                f"   2. Call config.generate_annotated_yaml('your_config.yml')\n"
                f"   3. An annotated file with inline guidance will be generated"
            )

        logger_supy.warning(
            f"\n{'=' * 60}\n"
            f"VALIDATION SUMMARY\n"
            f"{'=' * 60}\n"
            f"Found {self._validation_summary['total_warnings']} parameter issue(s) across "
            f"{len(self._validation_summary['sites_with_issues'])} site(s).\n\n"
            f"Issue types:\n"
            f"  - "
            + "\n  - ".join(sorted(self._validation_summary["issue_types"]))
            + "\n\n"
            f"{fix_instructions}\n"
            f"{'=' * 60}"
        )

    def _validate_site_parameters(self, site: Site, site_index: int) -> None:
        """Validate all parameters for a single site."""

        if not site.properties:
            return

        site_name = getattr(site, "name", f"Site {site_index}")
        site_has_issues = False

        # Validate conductance parameters
        if hasattr(site.properties, "conductance") and site.properties.conductance:
            if self._check_conductance(site.properties.conductance, site_name):
                site_has_issues = True

        # Validate CO2 parameters
        if (
            hasattr(site.properties, "anthropogenic_emissions")
            and site.properties.anthropogenic_emissions
            and hasattr(site.properties.anthropogenic_emissions, "co2")
            and site.properties.anthropogenic_emissions.co2
        ):
            if self._check_co2_params(
                site.properties.anthropogenic_emissions.co2, site_name
            ):
                site_has_issues = True

        # Validate land cover parameters
        if hasattr(site.properties, "land_cover") and site.properties.land_cover:
            if self._check_land_cover(site.properties.land_cover, site_name):
                site_has_issues = True

        # Track sites with issues
        if (
            site_has_issues
            and site_name not in self._validation_summary["sites_with_issues"]
        ):
            self._validation_summary["sites_with_issues"].append(site_name)

    def _check_conductance(self, conductance, site_name: str) -> bool:
        """Check for missing conductance parameters. Returns True if issues found."""
        from .validation_utils import check_missing_params

        critical_params = {
            "g_max": "Maximum surface conductance",
            "g_k": "Conductance parameter for solar radiation",
            "g_sm": "Conductance parameter for soil moisture",
            "s1": "Lower soil moisture threshold",
            "s2": "Soil moisture dependence parameter",
        }

        missing_params = check_missing_params(
            critical_params,
            conductance,
            "surface conductance",
            "evapotranspiration calculations",
        )

        if missing_params:
            self._validation_summary["total_warnings"] += len(missing_params)
            self._validation_summary["issue_types"].add(
                "Missing conductance parameters"
            )
            return True
        return False

    def _check_co2_params(self, co2, site_name: str) -> bool:
        """Check for missing CO2 parameters. Returns True if issues found."""
        from .validation_utils import check_missing_params

        critical_params = {
            "co2pointsource": "CO2 point source emission factor",
            "ef_umolco2perj": "CO2 emission factor per unit of fuel energy",
            "frfossilfuel_heat": "Fraction of heating energy from fossil fuels",
            "frfossilfuel_nonheat": "Fraction of non-heating energy from fossil fuels",
        }

        missing_params = check_missing_params(
            critical_params, co2, "CO2 emission", "model accuracy"
        )

        if missing_params:
            self._validation_summary["total_warnings"] += len(missing_params)
            self._validation_summary["issue_types"].add(
                "Missing CO2 emission parameters"
            )
            return True
        return False

    def _check_land_cover(self, land_cover, site_name: str) -> bool:
        """Check land cover parameters. Returns True if issues found."""
        # Check each surface type
        surface_types = ["bldgs", "grass", "dectr", "evetr", "bsoil", "paved", "water"]
        has_issues = False

        for surface_type in surface_types:
            if hasattr(land_cover, surface_type):
                surface = getattr(land_cover, surface_type)
                if surface:
                    if self._check_surface_parameters(surface, surface_type, site_name):
                        has_issues = True

        return has_issues

    def _check_surface_parameters(
        self, surface, surface_type: str, site_name: str
    ) -> bool:
        """Check parameters for a specific surface type. Returns True if issues found."""
        from .validation_utils import check_missing_params

        has_issues = False

        # Get surface fraction value
        sfr_value = 0
        if hasattr(surface, "sfr") and surface.sfr is not None:
            sfr_value = getattr(surface.sfr, "value", surface.sfr)

        # Only validate if surface fraction > 0
        if sfr_value > 0:
            # Check building-specific parameters
            if surface_type == "bldgs" and sfr_value > 0.05:
                missing_params = []

                if not hasattr(surface, "bldgh") or surface.bldgh is None:
                    missing_params.append("bldgh (Building height)")
                if not hasattr(surface, "faibldg") or surface.faibldg is None:
                    missing_params.append("faibldg (Frontal area index)")

                if missing_params:
                    self._validation_summary["total_warnings"] += len(missing_params)
                    self._validation_summary["issue_types"].add(
                        "Missing building parameters"
                    )
                    has_issues = True

            # Check vegetation parameters for grass, dectr, evetr
            if surface_type in ["grass", "dectr", "evetr"]:
                vegetation_params = {
                    "beta_bioco2": "Biogenic CO2 exchange coefficient",
                    "alpha_bioco2": "Biogenic CO2 exchange coefficient",
                    "resp_a": "Respiration coefficient",
                    "resp_b": "Respiration coefficient",
                }

                missing_params = check_missing_params(
                    vegetation_params, surface, "vegetation", "CO2 flux calculations"
                )

                if missing_params:
                    self._validation_summary["total_warnings"] += len(missing_params)
                    self._validation_summary["issue_types"].add(
                        "Missing vegetation parameters"
                    )
                    has_issues = True

            # Check thermal layers for all surfaces
            if hasattr(surface, "thermal_layers") and surface.thermal_layers:
                if self._check_thermal_layers(
                    surface.thermal_layers, surface_type, site_name
                ):
                    has_issues = True

        return has_issues

    def _check_thermal_layers(
        self, thermal_layers, surface_type: str, site_name: str
    ) -> bool:
        """Check thermal layer parameters. Returns True if issues found."""
        missing_params = []

        if not hasattr(thermal_layers, "dz") or thermal_layers.dz is None:
            missing_params.append("dz (Layer thickness)")
        if not hasattr(thermal_layers, "k") or thermal_layers.k is None:
            missing_params.append("k (Thermal conductivity)")
        if not hasattr(thermal_layers, "rho_cp") or thermal_layers.rho_cp is None:
            missing_params.append("rho_cp (Volumetric heat capacity)")

        if missing_params:
            self._validation_summary["total_warnings"] += len(missing_params)
            self._validation_summary["issue_types"].add(
                "Missing thermal layer parameters"
            )
            return True
        return False

    def generate_annotated_yaml(
        self, yaml_path: str, output_path: Optional[str] = None
    ) -> str:
        """
        Generate an annotated YAML file with validation feedback.

        Args:
            yaml_path: Path to the original YAML file
            output_path: Optional path for the annotated file

        Returns:
            Path to the generated annotated file
        """
        from pathlib import Path

        annotator = YAMLAnnotator()

        # Collect validation issues by running validation
        for i, site in enumerate(self.sites):
            site_name = getattr(site, "name", f"Site {i}")
            self._collect_validation_issues(site, site_name, i, annotator)

        # Generate annotated file
        input_path = Path(yaml_path)
        if output_path:
            output_path = Path(output_path)
        else:
            output_path = input_path.parent / f"{input_path.stem}_annotated.yml"

        annotated_path = annotator.generate_annotated_file(input_path, output_path)

        logger_supy.info(f"Generated annotated YAML file: {annotated_path}")
        return str(annotated_path)

    def _collect_validation_issues(
        self, site: Site, site_name: str, site_index: int, annotator: YAMLAnnotator
    ) -> None:
        """Collect validation issues for annotation."""

        if not hasattr(site, "properties") or not site.properties:
            return

        # Check conductance
        if hasattr(site.properties, "conductance") and site.properties.conductance:
            from .validation_utils import check_missing_params

            critical_params = {
                "g_max": "Maximum surface conductance",
                "g_k": "Conductance parameter for solar radiation",
                "g_sm": "Conductance parameter for soil moisture",
                "s1": "Lower soil moisture threshold",
                "s2": "Soil moisture dependence parameter",
            }

            missing_params = check_missing_params(
                critical_params,
                site.properties.conductance,
                "surface conductance",
                "evapotranspiration calculations",
            )

            for param, desc in critical_params.items():
                if param in missing_params:
                    annotator.add_issue(
                        path=f"sites[{site_index}]/properties/conductance",
                        param=param,
                        message=f"Missing {desc}",
                        fix=f"Add {param} value for accurate evapotranspiration",
                        level="WARNING",
                    )

        # Check CO2 parameters
        if (
            hasattr(site.properties, "anthropogenic_emissions")
            and site.properties.anthropogenic_emissions
            and hasattr(site.properties.anthropogenic_emissions, "co2")
            and site.properties.anthropogenic_emissions.co2
        ):
            from .validation_utils import check_missing_params

            critical_params = {
                "co2pointsource": "CO2 point source emission factor",
                "ef_umolco2perj": "CO2 emission factor per unit of fuel energy",
                "frfossilfuel_heat": "Fraction of heating energy from fossil fuels",
                "frfossilfuel_nonheat": "Fraction of non-heating energy from fossil fuels",
            }

            missing_params = check_missing_params(
                critical_params,
                site.properties.anthropogenic_emissions.co2,
                "CO2 emission",
                "model accuracy",
            )

            for param, desc in critical_params.items():
                if param in missing_params:
                    annotator.add_issue(
                        path=f"sites[{site_index}]/properties/anthropogenic_emissions/co2",
                        param=param,
                        message=f"Missing {desc}",
                        fix=f"Add {param} value for CO2 emission calculations",
                        level="WARNING",
                    )

        # Check land cover
        if hasattr(site.properties, "land_cover") and site.properties.land_cover:
            self._collect_land_cover_issues(
                site.properties.land_cover, site_name, site_index, annotator
            )

    def _collect_land_cover_issues(
        self, land_cover, site_name: str, site_index: int, annotator: YAMLAnnotator
    ) -> None:
        """Collect land cover validation issues."""
        surface_types = ["bldgs", "grass", "dectr", "evetr", "bsoil", "paved", "water"]

        for surface_type in surface_types:
            if hasattr(land_cover, surface_type):
                surface = getattr(land_cover, surface_type)
                if surface:
                    # Get surface fraction
                    sfr_value = 0
                    if hasattr(surface, "sfr") and surface.sfr is not None:
                        sfr_value = getattr(surface.sfr, "value", surface.sfr)

                    if sfr_value > 0:
                        path = (
                            f"sites[{site_index}]/properties/land_cover/{surface_type}"
                        )

                        # Building-specific checks
                        if surface_type == "bldgs" and sfr_value > 0.05:
                            if not hasattr(surface, "bldgh") or surface.bldgh is None:
                                annotator.add_issue(
                                    path=path,
                                    param="bldgh",
                                    message=f"Building height required (fraction: {sfr_value:.1%})",
                                    fix="Add building height in meters (e.g., 10-50m for urban areas)",
                                    level="WARNING",
                                )

                            if (
                                not hasattr(surface, "faibldg")
                                or surface.faibldg is None
                            ):
                                annotator.add_issue(
                                    path=path,
                                    param="faibldg",
                                    message="Frontal area index needed for wind calculations",
                                    fix="Add frontal area index (typical: 0.1-0.7)",
                                    level="WARNING",
                                )

                        # Thermal layers check
                        if (
                            hasattr(surface, "thermal_layers")
                            and surface.thermal_layers
                        ):
                            thermal = surface.thermal_layers
                            if (
                                not hasattr(thermal, "dz")
                                or thermal.dz is None
                                or not hasattr(thermal, "k")
                                or thermal.k is None
                                or not hasattr(thermal, "rho_cp")
                                or thermal.rho_cp is None
                            ):
                                annotator.add_issue(
                                    path=f"{path}/thermal_layers",
                                    param="thermal_layers",
                                    message="Incomplete thermal layer properties",
                                    fix="Add dz (thickness), k (conductivity), and rho_cp (heat capacity) arrays",
                                    level="WARNING",
                                )

    # @model_validator(mode="after")
    # def check_forcing(self):
    #     from .._load import load_SUEWS_Forcing_met_df_yaml
    #     forcing = load_SUEWS_Forcing_met_df_yaml(self.model.control.forcing_file.value)
    #
    #     # Cut the forcing data to model period
    #     cut_forcing = forcing.loc[self.model.control.start_time: self.model.control.end_time]
    #
    #     # Check for missing forcing data
    #     missing_data = any(cut_forcing.isna().any())
    #     if missing_data:
    #         raise ValueError("Forcing data contains missing values.")

    #     # Check initial meteorology (for initial_states)
    #     first_day_forcing = cut_forcing.loc[self.model.control.start_time]
    #     first_day_min_temp = first_day_forcing.iloc[0]["Tair"]
    #     first_day_precip = first_day_forcing.iloc[0]["rain"] # Could check previous day if available

    #     # Use min temp for surface temperature states
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface = getattr(site.initial_states, surf_type)
    #             surface.temperature.value = [first_day_min_temp]*5
    #             surface.tsfc.value = first_day_min_temp
    #             surface.tin.value = first_day_min_temp

    #     # Use precip to determine wetness state
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface_is = getattr(site.initial_states, surf_type)
    #             surface_props =getattr(site.properties.land_cover, surf_type)
    #             if first_day_precip:
    #                 surface_is.state.value = surface_props.statelimit
    #                 surface_is.soilstore.value = surface_props.soilstorecap
    #                 if first_day_min_temp < 4:
    #                     surface_is.snowpack.value = surface_props.snowpacklimit
    #                     surface_is.snowfrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.icefrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.snowwater.value = 1 # TODO: What is the limit to this?
    #                     surface_is.snowdens.value = surface_props.snowdensmax
    #             else:
    #                 surface_is.state.value = 0
    #     return self

    @classmethod
    def from_yaml(
        cls, path: str, use_conditional_validation: bool = True, strict: bool = True
    ) -> "SUEWSConfig":
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

        # Store yaml path in config data for later use
        config_data["_yaml_path"] = path

        if (
            use_conditional_validation and _validation_available
        ):  # _validation_available is always FALSE -- need to fix this
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
                    raise ValueError(
                        f"Failed to create SUEWSConfig after conditional validation: {e}"
                    )
                else:
                    warnings.warn(f"Config creation warning: {e}")
                    # Try with model_construct to bypass strict validation
                    return cls.model_construct(**config_data)
        elif use_conditional_validation and not _validation_available:
            warnings.warn(
                "Conditional validation requested but not available. Using standard validation."
            )
            # Fall back to original behavior
            return cls(**config_data)
        else:
            # Original behavior - validate everything
            logger_supy.info("Entering SUEWSConfig pydantic validator...")
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

    def to_df_state(
        self, use_conditional_validation: bool = True, strict: bool = False
    ) -> pd.DataFrame:
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
            df["config"] = self.name
            df["description"] = self.description
            # remove duplicate columns
            df = df.loc[:, ~df.columns.duplicated()]
        except Exception as e:
            if use_conditional_validation and not strict:
                warnings.warn(
                    f"Error during to_df_state conversion: {e}. This may be due to invalid parameters for disabled methods."
                )
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
        sorted_columns = sorted(
            columns, key=lambda col: (col[0], parse_level_1(col[1]))
        )

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

        config.name = df["config"].iloc[0]
        config.description = df["description"].iloc[0]

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
