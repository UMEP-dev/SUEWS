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
    ClassVar,
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

import calendar
import csv
import os
from copy import deepcopy
from pathlib import Path
import warnings

from .model import Model, OutputConfig
from .site import Site, SiteProperties, InitialStates, LandCover, LAIParams
from .type import SurfaceType

from ..validation.core.yaml_helpers import unwrap_value as _unwrap_value

from datetime import datetime
import pytz

from types import SimpleNamespace

# Optional import of logger - use standalone if supy not available
try:
    from ..._env import logger_supy
except ImportError:
    import logging

    logger_supy = logging.getLogger("supy.data_model")
    if not logger_supy.handlers:
        handler = logging.StreamHandler()
        handler.setFormatter(
            logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
        )
        logger_supy.addHandler(handler)
        logger_supy.setLevel(logging.INFO)

from ..validation.pipeline.yaml_annotator import YAMLAnnotator

_validation_available = False
enhanced_from_yaml_validation = None
enhanced_to_df_state_validation = None

import os
import warnings

SAME_SURFACE_TOL = 1e-6  # Tolerance for same-surface-property checks

class ConditionalValidationWarning(UserWarning):
    """Warning issued when conditional validation is requested but not available.

    This warning indicates that the enhanced validation feature has been requested
    via use_conditional_validation=True, but the validation module is not loaded.
    The conversion will proceed without additional validation checks.

    To suppress this warning, either:
    - Set use_conditional_validation=False when calling to_df_state()
    - Filter with: warnings.filterwarnings('ignore', category=ConditionalValidationWarning)
    """

    pass


def _is_valid_layer_array(field) -> bool:
    return (
        hasattr(field, "value")
        and isinstance(field.value, list)
        and len(field.value) > 0
    )


def _get_basemodel_type(annotation):
    """Extract BaseModel subclass from a type annotation, unwrapping Optional/Union/List."""
    from typing import get_origin, get_args

    # Direct class check
    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return annotation

    origin = get_origin(annotation)
    args = get_args(annotation)

    # Union (includes Optional)
    if origin is Union:
        for arg in args:
            if arg is type(None):
                continue
            result = _get_basemodel_type(arg)
            if result is not None:
                return result
        return None

    # List
    if origin is list:
        if args:
            return _get_basemodel_type(args[0])
        return None

    # Generic BaseModel subclass (e.g. RefValue[int])
    if isinstance(origin, type) and issubclass(origin, BaseModel):
        return origin

    return None


def _strip_internal_fields(data, model_cls):
    """Recursively remove fields marked with internal_only=True from a model_dump dict.

    Modifies data in-place.

    Parameters
    ----------
    data : dict
        Dictionary from model_dump() to strip internal fields from.
    model_cls : type
        The Pydantic model class corresponding to data.
    """
    if not isinstance(data, dict) or not hasattr(model_cls, "model_fields"):
        return

    keys_to_remove = []

    for field_name, field_info in model_cls.model_fields.items():
        extra = field_info.json_schema_extra
        if isinstance(extra, dict) and extra.get("internal_only"):
            keys_to_remove.append(field_name)
            continue

        if field_name not in data:
            continue

        inner_cls = _get_basemodel_type(field_info.annotation)
        if inner_cls is None:
            continue

        value = data[field_name]
        if isinstance(value, list):
            for item in value:
                if isinstance(item, dict):
                    _strip_internal_fields(item, inner_cls)
        elif isinstance(value, dict):
            _strip_internal_fields(value, inner_cls)

    for key in keys_to_remove:
        data.pop(key, None)


class SUEWSConfig(BaseModel):
    """Main SUEWS configuration."""

    model_config = ConfigDict(title="SUEWS Configuration", extra="allow")

    name: str = Field(
        default="sample config",
        description="Name of the SUEWS configuration",
        json_schema_extra={"display_name": "Configuration Name"},
    )
    schema_version: Optional[str] = Field(
        default=None,
        description="Configuration schema version (for example, '2025.12'). Only changes when configuration structure changes.",
        json_schema_extra={"display_name": "Schema Version"},
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

    # Class-level constant for STEBBS validation parameters
    STEBBS_REQUIRED_PARAMS: ClassVar[List[str]] = [
        "wall_internal_convection_coefficient",
        "internal_mass_convection_coefficient",
        "floor_internal_convection_coefficient",
        "window_internal_convection_coefficient",
        "wall_external_convection_coefficient",
        "window_external_convection_coefficient",
        "ground_depth",
        "external_ground_conductivity",
        "metabolism_threshold",
        "latent_sensible_ratio",
        "daylight_control",
        "lighting_illuminance_threshold",
        "appliance_profile",
        "lighting_power_density",
        "heating_system_efficiency",
        "max_cooling_power",
        "cooling_system_cop",
        "ventilation_rate",
        "initial_outdoor_temperature",
        "initial_indoor_temperature",
        "annual_mean_air_temperature",
        "month_mean_air_temperature_diffmax",
        "hot_water_tank_wall_thickness",
        "mains_water_temperature",
        "hot_water_tank_surface_area",
        "hot_water_heating_setpoint_temperature",
        "hot_water_tank_wall_emissivity",
        "hot_water_vessel_wall_thickness",
        "hot_water_volume",
        "hot_water_surface_area",
        "hot_water_flow_rate",
        "hot_water_flow_profile",
        "hot_water_specific_heat_capacity",
        "hot_water_tank_specific_heat_capacity",
        "hot_water_vessel_specific_heat_capacity",
        "hot_water_density",
        "hot_water_tank_wall_density",
        "hot_water_vessel_density",
        "hot_water_tank_building_wall_view_factor",
        "hot_water_tank_internal_mass_view_factor",
        "hot_water_tank_wall_conductivity",
        "hot_water_tank_internal_wall_convection_coefficient",
        "hot_water_tank_external_wall_convection_coefficient",
        "hot_water_vessel_wall_conductivity",
        "hot_water_vessel_internal_wall_convection_coefficient",
        "hot_water_vessel_external_wall_convection_coefficient",
        "hot_water_vessel_wall_emissivity",
        "hot_water_heating_efficiency",
    ]

    ARCHETYPE_REQUIRED_PARAMS: ClassVar[List[str]] = [
        "building_type",
        "building_name",
        "building_count",
        "occupants",
        "metabolism_profile",
        "building_height",
        "footprint_area",
        "wall_external_area",
        "internal_volume_ratio",
        "internal_mass_area",
        "window_to_wall_ratio",
        "wall_thickness",
        "wall_effective_conductivity",
        "wall_density",
        "wall_specific_heat_capacity",
        "wall_outer_heat_capacity_fraction",
        "wall_external_emissivity",
        "wall_internal_emissivity",
        "wall_transmissivity",
        "wall_absorptivity",
        "wall_reflectivity",
        "ground_floor_thickness",
        "ground_floor_effective_conductivity",
        "ground_floor_density",
        "ground_floor_specific_heat_capacity",
        "window_thickness",
        "window_effective_conductivity",
        "window_density",
        "window_specific_heat_capacity",
        "window_external_emissivity",
        "window_internal_emissivity",
        "window_transmissivity",
        "window_absorptivity",
        "window_reflectivity",
        "internal_mass_density",
        "internal_mass_specific_heat_capacity",
        "internal_mass_emissivity",
        "max_heating_power",
        "hot_water_tank_volume",
        "maximum_hot_water_heating_power",
        "heating_setpoint_temperature",
        "cooling_setpoint_temperature",
        "heating_setpoint_temperature_profile",
        "cooling_setpoint_temperature_profile",
    ]

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
        Validate all parameters after full construction.

        This method is called after all values have been populated from YAML or other sources.
        It performs comprehensive validation of the configuration, including:
            - Summarizing validation issues
            - Running standard site-by-site checks
            - Running conditional validations (e.g., STEBBS, RSL, StorageHeat, SPARTACUS)
            - Checking for critical null physics parameters
            - Displaying a summary of warnings if present

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If any critical validation errors are found (e.g., missing required parameters).
        """
        ### 1) Initialize the summary of validation issues
        self._validation_summary = {
            "total_warnings": 0,
            "sites_with_issues": [],
            "issue_types": set(),
            "yaml_path": getattr(self, "_yaml_path", None),
            "detailed_messages": [],
            "info_messages": [],
        }

        ### 2) Run the standard site-by-site checks
        for i, site in enumerate(self.sites):
            self._validate_site_parameters(site, site_index=i)

        ### 3) Run any conditional validations (e.g. STEBBS when stebbs_method==1)
        cond_issues = self._validate_conditional_parameters()

        ### 4) Check for critical null physics parameters (top-level ModelPhysics switches)
        critical_nulls = self._check_critical_null_physics_params()

        ### 4b) Check for critical null site-level parameters (gh#1333)
        critical_site_nulls = self._check_critical_null_site_params()

        ### 5) If we have either conditional issues or critical nulls, raise validation error
        all_critical_issues = []
        if cond_issues:
            all_critical_issues.extend(cond_issues)
        if critical_nulls:
            all_critical_issues.extend(critical_nulls)
        if critical_site_nulls:
            all_critical_issues.extend(critical_site_nulls)

        if all_critical_issues:
            # Preserve the annotated-YAML UX on the failure path: if the caller
            # asked for auto-generation, emit it before raising so users get a
            # template alongside the error message (gh#1333).
            yaml_path = getattr(self, "_yaml_path", None)
            auto_generate = getattr(self, "_auto_generate_annotated", False)
            if auto_generate and yaml_path and Path(yaml_path).exists():
                try:
                    generated_path = self.generate_annotated_yaml(yaml_path)
                    logger_supy.info(
                        f"Annotated YAML file generated: {generated_path}"
                    )
                except Exception as exc:  # pragma: no cover - best-effort UX
                    logger_supy.warning(
                        f"Annotated YAML generation failed: {exc}"
                    )

            # Put each critical issue on its own line for readability
            error_message = "\n".join(all_critical_issues)
            raise ValueError(f"Critical validation failed:\n{error_message}")

        ### 4) If there were any warnings, show the summary (only for non-conditional issues)
        if self._validation_summary["total_warnings"] > 0:
            self._show_validation_summary()

        return self

    @model_validator(mode="after")
    def validate_schema_compatibility(self) -> "SUEWSConfig":
        """
        Check if the configuration schema version is compatible.

        Issues a warning if the schema version used in the configuration
        does not match the current supported schema version.

        Notes
        -----
        - If `schema_version` is not specified, it will be set to the current schema version.
        - Compatibility is checked using `validate_schema_version` from the schema module.
        - Any mismatch or compatibility concern will be logged as a warning.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance (self).
        """
        from ..schema import validate_schema_version, CURRENT_SCHEMA_VERSION

        # If no schema version specified, set to current
        if self.schema_version is None:
            self.schema_version = CURRENT_SCHEMA_VERSION

        # Validate compatibility (will warn if incompatible)
        validate_schema_version(self.schema_version, strict=False)

        # Log to detailed messages for validation summary if needed
        if (
            hasattr(self, "_validation_summary")
            and self.schema_version != CURRENT_SCHEMA_VERSION
        ):
            self._validation_summary["detailed_messages"].append(
                f"Schema version: Config uses {self.schema_version}, current is {CURRENT_SCHEMA_VERSION}"
            )

        return self

    @model_validator(mode="after")
    def validate_model_output_config(self) -> "SUEWSConfig":
        """
        Validate output configuration, especially frequency vs timestep.

        This validator ensures that the output frequency specified in the OutputConfig
        is positive and an integer multiple of the model timestep. It also warns if
        a deprecated string value is used for the output_file parameter.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If output frequency is not positive or not a multiple of timestep.
        DeprecationWarning
            If a deprecated string value is used for output_file.
        """
        if isinstance(self.model.control.output_file, OutputConfig):
            output_config = self.model.control.output_file
            if output_config.freq is not None:
                # Validate frequency is positive
                if output_config.freq <= 0:
                    raise ValueError(
                        f"Output frequency must be positive, got {output_config.freq}s"
                    )

                tstep = self.model.control.tstep
                if output_config.freq % tstep != 0:
                    raise ValueError(
                        f"Output frequency ({output_config.freq}s) must be a multiple of timestep ({tstep}s)"
                    )
        elif (
            isinstance(self.model.control.output_file, str)
            and self.model.control.output_file != "output.txt"
        ):
            # Issue warning for non-default string values
            import warnings

            warnings.warn(
                f"The 'output_file' parameter with value '{self.model.control.output_file}' is deprecated and was never used. "
                "Please use the new OutputConfig format or remove this parameter. "
                "Example: output_file: {format: 'parquet', freq: 3600}",
                DeprecationWarning,
                stacklevel=3,
            )
        return self

    @model_validator(mode="after")
    def validate_model_radiation_method(self) -> "SUEWSConfig":
        """
        Validate radiation method configuration compatibility with forcing file.

        This validator checks that the selected net radiation method is compatible
        with the provided forcing file. Specifically, it warns if
        `net_radiation_method=1` (which requires observed Ldown) is used with a
        sample forcing file that typically lacks Ldown data.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Warns
        -----
        UserWarning
            If net_radiation_method=1 is used with a sample forcing file.
        """
        # Use the helper for consistent unwrapping
        net_radiation_method_val = _unwrap_value(self.model.physics.net_radiation)
        forcing_file_val = _unwrap_value(self.model.control.forcing_file)

        # Check for the sample forcing file - this is still based on filename
        # TODO: Future improvement - add a flag to indicate sample forcing or check actual column presence
        # For now, we check both common sample forcing filenames
        sample_forcing_names = ["forcing.txt", "sample_forcing.txt", "test_forcing.txt"]

        if net_radiation_method_val == 1 and any(
            name in str(forcing_file_val).lower() for name in sample_forcing_names
        ):
            import warnings

            warnings.warn(
                f"NetRadiationMethod is set to 1 (using observed Ldown) with what appears to be a sample forcing file '{forcing_file_val}'. "
                "Sample forcing files typically lack observed Ldown data. "
                "If this is sample data, use net_radiation_method = 3. "
                "If this is real data with Ldown, consider renaming the file to avoid this warning.",
                UserWarning,
                stacklevel=2,
            )
        return self

    @model_validator(mode="after")
    def validate_site_required_fields(self) -> "SUEWSConfig":
        """
        Validate that all sites have required fields with valid values.

        This validator ensures that each site in the configuration has all critical
        required fields present and valid. It checks for the presence of essential
        site properties, validates RefValue wrappers, and enforces physical constraints.

        Parameters
        ----------
        self : SUEWSConfig
            The SUEWSConfig instance being validated.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If any required field is missing or invalid, or if physical constraints are violated.

        Notes
        -----
        - Required fields include: lat, lng, alt, timezone, surfacearea, z, z0m_in, zdm_in,
          pipecapacity, runofftowater, narp_trans_site, lumps, spartacus, conductance,
          irrigation, anthropogenic_emissions, snow, land_cover, vertical_layers.
        - RefValue wrappers are unwrapped to check for None values.
        - Enforces z0m_in < zdm_in physical constraint.
        """
        from .type import RefValue  # Import here to avoid circular import

        errors = []

        # Required fields that must be present and non-None
        required_fields = [
            "lat",
            "lng",
            "alt",
            "timezone",
            "surfacearea",
            "z",
            "z0m_in",
            "zdm_in",
            "pipecapacity",
            "runofftowater",
            "narp_trans_site",
            "lumps",
            "spartacus",
            "conductance",
            "irrigation",
            "anthropogenic_emissions",
            "snow",
            "land_cover",
            "vertical_layers",
        ]

        for i, site in enumerate(self.sites):
            if not site.properties:
                errors.append(
                    f"Site {i} ({getattr(site, 'name', 'unnamed')}) is missing properties"
                )
                continue

            site_name = getattr(site, "name", f"Site {i}")

            # Check required fields
            for field in required_fields:
                value = getattr(site.properties, field, None)
                if value is None:
                    errors.append(f"{site_name}: Required field '{field}' is missing")
                elif isinstance(value, RefValue) and value.value is None:
                    errors.append(f"{site_name}: Required field '{field}' has no value")

            # Additional physical constraint validation
            if (
                site.properties.z0m_in is not None
                and site.properties.zdm_in is not None
            ):
                z0m_val = _unwrap_value(site.properties.z0m_in)
                zdm_val = _unwrap_value(site.properties.zdm_in)
                if z0m_val is not None and zdm_val is not None and z0m_val >= zdm_val:
                    errors.append(
                        f"{site_name}: z0m_in ({z0m_val}) must be less than zdm_in ({zdm_val})"
                    )

        if errors:
            raise ValueError("; ".join(errors))

        return self

    @model_validator(mode="after")
    def validate_snow_parameters(self) -> "SUEWSConfig":
        """
        Validate snow parameters for all sites in the configuration.

        This validator enforces critical physical constraints on snow parameter values
        for each site. It is migrated from SnowParams.validate_all for centralized
        validation.

        Parameters
        ----------
        self : SUEWSConfig
            The SUEWSConfig instance being validated.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If any snow parameter constraint is violated.

        Notes
        -----
        The following checks are performed for each site:
        - crwmin < crwmax : Critical water content minimum must be less than maximum.
        - 0 <= crwmin <= 1 and 0 <= crwmax <= 1 : Critical water content must be within [0, 1].
        - snowalbmin < snowalbmax : Minimum snow albedo must be less than maximum.
        - 0 <= snowalbmin <= 1 and 0 <= snowalbmax <= 1 : Snow albedo must be within [0, 1].

        These constraints are essential for proper snow modeling and will raise
        a ValueError if violated.
        """
        from .type import RefValue  # Import here to avoid circular import

        errors = []

        for i, site in enumerate(self.sites):
            if not site.properties or not site.properties.snow:
                continue

            site_name = getattr(site, "name", f"Site {i}")
            snow_params = site.properties.snow

            # Extract values using helper for consistent unwrapping
            crwmin_val = _unwrap_value(snow_params.water_holding_capacity_min)
            crwmax_val = _unwrap_value(snow_params.water_holding_capacity_max)
            snowalbmin_val = _unwrap_value(snow_params.snow_albedo_min)
            snowalbmax_val = _unwrap_value(snow_params.snow_albedo_max)

            # Validate critical water content range
            if crwmin_val >= crwmax_val:
                errors.append(
                    f"{site_name}: crwmin ({crwmin_val}) must be less than crwmax ({crwmax_val})"
                )

            # Validate physical bounds for critical water content
            if not (0 <= crwmin_val <= 1):
                errors.append(
                    f"{site_name}: crwmin ({crwmin_val}) must be in range [0, 1]"
                )
            if not (0 <= crwmax_val <= 1):
                errors.append(
                    f"{site_name}: crwmax ({crwmax_val}) must be in range [0, 1]"
                )

            # Validate physical bounds for snow albedo
            if not (0 <= snowalbmin_val <= 1):
                errors.append(
                    f"{site_name}: snowalbmin ({snowalbmin_val}) must be in range [0, 1]"
                )
            if not (0 <= snowalbmax_val <= 1):
                errors.append(
                    f"{site_name}: snowalbmax ({snowalbmax_val}) must be in range [0, 1]"
                )

            # Validate snow albedo range
            if snowalbmin_val >= snowalbmax_val:
                errors.append(
                    f"{site_name}: snowalbmin ({snowalbmin_val}) must be less than snowalbmax ({snowalbmax_val})"
                )

        if errors:
            raise ValueError("; ".join(errors))

        return self

    @model_validator(mode="after")
    def set_default_vegetation_albedo(self) -> "SUEWSConfig":
        """Auto-calculate initial vegetation albedo from LAI state when missing.

        The relationship between canopy albedo and LAI differs between vegetation
        types. As stated in Omidvar et al. (2022) Section 4.1: "For evergreen and
        deciduous trees, alpha_LAI,min (alpha_LAI,max) in Eq. (6) typically
        corresponds to alpha_min (alpha_max), while for grassland a reverse
        relation holds (i.e. alpha_LAI,min corresponds to alpha_max and vice versa)."

        **Trees (evetr, dectr) - Direct relationship (higher LAI -> higher albedo):**

        Tree canopies have dark bark and branches as the background surface.
        At low LAI (sparse canopy), dark woody elements are visible through gaps,
        reducing overall albedo. At high LAI (dense foliage), leaves with higher
        reflectance dominate the surface.

        Formula: alb_id = alb_min + (alb_max - alb_min) * lai_ratio

        **Grass - Reversed relationship (higher LAI -> lower albedo):**

        Grass surfaces have bright soil or litter as the background. At low LAI
        (sparse grass), exposed soil with higher reflectance increases overall
        albedo. At high LAI (dense grass), green blades with lower reflectance
        dominate. Empirical evidence from Mu et al. (2024) shows a significant
        negative correlation (r = -0.462, P < 0.01) between EVI and albedo in
        East Asian grasslands.

        Formula: alb_id = alb_max - (alb_max - alb_min) * lai_ratio

        References:
            - Omidvar et al. (2022). Geosci. Model Dev., 15, 3041-3078.
              https://doi.org/10.5194/gmd-15-3041-2022
            - Mu et al. (2024). Ecological Processes.
              https://doi.org/10.1186/s13717-024-00493-w
            - Cescatti et al. (2012). Remote Sens. Environ., 121, 323-334.
              https://doi.org/10.1016/j.rse.2012.02.019
            - Song (1999). Int. J. Biometeorol., 42(3), 153-157.
              https://doi.org/10.1007/s004840050099
        """
        for site in self.sites:
            if (
                not site.initial_states
                or not site.properties
                or not site.properties.land_cover
            ):
                continue

            land_cover = site.properties.land_cover
            initial_states = site.initial_states

            vegetated_surfaces = [
                (land_cover.evetr, initial_states.evetr, False),
                (land_cover.dectr, initial_states.dectr, False),
                (land_cover.grass, initial_states.grass, True),
            ]

            for surface_props, surface_state, is_grass in vegetated_surfaces:
                if surface_props is None or surface_state is None:
                    continue

                alb_val = _unwrap_value(getattr(surface_state, "alb_id", None))
                if alb_val is not None:
                    continue

                lai_val = _unwrap_value(getattr(surface_state, "lai_id", None))
                if lai_val is None:
                    continue

                lai_params = getattr(surface_props, "lai", None)
                if lai_params is None:
                    continue

                lai_min_val = _unwrap_value(getattr(lai_params, "lai_min", None))
                lai_max_val = _unwrap_value(getattr(lai_params, "lai_max", None))
                # Fallback: LAIParams.lai_min defaults to 0.1; LAIParams.lai_max
                # defaults to None (the DataFrame serialisation path in
                # LAIParams.to_df_state uses 10.0 as its fallback).
                if lai_min_val is None:
                    lai_min_val = LAIParams.model_fields["lai_min"].default
                if lai_max_val is None:
                    lai_max_val = LAIParams.LAIMAX_DF_DEFAULT

                lai_range = lai_max_val - lai_min_val
                if lai_range <= 0:
                    lai_ratio = 0.0
                else:
                    lai_ratio = (lai_val - lai_min_val) / lai_range
                    lai_ratio = min(max(lai_ratio, 0.0), 1.0)

                alb_min_val = _unwrap_value(surface_props.alb_min)
                alb_max_val = _unwrap_value(surface_props.alb_max)
                if alb_min_val is None or alb_max_val is None:
                    continue

                alb_range = alb_max_val - alb_min_val
                if is_grass:
                    alb_calc = alb_max_val - alb_range * lai_ratio
                else:
                    alb_calc = alb_min_val + alb_range * lai_ratio

                # Intentional in-place mutation of the Pydantic model instance.
                # model_validator(mode='after') receives the fully-constructed
                # object, so attribute assignment is safe here. This runs before
                # validate_albedo_ranges (validators execute in definition order).
                surface_state.alb_id = alb_calc

        return self

    @model_validator(mode="after")
    def validate_albedo_ranges(self) -> "SUEWSConfig":
        """
        Validate albedo ranges for vegetated surfaces in all sites.

        This validator enforces that the minimum and maximum albedo values for
        all vegetated surfaces (evergreen trees, deciduous trees, grass) are
        within the physical range [0, 1] and that alb_min <= alb_max. It also
        checks that the initial albedo (alb_id) is within [alb_min, alb_max]
        when available.

        Notes
        -----
        - This validator depends on `set_default_vegetation_albedo` running first.
          (Pydantic v2 executes model_validators in definition order.)
        - Do not reorder these validators.

        Raises
        ------
        ValueError
            If any albedo parameter is out of bounds or inconsistent.
        """
        from .type import RefValue  # Import here to avoid circular import

        errors = []

        for i, site in enumerate(self.sites):
            if not site.properties or not site.properties.land_cover:
                continue

            site_name = getattr(site, "name", f"Site {i}")
            land_cover = site.properties.land_cover

            # Check all vegetated surface types
            vegetated_surfaces = [
                ("evetr", land_cover.evetr, "evergreen trees"),
                ("dectr", land_cover.dectr, "deciduous trees"),
                ("grass", land_cover.grass, "grass"),
            ]

            for surface_name, surface_props, surface_description in vegetated_surfaces:
                if not surface_props:
                    continue

                # Extract albedo values using helper for consistent unwrapping
                alb_min_val = _unwrap_value(surface_props.alb_min)
                alb_max_val = _unwrap_value(surface_props.alb_max)

                # Validate physical bounds for albedo values
                if not (0 <= alb_min_val <= 1):
                    errors.append(
                        f"{site_name} {surface_description}: alb_min ({alb_min_val}) must be in range [0, 1]"
                    )
                if not (0 <= alb_max_val <= 1):
                    errors.append(
                        f"{site_name} {surface_description}: alb_max ({alb_max_val}) must be in range [0, 1]"
                    )

                # Validate albedo range - allow equality for constant albedo
                if alb_min_val > alb_max_val:
                    errors.append(
                        f"{site_name} {surface_description}: alb_min ({alb_min_val}) must be less than or equal to alb_max ({alb_max_val})"
                    )

                # Only validate alb_id range when alb_min <= alb_max
                # (the inverted case is already reported above)
                if (
                    site.initial_states
                    and hasattr(site.initial_states, surface_name)
                    and alb_min_val <= alb_max_val
                ):
                    surface_state = getattr(site.initial_states, surface_name)
                    if surface_state and hasattr(surface_state, "alb_id"):
                        alb_id_val = _unwrap_value(surface_state.alb_id)
                        if alb_id_val is not None:
                            if not (alb_min_val <= alb_id_val <= alb_max_val):
                                errors.append(
                                    f"{site_name} {surface_description}: alb_id ({alb_id_val}) must be in range [alb_min, alb_max] ([{alb_min_val}, {alb_max_val}] provided)"
                                )

        if errors:
            raise ValueError("; ".join(errors))

        return self

    @model_validator(mode="after")
    def validate_deciduous_porosity_ranges(self) -> "SUEWSConfig":
        """
        Validate porosity ranges for deciduous trees in all sites.

        Ensures that the minimum and maximum porosity parameters for deciduous trees
        are within the physical range [0, 1] and that pormin_dec < pormax_dec.

        Notes
        -----
        - pormin_dec: Minimum porosity for deciduous trees (should be >= 0 and <= 1)
        - pormax_dec: Maximum porosity for deciduous trees (should be >= 0 and <= 1)
        - pormin_dec must be strictly less than pormax_dec

        Raises
        ------
        ValueError
            If any porosity parameter is out of bounds or inconsistent.
        """
        from .type import RefValue  # Import here to avoid circular import

        errors = []

        for i, site in enumerate(self.sites):
            if (
                not site.properties
                or not site.properties.land_cover
                or not site.properties.land_cover.dectr
            ):
                continue

            site_name = getattr(site, "name", f"Site {i}")
            dectr_props = site.properties.land_cover.dectr

            # Extract porosity values using helper for consistent unwrapping
            pormin_dec_val = _unwrap_value(dectr_props.porosity_min_deciduous)
            pormax_dec_val = _unwrap_value(dectr_props.porosity_max_deciduous)

            # Validate physical bounds for porosity
            if not (0 <= pormin_dec_val <= 1):
                errors.append(
                    f"{site_name} deciduous trees: pormin_dec ({pormin_dec_val}) must be in range [0, 1]"
                )
            if not (0 <= pormax_dec_val <= 1):
                errors.append(
                    f"{site_name} deciduous trees: pormax_dec ({pormax_dec_val}) must be in range [0, 1]"
                )

            # Validate porosity range
            if pormin_dec_val >= pormax_dec_val:
                errors.append(
                    f"{site_name} deciduous trees: pormin_dec ({pormin_dec_val}) must be less than pormax_dec ({pormax_dec_val})"
                )

        if errors:
            raise ValueError("; ".join(errors))

        return self

    def _show_validation_summary(self) -> None:
        """Show a concise summary of validation issues."""
        ## Check if we have a yaml path stored
        yaml_path = getattr(self, "_yaml_path", None)

        if yaml_path:
            ## When loaded from YAML, we know the source file
            yaml_path_obj = Path(yaml_path)
            annotated_path = (
                yaml_path_obj.parent / f"{yaml_path_obj.stem}_annotated.yml"
            )
            auto_generate = getattr(self, "_auto_generate_annotated", False)

            if auto_generate:
                fix_instructions = (
                    f"To see detailed fixes for each parameter: please refer to inline guidance "
                    f"in '{annotated_path}' that will be generated"
                )
            else:
                fix_instructions = (
                    f"To see detailed fixes for each parameter:\n"
                    f"   Run: config.generate_annotated_yaml('{yaml_path}')\n"
                    f"   This will create: {annotated_path}\n"
                    f"   with inline guidance showing exactly where to add missing parameters"
                )
        else:
            fix_instructions = (
                f"To see detailed fixes for each parameter:\n"
                f"   1. Save your configuration to a YAML file\n"
                f"   2. Call config.generate_annotated_yaml('your_config.yml')\n"
                f"   3. An annotated file with inline guidance will be generated"
            )

        ## Build the summary message
        summary_message = (
            f"\n{'=' * 60}\n"
            f"VALIDATION SUMMARY\n"
            f"{'=' * 60}\n"
            f"Found {self._validation_summary['total_warnings']} parameter issue(s) across "
            f"{len(self._validation_summary['sites_with_issues'])} site(s).\n\n"
        )

        ## Add issue types
        summary_message += (
            f"Issue types:\n"
            f"  - "
            + "\n  - ".join(sorted(self._validation_summary["issue_types"]))
            + "\n\n"
        )

        ## Add detailed messages if available
        if self._validation_summary.get("detailed_messages"):
            summary_message += "Detailed issues:\n"
            for msg in self._validation_summary["detailed_messages"]:
                summary_message += f"  - {msg}\n"
            summary_message += "\n"

        ## Add fix instructions
        summary_message += f"{fix_instructions}\n{'=' * 60}"

        ## Log the complete summary
        logger_supy.warning(summary_message)

        ## Optionally generate the annotated YAML file automatically
        auto_generate = getattr(self, "_auto_generate_annotated", False)
        if auto_generate and yaml_path and Path(yaml_path).exists():
            try:
                generated_path = self.generate_annotated_yaml(yaml_path)
                logger_supy.info(f"Annotated YAML file generated: {generated_path}")
            except Exception as e:
                logger_supy.info(f"Could not generate annotated YAML: {e}")

    def _validate_site_parameters(self, site: Site, site_index: int) -> None:
        """
        Validate all parameters for a single site.

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Notes
        -----
        This method performs a series of checks on the provided site, including:
        - Conductance parameters
        - CO2 emission parameters
        - Land cover and surface parameters
        - LAI (Leaf Area Index) range parameters

        Any issues found are recorded in the validation summary for reporting.
        """

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

        # Validate LAI range parameters
        if hasattr(site.properties, "land_cover") and site.properties.land_cover:
            if self._check_lai_ranges(site.properties.land_cover, site_name):
                site_has_issues = True

        # Track sites with issues
        if (
            site_has_issues
            and site_name not in self._validation_summary["sites_with_issues"]
        ):
            self._validation_summary["sites_with_issues"].append(site_name)

    def _check_conductance(self, conductance, site_name: str) -> bool:
        """
        Check for missing conductance parameters.

        Parameters
        ----------
        conductance : object
            The conductance object to validate.
        site_name : str
            Name of the site being validated.

        Returns
        -------
        bool
            True if any critical conductance parameters are missing, False otherwise.

        Notes
        -----
        This method checks for the presence of critical conductance parameters
        required for evapotranspiration calculations. If any are missing, a warning
        is recorded in the validation summary.
        """
        from ..validation.core.utils import check_missing_params

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
        """
        Check for missing CO2 emission parameters.

        This method checks for the presence of critical CO2 emission parameters
        required for accurate model calculations. If any are missing, a warning
        is recorded in the validation summary.

        Parameters
        ----------
        co2 : object
            The CO2 emissions object to validate.
        site_name : str
            Name of the site being validated.

        Returns
        -------
        bool
            True if any critical CO2 emission parameters are missing, otherwise False.

        Notes
        -----
        The following parameters are checked:
        - co2pointsource: CO2 point source emission factor
        - ef_umolco2perj: CO2 emission factor per unit of fuel energy
        - frfossilfuel_heat: Fraction of heating energy from fossil fuels
        - frfossilfuel_nonheat: Fraction of non-heating energy from fossil fuels

        Any missing parameters are added to the validation summary.
        """
        from ..validation.core.utils import check_missing_params

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
        """
        Check land cover parameters for all surface types.

        This method validates the parameters for each surface type within the land cover.
        It checks for missing or invalid parameters and records any issues found.

        Returns
        -------
        bool
            True if any issues are found, False otherwise.

        Notes
        -----
        - Iterates over all defined surface types (bldgs, grass, dectr, evetr, bsoil, paved, water).
        - For each surface type present in the land cover, checks for required parameters.
        - Delegates detailed surface parameter checks to `_check_surface_parameters`.
        - Accumulates and returns whether any issues were found.
        """
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
        """
        Check parameters for a specific surface type.

        Parameters
        ----------
        surface : object
            The surface object to validate (e.g., bldgs, grass, dectr, etc.).
        surface_type : str
            The type of surface being checked (e.g., "bldgs", "grass").
        site_name : str
            The name of the site for reporting issues.

        Returns
        -------
        bool
            True if any issues are found for the given surface, False otherwise.

        Notes
        -----
        - For building surfaces (bldgs) with sfr > 0.05, checks for required parameters
          such as building height and frontal area index.
        - For vegetated surfaces (grass, dectr, evetr), checks for required CO2 and
          respiration parameters.
        - For surfaces with thermal layers, checks for completeness of thermal properties.
        - Only surfaces with a positive surface fraction (sfr > 0) are validated.
        - Issues found are recorded in the validation summary for reporting.
        """
        from ..validation.core.utils import check_missing_params

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
                    "beta_bio_co2": "Biogenic CO2 exchange coefficient",
                    "alpha_bio_co2": "Biogenic CO2 exchange coefficient",
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

            # Check thermal layers only for surfaces that typically need them
            # or where user has explicitly provided non-None values
            if hasattr(surface, "thermal_layers") and surface.thermal_layers:
                # Only validate if at least one thermal property is explicitly set
                thermal_layers = surface.thermal_layers
                has_thermal_data = (
                    (hasattr(thermal_layers, "dz") and thermal_layers.dz is not None)
                    or (hasattr(thermal_layers, "k") and thermal_layers.k is not None)
                    or (
                        hasattr(thermal_layers, "rho_cp")
                        and thermal_layers.rho_cp is not None
                    )
                )

                if has_thermal_data:
                    if self._check_thermal_layers(
                        surface.thermal_layers, surface_type, site_name
                    ):
                        has_issues = True

        return has_issues

    def _check_thermal_layers(
        self, thermal_layers, surface_type: str, site_name: str
    ) -> bool:
        """
        Check thermal layer parameters for a specific surface type.

        This method validates the presence and completeness of thermal layer parameters
        (dz, k, rho_cp) for a given surface type (e.g., walls, roofs, etc.) in the
        configuration. It also detects common YAML naming issues (e.g., 'cp' instead of 'rho_cp').

        Parameters
        ----------
        thermal_layers : object
            The thermal_layers object to validate.
        surface_type : str
            The type of surface being checked (e.g., "walls", "roofs").
        site_name : str
            The name of the site for reporting issues.

        Returns
        -------
        bool
            True if any issues are found for the given thermal layers, False otherwise.

        Notes
        -----
        - Checks that dz (layer thickness), k (thermal conductivity), and rho_cp (volumetric heat capacity)
          are present and non-empty lists (either as plain lists or RefValue wrappers).
        - If 'cp' is found instead of 'rho_cp' in the YAML, records a naming issue instead of a missing parameter.
        - Updates the validation summary with the type of issue found.
        """
        missing_params = []

        def _is_valid_layer_array(field):
            # Handle both RefValue wrappers and plain lists
            if hasattr(field, "value") and isinstance(field.value, list):
                # RefValue wrapper case
                return len(field.value) > 0
            elif isinstance(field, list):
                # Plain list case
                return len(field) > 0
            else:
                # Neither RefValue nor list
                return False

        if not hasattr(thermal_layers, "dz") or not _is_valid_layer_array(
            thermal_layers.dz
        ):
            missing_params.append("dz (Layer thickness)")
        if not hasattr(thermal_layers, "k") or not _is_valid_layer_array(
            thermal_layers.k
        ):
            missing_params.append("k (Thermal conductivity)")

        missing_rho_cp = not hasattr(
            thermal_layers, "rho_cp"
        ) or not _is_valid_layer_array(thermal_layers.rho_cp)
        if missing_rho_cp:
            missing_params.append("rho_cp (Volumetric heat capacity)")

        if missing_params:
            # Check if this is a cp naming issue (cp instead of rho_cp)
            yaml_path = getattr(self, "_yaml_path", None)
            surface_path = f"sites/0/properties/land_cover/{surface_type}"

            if (
                missing_rho_cp
                and yaml_path
                and self._check_raw_yaml_for_cp_field(yaml_path, surface_path)
            ):
                # This is a naming issue, not a missing parameter issue
                self._validation_summary["total_warnings"] += 1
                self._validation_summary["issue_types"].add(
                    "Incorrect naming of thermal layer parameters"
                )
            else:
                # Regular missing parameters
                self._validation_summary["total_warnings"] += len(missing_params)
                self._validation_summary["issue_types"].add(
                    "Missing thermal layer parameters"
                )
            return True
        return False

    def _check_lai_ranges(self, land_cover, site_name: str) -> bool:
        """
        Check LAI (Leaf Area Index) range parameters for vegetation surfaces.

        This method validates the LAI parameters for all vegetated surface types
        (grass, dectr, evetr) within the provided land cover. It checks that:
        - lai_min <= lai_max
        - base_temperature <= gddfull

        Returns
        -------
        bool
            True if any issues are found, False otherwise.

        Notes
        -----
        - Issues found are recorded in the validation summary for reporting.
        - Uses consistent unwrapping of RefValue wrappers.
        """
        has_issues = False

        # Initialize validation summary if it doesn't exist (for testing)
        if not hasattr(self, "_validation_summary"):
            self._validation_summary = {
                "total_warnings": 0,
                "sites_with_issues": [],
                "issue_types": set(),
                "yaml_path": getattr(self, "_yaml_path", None),
                "detailed_messages": [],
                "info_messages": [],
            }

        # Return early if no land cover
        if land_cover is None:
            return False

        # Check vegetation surface types that have LAI parameters
        vegetation_surfaces = ["grass", "dectr", "evetr"]

        for surface_type in vegetation_surfaces:
            if hasattr(land_cover, surface_type):
                surface = getattr(land_cover, surface_type)
                if surface and hasattr(surface, "lai"):
                    lai = surface.lai
                    if lai:
                        # Check lai_min vs lai_max
                        if (
                            hasattr(lai, "lai_min")
                            and lai.lai_min is not None
                            and hasattr(lai, "lai_max")
                            and lai.lai_max is not None
                        ):
                            lai_min_val = (
                                lai.lai_min.value
                                if hasattr(lai.lai_min, "value")
                                else lai.lai_min
                            )
                            lai_max_val = (
                                lai.lai_max.value
                                if hasattr(lai.lai_max, "value")
                                else lai.lai_max
                            )

                            if lai_min_val > lai_max_val:
                                self._validation_summary["total_warnings"] += 1
                                self._validation_summary["issue_types"].add(
                                    "LAI range validation"
                                )
                                self._validation_summary["detailed_messages"].append(
                                    f"{site_name} {surface_type}: lai_min ({lai_min_val}) must be <= lai_max ({lai_max_val})"
                                )
                                has_issues = True

                        # Check base_temperature vs gddfull
                        if (
                            hasattr(lai, "base_temperature")
                            and lai.base_temperature is not None
                            and hasattr(lai, "gdd_full")
                            and lai.gdd_full is not None
                        ):
                            baset_val = (
                                lai.base_temperature.value
                                if hasattr(lai.base_temperature, "value")
                                else lai.base_temperature
                            )
                            gddfull_val = (
                                lai.gdd_full.value
                                if hasattr(lai.gdd_full, "value")
                                else lai.gdd_full
                            )

                            if baset_val > gddfull_val:
                                self._validation_summary["total_warnings"] += 1
                                self._validation_summary["issue_types"].add(
                                    "LAI range validation"
                                )
                                self._validation_summary["detailed_messages"].append(
                                    f"{site_name} {surface_type}: base_temperature ({baset_val}) must be <= gddfull ({gddfull_val})"
                                )
                                has_issues = True

        return has_issues

    def _check_raw_yaml_for_cp_field(self, yaml_path: str, surface_path: str) -> bool:
        """Check if the raw YAML file has 'cp' instead of 'rho_cp' in thermal_layers."""
        try:
            import yaml

            with open(yaml_path, "r") as f:
                data = yaml.safe_load(f)

            # Navigate to the surface using path like "sites[0]/properties/land_cover/paved"
            path_parts = surface_path.replace("[", "/").replace("]", "").split("/")

            current = data
            for part in path_parts:
                if part.isdigit():
                    current = current[int(part)]
                elif part in current:
                    current = current[part]
                else:
                    return False

            # Check if thermal_layers has 'cp' field
            if isinstance(current, dict) and "thermal_layers" in current:
                thermal_layers = current["thermal_layers"]
                if isinstance(thermal_layers, dict) and "cp" in thermal_layers:
                    return True

        except Exception:
            pass

        return False

    def _check_thermal_layers_naming_issue(
        self, thermal_layers, surface_type: str, site_name: str
    ) -> bool:
        """Check for thermal layer naming issues (cp vs rho_cp). Returns True if issues found."""
        self._validation_summary["total_warnings"] += 1
        self._validation_summary["issue_types"].add(
            "Incorrect naming of thermal layer parameters"
        )
        if site_name not in self._validation_summary["sites_with_issues"]:
            self._validation_summary["sites_with_issues"].append(site_name)
        return True

    def _needs_stebbs_validation(self) -> bool:
        """
        Return True if STEBBS should be validated,
        i.e. physics.stebbs == 1.
        """

        if not hasattr(self.model, "physics") or not hasattr(
            self.model.physics, "stebbs"
        ):
            return False

        stebbs_method = self.model.physics.stebbs

        if hasattr(stebbs_method, "value"):
            stebbs_method = stebbs_method.value
        if hasattr(stebbs_method, "__int__"):
            stebbs_method = int(stebbs_method)
        if isinstance(stebbs_method, str) and stebbs_method == "1":
            stebbs_method = 1

        # print(f"Final stebbs_method value for validation: {stebbs_method} (type: {type(stebbs_method)})")

        return stebbs_method == 1

    def _validate_stebbs(self, site: Site, site_index: int) -> List[str]:
        """
        Validate required STEBBS and building archetype parameters when stebbs_method==1.

        If `stebbs_method==1`, this function enforces that both `site.properties.stebbs`
        and `site.properties.building_archetype` contain all required parameters with
        non-null values. The required parameter lists are defined by
        `STEBBS_REQUIRED_PARAMS` and `ARCHETYPE_REQUIRED_PARAMS`, with dynamic
        filtering based on the Window-to-Wall Ratio (WWR).

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Returns
        -------
        issues : list of str
            List of issue messages describing any missing required parameters.

        Notes
        -----
        - If WWR is 0.0, window parameters are excluded from the required lists.
        - If WWR is 1.0, external wall parameters are excluded from the required lists.
        - All missing parameters are reported, regardless of count.
        """
        issues: List[str] = []

        ## First check if properties exists and is not None
        if not hasattr(site, "properties") or site.properties is None:
            issues.append(
                "Missing 'properties' section (required for STEBBS validation)"
            )
            return issues

        props = site.properties

        ## Must have a stebbs block
        if not hasattr(props, "stebbs") or props.stebbs is None:
            issues.append("Missing 'stebbs' section (required when stebbs=1)")
            return issues

        ## Must have a building_archetype block
        if not hasattr(props, "building_archetype") or props.building_archetype is None:
            # Do not return early — create an empty container so we can list all
            # missing ARCHETYPE_REQUIRED_PARAMS alongside missing stebbs params.
            building_archetype = SimpleNamespace()
        else:
            building_archetype = props.building_archetype

        stebbs = props.stebbs

        missing_params: List[str] = []

        # helper to check and append missing params
        def _check_required(container, param_list):
            for param in param_list:
                # existence
                if not hasattr(container, param):
                    missing_params.append(param)
                    continue
                param_obj = getattr(container, param)
                # unwrap any RefValue/Enum wrappers
                val = _unwrap_value(param_obj) if param_obj is not None else None
                if val is None:
                    missing_params.append(param)

        # Check if window_to_wall_ratio is present and zero or one
        wwr = getattr(building_archetype, "window_to_wall_ratio", None)
        wwr_val = _unwrap_value(wwr) if wwr is not None else None

        # Window parameter lists
        window_params_stebbs = [
            "window_internal_convection_coefficient",
            "window_external_convection_coefficient",
        ]
        window_params_bldgarc = [
            "window_thickness",
            "window_effective_conductivity",
            "window_density",
            "window_specific_heat_capacity",
            "window_external_emissivity",
            "window_internal_emissivity",
            "window_transmissivity",
            "window_absorptivity",
            "window_reflectivity",
        ]

        # Wall parameter lists for window_to_wall_ratio == 1.0
        wall_params_stebbs = [
            "wall_external_convection_coefficient",
            "wall_internal_convection_coefficient",
            ]
        wall_params_bldgarc = [
            "wall_external_emissivity",
            "wall_internal_emissivity",
            "wall_transmissivity",
            "wall_absorptivity",
            "wall_reflectivity",
            "wall_thickness",
            "wall_effective_conductivity",
            "wall_density",
            "wall_specific_heat_capacity",
        ]

        # Check setpoint value
        setpointmethod = getattr(self.model.physics, "setpoint", None)
        setpointmethod_val = _unwrap_value(setpointmethod) if setpointmethod is not None else None
        try:
            setpointmethod_val = int(setpointmethod_val)
        except (TypeError, ValueError):
            setpointmethod_val = None

        # Setpoint parameter groups
        setpoint_params_bldgarc = [
            "heating_setpoint_temperature",
            "cooling_setpoint_temperature",
        ]
        setpoint_profile_params_bldgarc = [
            "heating_setpoint_temperature_profile",
            "cooling_setpoint_temperature_profile",
        ]

        # Daylight control parameter groups
        daylightcontrol = getattr(stebbs, "daylight_control", None)
        daylightcontrol_val = _unwrap_value(daylightcontrol) if daylightcontrol is not None else None
        try:
            daylightcontrol_val = int(daylightcontrol_val)
        except (TypeError, ValueError):
            daylightcontrol_val = None

        daylightcontrol_params_stebbs = ["lighting_illuminance_threshold"]

        # Determine which params to require based on WWR
        if wwr_val == 0.0:
            # Exclude window params if WWR is zero
            stebbs_required = [p for p in self.STEBBS_REQUIRED_PARAMS if p not in window_params_stebbs]
            archetype_required = [p for p in self.ARCHETYPE_REQUIRED_PARAMS if p not in window_params_bldgarc]
        elif wwr_val == 1.0:
            # Exclude external wall params if WWR is one
            stebbs_required = [p for p in self.STEBBS_REQUIRED_PARAMS if p not in wall_params_stebbs]
            archetype_required = [p for p in self.ARCHETYPE_REQUIRED_PARAMS if p not in wall_params_bldgarc]
        else:
            stebbs_required = self.STEBBS_REQUIRED_PARAMS
            archetype_required = self.ARCHETYPE_REQUIRED_PARAMS

        # Exclude setpoint params based on setpointmethod
        if setpointmethod_val == 2:
            # Only require the profile params, not the scalar setpoint temps
            archetype_required = [
            p for p in archetype_required if p not in setpoint_params_bldgarc
            ]
        else:
            # Only require the scalar setpoint temps, not the profile params
            archetype_required = [
            p for p in archetype_required if p not in setpoint_profile_params_bldgarc
            ]

        # Exclude daylight control params based on daylightcontrol
        if daylightcontrol_val == 0:
            stebbs_required = [p for p in stebbs_required if p not in daylightcontrol_params_stebbs]

        # Validate stebbs required params
        _check_required(stebbs, stebbs_required)
        # Validate building_archetype required params
        _check_required(building_archetype, archetype_required)

        ## Always list all missing parameters, regardless of count
        if missing_params:
            param_list = ", ".join(missing_params)
            issues.append(
                f"Missing required STEBBS parameters: {param_list} (required when stebbs=1)"
            )

        return issues

    def _needs_rsl_validation(self) -> bool:
        """
        Determine if the RSL method is explicitly enabled.

        Returns
        -------
        bool
            True if `rsl_method` is set to 2 and was explicitly configured by the user,
            False otherwise.

        Notes
        -----
        - Validation is only triggered if `rsl_method == 2` AND the value was explicitly set
          (not just the default value).
        - Uses Pydantic's `model_fields_set` to distinguish user-provided values from defaults.
        """
        if not hasattr(self.model, "physics") or not hasattr(
            self.model.physics, "roughness_sublayer"
        ):
            return False

        rm = self.model.physics.roughness_sublayer
        method = getattr(rm, "value", rm)
        try:
            method = int(method)
        except (TypeError, ValueError):
            pass

        # Only validate if method == 2 AND it was explicitly set
        if method == 2:
            return self._is_physics_explicitly_configured("roughness_sublayer")
        return False

    def _validate_rsl(self, site: Site, site_index: int) -> List[str]:
        """
        Validate RSL (Roughness Sublayer) method requirements for a site.

        If `rsl_method == 2`, then for any site where `bldgs.sfr > 0`,
        `bldgs.faibldg` must be set and non-null.

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Returns
        -------
        issues : list of str
            List of issue messages describing any missing required parameters.

        Notes
        -----
        - Only applies if `rsl_method == 2` is explicitly set.
        - Checks that for each site with buildings (`bldgs.sfr > 0`), the
          frontal area index (`bldgs.faibldg`) is provided.
        """
        issues: List[str] = []
        props = getattr(site, "properties", None)
        if not props or not hasattr(props, "land_cover") or not props.land_cover:
            return issues

        lc = props.land_cover
        bldgs = getattr(lc, "bldgs", None)
        if not bldgs or not hasattr(bldgs, "sfr") or bldgs.sfr is None:
            return issues

        sfr = getattr(bldgs.sfr, "value", bldgs.sfr)
        try:
            sfr = float(sfr)
        except (TypeError, ValueError):
            sfr = 0.0

        if sfr > 0:
            faibldg = getattr(bldgs, "faibldg", None)
            val = getattr(faibldg, "value", faibldg) if faibldg is not None else None
            if val is None:
                site_name = getattr(site, "name", f"Site {site_index}")
                issues.append(
                    f"{site_name}: for rsl_method=2 and bldgs.sfr={sfr}, bldgs.faibldg must be set"
                )
        return issues

    def _needs_storage_validation(self) -> bool:
        """
        Determine if DyOHM storage-heat method is explicitly enabled.

        Returns
        -------
        bool
            True if `storage_heat_method` is set to 6 or 7 and was explicitly configured by the user,
            False otherwise.

        Notes
        -----
        - Validation is only triggered if `storage_heat_method` is 6 or 7 AND the value was explicitly set
          (not just the default value).
        - Uses Pydantic's `model_fields_set` to distinguish user-provided values from defaults.
        """
        if not hasattr(self.model, "physics") or not hasattr(
            self.model.physics, "storage_heat"
        ):
            return False

        shm = getattr(self.model.physics.storage_heat, "value", None)
        try:
            shm = int(shm)
        except (TypeError, ValueError):
            pass

        # Only validate if method == 6 or 7 AND it was explicitly set
        if shm == 6 or shm == 7:
            return self._is_physics_explicitly_configured("storage_heat")
        return False
    
    def _needs_same_albedo_wall_validation(self) -> bool:
        """
        Check if the same_albedo_wall option is enabled.

        Returns
        -------
        bool
            True if the 'same_albedo_wall' option is set to 1, False otherwise.

        Notes
        -----
        This method determines whether the validation for identical wall albedo
        across all wall layers should be performed, based on the configuration
        of the 'same_albedo_wall' parameter in the model physics settings.
        """
        return self._needs_same_albedo_validation("same_albedo_wall")

    def _needs_same_albedo_roof_validation(self) -> bool:
        """
        Check if the same_albedo_roof option is enabled.

        Returns
        -------
        bool
            True if the 'same_albedo_roof' option is set to 1, False otherwise.

        Notes
        -----
        This method determines whether the validation for identical roof albedo
        across all roof layers should be performed, based on the configuration
        of the 'same_albedo_roof' parameter in the model physics settings.
        """
        return self._needs_same_albedo_validation("same_albedo_roof")

    def _needs_same_albedo_validation(self, attr: str) -> bool:
        """
        Helper for same_albedo_wall/roof validation.

        Parameters
        ----------
        attr : str
            Name of the physics attribute to check (e.g., "same_albedo_wall", "same_albedo_roof").

        Returns
        -------
        bool
            True if the specified attribute is set to 1 (enabled), False otherwise.

        Notes
        -----
        This method checks whether the given attribute in the model physics
        configuration is explicitly enabled (set to 1), which triggers
        validation for identical surface properties (e.g., albedo or emissivity)
        across all relevant layers.
        """
        physics = getattr(self.model, "physics", None)
        if not physics or not hasattr(physics, attr):
            return False
        val = getattr(getattr(physics, attr), "value", getattr(physics, attr))
        try:
            return int(val) == 1
        except (TypeError, ValueError):
            return False

    def _needs_same_emissivity_wall_validation(self) -> bool:
        """
        Check if the same_emissivity_wall option is enabled.

        Returns
        -------
        bool
            True if the 'same_emissivity_wall' option is set to 1, False otherwise.

        Notes
        -----
        This method determines whether the validation for identical wall emissivity
        across all wall layers should be performed, based on the configuration
        of the 'same_emissivity_wall' parameter in the model physics settings.
        """
        return self._needs_same_emissivity_validation("same_emissivity_wall")

    def _needs_same_emissivity_roof_validation(self) -> bool:
        """
        Check if the same_emissivity_roof option is enabled.

        Returns
        -------
        bool
            True if the 'same_emissivity_roof' option is set to 1, False otherwise.

        Notes
        -----
        This method determines whether the validation for identical roof emissivity
        across all roof layers should be performed, based on the configuration
        of the 'same_emissivity_roof' parameter in the model physics settings.
        """
        return self._needs_same_emissivity_validation("same_emissivity_roof")

    def _needs_same_emissivity_validation(self, attr: str) -> bool:
        """
        Helper for same_emissivity_wall/roof validation.

        Parameters
        ----------
        attr : str
            Name of the physics attribute to check (e.g., "same_emissivity_wall", "same_emissivity_roof").

        Returns
        -------
        bool
            True if the specified attribute is set to 1 (enabled), False otherwise.

        Notes
        -----
        This method checks whether the given attribute in the model physics
        configuration is explicitly enabled (set to 1), which triggers
        validation for identical surface emissivity across all relevant layers.
        """
        physics = getattr(self.model, "physics", None)
        if not physics or not hasattr(physics, attr):
            return False
        val = getattr(getattr(physics, attr), "value", getattr(physics, attr))
        try:
            return int(val) == 1
        except (TypeError, ValueError):
            return False

    def _is_physics_explicitly_configured(self, option_name: str) -> bool:
        """Check whether a physics option was explicitly set by the user.

        Uses Pydantic v2 ``model_fields_set`` to distinguish user-provided
        values from defaults, so conditional validation only fires when the
        user actively chose the option.

        Parameters
        ----------
        option_name : str
            Name of the physics field to check (e.g. ``"rsl_method"``).
        """
        physics = getattr(self.model, "physics", None)
        return bool(physics and hasattr(physics, "model_fields_set") and option_name in physics.model_fields_set)

    def _validate_storage(self, site: Site, site_index: int) -> List[str]:
        """
        Validate DyOHM storage-heat method requirements for a site.

        This function checks that all required parameters for the DyOHM storage-heat
        method (storage_heat_method 6 or 7) are present and valid for the given site.
        It ensures that vertical_layers.walls, thermal_layers, and initial_states
        arrays are non-empty and contain only numeric values, and that lambda_c is set.

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Returns
        -------
        issues : list of str
            List of issue messages describing any missing or invalid parameters.

        Notes
        -----
        - Checks for presence and validity of vertical_layers.walls and their thermal_layers.
        - Ensures that dz, k, and rho_cp arrays are non-empty and numeric.
        - Validates initial_states.qn_surfs and dqndt_surf arrays.
        - Checks that lambda_c is set and non-null.
        """
        issues: List[str] = []

        site_name = getattr(site, "name", f"Site {site_index}")
        props = getattr(site, "properties", None)
        states = getattr(site, "initial_states", None)
        if not props:
            return issues

        vl = getattr(props, "vertical_layers", None)
        walls = getattr(vl, "walls", None) if vl else None

        if not walls or len(walls) == 0:
            issues.append(
                f"{site_name}: storage_heat_method 6 or 7 (DyOHM) selected → missing vertical_layers.walls"
            )
            return issues

        th = getattr(walls[0], "thermal_layers", None)
        for arr in ("dz", "k", "rho_cp"):
            field = getattr(th, arr, None) if th else None
            vals = getattr(field, "value", None) if field else None
            if (
                not isinstance(vals, list)
                or len(vals) == 0
                or any(v is None for v in vals)
                or any(not isinstance(v, (int, float)) for v in vals)
            ):
                issues.append(
                    f"{site_name}: storage_heat_method 6 or 7 (DyOHM) selected → "
                    f"thermal_layers.{arr} must be a non‐empty list of numeric values (no nulls)"
                )

        for arr in ("qn_surfs", "dqndt_surf"):
            field = getattr(states, arr, None) if states else None
            # Handle both RefValue with .value and plain list
            if hasattr(field, "value"):
                vals = field.value
            else:
                vals = field
            if (
                not isinstance(vals, list)
                or len(vals) == 0
                or any(v is None for v in vals)
                or any(not isinstance(v, (int, float)) for v in vals)
            ):
                issues.append(
                    f"{site_name}: storage_heat_method 6 or 7 (DyOHM) selected → "
                    f"initial_states.{arr} must be a non‐empty list of numeric values (no nulls)"
                )

        lam = getattr(getattr(props, "lambda_c", None), "value", None)
        if lam in (None, ""):
            issues.append(
                f"{site_name}: storage_heat_method 6 or 7 (DyOHM) selected → properties.lambda_c must be set and non-null"
            )

        return issues

    def _validate_same_surface_property(
        self,
        site: Site,
        site_index: int,
        surface_type: str,
        layers_attr: str,
        layer_field: str,
        archetype_attr: str,
        param_name: str,
        property_name: str,
    ) -> List[str]:
        """Validate that a surface property is uniform across layers and matches the archetype.

        Used by same_albedo and same_emissivity validators. Checks that:
        - All values of ``layer_field`` in vertical_layers.<layers_attr> are identical
        - That common value equals properties.building_archetype.<archetype_attr>

        Parameters
        ----------
        layer_field : str
            Attribute name on each layer object (e.g. "alb", "emis").
        archetype_attr : str
            Attribute name on building_archetype (e.g. "wall_reflectivity").
        property_name : str
            Human-readable name for messages (e.g. "albedo", "emissivity").
        """
        issues: List[str] = []
        site_name = getattr(site, "name", f"Site {site_index}")

        # --- Get layers ---
        props = getattr(site, "properties", None)
        vl = getattr(props, "vertical_layers", None) if props is not None else None
        layers = getattr(vl, layers_attr, None) if vl is not None else None

        if not layers:
            issues.append(
                f"{site_name}: {param_name}=1, so vertical_layers.{layers_attr} must be defined and non-empty"
            )
            return issues

        vals: List[float] = []

        for i, layer in enumerate(layers):
            field = getattr(layer, layer_field, None)
            if field is None:
                issues.append(
                    f"{site_name}: {param_name}=1, so {layers_attr}[{i}].{layer_field} must be set"
                )
                continue

            raw = getattr(field, "value", field)
            try:
                val_float = float(raw)
                vals.append(val_float)
            except (TypeError, ValueError):
                issues.append(
                    f"{site_name}: {param_name}=1 but {layers_attr}[{i}].{layer_field} ({raw!r}) is not numeric"
                )

        if len(vals) == 0:
            issues.append(
                f"{site_name}: {param_name}=1 but no valid {layers_attr} {property_name} values found"
            )
            return issues

        # --- Check that all values are identical ---
        first_val = vals[0]
        tol = SAME_SURFACE_TOL
        mismatching = [
            (i, v) for i, v in enumerate(vals) if abs(v - first_val) > tol
        ]

        if mismatching:
            all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in enumerate(vals))
            issues.append(
                f"{site_name}: {param_name}=1, so all {layers_attr} {property_name} values must be identical; "
                f"found values: {all_str}"
            )

        # --- Get building_archetype.<archetype_attr> ---
        ba = getattr(props, "building_archetype", None)
        if ba is None:
            issues.append(
                f"{site_name}: {param_name}=1, so properties.building_archetype must be defined"
            )
            return issues

        arch_field = getattr(ba, archetype_attr, None)
        if arch_field is None:
            issues.append(
                f"{site_name}: {param_name}=1, so properties.building_archetype.{archetype_attr} must be set"
            )
            return issues

        arch_raw = getattr(arch_field, "value", arch_field)
        try:
            arch_val = float(arch_raw)
        except (TypeError, ValueError):
            issues.append(
                f"{site_name}: {param_name}=1 but properties.building_archetype.{archetype_attr} "
                f"({arch_raw!r}) is not numeric"
            )
            return issues

        # --- Check equality between common layer value and archetype value ---
        if abs(first_val - arch_val) > tol:
            all_str = ", ".join(f"{layers_attr}[{i}]={v}" for i, v in enumerate(vals))
            issues.append(
                f"{site_name}: {param_name}=1, so common {layers_attr} {property_name} (found {all_str}) must equal "
                f"properties.building_archetype.{archetype_attr} ({arch_val})"
            )

        return issues

    def _validate_same_albedo_wall(self, site: Site, site_index: int) -> List[str]:
        """
        Validate that wall albedo is uniform across all wall layers and matches the building archetype.

        Returns
        -------
        list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - Checks that all wall albedo values in vertical_layers.walls are identical (within tolerance).
        - Checks that the common wall albedo matches properties.building_archetype.wall_reflectivity.
        - Used when same_albedo_wall option is enabled.
        """
        return self._validate_same_surface_property(
            site, site_index, "wall", "walls", "alb", "wall_reflectivity",
            "same_albedo_wall", "albedo",
        )

    def _validate_same_albedo_roof(self, site: Site, site_index: int) -> List[str]:
        """
        Validate that roof albedo is uniform across all roof layers and matches the building archetype.

        Returns
        -------
        list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - Checks that all roof albedo values in vertical_layers.roofs are identical (within tolerance).
        - Checks that the common roof albedo matches properties.building_archetype.roof_reflectivity.
        - Used when same_albedo_roof option is enabled.
        """
        return self._validate_same_surface_property(
            site, site_index, "roof", "roofs", "alb", "roof_reflectivity",
            "same_albedo_roof", "albedo",
        )

    def _validate_same_emissivity_wall(self, site: Site, site_index: int) -> List[str]:
        """
        Validate that wall emissivity is uniform across all wall layers and matches the building archetype.

        Returns
        -------
        list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - Checks that all wall emissivity values in vertical_layers.walls are identical (within tolerance).
        - Checks that the common wall emissivity matches properties.building_archetype.wall_external_emissivity.
        - Used when same_emissivity_wall option is enabled.
        """
        return self._validate_same_surface_property(
            site, site_index, "wall", "walls", "emis", "wall_external_emissivity",
            "same_emissivity_wall", "emissivity",
        )

    def _validate_same_emissivity_roof(self, site: Site, site_index: int) -> List[str]:
        """
        Validate that roof emissivity is uniform across all roof layers and matches the building archetype.

        Returns
        -------
        list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - Checks that all roof emissivity values in vertical_layers.roofs are identical (within tolerance).
        - Checks that the common roof emissivity matches properties.building_archetype.roof_external_emissivity.
        - Used when same_emissivity_roof option is enabled.
        """
        return self._validate_same_surface_property(
            site, site_index, "roof", "roofs", "emis", "roof_external_emissivity",
            "same_emissivity_roof", "emissivity",
        )

    def _needs_spartacus_validation(self) -> bool:
        """
        Determine if SPARTACUS method is enabled.

        Returns
        -------
        bool
            True if SPARTACUS is enabled (i.e., net_radiation_method is 1001, 1002, or 1003), False otherwise.

        Notes
        -----
        SPARTACUS is enabled when the model physics parameter `net_radiation_method`
        is set to one of the following values: 1001, 1002, or 1003.
        """
        spartacus_methods = {1001, 1002, 1003}
        netrad_method = _unwrap_value(getattr(self.model.physics, "net_radiation", None))
        try:
            netrad_method = int(netrad_method)
        except (TypeError, ValueError):
            pass
        return netrad_method in spartacus_methods

    def _validate_spartacus_building_height(self, site: Site, site_index: int) -> List[str]:
        """
        Check that building heights do not exceed the SPARTACUS domain top.

        If SPARTACUS is enabled, this function enforces that:
        - The building height (bldgh) does not exceed the domain top (height[nlayer]).
        - If stebbs_method == 1, the archetype's building_height also does not exceed the domain top.

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Returns
        -------
        issues : list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - The domain top is defined as the last entry in the vertical_layers.height array (height[nlayer]).
        - If stebbs_method == 1, both bldgh and building_height are checked.
        - All issues are reported with the site name for clarity.
        """
        issues: List[str] = []
        site_name = getattr(site, "name", f"Site {site_index}")
        props = getattr(site, "properties", None)
        if not props or not hasattr(props, "land_cover") or not props.land_cover:
            return issues

        bldgs = getattr(props.land_cover, "bldgs", None)
        bldgh = _unwrap_value(getattr(bldgs, "bldgh", None)) if bldgs else None
        vertical_layers = getattr(props, "vertical_layers", None)
        height_arr = _unwrap_value(getattr(vertical_layers, "height", None)) if vertical_layers else None
        nlayer = _unwrap_value(getattr(vertical_layers, "nlayer", None)) if vertical_layers else None

        if (
            height_arr is not None and
            nlayer is not None and
            isinstance(height_arr, (list, tuple)) and
            len(height_arr) > nlayer
        ):
            spartacus_top = height_arr[nlayer]
            if bldgh is not None and bldgh > spartacus_top:
                issues.append(
                    f"Site '{site_name}' has bldgh={bldgh} exceeding SPARTACUS domain top (height[{nlayer}]={spartacus_top})."
                )

            # If stebbs == 1, also check building_height
            stebbs_method = _unwrap_value(getattr(self.model.physics, "stebbs", None))

            try:
                stebbs_method_val = int(stebbs_method)
            except (TypeError, ValueError):
                stebbs_method_val = None

            if stebbs_method_val == 1:
                building_archetype = getattr(props, "building_archetype", None)
                building_height = _unwrap_value(getattr(building_archetype, "building_height", None)) if building_archetype else None
                if building_height is not None and building_height > spartacus_top:
                    issues.append(
                        f"Site '{site_name}' has building_height={building_height} exceeding SPARTACUS domain top (height[{nlayer}]={spartacus_top})."
                    )
        return issues

    def _validate_spartacus_sfr(self, site: Site, site_index: int) -> list:
        """
        Validate SPARTACUS building and vegetation surface fractions for a site.

        If SPARTACUS is enabled, this function checks that:
        - The building surface fraction (bldgs.sfr) matches the first entry of vertical_layers.building_frac.
        - The sum of evergreen and deciduous tree surface fractions (evetr.sfr + dectr.sfr)
          matches the maximum value in vertical_layers.veg_frac.

        Returns
        -------
        list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - Uses a tolerance of 1e-6 for floating point comparisons.
        - Returns early if required properties are missing.
        """
        issues: list = []
        site_name = getattr(site, "name", f"Site {site_index}")
        props = getattr(site, "properties", None)
        if not props or not hasattr(props, "land_cover") or not props.land_cover:
            return issues

        lc = props.land_cover
        bldgs = getattr(lc, "bldgs", None)
        evetr = getattr(lc, "evetr", None)
        dectr = getattr(lc, "dectr", None)
        vertical_layers = getattr(props, "vertical_layers", None)
        if not vertical_layers:
            return issues

        # Unwrap values
        bldgs_sfr = _unwrap_value(getattr(bldgs, "sfr", None)) if bldgs else None
        evetr_sfr = _unwrap_value(getattr(evetr, "sfr", None)) if evetr else 0.0
        dectr_sfr = _unwrap_value(getattr(dectr, "sfr", None)) if dectr else 0.0
        veg_sfr = (evetr_sfr or 0.0) + (dectr_sfr or 0.0)

        building_frac = _unwrap_value(getattr(vertical_layers, "building_frac", None))
        veg_frac = _unwrap_value(getattr(vertical_layers, "veg_frac", None))

        tol = 1e-6

        # Buildings: surface fraction vs first SPARTACUS layer
        if (
            isinstance(building_frac, (list, tuple))
            and len(building_frac) > 0
            and bldgs_sfr is not None
        ):
            if not np.isclose(bldgs_sfr, building_frac[0], atol=tol):
                issues.append(
                    f"{site_name}: bldgs.sfr ({bldgs_sfr}) does not match "
                    f"vertical_layers.building_frac[0] ({building_frac[0]})"
                )

        # Vegetation: surface fraction vs maximum veg_frac across layers
        if isinstance(veg_frac, (list, tuple)) and len(veg_frac) > 0:
            veg_frac_max = max(veg_frac)
            if not np.isclose(veg_sfr, veg_frac_max, atol=tol):
                issues.append(
                    f"{site_name}: evetr.sfr + dectr.sfr ({veg_sfr}) does not match "
                    f"max(vertical_layers.veg_frac) ({veg_frac_max})"
                )

        return issues
    
    def _validate_spartacus_veg_dimensions(self, site: Site, site_index: int) -> list:
        """
        Validate that veg_scale and veg_frac are zero above the tree canopy layer.

        This check ensures that vegetation-related arrays (veg_scale and veg_frac)
        are zero for all layers above the tallest tree in the site. The procedure is:

        1. Determine the maximum tree height (max_tree) from dectr and evetr heights.
        2. Find the first vertical layer index (layer_index) such that
           max_tree <= height[layer_index].
        3. For all layers from layer_index to nlayer-1 (inclusive), check that
           veg_scale and veg_frac are zero (within a small tolerance).

        Parameters
        ----------
        site : Site
            The site object to validate.
        site_index : int
            Index of the site in the configuration.

        Returns
        -------
        issues : list of str
            List of issue messages if validation fails; empty if valid.

        Notes
        -----
        - If no tree heights are provided, or height array is too short, no issues are reported.
        - Uses np.isclose with atol=1e-6 for floating point comparisons.
        """
        issues: list = []
        site_name = getattr(site, "name", f"Site {site_index}")

        vertical_layers = getattr(getattr(site, "properties", None), "vertical_layers", None)
        land_cover = getattr(getattr(site, "properties", None), "land_cover", None)

        # Get tree heights
        height_deciduous_tree = _unwrap_value(getattr(getattr(land_cover, "dectr", None), "height_deciduous_tree", None)) if land_cover and getattr(land_cover, "dectr", None) else None
        height_evergreen_tree = _unwrap_value(getattr(getattr(land_cover, "evetr", None), "height_evergreen_tree", None)) if land_cover and getattr(land_cover, "evetr", None) else None

        # Compute max_tree
        tree_heights = [h for h in [height_deciduous_tree, height_evergreen_tree] if h is not None]
        if not tree_heights:
            return issues  # No tree heights to check

        max_tree = max(tree_heights)

        # Get height array (should be nlayer+1)
        height_arr = _unwrap_value(getattr(vertical_layers, "height", None)) if vertical_layers else None
        if not isinstance(height_arr, (list, tuple)) or len(height_arr) < 2:
            return issues  # Not enough height info

        # Find the first layer where max_tree <= height[layer] (layer index 1..nlayer)
        layer_index = None
        for i in range(1, len(height_arr)):
            if max_tree <= height_arr[i]:
                layer_index = i
                break
        if layer_index is None:
            # max_tree exceeds all layers
            issues.append(
                f"Site {site_name}: max_tree ({max_tree}) exceeds all vertical_layers heights."
            )
            return issues

        # Check veg_scale and veg_frac from the tree layer onward (layer_index to nlayer-1)
        veg_scale = _unwrap_value(getattr(vertical_layers, "veg_scale", None)) if vertical_layers else None
        veg_frac = _unwrap_value(getattr(vertical_layers, "veg_frac", None)) if vertical_layers else None

        nlayer = len(height_arr) - 1  # nlayer is height_arr length minus 1

        for arr_name, arr in [("veg_scale", veg_scale), ("veg_frac", veg_frac)]:
            if isinstance(arr, (list, tuple)):
                for i in range(layer_index, min(nlayer, len(arr))):
                    val = arr[i]
                    if not np.isclose(val, 0, atol=1e-6):
                        issues.append(
                            f"Site {site_name}: {arr_name}[{i}] should be zero (provided max tree height {max_tree} does not reach height {height_arr[i+1]} of layer {i+1})."
                        )
        return issues

    def _validate_conditional_parameters(self) -> List[str]:
        """
        Run conditional, method-specific validations for all sites.

        This method performs additional validation checks that depend on the
        configuration of specific model options, such as STEBBS, RSL, StorageHeat,
        same_albedo_wall/roof, same_emissivity_wall/roof, and SPARTACUS. These
        checks are only executed if the corresponding method is enabled in the
        model configuration.

        Returns
        -------
        list of str
            List of issue messages describing any validation failures found.

        Notes
        -----
        - STEBBS: Validates required STEBBS and building archetype parameters when
          `stebbs_method == 1`.
        - RSL: Validates that `bldgs.faibldg` is set when `rsl_method == 2`.
        - StorageHeat: Checks DyOHM storage-heat method requirements when
          `storage_heat_method == 6 or 7`.
        - same_albedo_wall/roof: Ensures uniform albedo across wall/roof layers and
          matches the building archetype if enabled.
        - same_emissivity_wall/roof: Ensures uniform emissivity across wall/roof
          layers and matches the building archetype if enabled.
        - SPARTACUS: Validates building height, surface fraction, and vegetation
          layer consistency when SPARTACUS is enabled.

        All issues found are accumulated and returned as a list of messages.
        """
        all_issues: List[str] = []

        # Determine which checks to run once up front
        needs_stebbs = self._needs_stebbs_validation()
        needs_rsl = self._needs_rsl_validation()
        needs_storage = self._needs_storage_validation()
        needs_same_albedo_wall = self._needs_same_albedo_wall_validation()
        needs_same_albedo_roof = self._needs_same_albedo_roof_validation()
        needs_same_emissivity_wall = self._needs_same_emissivity_wall_validation()
        needs_same_emissivity_roof = self._needs_same_emissivity_roof_validation()
        needs_spartacus = self._needs_spartacus_validation()

        # Nothing to do?
        if not (needs_stebbs or needs_rsl or needs_storage or needs_same_albedo_wall
                or needs_same_albedo_roof or needs_same_emissivity_wall or needs_same_emissivity_roof or needs_spartacus):
            return all_issues

        for idx, site in enumerate(self.sites):
            site_name = getattr(site, "name", f"Site {idx}")

            # STEBBS
            if needs_stebbs:
                stebbs_issues = self._validate_stebbs(site, idx)
                if stebbs_issues:
                    self._validation_summary["issue_types"].add("STEBBS parameters")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(stebbs_issues)

            # RSL
            if needs_rsl:
                rsl_issues = self._validate_rsl(site, idx)
                if rsl_issues:
                    self._validation_summary["issue_types"].add("RSL faibldg")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(rsl_issues)

            # StorageHeat (DyOHM)
            if needs_storage:
                storage_issues = self._validate_storage(site, idx)
                if storage_issues:
                    self._validation_summary["issue_types"].add(
                        "StorageHeat parameters"
                    )
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(storage_issues)
            
            # same_albedo_wall
            if needs_same_albedo_wall:
                same_albedo_wall_issues = self._validate_same_albedo_wall(site, idx)
                if same_albedo_wall_issues:
                    self._validation_summary["issue_types"].add(
                        "same_albedo_wall parameters"
                    )
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(same_albedo_wall_issues)

            # same_albedo_roof
            if needs_same_albedo_roof:
                same_albedo_roof_issues = self._validate_same_albedo_roof(site, idx)
                if same_albedo_roof_issues:
                    self._validation_summary["issue_types"].add(
                        "same_albedo_roof parameters"
                    )
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(same_albedo_roof_issues)

            # same_emissivity_wall
            if needs_same_emissivity_wall:
                same_emissivity_wall_issues = self._validate_same_emissivity_wall(site, idx)
                if same_emissivity_wall_issues:
                    self._validation_summary["issue_types"].add(
                        "same_emissivity_wall parameters"
                    )
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(same_emissivity_wall_issues)

            # same_emissivity_roof
            if needs_same_emissivity_roof:
                same_emissivity_roof_issues = self._validate_same_emissivity_roof(site, idx)
                if same_emissivity_roof_issues:
                    self._validation_summary["issue_types"].add(
                        "same_emissivity_roof parameters"
                    )
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(same_emissivity_roof_issues)

            # SPARTACUS building height, sfr, and vegetation consistency checks
            if needs_spartacus:
                spartacus_issues = self._validate_spartacus_building_height(site, idx)
                if spartacus_issues:
                    self._validation_summary["issue_types"].add("SPARTACUS building height")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(spartacus_issues)

                spartacus_sfr_issues = self._validate_spartacus_sfr(site, idx)
                if spartacus_sfr_issues:
                    self._validation_summary["issue_types"].add("SPARTACUS SFR")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(spartacus_sfr_issues)

                spartacus_veg_issues = self._validate_spartacus_veg_dimensions(site, idx)
                if spartacus_veg_issues:
                    self._validation_summary["issue_types"].add("SPARTACUS vegetation layer consistency")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(spartacus_veg_issues)
        return all_issues

    def _check_critical_null_physics_params(self) -> List[str]:
        """
        Check for critical null physics parameters that would cause runtime crashes.

        This method inspects the model physics parameters that are critical for
        simulation and must not be set to None. If any of these parameters are
        missing or set to null, a runtime crash will occur (e.g., when converting
        to DataFrame state or running the model).

        Returns
        -------
        List[str]
            List of error messages for each critical parameter found to be null.

        Notes
        -----
        - The list of critical parameters is defined in CRITICAL_PHYSICS_PARAMS.
        - Handles both direct values and RefValue wrappers.
        - Returns an empty list if all critical parameters are set.
        """
        # Critical physics parameters that get converted to int() in df_state
        CRITICAL_PHYSICS_PARAMS = [
            "net_radiation",
            "emissions",
            "storage_heat",
            "ohm_inc_qf",
            "roughness_length_momentum",
            "roughness_length_heat",
            "stability",
            "soil_moisture_deficit",
            "water_use",
            "roughness_sublayer",
            "frontal_area_index",
            "roughness_sublayer_level",
            "surface_conductance",
            "snow_use",
            "stebbs",
            "outer_cap_fraction",
            "setpoint",
            "same_albedo_wall",
            "same_albedo_roof",
            "same_emissivity_wall",
            "same_emissivity_roof",
        ]

        critical_issues = []

        if not hasattr(self, "model") or not self.model or not self.model.physics:
            return critical_issues

        physics = self.model.physics

        for param_name in CRITICAL_PHYSICS_PARAMS:
            if hasattr(physics, param_name):
                param_value = getattr(physics, param_name)
                # Handle RefValue wrapper
                if hasattr(param_value, "value"):
                    actual_value = param_value.value
                else:
                    actual_value = param_value

                # Check if the parameter is null
                if actual_value is None:
                    critical_issues.append(
                        f"{param_name} is set to null and will cause runtime crash - must be set to appropriate non-null value"
                    )

        return critical_issues

    def _iter_critical_null_site_param_issues(self) -> List[Dict[str, str]]:
        """Return structured issues for critical null site-level parameters.

        Complements :meth:`_check_critical_null_physics_params` (which covers
        the top-level ``ModelPhysics`` switches) by auditing site- and
        surface-level fields that are presence- or physics-conditional.
        Missing values here are stripped from the YAML by
        ``model_dump(exclude_none=True, mode="json")`` and reach the Rust
        backend as zero; on x86_64 this produces NaN via ``0/0`` and
        ``0*Inf`` in the Fortran stomatal-conductance and LAI paths. See
        gh#1333.

        Rules applied per site:

        - For each vegetated surface (``dectr`` / ``evetr`` / ``grass``)
          with ``sfr > 0``, the surface's ``lai`` block must carry
          non-None values for ``lai_max``, ``base_temperature``,
          ``base_temperature_senescence``, ``gdd_full``, ``sdd_full``.
        - For ``bldgs`` with ``sfr > 0``: ``bldgh`` and ``faibldg`` must
          be non-None.
        - For ``evetr`` with ``sfr > 0``: ``height_evergreen_tree`` and
          ``fai_evergreen_tree`` must be non-None.
        - For ``dectr`` with ``sfr > 0``: ``height_deciduous_tree`` and
          ``fai_deciduous_tree`` must be non-None.
        - If any vegetated surface is active, the site's ``conductance``
          block must carry non-None values for all eleven fields
          (``g_max``, ``g_k``, ``g_q_base``, ``g_q_shape``, ``g_t``,
          ``g_sm``, ``kmax``, ``s1``, ``s2``, ``tl``, ``th``). Both
          ``GSModel.JARVI`` and ``GSModel.WARD`` consume these.

        CO2 / OHM blocks are out of scope for this check: in the gh#1333
        reproducer those fields are populated with a sentinel value
        (``1``), not ``None`` -- a different bug class handled in a
        follow-up.

        Returns
        -------
        List[Dict[str, str]]
            One structured issue per missing field. Each issue carries an
            ``error_text`` for the raised exception plus the ``path`` /
            ``param`` / ``message`` / ``fix`` metadata used by the
            annotated-YAML generator.

        Notes
        -----
        Gated on ``self._yaml_path`` AND on explicit user declaration in
        the raw YAML source (``self._yaml_raw``). The check fires only
        when:

        1. The configuration was loaded from a YAML file (not a
           programmatic ``SUEWSConfig(sites=[Site(...)])`` construction),
           AND
        2. The user explicitly declared the surface (or conductance) as
           a mapping in the raw YAML. Surfaces that were populated by
           ``default_factory`` because the user omitted them are skipped.

        Rationale: the pydantic default factories materialise every
        surface with ``sfr=1/7`` and every phenology/conductance field
        as ``None``, so a naive check fires on any sparse YAML — test
        fixtures that only exercise timezone/schema-version handling,
        docs examples that illustrate a single feature, etc. By
        requiring the user to have explicitly written a mapping for the
        surface (or the conductance block) before demanding physics
        completeness, the check stays focused on real user-assembled
        configurations where the intent is to run SUEWS and the missing
        fields would silently produce NaN.
        """
        issues: List[Dict[str, str]] = []

        if getattr(self, "_yaml_path", None) is None:
            return issues

        if not getattr(self, "sites", None):
            return issues

        yaml_raw = getattr(self, "_yaml_raw", None)

        def _raw_site_properties(site_index: int) -> Dict[str, Any]:
            """Return the raw ``sites[i].properties`` dict, or an empty
            dict if absent or malformed."""
            if not isinstance(yaml_raw, dict):
                return {}
            raw_sites = yaml_raw.get("sites")
            if not isinstance(raw_sites, list) or site_index >= len(raw_sites):
                return {}
            raw_site = raw_sites[site_index]
            if not isinstance(raw_site, dict):
                return {}
            raw_props = raw_site.get("properties")
            if not isinstance(raw_props, dict):
                return {}
            return raw_props

        def _raw_surface(site_index: int, surface_name: str) -> Dict[str, Any]:
            """Return the raw ``sites[i].properties.land_cover.<surface>`` dict."""
            raw_props = _raw_site_properties(site_index)
            raw_lc = raw_props.get("land_cover")
            if not isinstance(raw_lc, dict):
                return {}
            raw_surface = raw_lc.get(surface_name)
            if not isinstance(raw_surface, dict):
                return {}
            return raw_surface

        def _user_declared_surface(site_index: int, surface_name: str) -> bool:
            """True if the user explicitly declared this surface as a
            mapping in the raw YAML. Invalid shorthand (``bldgs: 0.3``)
            is treated as not declared, since pydantic silently dropped
            the value to the factory default.

            The conductance block does not need its own gate: the outer
            ``active_veg`` test already covers it, because a non-empty
            ``active_veg`` implies at least one user-declared vegetated
            surface with ``sfr > 0`` — at which point conductance is
            required regardless of whether the user wrote a
            ``conductance`` block explicitly.
            """
            raw_props = _raw_site_properties(site_index)
            raw_lc = raw_props.get("land_cover")
            if not isinstance(raw_lc, dict):
                return False
            return isinstance(raw_lc.get(surface_name), dict)

        lai_required = {
            "lai_max": (
                "Maximum LAI is required for active vegetation",
                "Add maximum leaf area index for full leaf-on conditions",
            ),
            "base_temperature": (
                "Base temperature is required for active vegetation",
                "Add the base temperature for growing degree day accumulation",
            ),
            "base_temperature_senescence": (
                "Senescence base temperature is required for active vegetation",
                "Add the base temperature for senescence degree day accumulation",
            ),
            "gdd_full": (
                "Growing degree days for full LAI are required for active vegetation",
                "Add the growing degree day threshold for full leaf-on conditions",
            ),
            "sdd_full": (
                "Senescence degree days are required for active vegetation",
                "Add the senescence degree day threshold for leaf-off conditions",
            ),
        }
        conductance_required = {
            "g_max": (
                "Maximum surface conductance is required for active vegetation",
                "Add g_max for evapotranspiration calculations",
            ),
            "g_k": (
                "Solar radiation response parameter is required for active vegetation",
                "Add g_k for evapotranspiration calculations",
            ),
            "g_q_base": (
                "Vapour pressure deficit base parameter is required for active vegetation",
                "Add g_q_base for evapotranspiration calculations",
            ),
            "g_q_shape": (
                "Vapour pressure deficit shape parameter is required for active vegetation",
                "Add g_q_shape for evapotranspiration calculations",
            ),
            "g_t": (
                "Temperature response parameter is required for active vegetation",
                "Add g_t for evapotranspiration calculations",
            ),
            "g_sm": (
                "Soil moisture response parameter is required for active vegetation",
                "Add g_sm for evapotranspiration calculations",
            ),
            "kmax": (
                "Maximum shortwave radiation parameter is required for active vegetation",
                "Add kmax for evapotranspiration calculations",
            ),
            "s1": (
                "Lower soil moisture threshold is required for active vegetation",
                "Add s1 for evapotranspiration calculations",
            ),
            "s2": (
                "Soil moisture dependence parameter is required for active vegetation",
                "Add s2 for evapotranspiration calculations",
            ),
            "tl": (
                "Lower temperature threshold is required for active vegetation",
                "Add tl for evapotranspiration calculations",
            ),
            "th": (
                "Upper temperature threshold is required for active vegetation",
                "Add th for evapotranspiration calculations",
            ),
        }
        building_required = {
            "bldgh": (
                "Building height is required when buildings are active",
                "Add building height in meters",
            ),
            "faibldg": (
                "Building frontal area index is required when buildings are active",
                "Add frontal area index for wind and roughness calculations",
            ),
        }
        evergreen_required = {
            "height_evergreen_tree": (
                "Evergreen tree height is required when evergreen vegetation is active",
                "Add evergreen tree height in meters",
            ),
            "fai_evergreen_tree": (
                "Evergreen tree frontal area index is required when evergreen vegetation is active",
                "Add evergreen tree frontal area index",
            ),
        }
        deciduous_required = {
            "height_deciduous_tree": (
                "Deciduous tree height is required when deciduous vegetation is active",
                "Add deciduous tree height in meters",
            ),
            "fai_deciduous_tree": (
                "Deciduous tree frontal area index is required when deciduous vegetation is active",
                "Add deciduous tree frontal area index",
            ),
        }

        def _add_issue(
            *,
            error_text: str,
            path: str,
            param: str,
            message: str,
            fix: str,
        ) -> None:
            issues.append(
                {
                    "error_text": error_text,
                    "path": path,
                    "param": param,
                    "message": message,
                    "fix": fix,
                }
            )

        for i, site in enumerate(self.sites):
            site_name = getattr(site, "name", f"site[{i}]")
            props = getattr(site, "properties", None)
            if props is None:
                continue

            land_cover = getattr(props, "land_cover", None)

            active_veg: Dict[str, float] = {}

            if land_cover is not None:
                for surface_name in ("dectr", "evetr", "grass"):
                    if not _user_declared_surface(i, surface_name):
                        continue
                    surface = getattr(land_cover, surface_name, None)
                    if surface is None:
                        continue
                    sfr_raw = getattr(surface, "sfr", None)
                    sfr_value = getattr(sfr_raw, "value", sfr_raw)
                    if sfr_value is None or sfr_value <= 0:
                        continue

                    active_veg[surface_name] = sfr_value

                    raw_surface = _raw_surface(i, surface_name)
                    if not isinstance(raw_surface.get("lai"), dict):
                        _add_issue(
                            error_text="",
                            path=(
                                f"sites[{i}]/properties/land_cover/{surface_name}"
                            ),
                            param="lai",
                            message=(
                                f"LAI block is required when {surface_name}.sfr > 0"
                            ),
                            fix=(
                                "Add an lai block with phenology parameters for the active vegetated surface"
                            ),
                        )

                    lai = getattr(surface, "lai", None)
                    if lai is None:
                        _add_issue(
                            error_text=(
                                f"sites[{i}] ({site_name}), land_cover.{surface_name}: "
                                f"lai block is missing but sfr={sfr_value} > 0"
                            ),
                            path=(
                                f"sites[{i}]/properties/land_cover/{surface_name}"
                            ),
                            param="lai",
                            message=(
                                f"LAI block is required when {surface_name}.sfr > 0"
                            ),
                            fix=(
                                "Add an lai block with phenology parameters for the active vegetated surface"
                            ),
                        )
                        continue
                    for field_name, (message, fix) in lai_required.items():
                        raw = getattr(lai, field_name, None)
                        if getattr(raw, "value", raw) is None:
                            _add_issue(
                                error_text=(
                                    f"sites[{i}] ({site_name}), land_cover.{surface_name}.lai: "
                                    f"{field_name} is None but {surface_name}.sfr={sfr_value} > 0"
                                ),
                                path=(
                                    f"sites[{i}]/properties/land_cover/{surface_name}/lai"
                                ),
                                param=field_name,
                                message=message,
                                fix=fix,
                            )

                if _user_declared_surface(i, "bldgs"):
                    bldgs = getattr(land_cover, "bldgs", None)
                    if bldgs is not None:
                        sfr_raw = getattr(bldgs, "sfr", None)
                        sfr_value = getattr(sfr_raw, "value", sfr_raw)
                        if sfr_value is not None and sfr_value > 0:
                            for field_name, (message, fix) in building_required.items():
                                raw = getattr(bldgs, field_name, None)
                                if getattr(raw, "value", raw) is None:
                                    _add_issue(
                                        error_text=(
                                            f"sites[{i}] ({site_name}), land_cover.bldgs: "
                                            f"{field_name} is None but bldgs.sfr={sfr_value} > 0"
                                        ),
                                        path=f"sites[{i}]/properties/land_cover/bldgs",
                                        param=field_name,
                                        message=message,
                                        fix=fix,
                                    )

                if _user_declared_surface(i, "evetr"):
                    evetr = getattr(land_cover, "evetr", None)
                    if evetr is not None:
                        sfr_raw = getattr(evetr, "sfr", None)
                        sfr_value = getattr(sfr_raw, "value", sfr_raw)
                        if sfr_value is not None and sfr_value > 0:
                            for field_name, (message, fix) in evergreen_required.items():
                                raw = getattr(evetr, field_name, None)
                                if getattr(raw, "value", raw) is None:
                                    _add_issue(
                                        error_text=(
                                            f"sites[{i}] ({site_name}), land_cover.evetr: "
                                            f"{field_name} is None but evetr.sfr={sfr_value} > 0"
                                        ),
                                        path=f"sites[{i}]/properties/land_cover/evetr",
                                        param=field_name,
                                        message=message,
                                        fix=fix,
                                    )

                if _user_declared_surface(i, "dectr"):
                    dectr = getattr(land_cover, "dectr", None)
                    if dectr is not None:
                        sfr_raw = getattr(dectr, "sfr", None)
                        sfr_value = getattr(sfr_raw, "value", sfr_raw)
                        if sfr_value is not None and sfr_value > 0:
                            for field_name, (message, fix) in deciduous_required.items():
                                raw = getattr(dectr, field_name, None)
                                if getattr(raw, "value", raw) is None:
                                    _add_issue(
                                        error_text=(
                                            f"sites[{i}] ({site_name}), land_cover.dectr: "
                                            f"{field_name} is None but dectr.sfr={sfr_value} > 0"
                                        ),
                                        path=f"sites[{i}]/properties/land_cover/dectr",
                                        param=field_name,
                                        message=message,
                                        fix=fix,
                                    )

            if active_veg:
                conductance = getattr(props, "conductance", None)
                raw_props = _raw_site_properties(i)
                readout = ", ".join(
                    f"{name}.sfr={sfr}" for name, sfr in active_veg.items()
                )
                if not isinstance(raw_props.get("conductance"), dict):
                    _add_issue(
                        error_text="",
                        path=f"sites[{i}]/properties",
                        param="conductance",
                        message=(
                            "Conductance block is required when vegetated surfaces are active"
                        ),
                        fix=(
                            "Add a conductance block with the required evapotranspiration parameters"
                        ),
                    )
                if conductance is None:
                    _add_issue(
                        error_text=(
                            f"sites[{i}] ({site_name}): conductance block is missing "
                            f"but vegetated surfaces are active ({readout})"
                        ),
                        path=f"sites[{i}]/properties",
                        param="conductance",
                        message=(
                            "Conductance block is required when vegetated surfaces are active"
                        ),
                        fix=(
                            "Add a conductance block with the required evapotranspiration parameters"
                        ),
                    )
                else:
                    for field_name, (message, fix) in conductance_required.items():
                        raw = getattr(conductance, field_name, None)
                        if getattr(raw, "value", raw) is None:
                            _add_issue(
                                error_text=(
                                    f"sites[{i}] ({site_name}): conductance.{field_name} "
                                    f"is None but vegetated surfaces are active ({readout})"
                                ),
                                path=f"sites[{i}]/properties/conductance",
                                param=field_name,
                                message=message,
                                fix=fix,
                            )

        return issues

    def _check_critical_null_site_params(self) -> List[str]:
        """Check for critical null site-level parameters that silently yield NaN output."""
        return [
            issue["error_text"]
            for issue in self._iter_critical_null_site_param_issues()
            if issue["error_text"]
        ]

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
        try:
            input_path = Path(yaml_path)
            if not input_path.exists():
                logger_supy.error(f"Input file does not exist: {yaml_path}")
                return None

            if output_path:
                output_path = Path(output_path)
                # Check if output directory exists
                if not output_path.parent.exists():
                    logger_supy.error(
                        f"Output directory does not exist: {output_path.parent}"
                    )
                    return None
            else:
                output_path = input_path.parent / f"{input_path.stem}_annotated.yml"

            annotated_path = annotator.generate_annotated_file(input_path, output_path)

            logger_supy.info(f"Generated annotated YAML file: {annotated_path}")
            return str(annotated_path)
        except Exception as e:
            logger_supy.error(f"Failed to generate annotated YAML: {e}")
            return None

    def _collect_validation_issues(
        self, site: Site, site_name: str, site_index: int, annotator: YAMLAnnotator
    ) -> None:
        """Collect validation issues for annotation."""

        if not hasattr(site, "properties") or not site.properties:
            return

        # Mirror the hard-failure sparse-YAML checks in the annotated output so
        # auto_generate_annotated=True remains actionable on the raise path.
        site_prefix = f"sites[{site_index}]"
        for issue in self._iter_critical_null_site_param_issues():
            if issue["path"] == site_prefix or issue["path"].startswith(
                f"{site_prefix}/"
            ):
                annotator.add_issue(
                    path=issue["path"],
                    param=issue["param"],
                    message=issue["message"],
                    fix=issue["fix"],
                    level="ERROR",
                )

        # Check conductance
        if hasattr(site.properties, "conductance") and site.properties.conductance:
            from ..validation.core.utils import check_missing_params

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
            from ..validation.core.utils import check_missing_params

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
                        if surface_type == "bldgs" and sfr_value > 0:
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

                            # First check if the raw YAML contains 'cp' instead of 'rho_cp'
                            yaml_path = getattr(self, "_yaml_path", None)
                            if yaml_path and self._check_raw_yaml_for_cp_field(
                                yaml_path, path
                            ):
                                annotator.add_issue(
                                    path=f"{path}/thermal_layers",
                                    param="cp_field",
                                    message="Found 'cp' field - should be 'rho_cp'",
                                    fix="Change 'cp:' to 'rho_cp:' in your YAML file",
                                    level="WARNING",
                                )
                                # This is a naming issue, not a missing parameter issue
                                if self._check_thermal_layers_naming_issue(
                                    surface.thermal_layers, surface_type, site_name
                                ):
                                    has_issues = True
                            elif (
                                not _is_valid_layer_array(getattr(thermal, "dz", None))
                                or not _is_valid_layer_array(
                                    getattr(thermal, "k", None)
                                )
                                or not _is_valid_layer_array(
                                    getattr(thermal, "rho_cp", None)
                                )
                            ):
                                annotator.add_issue(
                                    path=f"{path}/thermal_layers",
                                    param="thermal_layers",
                                    message="Incomplete thermal layer properties",
                                    fix="Add dz (thickness), k (conductivity), and rho_cp (heat capacity) arrays",
                                    level="WARNING",
                                )
                                # Add to validation summary for missing parameters
                                self._validation_summary["total_warnings"] += 1
                                self._validation_summary["issue_types"].add(
                                    "Missing thermal layer parameters"
                                )
                                if (
                                    site_name
                                    not in self._validation_summary["sites_with_issues"]
                                ):
                                    self._validation_summary[
                                        "sites_with_issues"
                                    ].append(site_name)
                                has_issues = True

                        # LAI range check for vegetation surfaces
                        if (
                            surface_type in ["grass", "dectr", "evetr"]
                            and hasattr(surface, "lai")
                            and surface.lai
                        ):
                            lai = surface.lai

                            # Check lai_min vs lai_max
                            if lai.lai_min is not None and lai.lai_max is not None:
                                lai_min_val = (
                                    lai.lai_min.value
                                    if hasattr(lai.lai_min, "value")
                                    else lai.lai_min
                                )
                                lai_max_val = (
                                    lai.lai_max.value
                                    if hasattr(lai.lai_max, "value")
                                    else lai.lai_max
                                )

                                if lai_min_val > lai_max_val:
                                    annotator.add_issue(
                                        path=f"{path}/lai",
                                        param="lai_min_lai_max",
                                        message=f"LAI range invalid: lai_min ({lai_min_val}) > lai_max ({lai_max_val})",
                                        fix="Set lai_min <= lai_max (typical values: lai_min=0.1-1.0, lai_max=3.0-8.0)",
                                        level="WARNING",
                                    )

                            # Check base_temperature vs gddfull
                            if lai.base_temperature is not None and lai.gdd_full is not None:
                                baset_val = (
                                    lai.base_temperature.value
                                    if hasattr(lai.base_temperature, "value")
                                    else lai.base_temperature
                                )
                                gddfull_val = (
                                    lai.gdd_full.value
                                    if hasattr(lai.gdd_full, "value")
                                    else lai.gdd_full
                                )

                                if baset_val > gddfull_val:
                                    annotator.add_issue(
                                        path=f"{path}/lai",
                                        param="base_temperature_gddfull",
                                        message=f"GDD range invalid: base_temperature ({baset_val}) > gddfull ({gddfull_val})",
                                        fix="Set base_temperature <= gddfull (typical values: base_temperature=5-10 C, gddfull=200-1000 C.day)",
                                        level="WARNING",
                                    )

                        # Check vegetation parameters for biogenic CO2 calculations
                        if (
                            surface_type in ["grass", "dectr", "evetr"]
                            and sfr_value > 0
                        ):
                            from ..validation.core.utils import check_missing_params

                            vegetation_params = {
                                "beta_bio_co2": "Biogenic CO2 exchange coefficient",
                                "alpha_bio_co2": "Biogenic CO2 exchange coefficient",
                                "resp_a": "Respiration coefficient",
                                "resp_b": "Respiration coefficient",
                            }

                            missing_params = check_missing_params(
                                vegetation_params,
                                surface,
                                "vegetation",
                                "CO2 flux calculations",
                            )

                            for param, desc in vegetation_params.items():
                                param_with_desc = f"{param} ({desc})"
                                if param_with_desc in missing_params:
                                    annotator.add_issue(
                                        path=path,
                                        param=param,
                                        message=f"Missing {desc}",
                                        fix=f"Add {param} value for accurate CO2 flux calculations",
                                        level="WARNING",
                                    )

        # Check land cover fractions sum to 1.0
        surface_types = ["bldgs", "grass", "dectr", "evetr", "bsoil", "paved", "water"]
        fractions = {}

        for surface_type in surface_types:
            if hasattr(land_cover, surface_type):
                surface = getattr(land_cover, surface_type)
                if surface and hasattr(surface, "sfr") and surface.sfr is not None:
                    sfr_value = getattr(surface.sfr, "value", surface.sfr)
                    fractions[surface_type] = (
                        float(sfr_value) if sfr_value is not None else 0.0
                    )
                else:
                    fractions[surface_type] = 0.0
            else:
                fractions[surface_type] = 0.0

        total_fraction = sum(fractions.values())
        tolerance = 1e-6

        if abs(total_fraction - 1.0) > tolerance:
            fraction_details = ", ".join([f"{k}={v:.3f}" for k, v in fractions.items()])
            difference = abs(total_fraction - 1.0)
            annotator.add_issue(
                path=f"sites[{site_index}]/properties/land_cover",
                param="surface_fractions",
                message=f"Land cover fractions must sum to 1.0 within tolerance {tolerance} (got {total_fraction:.6f}, difference {difference:.2e}): {fraction_details}",
                fix=f"Adjust surface fractions so they sum to 1.0 within tolerance {tolerance}",
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
    #
    #     # Check initial meteorology (for initial_states)
    #     first_day_forcing = cut_forcing.loc[self.model.control.start_time]
    #     first_day_min_temp = first_day_forcing.iloc[0]["Tair"]
    #     first_day_precip = first_day_forcing.iloc[0]["rain"] # Could check previous day if available
    #
    #     # Use min temp for surface temperature states
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface = getattr(site.initial_states, surf_type)
    #             surface.temperature.value = [first_day_min_temp]*5
    #             surface.tsfc.value = first_day_min_temp
    #             surface.tin.value = first_day_min_temp
    #
    #     # Use precip to determine wetness state
    #     for site in self.site:
    #         for surf_type in SurfaceType:
    #             surface_is = getattr(site.initial_states, surf_type)
    #             surface_props =getattr(site.properties.land_cover, surf_type)
    #             if first_day_precip:
    #                 surface_is.state.value = surface_props.state_limit
    #                 surface_is.soilstore.value = surface_props.soil_store_capacity
    #                 if first_day_min_temp < 4:
    #                     surface_is.snowpack.value = surface_props.snowpack_limit
    #                     surface_is.snowfrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.icefrac.value = 0.5 # Can these sum to greater than 1?
    #                     surface_is.snowwater.value = 1 # TODO: What is the limit to this?
    #                     surface_is.snowdens.value = surface_props.snow_density_max
    #             else:
    #                 surface_is.state.value = 0
    #     return self

    @model_validator(mode="after")
    def validate_building_layers(self) -> "SUEWSConfig":
        """
        Validate building layer consistency across all sites.

        Ensures that building-related arrays and lists have consistent lengths
        with respect to the number of vertical layers (`nlayer`). This is
        critical for correct model operation and prevents runtime errors.

        Checks performed for each site:
            - Building heights array (`height`) must have `nlayer + 1` elements.
            - Building fractions array (`building_frac`) must have `nlayer` elements.
            - Building scales array (`building_scale`) must have `nlayer` elements.
            - Roof layers list (`roofs`) must have `nlayer` elements.
            - Wall layers list (`walls`) must have `nlayer` elements.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If any array or list does not match the expected length for `nlayer`.

        Notes
        -----
        - Uses `_unwrap_value` to handle RefValue wrappers.
        - Skips validation for missing properties or arrays.
        """
        from .type import RefValue  # Import here to avoid circular import

        for site_index, site in enumerate(self.sites):
            # Get site name (Site class has name field with default "test site")
            site_name = getattr(site, "name", f"Site {site_index + 1}")

            # Get vertical layers (building validation is on vertical layers, not bldgs)
            if not site.properties or not site.properties.vertical_layers:
                continue

            vertical_layers = site.properties.vertical_layers

            # Extract nlayer value using helper for consistent unwrapping
            nlayer_val = _unwrap_value(vertical_layers.nlayer)

            # Validate building heights array
            if (
                hasattr(vertical_layers, "height")
                and vertical_layers.height is not None
            ):
                heights_val = _unwrap_value(vertical_layers.height)
                expected_height_len = nlayer_val + 1
                if len(heights_val) != expected_height_len:
                    raise ValueError(
                        f"{site_name}: Building heights array length ({len(heights_val)}) "
                        f"must be nlayer+1 ({expected_height_len})"
                    )

            # Validate building fractions array
            if (
                hasattr(vertical_layers, "building_frac")
                and vertical_layers.building_frac is not None
            ):
                fractions_val = _unwrap_value(vertical_layers.building_frac)
                if len(fractions_val) != nlayer_val:
                    raise ValueError(
                        f"{site_name}: Building fractions array length ({len(fractions_val)}) "
                        f"must match nlayer ({nlayer_val})"
                    )

            # Validate building scales array
            if (
                hasattr(vertical_layers, "building_scale")
                and vertical_layers.building_scale is not None
            ):
                scales_val = (
                    vertical_layers.building_scale.value
                    if isinstance(vertical_layers.building_scale, RefValue)
                    else vertical_layers.building_scale
                )
                if len(scales_val) != nlayer_val:
                    raise ValueError(
                        f"{site_name}: Building scales array length ({len(scales_val)}) "
                        f"must match nlayer ({nlayer_val})"
                    )

            # Validate roof layers count
            if hasattr(vertical_layers, "roofs") and vertical_layers.roofs is not None:
                if len(vertical_layers.roofs) != nlayer_val:
                    raise ValueError(
                        f"{site_name}: Roof layers count ({len(vertical_layers.roofs)}) "
                        f"must match nlayer ({nlayer_val})"
                    )

            # Validate wall layers count
            if hasattr(vertical_layers, "walls") and vertical_layers.walls is not None:
                if len(vertical_layers.walls) != nlayer_val:
                    raise ValueError(
                        f"{site_name}: Wall layers count ({len(vertical_layers.walls)}) "
                        f"must match nlayer ({nlayer_val})"
                    )

        return self

    @model_validator(mode="after")
    def validate_surface_states(self) -> "SUEWSConfig":
        """Validate surface state types match expected surface types across all sites.

        Ensures that initial states have appropriate surface types:
        - InitialStateVeg: DECTR, EVETR, or GRASS
        - InitialStateDectr: DECTR only
        - All surface-specific initial state classes have correct surface types
        """
        from .type import SurfaceType  # Import here to avoid circular import

        for site_index, site in enumerate(self.sites):
            # Get site name (Site class has name field with default "test site")
            site_name = getattr(site, "name", f"Site {site_index + 1}")

            # Get initial states
            if not site.initial_states:
                continue

            initial_states = site.initial_states

            # Validate vegetated surface states (evetr, dectr, grass)
            vegetated_surfaces = ["evetr", "dectr", "grass"]
            for surface_name in vegetated_surfaces:
                if hasattr(initial_states, surface_name):
                    surface_state = getattr(initial_states, surface_name)
                    if surface_state and hasattr(surface_state, "_surface_type"):
                        surface_type = surface_state._surface_type
                        expected_types = [
                            SurfaceType.DECTR,
                            SurfaceType.EVETR,
                            SurfaceType.GRASS,
                        ]

                        # For vegetated surfaces, check they're in valid vegetated types
                        if (
                            surface_name in ["evetr", "grass"]
                            and surface_type not in expected_types
                        ):
                            raise ValueError(
                                f"{site_name}: Invalid surface type {surface_type} for vegetated surface {surface_name}"
                            )

                        # For deciduous trees, check it's specifically DECTR
                        if (
                            surface_name == "dectr"
                            and surface_type != SurfaceType.DECTR
                        ):
                            raise ValueError(
                                f"{site_name}: {surface_name} state is only valid for deciduous trees, got {surface_type}"
                            )

        return self

    @model_validator(mode="before")
    @classmethod
    def convert_legacy_hdd_formats(cls, data):
        """Convert legacy HDD_ID list formats across all sites.

        This handles backward compatibility for HDD_ID data that may be provided
        as lists instead of dictionaries. Migrated from InitialStates class
        to ensure consistent handling across all sites in configuration.
        """
        if isinstance(data, dict) and "sites" in data:
            sites = data["sites"]
            if isinstance(sites, list):
                for site in sites:
                    if isinstance(site, dict) and "initial_states" in site:
                        initial_states = site["initial_states"]
                        if (
                            isinstance(initial_states, dict)
                            and "hdd_id" in initial_states
                        ):
                            hdd_value = initial_states["hdd_id"]
                            if isinstance(hdd_value, list):
                                # Convert from legacy list format to HDD_ID object
                                if len(hdd_value) >= 12:
                                    initial_states["hdd_id"] = {
                                        "hdd_accum": hdd_value[0],
                                        "cdd_accum": hdd_value[1],
                                        "temp_accum": hdd_value[2],
                                        "temp_5day_accum": hdd_value[3],
                                        "precip_accum": hdd_value[4],
                                        "days_since_rain_accum": hdd_value[5],
                                        "hdd_daily": hdd_value[6],
                                        "cdd_daily": hdd_value[7],
                                        "temp_daily_mean": hdd_value[8],
                                        "temp_5day_mean": hdd_value[9],
                                        "precip_daily_total": hdd_value[10],
                                        "days_since_rain": hdd_value[11],
                                    }
                                else:
                                    # If list is too short, create default HDD_ID
                                    initial_states["hdd_id"] = {}
        return data

    @model_validator(mode="after")
    def set_surface_types_validation(self) -> "SUEWSConfig":
        """Set surface types on all land cover properties across all sites.

        This validator ensures that all surface property objects have their
        surface type identifiers properly set. This is required for internal
        validation and processing logic. Migrated from LandCover.set_surface_types
        to provide centralized validation across all sites.
        """
        from .type import SurfaceType  # Import here to avoid circular import

        # Surface type mapping
        surface_map = {
            "paved": SurfaceType.PAVED,
            "bldgs": SurfaceType.BLDGS,
            "dectr": SurfaceType.DECTR,
            "evetr": SurfaceType.EVETR,
            "grass": SurfaceType.GRASS,
            "bsoil": SurfaceType.BSOIL,
            "water": SurfaceType.WATER,
        }

        for site_index, site in enumerate(self.sites):
            if site.properties and site.properties.land_cover:
                land_cover = site.properties.land_cover

                # Set surface types for each surface property
                for surface_name, surface_type in surface_map.items():
                    if hasattr(land_cover, surface_name):
                        surface_prop = getattr(land_cover, surface_name)
                        if surface_prop and hasattr(surface_prop, "set_surface_type"):
                            try:
                                surface_prop.set_surface_type(surface_type)
                            except Exception as e:
                                # Log the error but continue processing other surfaces
                                site_name = getattr(site, "name", f"Site {site_index}")
                                import warnings

                                warnings.warn(
                                    f"{site_name}: Failed to set surface type for {surface_name}: {str(e)}",
                                    UserWarning,
                                    stacklevel=2,
                                )

        return self

    @model_validator(mode="after")
    def validate_hourly_profile_hours(self) -> "SUEWSConfig":
        """
        Validate hourly profiles have complete and valid hour coverage.

        Ensures all HourlyProfile instances across all sites have:
            - All hour keys between 1 and 24 (inclusive)
            - Exactly hours 1-24 with no missing or extra hours

        This applies to profiles such as snow, irrigation, anthropogenic heat,
        population, traffic, and human activity profiles.

        Notes
        -----
        - Migrated from HourlyProfile.validate_hours for centralized validation.
        - Raises ValueError if any profile is missing required hours or contains invalid hour keys.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If any hourly profile is missing required hours, contains invalid hour keys,
            or has hours outside the 1-24 range.
        """
        for site_index, site in enumerate(self.sites):
            # Get site name (Site class has name field with default "test site")
            site_name = getattr(site, "name", f"Site {site_index + 1}")
            errors = []

            # Collect all HourlyProfile instances from this site
            hourly_profiles = []

            # Snow profiles
            if site.properties and site.properties.snow:
                hourly_profiles.append((
                    "snow.snow_profile_24hr",
                    site.properties.snow.snow_profile_24hr,
                ))

            # Irrigation profiles
            if site.properties and site.properties.irrigation:
                irrigation = site.properties.irrigation
                hourly_profiles.extend([
                    ("irrigation.wuprofa_24hr", irrigation.wuprofa_24hr),
                    ("irrigation.wuprofm_24hr", irrigation.wuprofm_24hr),
                ])

            # Anthropogenic emissions profiles (heat)
            if site.properties and site.properties.anthropogenic_emissions:
                anthro_heat = site.properties.anthropogenic_emissions.heat
                hourly_profiles.extend([
                    (
                        "anthropogenic_emissions.heat.ahprof_24hr",
                        anthro_heat.ahprof_24hr,
                    ),
                    (
                        "anthropogenic_emissions.heat.popprof_24hr",
                        anthro_heat.popprof_24hr,
                    ),
                ])

                # CO2 profiles (traffic and human activity)
                anthro_co2 = site.properties.anthropogenic_emissions.co2
                hourly_profiles.extend([
                    (
                        "anthropogenic_emissions.co2.traffprof_24hr",
                        anthro_co2.traffprof_24hr,
                    ),
                    (
                        "anthropogenic_emissions.co2.humactivity_24hr",
                        anthro_co2.humactivity_24hr,
                    ),
                ])

            # Validate each profile
            for profile_name, profile in hourly_profiles:
                if profile is None:
                    continue

                # Validate both working_day and holiday profiles
                for day_type in ["working_day", "holiday"]:
                    day_profile = getattr(profile, day_type, None)
                    if day_profile is None:
                        continue

                    # Check hour keys can be converted to integers and are in valid range
                    try:
                        hours = [int(h) for h in day_profile.keys()]
                    except (ValueError, TypeError):
                        errors.append(
                            f"{site_name}: {profile_name}.{day_type} has invalid hour keys. "
                            f"Hour keys must be convertible to integers."
                        )
                        continue

                    # Check hour range (1-24)
                    if not all(1 <= h <= 24 for h in hours):
                        errors.append(
                            f"{site_name}: {profile_name}.{day_type} has hour values outside range 1-24. "
                            f"Found hours: {sorted(hours)}"
                        )

                    # Check complete coverage (must have hours 1-24)
                    if sorted(hours) != list(range(1, 25)):
                        missing_hours = set(range(1, 25)) - set(hours)
                        extra_hours = set(hours) - set(range(1, 25))
                        error_parts = []
                        if missing_hours:
                            error_parts.append(
                                f"missing hours: {sorted(missing_hours)}"
                            )
                        if extra_hours:
                            error_parts.append(f"extra hours: {sorted(extra_hours)}")

                        errors.append(
                            f"{site_name}: {profile_name}.{day_type} must have all hours from 1 to 24. "
                            f"Issues: {', '.join(error_parts)}"
                        )

            if errors:
                raise ValueError("\n".join(errors))

        return self

    @model_validator(mode="after")
    def validate_dls_parameters(self) -> "SUEWSConfig":
        """
        Validate daylight saving time (DLS) parameters across all sites.

        This validator performs the following checks for each site:

        1. Consistency: Ensures that both `startdls` and `enddls` are either set or both None.
           Raises an error if only one is set.

        2. Leap year validation: Checks that the day-of-year (DOY) values for `startdls` and `enddls`
           do not exceed 365 for non-leap years or 366 for leap years. Raises an error if invalid.

        3. Informational comparison: If latitude, longitude, and simulation year are available,
           compares user-provided DLS values with those calculated for the site's location and year.
           Adds informational messages if they differ.

        These checks help ensure correct DLS configuration, especially when running Phase C
        standalone or loading YAML directly via `SUEWSConfig.from_yaml()`.

        Returns
        -------
        SUEWSConfig
            The validated SUEWSConfig instance.

        Raises
        ------
        ValueError
            If DLS parameters are inconsistent or out of valid range for the simulation year.

        Notes
        -----
        - Leap year is determined from `model.control.start_time`.
        - Informational messages are added to the validation summary if user values differ from calculated values.
        """
        # Initialize validation summary if not already present
        if not hasattr(self, "_validation_summary"):
            self._validation_summary = {
                "total_warnings": 0,
                "sites_with_issues": [],
                "issue_types": set(),
                "yaml_path": getattr(self, "_yaml_path", None),
                "detailed_messages": [],
                "info_messages": [],
            }

        # Get simulation year from model.control.start_time
        try:
            year = int(str(self.model.control.start_time)[:4])
        except (AttributeError, ValueError, IndexError):
            year = None  # Cannot determine year, skip leap year check

        is_leap = calendar.isleap(year) if year else None
        max_doy = 366 if (is_leap or year is None) else 365

        errors = []

        for site_index, site in enumerate(self.sites):
            # Get site name (Site class has name field with default "test site")
            site_name = getattr(site, "name", f"Site {site_index + 1}")

            if not (site.properties and site.properties.anthropogenic_emissions):
                continue

            anthro = site.properties.anthropogenic_emissions

            # Extract values (handle RefValue wrapper)
            startdls_val = (
                anthro.startdls.value
                if hasattr(anthro.startdls, "value")
                else anthro.startdls
            )
            enddls_val = (
                anthro.enddls.value
                if hasattr(anthro.enddls, "value")
                else anthro.enddls
            )

            # 1. Consistency check: both set or both None
            if (startdls_val is None) != (enddls_val is None):
                errors.append(
                    f"{site_name}: startdls and enddls must both be set or both be None. "
                    "Cannot have only one parameter set."
                )
                continue  # Skip other checks if inconsistent

            # If both None, skip remaining checks
            if startdls_val is None and enddls_val is None:
                continue

            # 2. Leap year refinement
            if year:
                if startdls_val > max_doy:
                    errors.append(
                        f"{site_name}: startdls DOY {int(startdls_val)} invalid for "
                        f"{'leap' if is_leap else 'non-leap'} year {year}. "
                        f"Must be in range [1, {max_doy}]"
                    )

                if enddls_val > max_doy:
                    errors.append(
                        f"{site_name}: enddls DOY {int(enddls_val)} invalid for "
                        f"{'leap' if is_leap else 'non-leap'} year {year}. "
                        f"Must be in range [1, {max_doy}]"
                    )

            # 3. Compare user values with calculated DLS values (informational only)
            # Get latitude and longitude for DLS calculation
            try:
                lat_val = (
                    site.properties.lat.value
                    if hasattr(site.properties.lat, "value")
                    else site.properties.lat
                )
                lng_val = (
                    site.properties.lng.value
                    if hasattr(site.properties.lng, "value")
                    else site.properties.lng
                )

                if lat_val is not None and lng_val is not None and year:
                    # Import DLSCheck dynamically to avoid circular imports
                    from ..validation.core.yaml_helpers import DLSCheck

                    # Calculate what DLS values should be
                    dls = DLSCheck(lat=lat_val, lng=lng_val, year=year)
                    calculated_start, calculated_end, _ = dls.compute_dst_transitions()

                    if calculated_start and calculated_end:
                        # Compare user values with calculated values
                        # Create separate messages for each parameter

                        if calculated_start != startdls_val:
                            info_msg = (
                                f"startdls for site {site_name}: DLS values differ from calculated "
                                f"values based on your location (lat={lat_val:.2f}, lng={lng_val:.2f}). "
                                f"Check your value ({int(startdls_val)}) against the calculated one "
                                f"({int(calculated_start)}). You might need to change your startdls."
                            )
                            self._validation_summary["detailed_messages"].append(
                                info_msg
                            )
                            self._validation_summary["info_messages"].append(info_msg)

                        if calculated_end != enddls_val:
                            info_msg = (
                                f"enddls for site {site_name}: DLS values differ from calculated "
                                f"values based on your location (lat={lat_val:.2f}, lng={lng_val:.2f}). "
                                f"Check your value ({int(enddls_val)}) against the calculated one "
                                f"({int(calculated_end)}). You might need to change your enddls."
                            )
                            self._validation_summary["detailed_messages"].append(
                                info_msg
                            )
                            self._validation_summary["info_messages"].append(info_msg)
                elif lat_val is not None and lng_val is not None:
                    info_msg = (
                        f"{site_name}: Cannot compare DLS values because simulation year "
                        f"is not specified. Consider setting model.control.start_time."
                    )
                    self._validation_summary["info_messages"].append(info_msg)

            except (AttributeError, TypeError, ImportError):
                # Cannot calculate DLS, skip comparison
                pass

        # Raise errors if any
        if errors:
            raise ValueError("\n".join(errors))

        return self

    @classmethod
    def _transform_validation_error(
        cls,
        error: ValidationError,
        config_data: dict,
        *,
        had_signature: bool = True,
    ) -> ValidationError:
        """Transform Pydantic validation errors to use GRIDID instead of array indices.

        Uses structured error data to avoid string replacement collisions when
        GRIDID values overlap with array indices (e.g., site 0 has GRIDID=1).

        `had_signature` carries whether the source YAML actually shipped a
        `schema_version` field. It is forwarded to `_drift_hint` so unsigned
        YAMLs get a hint that asks for `-f/--from <release-tag>` rather
        than a bare `suews-convert` invocation that the CLI would reject.
        """

        # Extract GRIDID mapping from sites
        sites = config_data.get("sites", [])
        site_gridid_map = {}
        for idx, site in enumerate(sites):
            if isinstance(site, dict):
                gridiv = site.get("gridiv")
                if isinstance(gridiv, dict) and "value" in gridiv:
                    site_gridid_map[idx] = gridiv["value"]
                elif gridiv is not None:
                    site_gridid_map[idx] = gridiv
                else:
                    site_gridid_map[idx] = idx  # Fallback to index
            else:
                site_gridid_map[idx] = idx  # Fallback to index

        # Process structured errors (not string manipulation!)
        modified_errors = []
        for err in error.errors():
            err_copy = err.copy()
            loc_list = list(err_copy["loc"])

            # Replace numeric site index with GRIDID in location tuple
            if (
                len(loc_list) >= 2
                and loc_list[0] == "sites"
                and isinstance(loc_list[1], int)
            ):
                site_idx = loc_list[1]
                if site_idx in site_gridid_map:
                    loc_list[1] = site_gridid_map[site_idx]

            err_copy["loc"] = tuple(loc_list)
            modified_errors.append(err_copy)

        # Format into readable message
        error_lines = [
            f"{error.error_count()} validation error{'s' if error.error_count() > 1 else ''} for SUEWSConfig"
        ]

        for err in modified_errors:
            loc_str = ".".join(str(x) for x in err["loc"])
            error_lines.append(loc_str)
            error_lines.append(
                f"  {err['msg']} [type={err['type']}]"
            )
            if "url" in err:
                error_lines.append(f"    For further information visit {err['url']}")

        # If any key failed with `extra_forbidden`, the error is very likely
        # YAML schema drift (a field from an older release that has been
        # removed, renamed, or moved). Append the drift hint so users know
        # where to look. `extra_forbidden` at the SUEWSConfig root is not
        # possible (it uses `extra="allow"`), but nested models (e.g.
        # SiteProperties, RefValue, and others) do enforce `extra="forbid"`
        # and surface the smell when a drifted YAML reaches them.
        has_extra_forbidden = any(
            err.get("type") == "extra_forbidden" for err in modified_errors
        )
        if has_extra_forbidden:
            error_lines.append("")
            error_lines.append(
                cls._drift_hint(config_data, had_signature=had_signature)
            )

        error_msg = "\n".join(error_lines)
        raise ValueError(f"SUEWS Configuration Validation Error:\n{error_msg}")

    @classmethod
    def _drift_hint(
        cls, config_data: dict, *, had_signature: bool = True
    ) -> str:
        """Build the actionable hint shown when YAML schema drift is suspected.

        Returns a multi-line string naming the detected schema version (or
        noting its absence), the current schema version, and the
        `suews-convert` command the user should run. Kept in one place so
        the loader's `TypeError`/`AttributeError` path and the
        `extra_forbidden` branch in `_transform_validation_error` agree.

        When `had_signature` is False the YAML predates schema versioning,
        so the recommended command includes an explicit `-f/--from-ver` flag
        (the CLI rejects unsigned YAMLs without one). This avoids the
        self-contradictory hint that pointed users at a command that could
        not work.
        """
        from ..schema import CURRENT_SCHEMA_VERSION
        from ..schema.migration import SchemaMigrator

        if had_signature:
            try:
                detected = SchemaMigrator().auto_detect_version(config_data)
            except Exception:  # noqa: BLE001 - detection is best-effort
                detected = "unspecified"
            detected_line = f"  Detected schema version: {detected}\n"
            upgrade_cmd = "suews-convert -i <old.yml> -o <new.yml>"
        else:
            detected_line = (
                "  No schema_version field in YAML "
                "(predates schema versioning).\n"
            )
            upgrade_cmd = (
                "suews-convert -i <old.yml> -o <new.yml> -f <release-tag>"
            )

        return (
            "This usually means the YAML was produced by an older supy release "
            "and no longer matches the current schema.\n"
            f"{detected_line}"
            f"  Current schema version:  {CURRENT_SCHEMA_VERSION}\n"
            f"  Try: {upgrade_cmd}\n"
            "  (see https://github.com/UMEP-dev/SUEWS/issues/1304)\n"
            "  Or share the YAML on the community forum for migration guidance."
        )

    @classmethod
    def _build_drift_error(
        cls,
        exc: Exception,
        config_data: dict,
        *,
        had_signature: bool = True,
    ) -> ValueError:
        """Wrap a raw `TypeError`/`AttributeError` from validation with drift context.

        Pydantic union validation can raise low-level Python exceptions when a
        dict with drifted keys reaches a model whose `__init__` rejects them.
        The bare traceback is opaque, so we re-raise as a `ValueError` that
        names the detected/current schema versions and points at the upgrade
        tool. `had_signature` is forwarded to `_drift_hint`.
        """
        original = f"{type(exc).__name__}: {exc}"
        hint = cls._drift_hint(config_data, had_signature=had_signature)
        return ValueError(
            "SUEWS Configuration Validation Error (suspected schema drift):\n"
            f"  {original}\n\n"
            f"{hint}"
        )

    @classmethod
    def from_yaml(
        cls,
        path: str,
        use_conditional_validation: bool = True,
        strict: bool = True,
        auto_generate_annotated: bool = False,
    ) -> "SUEWSConfig":
        """Initialize SUEWSConfig from YAML file with conditional validation.

        Args:
            path (str): Path to YAML configuration file
            use_conditional_validation (bool): Whether to use conditional validation
            strict (bool): If True, raise errors on validation failure
            auto_generate_annotated (bool): If True, automatically generate annotated YAML when validation issues found

        Returns:
            SUEWSConfig: Instance of SUEWSConfig initialized from YAML
        """
        with open(path, "r") as file:
            config_data = yaml.load(file, Loader=yaml.FullLoader)

        # Snapshot the raw user YAML so site-level completeness checks can
        # distinguish user-declared surfaces from pydantic-factory defaults
        # (gh#1333 follow-up). Deep-copied so later mutations of
        # ``config_data`` do not bleed into the validator's view.
        yaml_raw_snapshot = deepcopy(config_data)

        # Store yaml path in config data for later use
        config_data["_yaml_path"] = path
        config_data["_auto_generate_annotated"] = auto_generate_annotated
        config_data["_yaml_raw"] = yaml_raw_snapshot

        # Log schema version information if present
        from ..schema import CURRENT_SCHEMA_VERSION, get_schema_compatibility_message

        # Remember whether the source YAML carried a schema_version field so the
        # drift-hint builder does not report the default-stamped CURRENT_SCHEMA_VERSION
        # as "detected". Unsigned YAMLs need a hint that asks for -f/--from-ver.
        had_signature = "schema_version" in config_data

        if had_signature:
            logger_supy.info(
                f"Loading config with schema version: {config_data['schema_version']}"
            )
            # Check compatibility and log any concerns
            message = get_schema_compatibility_message(config_data["schema_version"])
            if message:
                logger_supy.info(message)
        else:
            logger_supy.debug(
                f"No schema version specified, assuming current ({CURRENT_SCHEMA_VERSION})"
            )
            # Set default schema version
            config_data["schema_version"] = CURRENT_SCHEMA_VERSION

        if use_conditional_validation:
            logger_supy.debug(
                "Running comprehensive Pydantic validation with conditional checks."
            )
            try:
                return cls(**config_data)
            except ValidationError as e:
                # Transform Pydantic validation error messages to use GRIDID instead of array indices
                transformed_error = cls._transform_validation_error(
                    e, config_data, had_signature=had_signature
                )
                raise transformed_error
            except (TypeError, AttributeError) as e:
                # Raw Python exceptions (e.g. TypeError from a custom __init__ or
                # AttributeError from an unexpected attribute access) can escape
                # Pydantic's union validation when the YAML has drifted. Wrap
                # them with actionable schema-drift context (gh#1303).
                raise cls._build_drift_error(
                    e, config_data, had_signature=had_signature
                ) from e
        else:
            logger_supy.info("Validation disabled by user. Loading without checks.")
            return cls.model_construct(**config_data)

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
        self, use_conditional_validation: bool = False, strict: bool = False
    ) -> pd.DataFrame:
        """Convert config to DataFrame state format with optional conditional validation.

        Args:
            use_conditional_validation (bool): Whether to run conditional validation before conversion.
                Defaults to False since validation module is not loaded by default.
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
            warnings.warn(
                "Conditional validation requested but validation module not available. "
                "Proceeding without additional validation checks.",
                ConditionalValidationWarning,
                stacklevel=2,
            )

        # Proceed with DataFrame conversion
        try:
            list_df_site = []
            for i in range(len(self.sites)):
                grid_id = self.sites[i].gridiv
                df_site = self.sites[i].to_df_state(grid_id)
                df_model = self.model.to_df_state(grid_id)
                df_site = pd.concat([df_site, df_model], axis=1)
                # Remove duplicate columns immediately after combining site+model
                # This prevents InvalidIndexError when concatenating multiple sites (axis=0)
                df_site = df_site.loc[:, ~df_site.columns.duplicated()]
                list_df_site.append(df_site)

            df = pd.concat(list_df_site, axis=0)

            # Add metadata columns using batch-concat pattern to avoid fragmentation warning
            metadata_df = pd.DataFrame(
                {
                    ("config", "0"): self.name,
                    ("description", "0"): self.description,
                },
                index=df.index,
            )
            df = pd.concat([df, metadata_df], axis=1)
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

        # Set name and description, using defaults if columns don't exist
        if ("config", "0") in df.columns:
            config.name = df.loc[grid_ids[0], ("config", "0")]
        elif "config" in df.columns:
            config.name = df["config"].iloc[0]
        else:
            config.name = "Converted from legacy format"

        if ("description", "0") in df.columns:
            config.description = df.loc[grid_ids[0], ("description", "0")]
        elif "description" in df.columns:
            config.description = df["description"].iloc[0]
        else:
            config.description = (
                "Configuration converted from legacy SUEWS table format"
            )

        return config

    def to_yaml(
        self, path: str = "./config-suews.yml", include_internal: bool = True
    ):
        """Convert config to YAML format.

        Parameters
        ----------
        path : str
            Output YAML file path.
        include_internal : bool
            If True, preserve internal-only fields for lossless round-tripping.
            Set to False to produce a clean user-facing YAML export.
        """
        # Use mode='json' to serialize enums as their values
        config_dict = self.model_dump(exclude_none=True, mode="json")

        # Strip private implementation fields
        for key in ("_yaml_path", "_auto_generate_annotated", "_yaml_raw"):
            config_dict.pop(key, None)

        if not include_internal:
            _strip_internal_fields(config_dict, type(self))

        with open(path, "w", encoding="utf-8") as file:
            yaml.dump(
                config_dict,
                file,
                sort_keys=False,
                allow_unicode=True,
            )


def init_config_from_yaml(path: str = "./config-suews.yml") -> SUEWSConfig:
    """Initialize SUEWSConfig from YAML file.

    This is a convenience function that delegates to SUEWSConfig.from_yaml
    for consistency in version checking and validation.
    """
    return SUEWSConfig.from_yaml(path)
