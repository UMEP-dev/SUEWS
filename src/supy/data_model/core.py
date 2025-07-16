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

_validation_available = False
enhanced_from_yaml_validation = None
enhanced_to_df_state_validation = None

import os
import warnings

def _is_valid_layer_array(field) -> bool:
    return hasattr(field, "value") and isinstance(field.value, list) and len(field.value) > 0



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

    # Class-level constant for STEBBS validation parameters
    STEBBS_REQUIRED_PARAMS: ClassVar[List[str]] = [
        "WallInternalConvectionCoefficient",
        "InternalMassConvectionCoefficient",
        "FloorInternalConvectionCoefficient",
        "WindowInternalConvectionCoefficient",
        "WallExternalConvectionCoefficient",
        "WindowExternalConvectionCoefficient",
        "GroundDepth",
        "ExternalGroundConductivity",
        "IndoorAirDensity",
        "IndoorAirCp",
        "WallBuildingViewFactor",
        "WallGroundViewFactor",
        "WallSkyViewFactor",
        "MetabolicRate",
        "LatentSensibleRatio",
        "ApplianceRating",
        "TotalNumberofAppliances",
        "ApplianceUsageFactor",
        "HeatingSystemEfficiency",
        "MaxCoolingPower",
        "CoolingSystemCOP",
        "VentilationRate",
        "IndoorAirStartTemperature",
        "IndoorMassStartTemperature",
        "WallIndoorSurfaceTemperature",
        "WallOutdoorSurfaceTemperature",
        "WindowIndoorSurfaceTemperature",
        "WindowOutdoorSurfaceTemperature",
        "GroundFloorIndoorSurfaceTemperature",
        "GroundFloorOutdoorSurfaceTemperature",
        "WaterTankTemperature",
        "InternalWallWaterTankTemperature",
        "ExternalWallWaterTankTemperature",
        "WaterTankWallThickness",
        "MainsWaterTemperature",
        "WaterTankSurfaceArea",
        "HotWaterHeatingSetpointTemperature",
        "HotWaterTankWallEmissivity",
        "DomesticHotWaterTemperatureInUseInBuilding",
        "InternalWallDHWVesselTemperature",
        "ExternalWallDHWVesselTemperature",
        "DHWVesselWallThickness",
        "DHWWaterVolume",
        "DHWSurfaceArea",
        "DHWVesselEmissivity",
        "HotWaterFlowRate",
        "DHWDrainFlowRate",
        "DHWSpecificHeatCapacity",
        "HotWaterTankSpecificHeatCapacity",
        "DHWVesselSpecificHeatCapacity",
        "DHWDensity",
        "HotWaterTankWallDensity",
        "DHWVesselDensity",
        "HotWaterTankBuildingWallViewFactor",
        "HotWaterTankInternalMassViewFactor",
        "HotWaterTankWallConductivity",
        "HotWaterTankInternalWallConvectionCoefficient",
        "HotWaterTankExternalWallConvectionCoefficient",
        "DHWVesselWallConductivity",
        "DHWVesselInternalWallConvectionCoefficient",
        "DHWVesselExternalWallConvectionCoefficient",
        "DHWVesselWallEmissivity",
        "HotWaterHeatingEfficiency",
        "MinimumVolumeOfDHWinUse",
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
        This runs AFTER all values have been populated from YAML.
        """
        ### 1) Initialize the summary of validation issues
        self._validation_summary = {
            "total_warnings": 0,
            "sites_with_issues": [],
            "issue_types": set(),
            "yaml_path": getattr(self, "_yaml_path", None),
            "detailed_messages": [],  ## Add this line to store detailed messages
        }

        ### 2) Run the standard site-by-site checks
        for i, site in enumerate(self.sites):
            self._validate_site_parameters(site, site_index=i)

        ### 3) Run any conditional validations (e.g. STEBBS when stebbsmethod==1)
        cond_issues = self._validate_conditional_parameters()
        if cond_issues:
            ### Tally the warnings
            self._validation_summary["total_warnings"] += len(cond_issues)
            
            ### Store the detailed messages for the summary
            self._validation_summary["detailed_messages"].extend(cond_issues)
            
            ### No need to log each issue individually
            ## for issue_msg in cond_issues:
            ##    logger_supy.warning(f"Conditional validation issue: {issue_msg}")

        ### 4) If there were any warnings, show the summary
        if self._validation_summary["total_warnings"] > 0:
            self._show_validation_summary()

        return self



    def _show_validation_summary(self) -> None:
        """Show a concise summary of validation issues."""
        ## Check if we have a yaml path stored
        yaml_path = getattr(self, "_yaml_path", None)

        if yaml_path:
            ## When loaded from YAML, we know the source file
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

        # Validate LAI range parameters
        if hasattr(site.properties, "land_cover") and site.properties.land_cover:
            if self._check_lai_ranges(site.properties.land_cover, site_name):
                site_has_issues = True

        # Validate land cover fractions sum to 1.0
        if hasattr(site.properties, "land_cover") and site.properties.land_cover:
            if self._check_land_cover_fractions(site.properties.land_cover, site_name):
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

        def _is_valid_layer_array(field):
            return hasattr(field, "value") and isinstance(field.value, list) and len(field.value) > 0

        if not hasattr(thermal_layers, "dz") or not _is_valid_layer_array(thermal_layers.dz):
            missing_params.append("dz (Layer thickness)")
        if not hasattr(thermal_layers, "k") or not _is_valid_layer_array(thermal_layers.k):
            missing_params.append("k (Thermal conductivity)")
        if not hasattr(thermal_layers, "rho_cp") or not _is_valid_layer_array(thermal_layers.rho_cp):
            missing_params.append("rho_cp (Volumetric heat capacity)")


        if missing_params:
            self._validation_summary["total_warnings"] += len(missing_params)
            self._validation_summary["issue_types"].add(
                "Missing thermal layer parameters"
            )
            return True
        return False
    
    def _check_lai_ranges(self, land_cover, site_name: str) -> bool:
        """Check LAI range parameters for vegetation surfaces. Returns True if issues found."""
        has_issues = False
        
        # Initialize validation summary if it doesn't exist (for testing)
        if not hasattr(self, '_validation_summary'):
            self._validation_summary = {
                "total_warnings": 0,
                "sites_with_issues": [],
                "issue_types": set(),
                "yaml_path": getattr(self, "_yaml_path", None),
                "detailed_messages": [],
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
                        # Check laimin vs laimax
                        if (hasattr(lai, 'laimin') and lai.laimin is not None and 
                            hasattr(lai, 'laimax') and lai.laimax is not None):
                            laimin_val = (
                                lai.laimin.value if hasattr(lai.laimin, 'value') else lai.laimin
                            )
                            laimax_val = (
                                lai.laimax.value if hasattr(lai.laimax, 'value') else lai.laimax
                            )
                            
                            if laimin_val > laimax_val:
                                self._validation_summary["total_warnings"] += 1
                                self._validation_summary["issue_types"].add(
                                    "LAI range validation"
                                )
                                self._validation_summary["detailed_messages"].append(
                                    f"{site_name} {surface_type}: laimin ({laimin_val}) must be ≤ laimax ({laimax_val})"
                                )
                                has_issues = True
                        
                        # Check baset vs gddfull
                        if (hasattr(lai, 'baset') and lai.baset is not None and 
                            hasattr(lai, 'gddfull') and lai.gddfull is not None):
                            baset_val = (
                                lai.baset.value if hasattr(lai.baset, 'value') else lai.baset
                            )
                            gddfull_val = (
                                lai.gddfull.value if hasattr(lai.gddfull, 'value') else lai.gddfull
                            )
                            
                            if baset_val > gddfull_val:
                                self._validation_summary["total_warnings"] += 1
                                self._validation_summary["issue_types"].add(
                                    "LAI range validation"
                                )
                                self._validation_summary["detailed_messages"].append(
                                    f"{site_name} {surface_type}: baset ({baset_val}) must be ≤ gddfull ({gddfull_val})"
                                )
                                has_issues = True
        
        return has_issues
    
    def _check_land_cover_fractions(self, land_cover, site_name: str) -> bool:
        """Check that land cover fractions sum to 1.0. Returns True if issues found."""
        has_issues = False
        
        # Initialize validation summary if it doesn't exist (for testing)
        if not hasattr(self, '_validation_summary'):
            self._validation_summary = {
                "total_warnings": 0,
                "sites_with_issues": [],
                "issue_types": set(),
                "yaml_path": getattr(self, "_yaml_path", None),
                "detailed_messages": [],
            }
        
        # Return early if no land cover
        if land_cover is None:
            return False
        
        # Get all surface types and their fractions
        surface_types = ["bldgs", "grass", "dectr", "evetr", "bsoil", "paved", "water"]
        fractions = {}
        
        for surface_type in surface_types:
            if hasattr(land_cover, surface_type):
                surface = getattr(land_cover, surface_type)
                if surface and hasattr(surface, "sfr") and surface.sfr is not None:
                    # Extract fraction value (handle RefValue)
                    sfr_value = getattr(surface.sfr, "value", surface.sfr)
                    fractions[surface_type] = float(sfr_value) if sfr_value is not None else 0.0
                else:
                    fractions[surface_type] = 0.0
            else:
                fractions[surface_type] = 0.0
        
        # Check if fractions sum to exactly 1.0
        total_fraction = sum(fractions.values())
        
        if total_fraction != 1.0:
            self._validation_summary["total_warnings"] += 1
            self._validation_summary["issue_types"].add(
                "Land cover fraction validation"
            )
            
            # Create detailed message with breakdown
            fraction_details = ", ".join([f"{k}={v:.3f}" for k, v in fractions.items()])
            self._validation_summary["detailed_messages"].append(
                f"{site_name}: Land cover fractions must sum to 1.0 (got {total_fraction:.6f}): {fraction_details}"
            )
            has_issues = True
        
        return has_issues
    
    def _needs_stebbs_validation(self) -> bool:
        """
        Return True if STEBBS should be validated,
        i.e. physics.stebbsmethod == 1.
        """

        if not hasattr(self.model, 'physics') or not hasattr(self.model.physics, 'stebbsmethod'):
            return False
            
        stebbsmethod = self.model.physics.stebbsmethod
        

        if hasattr(stebbsmethod, 'value'):
            stebbsmethod = stebbsmethod.value
        if hasattr(stebbsmethod, '__int__'):
            stebbsmethod = int(stebbsmethod)
        if isinstance(stebbsmethod, str) and stebbsmethod == "1":
            stebbsmethod = 1
            
        #print(f"Final stebbsmethod value for validation: {stebbsmethod} (type: {type(stebbsmethod)})")
        
        return stebbsmethod == 1

    def _validate_stebbs(self, site: Site, site_index: int) -> List[str]:
        """
        If stebbsmethod==1, enforce that site.properties.stebbs
        has all required parameters with non-null values.
        Returns a list of issue messages.
        """
        issues: List[str] = []
        
        ## First check if properties exists and is not None
        if not hasattr(site, 'properties') or site.properties is None:
            issues.append("Missing 'properties' section (required for STEBBS validation)")
            return issues
        
        props = site.properties
        
        ## Must have a stebbs block
        if not hasattr(props, 'stebbs') or props.stebbs is None:
            issues.append("Missing 'stebbs' section (required when stebbsmethod=1)")
            return issues
        
        stebbs = props.stebbs
        
        # ## Define all required STEBBS parameters
        # required_params = [
        #     "WallInternalConvectionCoefficient",
        #     "InternalMassConvectionCoefficient",
        #     "FloorInternalConvectionCoefficient",
        #     "WindowInternalConvectionCoefficient",
        #     "WallExternalConvectionCoefficient",
        #     "WindowExternalConvectionCoefficient",
        #     "GroundDepth",
        #     "ExternalGroundConductivity",
        #     "IndoorAirDensity",
        #     "IndoorAirCp",
        #     "WallBuildingViewFactor",
        #     "WallGroundViewFactor",
        #     "WallSkyViewFactor",
        #     "MetabolicRate",
        #     "LatentSensibleRatio",
        #     "ApplianceRating",
        #     "TotalNumberofAppliances",
        #     "ApplianceUsageFactor",
        #     "HeatingSystemEfficiency",
        #     "MaxCoolingPower",
        #     "CoolingSystemCOP",
        #     "VentilationRate",
        #     "IndoorAirStartTemperature",
        #     "IndoorMassStartTemperature",
        #     "WallIndoorSurfaceTemperature",
        #     "WallOutdoorSurfaceTemperature",
        #     "WindowIndoorSurfaceTemperature",
        #     "WindowOutdoorSurfaceTemperature",
        #     "GroundFloorIndoorSurfaceTemperature",
        #     "GroundFloorOutdoorSurfaceTemperature",
        #     "WaterTankTemperature",
        #     "InternalWallWaterTankTemperature",
        #     "ExternalWallWaterTankTemperature",
        #     "WaterTankWallThickness",
        #     "MainsWaterTemperature",
        #     "WaterTankSurfaceArea",
        #     "HotWaterHeatingSetpointTemperature",
        #     "HotWaterTankWallEmissivity",
        #     "DomesticHotWaterTemperatureInUseInBuilding",
        #     "InternalWallDHWVesselTemperature",
        #     "ExternalWallDHWVesselTemperature",
        #     "DHWVesselWallThickness",
        #     "DHWWaterVolume",
        #     "DHWSurfaceArea",
        #     "DHWVesselEmissivity",
        #     "HotWaterFlowRate",
        #     "DHWDrainFlowRate",
        #     "DHWSpecificHeatCapacity",
        #     "HotWaterTankSpecificHeatCapacity",
        #     "DHWVesselSpecificHeatCapacity",
        #     "DHWDensity",
        #     "HotWaterTankWallDensity",
        #     "DHWVesselDensity",
        #     "HotWaterTankBuildingWallViewFactor",
        #     "HotWaterTankInternalMassViewFactor",
        #     "HotWaterTankWallConductivity",
        #     "HotWaterTankInternalWallConvectionCoefficient",
        #     "HotWaterTankExternalWallConvectionCoefficient",
        #     "DHWVesselWallConductivity",
        #     "DHWVesselInternalWallConvectionCoefficient",
        #     "DHWVesselExternalWallConvectionCoefficient",
        #     "DHWVesselWallEmissivity",
        #     "HotWaterHeatingEfficiency",
        #     "MinimumVolumeOfDHWinUse"
        # ]
        
        ## Check each parameter
        missing_params = []
        for param in self.STEBBS_REQUIRED_PARAMS:
            ## Check if parameter exists
            if not hasattr(stebbs, param):
                missing_params.append(param)
                continue
                
            ## Get parameter value
            param_obj = getattr(stebbs, param)
            
            ## Check if the parameter has a value attribute that is None
            if hasattr(param_obj, 'value') and param_obj.value is None:
                missing_params.append(param)
                continue
                
            ## If the parameter itself is None
            if param_obj is None:
                missing_params.append(param)
        
        ## Always list all missing parameters, regardless of count
        if missing_params:
            param_list = ", ".join(missing_params)
            issues.append(f"Missing required STEBBS parameters: {param_list} (required when stebbsmethod=1)")
        
        return issues
    
    def _needs_rsl_validation(self) -> bool:
        """
        Return True if RSL diagnostic method is enabled,
        i.e. physics.rslmethod == 2.
        """
        rm = self.model.physics.rslmethod
        method = getattr(rm, "value", rm)
        try:
            method = int(method)
        except (TypeError, ValueError):
            pass
        return method == 2

    def _validate_rsl(self, site: Site, site_index: int) -> List[str]:
        """
        If rslmethod==2, then for any site where bldgs.sfr > 0,
        bldgs.faibldg must be set and non-null.
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
                    f"{site_name}: for rslmethod=2 and bldgs.sfr={sfr}, bldgs.faibldg must be set"
                )
        return issues

    def _needs_storage_validation(self) -> bool:
        """
        Return True if DyOHM storage‐heat method is enabled,
        i.e. physics.storageheatmethod == 6.
        """
        shm = getattr(self.model.physics.storageheatmethod, "value", None)
        try:
            shm = int(shm)
        except (TypeError, ValueError):
            pass
        return shm == 6

    def _validate_storage(self, site: Site, site_index: int) -> List[str]:
        issues: List[str] = []
        # prendi sempre il nome
        site_name = getattr(site, "name", f"Site {site_index}")
        props = getattr(site, "properties", None)
        if not props:
            return issues

        vl = getattr(props, "vertical_layers", None)
        walls = getattr(vl, "walls", None) if vl else None
        if not walls or len(walls) == 0:
            issues.append(
                f"{site_name}: storageheatmethod=6 → missing vertical_layers.walls"
            )
            return issues

        th = getattr(walls[0], "thermal_layers", None)
        for arr in ("dz", "k", "rho_cp"):
            field = getattr(th, arr, None) if th else None
            vals = getattr(field, "value", None) if field else None
            if not isinstance(vals, list) or len(vals) == 0 or any(v is None for v in vals) or any(not isinstance(v, (int, float)) for v in vals):
                issues.append(
                    f"{site_name}: storageheatmethod=6 → "
                    f"thermal_layers.{arr} must be a non‐empty list of numeric values (no nulls)"
                )

        lam = getattr(getattr(props, "lambda_c", None), "value", None)
        if lam in (None, ""):
            issues.append(
                f"{site_name}: storageheatmethod=6 → properties.lambda_c must be set and non-null"
            )

        return issues

    def _validate_conditional_parameters(self) -> List[str]:
        """
        Run any method‐specific validations (STEBBS, RSL, StorageHeat) in one
        site-loop. Returns all issue messages.
        """
        all_issues: List[str] = []

        # Determine which checks to run once up front
        needs_stebbs  = self._needs_stebbs_validation()
        needs_rsl     = self._needs_rsl_validation()
        needs_storage = self._needs_storage_validation()

        # Nothing to do?
        if not (needs_stebbs or needs_rsl or needs_storage):
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
                    self._validation_summary["issue_types"].add("StorageHeat parameters")
                    if site_name not in self._validation_summary["sites_with_issues"]:
                        self._validation_summary["sites_with_issues"].append(site_name)
                    all_issues.extend(storage_issues)

        return all_issues

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
                            not _is_valid_layer_array(getattr(thermal, "dz", None)) or
                            not _is_valid_layer_array(getattr(thermal, "k", None)) or
                            not _is_valid_layer_array(getattr(thermal, "rho_cp", None))
                        ):

                                annotator.add_issue(
                                    path=f"{path}/thermal_layers",
                                    param="thermal_layers",
                                    message="Incomplete thermal layer properties",
                                    fix="Add dz (thickness), k (conductivity), and rho_cp (heat capacity) arrays",
                                    level="WARNING",
                                )

                        # LAI range check for vegetation surfaces
                        if surface_type in ["grass", "dectr", "evetr"] and hasattr(surface, "lai") and surface.lai:
                            lai = surface.lai
                            
                            # Check laimin vs laimax
                            if lai.laimin is not None and lai.laimax is not None:
                                laimin_val = (
                                    lai.laimin.value if hasattr(lai.laimin, 'value') else lai.laimin
                                )
                                laimax_val = (
                                    lai.laimax.value if hasattr(lai.laimax, 'value') else lai.laimax
                                )
                                
                                if laimin_val > laimax_val:
                                    annotator.add_issue(
                                        path=f"{path}/lai",
                                        param="laimin_laimax",
                                        message=f"LAI range invalid: laimin ({laimin_val}) > laimax ({laimax_val})",
                                        fix="Set laimin ≤ laimax (typical values: laimin=0.1-1.0, laimax=3.0-8.0)",
                                        level="WARNING",
                                    )
                            
                            # Check baset vs gddfull
                            if lai.baset is not None and lai.gddfull is not None:
                                baset_val = (
                                    lai.baset.value if hasattr(lai.baset, 'value') else lai.baset
                                )
                                gddfull_val = (
                                    lai.gddfull.value if hasattr(lai.gddfull, 'value') else lai.gddfull
                                )
                                
                                if baset_val > gddfull_val:
                                    annotator.add_issue(
                                        path=f"{path}/lai",
                                        param="baset_gddfull",
                                        message=f"GDD range invalid: baset ({baset_val}) > gddfull ({gddfull_val})",
                                        fix="Set baset ≤ gddfull (typical values: baset=5-10°C, gddfull=200-1000°C·day)",
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
                    fractions[surface_type] = float(sfr_value) if sfr_value is not None else 0.0
                else:
                    fractions[surface_type] = 0.0
            else:
                fractions[surface_type] = 0.0
        
        total_fraction = sum(fractions.values())
        
        if total_fraction != 1.0:
            fraction_details = ", ".join([f"{k}={v:.3f}" for k, v in fractions.items()])
            annotator.add_issue(
                path=f"sites[{site_index}]/properties/land_cover",
                param="surface_fractions",
                message=f"Land cover fractions must sum to 1.0 (got {total_fraction:.6f}): {fraction_details}",
                fix="Adjust surface fractions so they sum to exactly 1.0",
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

        if use_conditional_validation:
            logger_supy.info("Using internal validation only (SUEWSConfig.validate_parameter_completeness).")
            return cls(**config_data)
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
