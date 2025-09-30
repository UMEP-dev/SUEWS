"""
SUEWS Physics Validation Check - Phase B

This module performs physics validation and consistency checks on YAML configurations
that have already been processed by Phase A.

Phase B focuses on:
- Physics parameter validation
- Geographic coordinate and timezone validation
- Seasonal parameter adjustments (LAI, snowalb, surface temperatures)
- Land cover fraction validation and consistency
- Model physics option interdependency checks
- Automatic physics-based corrections where appropriate

Phase B assumes Phase A has completed successfully and builds upon clean YAML output
without duplicating parameter detection or YAML structure validation.
"""

import yaml
import os
from typing import Dict, List, Optional, Union, Any, Tuple
from dataclasses import dataclass
from datetime import datetime
from copy import deepcopy
import pandas as pd
import numpy as np
from pydantic import BaseModel
import pytz

# Use tzfpy instead of timezonefinder for Windows compatibility
# tzfpy has pre-built Windows wheels and provides similar functionality
try:
    from tzfpy import get_tz

    HAS_TIMEZONE_FINDER = True

    # Create a compatibility wrapper for timezonefinder API
    class TimezoneFinder:
        def timezone_at(self, lat, lng):
            """Wrapper to match timezonefinder API."""
            # tzfpy uses (longitude, latitude) order
            return get_tz(lng, lat)

except ImportError:
    # Fallback to original timezonefinder if tzfpy not available
    try:
        from timezonefinder import TimezoneFinder

        HAS_TIMEZONE_FINDER = True
    except ImportError:
        HAS_TIMEZONE_FINDER = False
        import warnings

        warnings.warn(
            "Neither tzfpy nor timezonefinder available. DST calculations will be skipped.",
            UserWarning,
        )

# Try to import from supy if available, otherwise use standalone mode
try:
    from supy._env import logger_supy, trv_supy_module

    HAS_SUPY = True
except ImportError:
    # Standalone mode - create minimal dependencies
    import logging
    from pathlib import Path

    # Create standalone logger
    logger_supy = logging.getLogger("supy.data_model")
    if not logger_supy.handlers:
        handler = logging.StreamHandler()
        handler.setFormatter(
            logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
        )
        logger_supy.addHandler(handler)
        logger_supy.setLevel(logging.INFO)

    # Create mock traversable for resource access
    class MockTraversable:
        """Mock for accessing package resources in standalone mode."""

        def __init__(self):
            # Try to find the package root
            current = Path(__file__).parent
            while current.parent != current:
                if (current / "ext_data").exists():
                    self.base = current
                    break
                current = current.parent
            else:
                self.base = Path(__file__).parent.parent.parent

        def __truediv__(self, other):
            return self.base / other

        def exists(self):
            # In standalone mode, CRU data might not be available
            return False

    trv_supy_module = MockTraversable()
    HAS_SUPY = False


@dataclass
class ValidationResult:
    """Structured result from scientific validation checks."""

    status: str  # 'PASS', 'WARNING', 'ERROR'
    category: str  # 'PHYSICS', 'GEOGRAPHY', 'SEASONAL', 'LAND_COVER', 'MODEL_OPTIONS'
    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    message: str = ""
    suggested_value: Any = None
    applied_fix: bool = False


@dataclass
class ScientificAdjustment:
    """Record of automatic scientific adjustment applied."""

    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


class DLSCheck(BaseModel):
    """Calculate daylight saving time transitions and timezone offset from coordinates."""

    lat: float
    lng: float
    year: int
    startdls: Optional[int] = None
    enddls: Optional[int] = None

    def compute_dst_transitions(self):
        """Compute DST start/end days and timezone offset for coordinates and year."""
        if not HAS_TIMEZONE_FINDER:
            logger_supy.debug(
                "[DLS] No timezone finder available, skipping DST calculation."
            )
            return None, None, None

        tf = TimezoneFinder()
        tz_name = tf.timezone_at(lat=self.lat, lng=self.lng)
        if not tz_name:
            logger_supy.debug(
                f"[DLS] Cannot determine timezone for lat={self.lat}, lng={self.lng}"
            )
            return None, None, None

        logger_supy.debug(f"[DLS] Timezone identified as '{tz_name}'")
        tz = pytz.timezone(tz_name)

        def find_transition(month: int) -> Optional[int]:
            try:
                prev_dt = tz.localize(datetime(self.year, month, 1, 12), is_dst=None)
                prev_offset = prev_dt.utcoffset()

                for day in range(2, 32):
                    try:
                        curr_dt = tz.localize(
                            datetime(self.year, month, day, 12), is_dst=None
                        )
                        curr_offset = curr_dt.utcoffset()
                        if curr_offset != prev_offset:
                            return curr_dt.timetuple().tm_yday
                        prev_offset = curr_offset
                    except Exception:
                        continue
                return None
            except Exception:
                return None

        try:
            std_dt = tz.localize(datetime(self.year, 1, 15), is_dst=False)
            utc_offset_hours = std_dt.utcoffset().total_seconds() / 3600
            logger_supy.debug(f"[DLS] UTC offset in standard time: {utc_offset_hours}")
        except Exception as e:
            logger_supy.debug(f"[DLS] Failed to compute UTC offset: {e}")
            utc_offset_hours = None

        if self.lat >= 0:  # Northern Hemisphere
            start = find_transition(3) or find_transition(4)
            end = find_transition(10) or find_transition(11)
        else:  # Southern Hemisphere
            start = find_transition(9) or find_transition(10)
            end = find_transition(3) or find_transition(4)

        return start, end, utc_offset_hours


def get_value_safe(param_dict, param_key, default=None):
    """Safely extract value from RefValue or plain format."""
    param = param_dict.get(param_key, default)
    if isinstance(param, dict) and "value" in param:
        return param["value"]
    else:
        return param


def get_site_gridid(site_data: dict) -> int:
    """Extract GRIDID from site data, handling both direct and RefValue formats."""
    if isinstance(site_data, dict):
        gridiv = site_data.get("gridiv")
        if isinstance(gridiv, dict) and "value" in gridiv:
            return gridiv["value"]
        elif gridiv is not None:
            return gridiv
    return None


def validate_phase_b_inputs(
    uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str
) -> Tuple[dict, dict, dict]:
    """Validate Phase B inputs and load YAML files."""
    for file_path in [uptodate_yaml_file, user_yaml_file, standard_yaml_file]:
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"Required file not found: {file_path}")

    try:
        with open(uptodate_yaml_file, "r") as f:
            uptodate_content = f.read()
            uptodate_data = yaml.safe_load(uptodate_content)

        is_phase_a_output = "UP TO DATE YAML" in uptodate_content

        with open(user_yaml_file, "r") as f:
            user_data = yaml.safe_load(f)

        with open(standard_yaml_file, "r") as f:
            standard_data = yaml.safe_load(f)

    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}")

    return uptodate_data, user_data, standard_data


def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """Extract simulation parameters for validation."""
    control = yaml_data.get("model", {}).get("control", {})

    start_date = control.get("start_time")
    end_date = control.get("end_time")

    # Collect all validation errors instead of failing on first error
    errors = []

    if not isinstance(start_date, str) or "-" not in str(start_date):
        errors.append(
            "Missing or invalid 'start_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    if not isinstance(end_date, str) or "-" not in str(end_date):
        errors.append(
            "Missing or invalid 'end_time' in model.control - must be in 'YYYY-MM-DD' format"
        )

    # Try to extract model year if start_date looks valid
    model_year = None
    if isinstance(start_date, str) and "-" in str(start_date):
        try:
            model_year = int(start_date.split("-")[0])
        except Exception:
            errors.append(
                "Could not extract model year from 'start_time' - ensure 'YYYY-MM-DD' format"
            )

    # If we have errors, combine them into a single error message
    if errors:
        error_msg = "; ".join(errors)
        raise ValueError(error_msg)

    return model_year, start_date, end_date


def validate_physics_parameters(yaml_data: dict) -> List[ValidationResult]:
    """Validate required physics parameters."""
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    if not physics:
        results.append(
            ValidationResult(
                status="WARNING",
                category="PHYSICS",
                parameter="model.physics",
                message="Physics section is empty - skipping physics parameter validation",
            )
        )
        return results

    required_physics_params = [
        "netradiationmethod",
        "emissionsmethod",
        "storageheatmethod",
        "ohmincqf",
        "roughlenmommethod",
        "roughlenheatmethod",
        "stabilitymethod",
        "smdmethod",
        "waterusemethod",
        "rslmethod",
        "faimethod",
        "rsllevel",
        "gsmodel",
        "snowuse",
        "stebbsmethod",
    ]

    missing_params = [
        param for param in required_physics_params if param not in physics
    ]
    if missing_params:
        for param in missing_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=f"model.physics.{param}",
                    message=f"Physics parameter '{param}' is required but missing or null. This parameter controls critical model behaviour and must be specified for the simulation to run properly.",
                    suggested_value=f"Set '{param}' to an appropriate value. Consult the SUEWS documentation for parameter descriptions and typical values: https://suews.readthedocs.io/latest/",
                )
            )

    empty_params = [
        param
        for param in required_physics_params
        if param in physics and physics.get(param, {}).get("value") in ("", None)
    ]
    if empty_params:
        for param in empty_params:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="PHYSICS",
                    parameter=f"model.physics.{param}",
                    message=f"Physics parameter '{param}' has null value. This parameter controls critical model behaviour and must be set for proper simulation.",
                    suggested_value=f"Set '{param}' to an appropriate non-null value. Check documentation for parameter details: https://suews.readthedocs.io/en/latest",
                )
            )

    if not missing_params and not empty_params:
        results.append(
            ValidationResult(
                status="PASS",
                category="PHYSICS",
                parameter="model.physics",
                message="All required physics parameters present and non-empty",
            )
        )

    return results


def validate_model_option_dependencies(yaml_data: dict) -> List[ValidationResult]:
    """Validate consistency between model physics options."""
    results = []
    physics = yaml_data.get("model", {}).get("physics", {})

    rslmethod = get_value_safe(physics, "rslmethod")
    stabilitymethod = get_value_safe(physics, "stabilitymethod")
    storageheatmethod = get_value_safe(physics, "storageheatmethod")
    ohmincqf = get_value_safe(physics, "ohmincqf")

    # RSL method and stability method dependencies
    if rslmethod == 2 and stabilitymethod != 3:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="rslmethod-stabilitymethod",
                message="If rslmethod == 2, stabilitymethod must be 3",
                suggested_value="Set stabilitymethod to 3",
            )
        )

    elif stabilitymethod == 1 and rslmethod is None:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="stabilitymethod-rslmethod",
                message="If stabilitymethod == 1, rslmethod parameter is required for atmospheric stability calculations",
                suggested_value="Set rslmethod to appropriate value",
            )
        )

    else:
        results.append(
            ValidationResult(
                status="PASS",
                category="MODEL_OPTIONS",
                parameter="rslmethod-stabilitymethod",
                message="rslmethod-stabilitymethod constraints satisfied",
            )
        )

    # Storage heat method and OhmIncQf compatibility check
    # Only method 1 (OHM_WITHOUT_QF) has specific compatibility requirements
    if storageheatmethod == 1 and ohmincqf != 0:
        results.append(
            ValidationResult(
                status="ERROR",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message=f"StorageHeatMethod is set to {storageheatmethod} and OhmIncQf is set to {ohmincqf}. You should switch to OhmIncQf=0.",
                suggested_value="Set OhmIncQf to 0",
            )
        )
    else:
        results.append(
            ValidationResult(
                status="PASS",
                category="MODEL_OPTIONS",
                parameter="storageheatmethod-ohmincqf",
                message="StorageHeatMethod-OhmIncQf compatibility validated",
            )
        )

    return results


def validate_land_cover_consistency(yaml_data: dict) -> List[ValidationResult]:
    """Validate land cover fractions and parameters."""
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")
        site_gridid = get_site_gridid(site)

        if not land_cover:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="LAND_COVER",
                    parameter="land_cover",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Missing land_cover block",
                    suggested_value="Add land_cover configuration with surface fractions",
                )
            )
            continue

        # Calculate sum of all surface fractions
        sfr_sum = 0.0
        surface_types = []

        for surface_type, surface_props in land_cover.items():
            if isinstance(surface_props, dict):
                sfr_value = surface_props.get("sfr", {}).get("value")
                if sfr_value is not None:
                    sfr_sum += sfr_value
                    surface_types.append((surface_type, sfr_value))

        if abs(sfr_sum - 1.0) > 0.0001:
            if sfr_sum == 0.0:
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter="land_cover.surface_fractions",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"All surface fractions are zero or missing",
                        suggested_value="Set surface fractions (paved.sfr, bldgs.sfr, evetr.sfr, dectr.sfr, grass.sfr, bsoil.sfr, water.sfr) that sum to 1.0",
                    )
                )
            else:
                surface_list = ", ".join([
                    f"{surf}={val:.3f}" for surf, val in surface_types
                ])
                # Identify the surface with the largest fraction (same as auto-correction logic)
                surface_dict = dict(surface_types)
                max_surface = (
                    max(surface_dict.keys(), key=lambda k: surface_dict[k])
                    if surface_dict
                    else "surface"
                )
                results.append(
                    ValidationResult(
                        status="ERROR",
                        category="LAND_COVER",
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message=f"Surface fractions sum to {sfr_sum:.6f}, should equal 1.0 (auto-correction range: 0.9999-1.0001, current: {surface_list})",
                        suggested_value=f"Adjust {max_surface}.sfr or other surface fractions so they sum to exactly 1.0",
                    )
                )

        for surface_type, sfr_value in surface_types:
            if sfr_value > 0:
                surface_props = land_cover[surface_type]
                missing_params = _check_surface_parameters(surface_props, surface_type)

                for param_name in missing_params:
                    readable_message = (
                        f"Surface '{surface_type}' is active (sfr > 0) but parameter '{param_name}' "
                        f"is missing or null. Active surfaces require all their parameters to be "
                        f"properly configured for accurate simulation results."
                    )

                    actionable_suggestion = (
                        f"Set parameter '{param_name}' to an appropriate non-null value. "
                        f"Refer to SUEWS documentation for typical values for '{surface_type}' surfaces."
                    )

                    results.append(
                        ValidationResult(
                            status="ERROR",
                            category="LAND_COVER",
                            parameter=f"{surface_type}.{param_name}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message=readable_message,
                            suggested_value=actionable_suggestion,
                        )
                    )

        zero_sfr_surfaces = [surf for surf, sfr in surface_types if sfr == 0]
        if zero_sfr_surfaces:
            for surf_type in zero_sfr_surfaces:
                param_list = []
                surf_props = (
                    site.get("properties", {}).get("land_cover", {}).get(surf_type, {})
                )

                def collect_param_names(d: dict, prefix: str = ""):
                    for k, v in d.items():
                        if k == "sfr":
                            continue
                        current_path = f"{prefix}.{k}" if prefix else k
                        if isinstance(v, dict):
                            if "value" in v:
                                param_list.append(current_path)
                            else:
                                collect_param_names(v, current_path)

                collect_param_names(surf_props)

                if param_list:
                    message = f"Parameters under sites.properties.land_cover.{surf_type} are not checked because '{surf_type}' surface fraction is 0."
                    param_names = ", ".join(param_list)
                    suggested_fix = f"Either set {surf_type} surface fraction > 0 to activate validation, or remove unused parameters: {param_names}"

                    results.append(
                        ValidationResult(
                            status="WARNING",
                            category="LAND_COVER",
                            parameter=f"land_cover.{surf_type}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            message=message,
                            suggested_value=suggested_fix,
                        )
                    )

    if not any(r.status == "ERROR" for r in results):
        results.append(
            ValidationResult(
                status="PASS",
                category="LAND_COVER",
                parameter="land_cover_validation",
                message="Land cover fractions and parameters validated successfully",
            )
        )

    return results


def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """Check for missing/empty parameters in surface configuration."""
    missing_params = []

    def _check_recursively(props: dict, path: str = ""):
        for key, value in props.items():
            if key == "sfr":
                continue

            current_path = f"{path}.{key}" if path else key

            if isinstance(value, dict):
                if "value" in value:
                    param_value = value["value"]
                    if param_value in (None, "") or (
                        isinstance(param_value, list)
                        and any(v in (None, "") for v in param_value)
                    ):
                        missing_params.append(current_path)
                else:
                    _check_recursively(value, current_path)

    _check_recursively(surface_props)
    return missing_params


def validate_geographic_parameters(yaml_data: dict) -> List[ValidationResult]:
    """Validate geographic coordinates and location parameters."""
    results = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        site_gridid = get_site_gridid(site)

        lat = get_value_safe(props, "lat")

        if lat is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Latitude is missing or null",
                    suggested_value="Set latitude value between -90 and 90 degrees",
                )
            )
        elif not isinstance(lat, (int, float)):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Latitude must be a numeric value",
                    suggested_value="Set latitude as a number between -90 and 90 degrees",
                )
            )
        elif not (-90 <= lat <= 90):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lat",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=f"Latitude {lat} is outside valid range [-90, 90]",
                    suggested_value="Set latitude between -90 and 90 degrees",
                )
            )

        lng = get_value_safe(props, "lng")

        if lng is None:
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Longitude is missing or null",
                    suggested_value="Set longitude value between -180 and 180 degrees",
                )
            )
        elif not isinstance(lng, (int, float)):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Longitude must be a numeric value",
                    suggested_value="Set longitude as a number between -180 and 180 degrees",
                )
            )
        elif not (-180 <= lng <= 180):
            results.append(
                ValidationResult(
                    status="ERROR",
                    category="GEOGRAPHY",
                    parameter="lng",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message=f"Longitude {lng} is outside valid range [-180, 180]",
                    suggested_value="Set longitude between -180 and 180 degrees",
                )
            )

        timezone = get_value_safe(props, "timezone")

        if timezone is None:
            results.append(
                ValidationResult(
                    status="WARNING",
                    category="GEOGRAPHY",
                    parameter="timezone",
                    site_index=site_idx,
                    site_gridid=site_gridid,
                    message="Timezone parameter is missing - will be calculated automatically from latitude and longitude",
                    suggested_value="Timezone will be set based on your coordinates. You can also manually set the timezone value if you prefer a specific UTC offset",
                )
            )

        anthro_emissions = props.get("anthropogenic_emissions", {})
        if anthro_emissions:
            startdls = get_value_safe(anthro_emissions, "startdls")
            enddls = get_value_safe(anthro_emissions, "enddls")

            if startdls is None or enddls is None:
                results.append(
                    ValidationResult(
                        status="WARNING",
                        category="GEOGRAPHY",
                        parameter="anthropogenic_emissions.startdls,enddls",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        message="Daylight saving parameters (startdls, enddls) are missing - will be calculated automatically from geographic coordinates",
                        suggested_value="Parameters will be set based on your location. You can also manually set startdls and enddls if you prefer specific values",
                    )
                )

    error_count = sum(1 for r in results if r.status == "ERROR")
    if error_count == 0:
        results.append(
            ValidationResult(
                status="PASS",
                category="GEOGRAPHY",
                parameter="geographic_coordinates",
                message="Geographic coordinates validated successfully",
            )
        )

    return results


def run_scientific_validation_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> List[ValidationResult]:
    """Execute all scientific validation checks."""
    validation_results = []

    validation_results.extend(validate_physics_parameters(yaml_data))

    validation_results.extend(validate_model_option_dependencies(yaml_data))

    validation_results.extend(validate_land_cover_consistency(yaml_data))

    validation_results.extend(validate_geographic_parameters(yaml_data))

    return validation_results


def get_mean_monthly_air_temperature(
    lat: float, lon: float, month: int, spatial_res: float = 0.5
) -> Optional[float]:
    """Calculate monthly air temperature using CRU TS4.06 data.

    Returns None if CRU data is not available (e.g., in standalone mode).
    """
    if not (1 <= month <= 12):
        raise ValueError(f"Month must be between 1 and 12, got {month}")
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    cru_resource = trv_supy_module / "ext_data" / "CRU_TS4.06_1991_2020.parquet"

    # In standalone mode, CRU data might not be available
    if not HAS_SUPY:
        logger_supy.warning(
            "Running in standalone mode - CRU climate data not available. "
            "Skipping temperature validation."
        )
        return None

    if not cru_resource.exists():
        logger_supy.warning(
            f"CRU data file not found at {cru_resource}. "
            "Temperature validation will be skipped."
        )
        return None

    try:
        df = pd.read_parquet(cru_resource)
    except Exception as e:
        logger_supy.warning(
            f"Could not read CRU data: {e}. Temperature validation skipped."
        )
        return None

    required_cols = ["Month", "Latitude", "Longitude", "NormalTemperature"]
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns in CRU data: {missing_cols}")

    month_data = df[df["Month"] == month]
    if month_data.empty:
        raise ValueError(f"No CRU data available for month {month}")

    distances = np.sqrt(
        (month_data["Latitude"] - lat) ** 2 + (month_data["Longitude"] - lon) ** 2
    )

    for spatial_res_expanded in [spatial_res, spatial_res * 2, spatial_res * 4]:
        nearby_indices = distances <= spatial_res_expanded
        nearby_data = month_data[nearby_indices]

        if not nearby_data.empty:
            break

        if nearby_data.empty:
            raise ValueError(
                f"No CRU data found within {spatial_res_expanded} degrees of coordinates "
                f"({lat}, {lon}) for month {month}. Try increasing spatial resolution or "
                f"check if coordinates are within CRU data coverage area."
            )

    nearby_distances = distances[nearby_indices]
    closest_idx = nearby_distances.idxmin()

    temperature = month_data.loc[closest_idx, "NormalTemperature"]

    closest_lat = month_data.loc[closest_idx, "Latitude"]
    closest_lon = month_data.loc[closest_idx, "Longitude"]
    logger_supy.debug(
        f"CRU temperature for ({lat:.2f}, {lon:.2f}) month {month}: "
        f"{temperature:.2f} C from grid cell ({closest_lat:.2f}, {closest_lon:.2f})"
    )

    return float(temperature)


def adjust_surface_temperatures(
    yaml_data: dict, start_date: str
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Set initial surface temperatures based on location and season."""
    adjustments = []
    month = datetime.strptime(start_date, "%Y-%m-%d").month

    sites = yaml_data.get("sites", [])
    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        stebbs = props.get("stebbs", {})
        site_gridid = get_site_gridid(site)

        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry

        if lat is None:
            continue  # Skip if no latitude (will be caught by validation)

        lng_entry = props.get("lng", {})
        lng = lng_entry.get("value") if isinstance(lng_entry, dict) else lng_entry

        if lng is None:
            continue  # Skip if no longitude (will be caught by validation)

        avg_temp = get_mean_monthly_air_temperature(lat, lng, month)

        # Skip temperature validation if CRU data not available
        if avg_temp is None:
            logger_supy.debug(
                "Skipping temperature validation - CRU data not available"
            )
            continue

        surface_types = ["paved", "bldgs", "evetr", "dectr", "grass", "bsoil", "water"]

        for surface_type in surface_types:
            surf = initial_states.get(surface_type, {})
            if not isinstance(surf, dict):
                continue

            temperature_updated = False
            tsfc_updated = False
            tin_updated = False

            if "temperature" in surf and isinstance(surf["temperature"], dict):
                current_temp = surf["temperature"].get("value")
                if current_temp != [avg_temp] * 5:
                    surf["temperature"]["value"] = [avg_temp] * 5
                    temperature_updated = True

            if "tsfc" in surf and isinstance(surf["tsfc"], dict):
                current_tsfc = surf["tsfc"].get("value")
                if current_tsfc != avg_temp:
                    surf["tsfc"]["value"] = avg_temp
                    tsfc_updated = True

            if "tin" in surf and isinstance(surf["tin"], dict):
                current_tin = surf["tin"].get("value")
                if current_tin != avg_temp:
                    surf["tin"]["value"] = avg_temp
                    tin_updated = True

            if temperature_updated or tsfc_updated or tin_updated:
                updated_params = []
                if temperature_updated:
                    updated_params.append("temperature")
                if tsfc_updated:
                    updated_params.append("tsfc")
                if tin_updated:
                    updated_params.append("tin")

                param_list = ", ".join(updated_params)

                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"initial_states.{surface_type}",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=param_list,
                        new_value=f"{avg_temp} C",
                        reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f}) for month {month}",
                    )
                )

            initial_states[surface_type] = surf

        # Update STEBBS temperature parameter values to avg_temp
        for key in ("WallOutdoorSurfaceTemperature", "WindowOutdoorSurfaceTemperature"):
            if key in stebbs and isinstance(stebbs[key], dict):
                old_val = stebbs[key].get("value")
                if old_val != avg_temp:
                    stebbs[key]["value"] = avg_temp
                    adjustments.append(
                        ScientificAdjustment(
                            parameter=f"stebbs.{key}",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=str(old_val),
                            new_value=f"{avg_temp} C",
                            reason=f"Set from CRU data for coordinates ({lat:.2f}, {lng:.2f}) for month {month}",
                        )
                    )

        # Save back to site
        site["initial_states"] = initial_states
        props["stebbs"] = stebbs
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def adjust_land_cover_fractions(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Auto-fix small floating point errors in surface fractions."""
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        land_cover = props.get("land_cover")
        site_gridid = get_site_gridid(site)

        if not land_cover:
            continue

        # Calculate sum of all surface fractions
        surface_fractions = {}
        sfr_sum = 0.0

        for surface_type, surface_props in land_cover.items():
            if isinstance(surface_props, dict):
                sfr_value = surface_props.get("sfr", {}).get("value")
                if sfr_value is not None:
                    surface_fractions[surface_type] = sfr_value
                    sfr_sum += sfr_value

        correction_applied = False

        # Auto-correct only small floating point errors (same as precheck logic)
        if 0.9999 <= sfr_sum < 1.0:
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = 1.0 - sfr_sum
            old_value = surface_fractions[max_surface]
            new_value = old_value + correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            # Always report adjustments, but use appropriate format based on visibility
            if abs(correction) >= 1e-6:  # If change is visible at 6 decimal places
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=f"{old_value:.6f}",
                        new_value=f"{new_value:.6f}",
                        reason=f"Auto-corrected sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
                    )
                )
            else:  # Tiny correction not visible at display precision
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value="rounded to achieve sum of land cover fractions equal to 1.0",
                        new_value=f"tolerance level: {abs(correction):.2e}",
                        reason="Small floating point rounding applied to surface with max surface fraction value",
                    )
                )

        elif 1.0 < sfr_sum <= 1.0001:
            max_surface = max(
                surface_fractions.keys(), key=lambda k: surface_fractions[k]
            )
            correction = sfr_sum - 1.0
            old_value = surface_fractions[max_surface]
            new_value = old_value - correction

            land_cover[max_surface]["sfr"]["value"] = new_value
            correction_applied = True

            # Always report adjustments, but use appropriate format based on visibility
            if abs(correction) >= 1e-6:  # If change is visible at 6 decimal places
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=f"{old_value:.6f}",
                        new_value=f"{new_value:.6f}",
                        reason=f"Auto-corrected sum from {sfr_sum:.6f} to 1.0 (small floating point error)",
                    )
                )
            else:  # Tiny correction not visible at display precision
                adjustments.append(
                    ScientificAdjustment(
                        parameter=f"{max_surface}.sfr",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value="rounded to achieve sum of land cover fractions equal to 1.0",
                        new_value=f"tolerance level: {abs(correction):.2e}",
                        reason="Small floating point rounding applied to surface with max surface fraction value",
                    )
                )

        if correction_applied:
            site["properties"] = props
            yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def adjust_model_dependent_nullification(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Nullify parameters for disabled model options."""
    adjustments = []
    physics = yaml_data.get("model", {}).get("physics", {})

    stebbsmethod = get_value_safe(physics, "stebbsmethod")

    if stebbsmethod == 0:
        sites = yaml_data.get("sites", [])

        for site_idx, site in enumerate(sites):
            props = site.get("properties", {})
            stebbs_block = props.get("stebbs", {})
            site_gridid = get_site_gridid(site)

            if stebbs_block:
                nullified_params = []

                def _recursive_nullify(block: dict, path: str = ""):
                    for key, val in block.items():
                        current_path = f"{path}.{key}" if path else key

                        if isinstance(val, dict):
                            if "value" in val and val["value"] is not None:
                                val["value"] = None
                                nullified_params.append(current_path)
                            else:
                                _recursive_nullify(val, current_path)

                _recursive_nullify(stebbs_block)

                if nullified_params:
                    param_list = ", ".join(nullified_params)

                    adjustments.append(
                        ScientificAdjustment(
                            parameter="stebbs",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value=f"stebbsmethod is switched off, nullified {len(nullified_params)} related parameters - {param_list}",
                            new_value="null",
                            reason=f"stebbsmethod switched off, nullified {len(nullified_params)} related parameters",
                        )
                    )

                props["stebbs"] = stebbs_block
                site["properties"] = props
                yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def get_season(start_date: str, lat: float) -> str:
    """Determine season based on start date and latitude."""
    try:
        start = datetime.strptime(start_date, "%Y-%m-%d").timetuple().tm_yday
    except ValueError:
        raise ValueError("start_date must be in YYYY-MM-DD format")

    abs_lat = abs(lat)

    if abs_lat <= 10:
        return "equatorial"
    if 10 < abs_lat < 23.5:
        return "tropical"

    if lat >= 0:  # Northern Hemisphere
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


def adjust_seasonal_parameters(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply seasonal adjustments including LAI, snowalb, and DLS calculations."""
    adjustments = []
    sites = yaml_data.get("sites", [])

    for site_idx, site in enumerate(sites):
        props = site.get("properties", {})
        initial_states = site.get("initial_states", {})
        site_gridid = get_site_gridid(site)

        # Get site coordinates
        lat_entry = props.get("lat", {})
        lat = lat_entry.get("value") if isinstance(lat_entry, dict) else lat_entry
        lng = get_value_safe(props, "lng")

        if lat is None:
            continue  # Skip if no latitude

        try:
            season = get_season(start_date, lat)
        except Exception as e:
            continue  # Skip on season detection error

        if (
            season in ("summer", "tropical", "equatorial")
            and "snowalb" in initial_states
        ):
            current_snowalb = initial_states["snowalb"].get("value")
            if current_snowalb is not None:
                initial_states["snowalb"]["value"] = None
                adjustments.append(
                    ScientificAdjustment(
                        parameter="snowalb",
                        site_index=site_idx,
                        site_gridid=site_gridid,
                        old_value=str(current_snowalb),
                        new_value="null",
                        reason=f"Nullified for {season} season (no snow expected)",
                    )
                )

        land_cover = props.get("land_cover", {})
        dectr = land_cover.get("dectr", {})
        if dectr:
            sfr = dectr.get("sfr", {}).get("value", 0)

            if sfr > 0:
                lai = dectr.get("lai", {})
                laimin = lai.get("laimin", {}).get("value")
                laimax = lai.get("laimax", {}).get("value")

                if laimin is not None and laimax is not None:
                    if season == "summer":
                        lai_val = laimax
                    elif season == "winter":
                        lai_val = laimin
                    elif season in ("spring", "fall"):
                        lai_val = (laimax + laimin) / 2
                    else:  # tropical/equatorial
                        lai_val = laimax

                    if "dectr" not in initial_states:
                        initial_states["dectr"] = {}

                    current_lai = initial_states["dectr"].get("lai_id", {}).get("value")
                    if current_lai != lai_val:
                        initial_states["dectr"]["lai_id"] = {"value": lai_val}
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="dectr.lai_id",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(current_lai)
                                if current_lai is not None
                                else "undefined",
                                new_value=str(lai_val),
                                reason=f"Set seasonal LAI for {season} (laimin={laimin}, laimax={laimax})",
                            )
                        )
            else:
                if (
                    "dectr" in initial_states
                    and initial_states["dectr"].get("lai_id", {}).get("value")
                    is not None
                ):
                    initial_states["dectr"]["lai_id"] = {"value": None}
                    adjustments.append(
                        ScientificAdjustment(
                            parameter="dectr.lai_id",
                            site_index=site_idx,
                            site_gridid=site_gridid,
                            old_value="previous value",
                            new_value="null",
                            reason="Nullified (no deciduous trees: sfr=0)",
                        )
                    )

        if lat is not None and lng is not None:
            try:
                dls = DLSCheck(lat=lat, lng=lng, year=model_year)
                start_dls, end_dls, tz_offset = dls.compute_dst_transitions()

                anthro_emissions = props.get("anthropogenic_emissions", {})
                if anthro_emissions and start_dls and end_dls:
                    current_startdls = anthro_emissions.get("startdls", {}).get("value")
                    current_enddls = anthro_emissions.get("enddls", {}).get("value")

                    dls_updated = False
                    if current_startdls != start_dls:
                        anthro_emissions["startdls"] = {"value": start_dls}
                        dls_updated = True

                    if current_enddls != end_dls:
                        anthro_emissions["enddls"] = {"value": end_dls}
                        dls_updated = True

                    if dls_updated:
                        # Add separate adjustments for each parameter
                        if current_startdls != start_dls:
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter="anthropogenic_emissions.startdls",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(current_startdls),
                                    new_value=str(start_dls),
                                    reason=f"Calculated DLS start for coordinates ({lat:.2f}, {lng:.2f})",
                                )
                            )
                        if current_enddls != end_dls:
                            adjustments.append(
                                ScientificAdjustment(
                                    parameter="anthropogenic_emissions.enddls",
                                    site_index=site_idx,
                                    site_gridid=site_gridid,
                                    old_value=str(current_enddls),
                                    new_value=str(end_dls),
                                    reason=f"Calculated DLS end for coordinates ({lat:.2f}, {lng:.2f})",
                                )
                            )
                        logger_supy.debug(
                            f"[site #{site_idx}] DLS: start={start_dls}, end={end_dls}"
                        )

                if tz_offset is not None:
                    current_timezone = props.get("timezone", {}).get("value")
                    if current_timezone != tz_offset:
                        props["timezone"] = {"value": tz_offset}
                        adjustments.append(
                            ScientificAdjustment(
                                parameter="timezone",
                                site_index=site_idx,
                                site_gridid=site_gridid,
                                old_value=str(current_timezone),
                                new_value=str(tz_offset),
                                reason=f"Calculated timezone offset for coordinates ({lat:.2f}, {lng:.2f})",
                            )
                        )
                        logger_supy.debug(
                            f"[site #{site_idx}] Timezone set to {tz_offset}"
                        )

            except Exception as e:
                logger_supy.debug(f"[site #{site_idx}] DLS calculation failed: {e}")
                pass

        # Save back to site
        site["properties"] = props
        site["initial_states"] = initial_states
        yaml_data["sites"][site_idx] = site

    return yaml_data, adjustments


def run_scientific_adjustment_pipeline(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply automatic scientific corrections and adjustments."""
    adjustments = []
    updated_data = deepcopy(yaml_data)

    updated_data, temp_adjustments = adjust_surface_temperatures(
        updated_data, start_date
    )
    adjustments.extend(temp_adjustments)

    updated_data, fraction_adjustments = adjust_land_cover_fractions(updated_data)
    adjustments.extend(fraction_adjustments)

    updated_data, nullify_adjustments = adjust_model_dependent_nullification(
        updated_data
    )
    adjustments.extend(nullify_adjustments)

    updated_data, seasonal_adjustments = adjust_seasonal_parameters(
        updated_data, start_date, model_year
    )
    adjustments.extend(seasonal_adjustments)

    return updated_data, adjustments


def create_science_report(
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    science_yaml_filename: str = None,
    phase_a_report_file: str = None,
    mode: str = "public",
    phase: str = "B",
) -> str:
    """Generate comprehensive scientific validation report."""
    report_lines = []

    # Use unified report title for all validation phases
    title = "SUEWS Validation Report"

    report_lines.append(f"# {title}")
    report_lines.append("# " + "=" * 50)
    report_lines.append(
        f"# Mode: {'Public' if mode.lower() == 'public' else mode.title()}"
    )
    report_lines.append("# " + "=" * 50)
    report_lines.append("")

    phase_a_renames = []
    phase_a_optional_missing = []
    phase_a_not_in_standard = []

    if phase_a_report_file and os.path.exists(phase_a_report_file):
        try:
            with open(phase_a_report_file, "r") as f:
                phase_a_content = f.read()

            lines = phase_a_content.split("\n")
            current_section = None

            for line in lines:
                line = line.strip()
                if "Updated (" in line and "renamed parameter" in line:
                    current_section = "renames"
                elif "Updated (" in line and "optional missing parameter" in line:
                    current_section = "optional"
                elif "parameter(s) not in standard" in line:
                    current_section = "not_standard"
                elif line.startswith("--") and current_section == "renames":
                    phase_a_renames.append(line[2:].strip())
                elif line.startswith("--") and current_section == "optional":
                    phase_a_optional_missing.append(line[2:].strip())
                elif line.startswith("--") and current_section == "not_standard":
                    phase_a_not_in_standard.append(line[2:].strip())
        except Exception:
            # If we can't read Phase A report, continue without it
            pass

    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]
    passed = [r for r in validation_results if r.status == "PASS"]

    if errors:
        report_lines.append("## ACTION NEEDED")
        report_lines.append(
            f"- Found ({len(errors)}) critical scientific parameter error(s):"
        )
        for error in errors:
            site_ref = (
                f" at site [{error.site_gridid}]"
                if error.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {error.parameter}{site_ref}: {error.message}")
            if error.suggested_value is not None:
                report_lines.append(f"   Suggested fix: {error.suggested_value}")
        report_lines.append("")

    report_lines.append("## NO ACTION NEEDED")

    if adjustments:
        total_params_changed = 0
        for adjustment in adjustments:
            if "temperature, tsfc, tin" in adjustment.old_value:
                total_params_changed += 3
            elif adjustment.parameter == "stebbs" and "nullified" in adjustment.reason:
                import re

                match = re.search(r"nullified (\d+) parameters", adjustment.reason)
                if match:
                    total_params_changed += int(match.group(1))
                else:
                    total_params_changed += 1
            elif adjustment.parameter in [
                "anthropogenic_emissions.startdls",
                "anthropogenic_emissions.enddls",
            ]:
                total_params_changed += 1
            else:
                total_params_changed += 1

        report_lines.append(f"- Updated ({total_params_changed}) parameter(s):")
        for adjustment in adjustments:
            site_ref = (
                f" at site [{adjustment.site_gridid}]"
                if adjustment.site_gridid is not None
                else ""
            )
            report_lines.append(
                f"-- {adjustment.parameter}{site_ref}: {adjustment.old_value} → {adjustment.new_value} ({adjustment.reason})"
            )

    phase_a_items = []
    if phase_a_renames:
        phase_a_items.append(
            f"- Updated ({len(phase_a_renames)}) renamed parameter(s) to current standards:"
        )
        for rename in phase_a_renames:
            phase_a_items.append(f"-- {rename}")

    if phase_a_optional_missing:
        phase_a_items.append(
            f"- Updated ({len(phase_a_optional_missing)}) optional missing parameter(s) with null values:"
        )
        for param in phase_a_optional_missing:
            phase_a_items.append(f"-- {param}")

    if phase_a_not_in_standard:
        phase_a_items.append(
            f"- Found ({len(phase_a_not_in_standard)}) parameter(s) not in standard:"
        )
        for param in phase_a_not_in_standard:
            phase_a_items.append(f"-- {param}")

    if phase_a_items:
        report_lines.extend(phase_a_items)
        if warnings or (not adjustments and not errors):
            report_lines.append("")

    if warnings:
        report_lines.append(f"- Revise ({len(warnings)}) warnings:")
        for warning in warnings:
            site_ref = (
                f" at site [{warning.site_gridid}]"
                if warning.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {warning.parameter}{site_ref}: {warning.message}")
        # Skip adding generic "passed" message when there are warnings
    else:
        if not adjustments and not errors:
            if not phase_a_items:
                report_lines.append("- All scientific validations passed")
                report_lines.append("- Model physics parameters are consistent")
                report_lines.append("- Geographic parameters are valid")
            # Skip generic messages when phase A items exist
        # Skip generic messages when there are no errors

    report_lines.append("")

    report_lines.append("# " + "=" * 50)

    return "\n".join(report_lines)


def print_critical_halt_message(critical_errors: List[ValidationResult]):
    """
    Print critical halt message when Phase B detects errors requiring Phase A.

    Args:
        critical_errors: List of ERROR-level validation results
    """
    print()
    print("=" * 60)
    print(" PHASE B HALTED - CRITICAL ERRORS DETECTED")
    print("=" * 60)
    print()
    print("Phase B detected critical scientific errors:")
    print()

    for error in critical_errors:
        site_ref = (
            f" at site [{error.site_gridid}]" if error.site_gridid is not None else ""
        )
        print(f"  ✗ {error.parameter}{site_ref}")
        print(f"    {error.message}")
        if error.suggested_value is not None:
            print(f"    Suggested: {error.suggested_value}")
        print()

    print("OPTIONS TO RESOLVE:")
    print("1. Fix the issues manually in your YAML file, or")
    print("2. Run Phase A first to auto-detect and fix missing parameters:")
    print("   python suews_yaml_processor.py user.yml --phase A")
    print("3. Then re-run Phase B")
    print()
    print("Phase A can help detect missing parameters and provide")
    print("appropriate defaults for critical physics options.")
    print()
    print("=" * 60)


def print_science_check_results(
    validation_results: List[ValidationResult], adjustments: List[ScientificAdjustment]
):
    """
    Print clean terminal output for Phase B results.

    Args:
        validation_results: List of validation results
        adjustments: List of automatic adjustments applied
    """
    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]

    if errors:
        print("PHASE B -- SCIENTIFIC ERRORS FOUND:")
        for error in errors:
            site_ref = (
                f" at site [{error.site_gridid}]"
                if error.site_gridid is not None
                else ""
            )
            print(f"  - {error.parameter}{site_ref}: {error.message}")
        print(
            "\nNext step: Check science_report_user.txt for detailed scientific guidance"
        )
    elif warnings:
        print(f"PHASE B -- SCIENTIFIC WARNINGS ({len(warnings)} found)")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")
        print("Check science_report_user.txt for details")
    else:
        print("PHASE B -- PASSED")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")


def create_science_yaml_header(phase_a_performed: bool = True) -> str:
    """Create header for final science-checked YAML file.

    Args:
        phase_a_performed: Whether Phase A was performed before Phase B
    """
    # Use the standardized header format for all Phase B outputs
    header = """# ==============================================================================
# Updated YAML
# ==============================================================================
#
# This file has been updated by the SUEWS processor and is the updated version of the user provided YAML.
# Details of changes are in the generated report.
#
# ==============================================================================

"""
    return header


def run_science_check(
    uptodate_yaml_file: str,
    user_yaml_file: str,
    standard_yaml_file: str,
    science_yaml_file: str = None,
    science_report_file: str = None,
    phase_a_report_file: str = None,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
) -> dict:
    """
    Main Phase B workflow - perform scientific validation and adjustments.

    Args:
        uptodate_yaml_file: Path to Phase A output (clean YAML)
        user_yaml_file: Path to original user YAML
        standard_yaml_file: Path to standard reference YAML
        science_yaml_file: Path for science-checked output YAML
        science_report_file: Path for scientific validation report
        phase_a_report_file: Path to Phase A report file (if available)
        phase_a_performed: Whether Phase A was performed before Phase B

    Returns:
        Final science-checked YAML configuration dictionary

    Raises:
        FileNotFoundError: If required input files are missing
        ValueError: If Phase A did not complete or YAML is invalid
    """
    try:
        uptodate_data, user_data, standard_data = validate_phase_b_inputs(
            uptodate_yaml_file, user_yaml_file, standard_yaml_file
        )

        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)

        validation_results = run_scientific_validation_pipeline(
            uptodate_data, start_date, model_year
        )
    except (ValueError, FileNotFoundError, KeyError) as e:
        # Handle initialization failures and create error report
        error_message = str(e)

        # Create individual validation results for each error if multiple errors are present
        validation_results = []
        if ";" in error_message:
            # Multiple errors - split them and create separate ValidationResult objects
            individual_errors = error_message.split("; ")
            for error in individual_errors:
                validation_results.append(
                    ValidationResult(
                        status="ERROR",
                        category="INITIALIZATION",
                        parameter="model.control",
                        message=f"Phase B initialization failed: {error.strip()}",
                        suggested_value=None,
                    )
                )
        else:
            # Single error
            validation_results = [
                ValidationResult(
                    status="ERROR",
                    category="INITIALIZATION",
                    parameter="model.control",
                    message=f"Phase B initialization failed: {error_message}",
                    suggested_value=None,
                )
            ]

        # Create error report
        science_yaml_filename = (
            os.path.basename(science_yaml_file) if science_yaml_file else None
        )
        report_content = create_science_report(
            validation_results,
            [],  # No adjustments since we failed early
            science_yaml_filename,
            phase_a_report_file,
            mode,
            phase,
        )

        # Write error report file
        if science_report_file:
            with open(science_report_file, "w") as f:
                f.write(report_content)

        # Re-raise the exception so orchestrator knows it failed
        raise e

    critical_errors = [r for r in validation_results if r.status == "ERROR"]
    if not critical_errors:
        science_checked_data, adjustments = run_scientific_adjustment_pipeline(
            uptodate_data, start_date, model_year
        )
    else:
        science_checked_data = deepcopy(uptodate_data)
        adjustments = []

    science_yaml_filename = (
        os.path.basename(science_yaml_file) if science_yaml_file else None
    )
    report_content = create_science_report(
        validation_results,
        adjustments,
        science_yaml_filename,
        phase_a_report_file,
        mode,
        phase,
    )

    if science_report_file:
        with open(science_report_file, "w") as f:
            f.write(report_content)

    if critical_errors:
        print_critical_halt_message(critical_errors)
        raise ValueError("Critical scientific errors detected - Phase B halted")

    print_science_check_results(validation_results, adjustments)

    if science_yaml_file and not critical_errors:
        header = create_science_yaml_header(phase_a_performed)
        with open(science_yaml_file, "w") as f:
            f.write(header)
            yaml.dump(
                science_checked_data, f, default_flow_style=False, sort_keys=False
            )

    return science_checked_data


def main():
    """Main entry point for science_check.py Phase B."""
    print(" SUEWS Scientific Validation (Phase B)")
    print("=" * 50)

    user_file = "src/supy/data_model/user.yml"
    uptodate_file = "src/supy/data_model/uptodate_user.yml"
    standard_file = "src/supy/sample_data/sample_config.yml"

    print(f"Phase A output (uptodate): {uptodate_file}")
    print(f"Original user YAML: {user_file}")
    print(f"Standard YAML: {standard_file}")
    print()

    basename = os.path.basename(user_file)
    dirname = os.path.dirname(user_file)
    name_without_ext = os.path.splitext(basename)[0]

    science_yaml_filename = f"science_checked_{basename}"
    science_report_filename = f"science_report_{name_without_ext}.txt"

    science_yaml_file = os.path.join(dirname, science_yaml_filename)
    science_report_file = os.path.join(dirname, science_report_filename)

    try:
        science_checked_data = run_science_check(
            uptodate_yaml_file=uptodate_file,
            user_yaml_file=user_file,
            standard_yaml_file=standard_file,
            science_yaml_file=science_yaml_file,
            science_report_file=science_report_file,
            phase_a_performed=True,  # Assumes Phase A was run (looking for uptodate_file)
            phase="B",
        )

        print(f"\nPhase B completed successfully!")
        print(f"Science-checked YAML: {science_yaml_file}")
        print(f"Science report: {science_report_file}")

    except ValueError as e:
        if "Critical scientific errors detected" in str(e):
            # Critical errors already printed by print_critical_halt_message
            return 1
        else:
            print(f"\nPhase B failed: {e}")
            return 1
    except Exception as e:
        print(f"\nPhase B failed with unexpected error: {e}")
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
