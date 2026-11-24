"""
SUEWS Physics Validation Check - Phase B

This module performs physics validation and consistency checks on YAML configurations
that have already been processed by Phase A.

Phase B focuses on:
- Physics parameter validation
- Geographic coordinate and timezone validation
- Seasonal parameter checks (LAI, snowalb, surface temperatures)
- Land cover fraction validation and consistency
- Model physics option interdependency checks
- Optional scientific initialisation suggestions and corrections

Phase B assumes Phase A has completed successfully and builds upon clean YAML output
without duplicating parameter detection or YAML structure validation.
"""

from copy import deepcopy
from dataclasses import dataclass
import os
from typing import Any, List, Optional, Tuple
import yaml

# Import from validation package
from .. import logger_supy
from ...core._yaml_io import safe_load_yaml
from ...core.field_renames import normalise_yaml_renames
from ...core.profile import normalise_hourly_profile_keys_in_raw_config
from ..core.yaml_helpers import (
    DLSCheck,
    get_mean_annual_air_temperature as _get_mean_annual_air_temperature,
    get_mean_monthly_air_temperature as _get_mean_monthly_air_temperature,
    get_monthly_air_temperature_diffmax as _get_monthly_air_temperature_diffmax,
    get_ref_or_bare,
    load_user_yaml_normalised,
    parse_simulation_bounds,
    set_ref_or_bare,
)
from .patches import (
    PatchPlan,
    PlannedAdjustment,
    get_season,
    get_season_from_doy,
    plan_land_cover_fraction_patches,
    plan_model_dependent_nullification_patches,
    plan_rcmethod_patches,
    plan_seasonal_patches,
    plan_setpointmethod_patches,
    plan_stebbsmethod_wwr_patches,
    plan_surface_temperature_patches,
)
from .phase_b_rules import (
    SFR_FRACTION_TOL,
    RulesRegistry,
    ValidationContext,
    ValidationResult,
)
from .phase_b_rules.other_rules import (
    _check_surface_parameters as _rules_check_surface_parameters,
    check_missing_vegetation_albedo as _rules_check_missing_vegetation_albedo,
    validate_geographic_parameters as _rules_validate_geographic_parameters,
    validate_irrigation_doy,
    validate_irrigation_parameters as _rules_validate_irrigation_parameters,
)
from .phase_b_rules.rules_core import rule_crash_result
from .report_schema import PhaseReport
from .report_writer import REPORT_WRITER

@dataclass
class ScientificAdjustment:
    """
    Record of automatic scientific adjustment applied.

    Attributes
    ----------
    parameter : str
        Name of the parameter that was adjusted.
    site_index : Optional[int], default=None
        Array index of the site (for internal use).
    site_gridid : Optional[int], default=None
        GRIDID value of the site (for display purposes).
    old_value : Any, default=None
        The original value of the parameter before adjustment.
    new_value : Any, default=None
        The new value of the parameter after adjustment.
    reason : str, default=""
        Description or reason for the adjustment.
    """

    parameter: str
    site_index: Optional[int] = None  # Array index (for internal use)
    site_gridid: Optional[int] = None  # GRIDID value (for display)
    old_value: Any = None
    new_value: Any = None
    reason: str = ""


@dataclass
class ScienceSuggestion:
    """
    Scientific transformation proposed by Phase B.

    Suggestions describe values that can help initialise a SUEWS run, but they are
    not treated as scientific truth. They are reported by default and only written
    to YAML when the user explicitly selects apply mode.
    """

    parameter: str
    site_index: Optional[int] = None
    site_gridid: Optional[int] = None
    old_value: Any = None
    suggested_value: Any = None
    message: str = ""
    source: str = ""
    code: Optional[str] = None
    severity: str = "SUGGESTION"
    path: Optional[str] = None

    def __post_init__(self):
        self.severity = self.severity.upper()
        if self.code is None:
            normalised_parameter = self.parameter.replace(".", "_").replace(" ", "_")
            self.code = f"SCIENCE.{normalised_parameter}".upper()
        if self.path is None:
            self.path = self.parameter

    def to_dict(self) -> dict:
        return {
            "code": self.code,
            "severity": self.severity,
            "path": self.path,
            "site_gridid": self.site_gridid,
            "message": self.message,
            "suggested_value": self.suggested_value,
            "source": self.source,
        }

    def to_issue(self):
        """Adapt to the canonical ``Issue`` schema."""
        from .report_schema import Issue

        return Issue(
            phase="B",
            severity=self.severity,
            code=self.code or f"B.SCIENCE.{self.parameter}".upper(),
            message=self.message,
            yaml_path=self.path or self.parameter,
            site_gridid=self.site_gridid,
            site_index=self.site_index,
            category="SCIENCE_SUGGESTION",
            suggested_value=self.suggested_value,
            applied_fix=False,
        )


def _adjustment_to_issue(adj: "ScientificAdjustment"):
    """Adapt a ``ScientificAdjustment`` to the canonical ``Issue`` schema."""
    from .report_schema import Issue, SEVERITY_APPLIED_FIX

    parameter = adj.parameter
    parameter_slug = parameter.replace(".", "_").replace(" ", "_").upper()
    return Issue(
        phase="B",
        severity=SEVERITY_APPLIED_FIX,
        code=f"B.APPLIED_FIX.{parameter_slug}",
        message=adj.reason or f"Adjusted {parameter}: {adj.old_value} -> {adj.new_value}",
        yaml_path=parameter,
        site_gridid=adj.site_gridid,
        site_index=adj.site_index,
        category="APPLIED_FIX",
        suggested_value=adj.new_value,
        applied_fix=True,
    )


VALID_SCIENCE_FIX_MODES = {"suggest", "apply", "off"}


def _normalise_science_fixes(science_fixes: str) -> str:
    science_fixes = (science_fixes or "suggest").lower()
    if science_fixes not in VALID_SCIENCE_FIX_MODES:
        raise ValueError(
            "science_fixes must be one of: suggest, apply, off"
        )
    return science_fixes


def _science_source(parameter: str, reason: str) -> str:
    parameter_lower = parameter.lower()
    reason_lower = reason.lower()

    if parameter_lower.startswith("initial_states") or "air_temperature" in parameter_lower:
        return (
            "CRU TS climatology-derived initialisation heuristic; may be inappropriate "
            "for observed initial states, spin-up workflows, historical weather cases, "
            "or specialist case studies."
        )
    if parameter_lower.startswith("stebbs.") and "temperature" in parameter_lower:
        return (
            "CRU TS climatology-derived STEBBS initialisation heuristic; review against "
            "observed building states, spin-up workflows, and case-specific assumptions."
        )
    if parameter_lower in {"timezone", "anthropogenic_emissions.startdls", "anthropogenic_emissions.enddls"}:
        return (
            "Timezone and daylight-saving calculation from site coordinates; review for "
            "historical timezone settings, policy changes, and local case-study choices."
        )
    if parameter_lower.endswith(".lai_id"):
        return (
            "Seasonal deciduous LAI initialisation based on start date and configured "
            "LAI bounds; review for observed phenology or specialist vegetation studies."
        )
    if parameter_lower.endswith(".alb_id"):
        return (
            "Seasonal vegetation albedo initialisation based on configured albedo bounds; "
            "review against observed surface state and site-specific calibration."
        )
    if parameter_lower == "snowalb":
        return (
            "Seasonal snow-albedo initialisation heuristic; review for observed snow cover, "
            "cold-season spin-up, and unusual local conditions."
        )
    if parameter_lower.endswith(".sfr"):
        return (
            f"Land-cover fractions must sum to 1.0 for SUEWS runtime consistency; "
            f"this suggestion only covers rounding drift within {SFR_FRACTION_TOL:g}."
        )
    if "stebbs" in parameter_lower or "wwr" in reason_lower:
        return (
            "Model-option dependency for STEBBS and window-to-wall-ratio parameters; "
            "review before removing case-specific building physics inputs."
        )
    if "co2" in parameter_lower or "biogenic" in parameter_lower:
        return (
            "Model-option dependency for emissions and CO2/STEBBS coupling; review "
            "before removing inputs needed by a specialist emissions workflow."
        )
    if parameter_lower.startswith("building_archetype"):
        return (
            "Model-option dependency for building archetype, setpoint, and RC settings; "
            "review before replacing calibrated building parameters."
        )
    return (
        "SUEWS option-dependent initialisation suggestion; review against the modelling "
        "purpose before applying."
    )


def _adjustment_to_suggestion(adjustment: ScientificAdjustment) -> ScienceSuggestion:
    source = _science_source(adjustment.parameter, adjustment.reason)
    return ScienceSuggestion(
        parameter=adjustment.parameter,
        site_index=adjustment.site_index,
        site_gridid=adjustment.site_gridid,
        old_value=adjustment.old_value,
        suggested_value=adjustment.new_value,
        message=f"{adjustment.reason}. Current value: {adjustment.old_value}; suggested value: {adjustment.new_value}.",
        source=source,
    )


def get_site_gridid(site_data: dict) -> int:
    """
    Extract GRIDID from site data, handling both direct and RefValue formats.

    Parameters
    ----------
    site_data : dict
        Dictionary containing site information, which may include a 'gridiv' key.

    Returns
    -------
    int or None
        The GRIDID value if found, otherwise None.
    """
    if isinstance(site_data, dict):
        gridiv = site_data.get("gridiv")
        if isinstance(gridiv, dict) and "value" in gridiv:
            return gridiv["value"]
        elif gridiv is not None:
            return gridiv
    return None


def _load_phase_b_input(uptodate_yaml_file: str) -> dict:
    """Load and normalise only the YAML file Phase B checks."""
    if not os.path.exists(uptodate_yaml_file):
        raise FileNotFoundError(f"Required file not found: {uptodate_yaml_file}")

    try:
        from ...core.physics_families import flatten_physics_in_config

        with open(uptodate_yaml_file, encoding="utf-8") as f:
            uptodate_data = normalise_yaml_renames(safe_load_yaml(f))
        uptodate_data, _ = normalise_hourly_profile_keys_in_raw_config(uptodate_data)
        flatten_physics_in_config(uptodate_data)
    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}") from e

    return uptodate_data


def validate_phase_b_inputs(
    uptodate_yaml_file: str, user_yaml_file: str, standard_yaml_file: str
) -> Tuple[dict, dict, dict]:
    """
    Validate Phase B inputs and load YAML files.

    Checks the existence of the provided YAML files, loads their contents, and parses them into dictionaries.
    Raises an error if any file is missing or if the YAML format is invalid.

    Parameters
    ----------
    uptodate_yaml_file : str
        Path to the "up to date" YAML file.
    user_yaml_file : str
        Path to the user-provided YAML file.
    standard_yaml_file : str
        Path to the standard YAML file.

    Returns
    -------
    tuple of dict
        A tuple containing three dictionaries: (uptodate_data, user_data, standard_data), each corresponding to the loaded contents of the respective YAML files.

    Raises
    ------
    FileNotFoundError
        If any of the specified YAML files do not exist.
    ValueError
        If any of the YAML files contain invalid YAML syntax.
    """
    for file_path in [uptodate_yaml_file, user_yaml_file, standard_yaml_file]:
        if not os.path.exists(file_path):
            raise FileNotFoundError(f"Required file not found: {file_path}")

    try:
        from ...core.physics_families import flatten_physics_in_config

        uptodate_data = _load_phase_b_input(uptodate_yaml_file)
        user_data = load_user_yaml_normalised(user_yaml_file)

        with open(standard_yaml_file, "r", encoding="utf-8") as f:
            standard_data = safe_load_yaml(f)

        # Collapse readable physics names for raw-dict science checks.
        flatten_physics_in_config(user_data)
        flatten_physics_in_config(standard_data)

    except yaml.YAMLError as e:
        raise ValueError(f"Invalid YAML format: {e}")

    return uptodate_data, user_data, standard_data


def extract_simulation_parameters(yaml_data: dict) -> Tuple[int, str, str]:
    """
    Extract model year, start date, and end date from YAML configuration.

    Parameters
    ----------
    yaml_data : dict
        Dictionary containing the parsed YAML data, expected to have the structure:
        {'model': {'control': {'start_time': str, 'end_time': str}}}

    Returns
    -------
    Tuple[int, str, str]
        A tuple containing:
            - model_year (int): The year extracted from 'start_time'.
            - start_date (str): The calendar day of 'start_time' as 'YYYY-MM-DD'.
            - end_date (str): The calendar day of 'end_time' as 'YYYY-MM-DD'.

    Raises
    ------
    ValueError
        If 'start_time' or 'end_time' is missing or is neither a date
        (YYYY-MM-DD) nor a timestamp (YYYY-MM-DD HH:MM).

    Notes
    -----
    All validation errors are collected and reported together in the exception
    message. A timestamp bound is reduced to its calendar day here, because the
    seasonal and climatological adjustments only need the day.
    """
    errors, model_year, start_date, end_date = parse_simulation_bounds(yaml_data)
    if errors:
        raise ValueError("; ".join(errors))
    return model_year, start_date, end_date


def _check_surface_parameters(surface_props: dict, surface_type: str) -> List[str]:
    """Delegate to the canonical surface-parameter check in ``phase_b_rules.other_rules``."""
    return _rules_check_surface_parameters(surface_props, surface_type)


def validate_geographic_parameters(yaml_data: dict) -> List[ValidationResult]:
    """Delegate to the canonical geographic rule in ``phase_b_rules.other_rules``."""
    return _rules_validate_geographic_parameters(ValidationContext(yaml_data=yaml_data))


def validate_irrigation_parameters(
    yaml_data: dict, model_year: int
) -> List[ValidationResult]:
    """Delegate to the canonical irrigation rule in ``phase_b_rules.other_rules``."""
    return _rules_validate_irrigation_parameters(
        ValidationContext(yaml_data=yaml_data, model_year=model_year)
    )


def check_missing_vegetation_albedo(yaml_data: dict) -> List[ValidationResult]:
    """Delegate to the canonical vegetation-albedo rule in ``phase_b_rules.other_rules``."""
    return _rules_check_missing_vegetation_albedo(ValidationContext(yaml_data=yaml_data))


def get_mean_monthly_air_temperature(
    lat: float, lon: float, month: int, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate mean monthly air temperature using CRU TS4.06 climatological data.

    This function wraps `yaml_helpers.get_mean_monthly_air_temperature` and returns
    None if CRU data is not available (e.g., in standalone mode).

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to 90).
    lon : float
        Longitude in degrees (-180 to 180).
    month : int
        Month of the year (1-12).
    spatial_res : float, optional
        Spatial resolution in degrees for finding the nearest CRU grid cell (default: 0.5).

    Returns
    -------
    float or None
        Mean monthly air temperature in Celsius for the specified location and month,
        or None if CRU data is not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (month, lat, or lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (1 <= month <= 12):
        raise ValueError(f"Month must be between 1 and 12, got {month}")
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Only catch availability errors, not validation errors
    try:
        return _get_mean_monthly_air_temperature(lat, lon, month, spatial_res)
    except (FileNotFoundError, IOError, OSError, ImportError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature validation skipped."
        )
        return None
    except ValueError as e:
        # Only catch data availability errors from CRU
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature validation skipped."
            )
            return None
        else:
            # Re-raise validation errors
            raise

def get_monthly_air_temperature_diffmax(
    lat: float, lon: float, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate the maximum difference between mean monthly air temperatures
    using CRU TS4.06 climatological data. This is defined as the difference 
    between the warmest and coldest mean monthly temperature within the annual 
    cycle at the specified location.

    Parameters
    ----------
    lat : float
        Site latitude in degrees (-90 to 90).
    lon : float
        Site longitude in degrees (-180 to 180).
    spatial_res : float, optional
        Search spatial resolution for nearest CRU grid cell, by default 0.5.

    Returns
    -------
    float or None
        Maximum difference in mean monthly air temperature (degC),
        or None if CRU data not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (lat, lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    try:
        return _get_monthly_air_temperature_diffmax(lat, lon, spatial_res)
    except (FileNotFoundError, IOError, OSError, ImportError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature diffmax validation skipped."
        )
        return None
    except ValueError as e:
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature diffmax validation skipped."
            )
            return None
        else:
            raise

def get_mean_annual_air_temperature(
    lat: float, lon: float, spatial_res: float = 0.5
) -> Optional[float]:
    """
    Calculate mean annual air temperature using CRU TS4.06 climate normals.

    This function wraps `yaml_helpers.get_mean_annual_air_temperature` and returns
    None if CRU data is not available (e.g., in standalone mode).

    Parameters
    ----------
    lat : float
        Latitude in degrees (-90 to 90).
    lon : float
        Longitude in degrees (-180 to 180).
    spatial_res : float, optional
        Spatial resolution in degrees for finding the nearest CRU grid cell (default: 0.5).

    Returns
    -------
    float or None
        Mean annual air temperature in Celsius for the specified location,
        or None if CRU data is not available.

    Raises
    ------
    ValueError
        If input parameters are invalid (lat, lon out of range).
    """
    # Validate parameters first - these errors should propagate
    if not (-90 <= lat <= 90):
        raise ValueError(f"Latitude must be between -90 and 90, got {lat}")
    if not (-180 <= lon <= 180):
        raise ValueError(f"Longitude must be between -180 and 180, got {lon}")

    # Only catch availability errors, not validation errors
    try:
        return _get_mean_annual_air_temperature(lat, lon, spatial_res)
    except (FileNotFoundError, IOError, OSError, ImportError) as e:
        logger_supy.warning(
            f"CRU data file not available: {e}. Temperature validation skipped."
        )
        return None
    except ValueError as e:
        # Only catch data availability errors from CRU
        if "CRU" in str(e) or "not found" in str(e).lower():
            logger_supy.warning(
                f"CRU data not available: {e}. Temperature validation skipped."
            )
            return None
        else:
            # Re-raise validation errors
            raise


def _to_scientific_adjustments(
    planned: List[PlannedAdjustment],
) -> List[ScientificAdjustment]:
    return [
        ScientificAdjustment(
            parameter=item.parameter,
            site_index=item.site_index,
            site_gridid=item.site_gridid,
            old_value=item.old_value,
            new_value=item.new_value,
            reason=item.reason,
        )
        for item in planned
    ]


def update_temperature_parameters(
    element: dict, avg_temp: float
) -> Tuple[dict, List[str]]:
    """Update temperature-related parameters ('temperature', 'tsfc', 'tin') in a dict element."""
    updated_params = []

    if "temperature" in element:
        current_temp = get_ref_or_bare(element, "temperature")
        if current_temp != [avg_temp] * 5:
            set_ref_or_bare(element, "temperature", [avg_temp] * 5)
            updated_params.append("temperature")

    if "tsfc" in element:
        current_tsfc = get_ref_or_bare(element, "tsfc")
        if current_tsfc != avg_temp:
            set_ref_or_bare(element, "tsfc", avg_temp)
            updated_params.append("tsfc")

    if "tin" in element:
        current_tin = get_ref_or_bare(element, "tin")
        if current_tin != avg_temp:
            set_ref_or_bare(element, "tin", avg_temp)
            updated_params.append("tin")

    return element, updated_params


def adjust_surface_temperatures(
    yaml_data: dict, start_date: str
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Set initial surface temperatures based on location and season (delegates to ``patches``)."""
    plan = plan_surface_temperature_patches(
        yaml_data,
        start_date,
        get_monthly_temp=get_mean_monthly_air_temperature,
        get_diffmax_temp=get_monthly_air_temperature_diffmax,
        get_annual_temp=get_mean_annual_air_temperature,
    )
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_land_cover_fractions(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Auto-fix small floating-point errors in surface fractions (delegates to ``patches``)."""
    plan = plan_land_cover_fraction_patches(yaml_data)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_model_dependent_nullification(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Nullify parameters for disabled model options (delegates to ``patches``)."""
    plan = plan_model_dependent_nullification_patches(yaml_data)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_seasonal_parameters(
    yaml_data: dict, start_date: str, model_year: int
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply seasonal LAI, snowalb, vegetation albedo, DLS, and timezone patches."""
    plan = plan_seasonal_patches(yaml_data, start_date, model_year, dls_cls=DLSCheck)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_model_option_rcmethod(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Adjust outer heat-capacity fractions when ``capacitance == 0`` (delegates to ``patches``)."""
    plan = plan_rcmethod_patches(yaml_data)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_model_option_setpointmethod(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Nullify unused setpoint scalars or profiles based on ``stebbs.setpoint``."""
    plan = plan_setpointmethod_patches(yaml_data, skip_when_stebbs_off=False)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


def adjust_model_option_stebbsmethod(
    yaml_data: dict,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Nullify WWR-dependent window or external wall parameters when STEBBS is active."""
    plan = plan_stebbsmethod_wwr_patches(yaml_data)
    plan.apply_to_dict(yaml_data, in_place=True)
    return yaml_data, _to_scientific_adjustments(plan.adjustments)


_DEFAULT_ADJUST_FUNCS = {
    "adjust_surface_temperatures": adjust_surface_temperatures,
    "adjust_land_cover_fractions": adjust_land_cover_fractions,
    "adjust_model_dependent_nullification": adjust_model_dependent_nullification,
    "adjust_seasonal_parameters": adjust_seasonal_parameters,
    "adjust_model_option_rcmethod": adjust_model_option_rcmethod,
    "adjust_model_option_setpointmethod": adjust_model_option_setpointmethod,
    "adjust_model_option_stebbsmethod": adjust_model_option_stebbsmethod,
}


def _plan_all_scientific_patches(
    yaml_data: dict,
    start_date: str,
    model_year: int,
    *,
    errors: Optional[List[ValidationResult]] = None,
    apply_changes: bool = False,
) -> Tuple[Optional[dict], List[ScientificAdjustment]]:
    """Run all seven Phase B planners cleanly with optional single-pass application.

    Uses pure read-only planners (zero ``deepcopy`` in suggest mode, single copy
    in apply mode) unless a test has monkeypatched one of the ``adjust_*``
    symbols on this module, in which case that override runs on an isolated copy.
    """
    steps = [
        (
            "adjust_surface_temperatures",
            lambda data: plan_surface_temperature_patches(
                data,
                start_date,
                get_monthly_temp=get_mean_monthly_air_temperature,
                get_diffmax_temp=get_monthly_air_temperature_diffmax,
                get_annual_temp=get_mean_annual_air_temperature,
            ),
            (start_date,),
        ),
        (
            "adjust_land_cover_fractions",
            plan_land_cover_fraction_patches,
            (),
        ),
        (
            "adjust_model_dependent_nullification",
            plan_model_dependent_nullification_patches,
            (),
        ),
        (
            "adjust_seasonal_parameters",
            lambda data: plan_seasonal_patches(
                data, start_date, model_year, dls_cls=DLSCheck
            ),
            (start_date, model_year),
        ),
        (
            "adjust_model_option_rcmethod",
            plan_rcmethod_patches,
            (),
        ),
        (
            "adjust_model_option_setpointmethod",
            lambda data: plan_setpointmethod_patches(
                data, skip_when_stebbs_off=True
            ),
            (),
        ),
        (
            "adjust_model_option_stebbsmethod",
            plan_stebbsmethod_wwr_patches,
            (),
        ),
    ]

    # Fast pure path when no adjust_* helper was monkeypatched.
    any_override = any(
        globals()[name] is not _DEFAULT_ADJUST_FUNCS[name] for name, _, _ in steps
    )

    if not any_override:
        combined = PatchPlan()
        for step_name, planner, _extra_args in steps:
            if errors is None:
                combined.extend(planner(yaml_data))
                continue
            try:
                combined.extend(planner(yaml_data))
            except Exception as exc:
                logger_supy.debug(
                    "Phase B adjustment %s failed", step_name, exc_info=True
                )
                errors.append(
                    rule_crash_result(f"adjustment '{step_name}'", step_name, exc)
                )
        adjustments = _to_scientific_adjustments(combined.adjustments)
        if not apply_changes:
            return None, adjustments
        return combined.apply_to_dict(yaml_data, in_place=False), adjustments

    # Fallback path preserving test monkeypatches on phase_b.adjust_* with rollback.
    updated_data = deepcopy(yaml_data)
    adjustments: List[ScientificAdjustment] = []
    for step_name, _planner, extra_args in steps:
        adjust_func = globals()[step_name]
        if errors is None:
            updated_data, adj = adjust_func(updated_data, *extra_args)
            adjustments.extend(adj)
            continue
        snapshot = deepcopy(updated_data)
        try:
            updated_data, adj = adjust_func(updated_data, *extra_args)
        except Exception as exc:
            logger_supy.debug(
                "Phase B adjustment %s failed", step_name, exc_info=True
            )
            updated_data = snapshot
            errors.append(
                rule_crash_result(
                    f"adjustment '{adjust_func.__name__}'", adjust_func.__name__, exc
                )
            )
            continue
        adjustments.extend(adj)
    return (updated_data if apply_changes else None), adjustments


def run_scientific_adjustment_pipeline(
    yaml_data: dict,
    start_date: str,
    model_year: int,
    errors: Optional[List[ValidationResult]] = None,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply automatic scientific corrections in a single patch pass."""
    updated_data, adjustments = _plan_all_scientific_patches(
        yaml_data,
        start_date,
        model_year,
        errors=errors,
        apply_changes=True,
    )
    assert updated_data is not None
    return updated_data, adjustments


def collect_science_suggestions(
    yaml_data: dict,
    start_date: str,
    model_year: int,
    errors: Optional[List[ValidationResult]] = None,
) -> List[ScienceSuggestion]:
    """Collect scientific initialisation suggestions without copying or mutating *yaml_data*."""
    _, adjustments = _plan_all_scientific_patches(
        yaml_data,
        start_date,
        model_year,
        errors=errors,
        apply_changes=False,
    )
    return [_adjustment_to_suggestion(adjustment) for adjustment in adjustments]


def apply_science_suggestions(
    yaml_data: dict,
    suggestions: List[ScienceSuggestion],
    start_date: str,
    model_year: int,
    errors: Optional[List[ValidationResult]] = None,
) -> Tuple[dict, List[ScientificAdjustment]]:
    """Apply the scientific transformation pipeline explicitly requested by the user."""
    del suggestions
    return run_scientific_adjustment_pipeline(
        yaml_data, start_date, model_year, errors=errors
    )


def create_science_report(
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    suggestions: Optional[List[ScienceSuggestion]] = None,
    science_yaml_filename: str = None,
    phase_a_report_file: str = None,
    mode: str = "public",
    phase: str = "B",
    science_fixes: str = "suggest",
) -> str:
    """
    Generate a comprehensive scientific validation report.

    Parameters
    ----------
    validation_results : List[ValidationResult]
        List of validation results from scientific checks.
    adjustments : List[ScientificAdjustment]
        List of automatic scientific adjustments applied.
    science_yaml_filename : str, optional
        Name of the science-checked YAML file (for display).
    phase_a_report_file : str, optional
        Path to the Phase A report file (for extracting previous changes).
    mode : str, default="public"
        Output mode for the report ("public" or other).
    phase : str, default="B"
        Validation phase identifier.

    Returns
    -------
    str
        The formatted scientific validation report as a string.

    Notes
    -----
    - Summarizes errors, warnings, adjustments, and informational notes.
    - Integrates Phase A report highlights if available.
    - Designed for both human readability and traceability.
    """
    suggestions = suggestions or []
    science_fixes = _normalise_science_fixes(science_fixes)
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
            phase_a_content = REPORT_WRITER.read(phase_a_report_file)

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

    errors = [
        r for r in validation_results if getattr(r, "severity", r.status) == "ERROR"
    ]
    warnings = [
        r for r in validation_results if getattr(r, "severity", r.status) == "WARNING"
    ]
    infos = [
        r for r in validation_results if getattr(r, "severity", r.status) == "INFO"
    ]

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

    if warnings:
        report_lines.append("## REVIEW ADVISED")
        report_lines.append(f"- Review ({len(warnings)}) scientific warning(s):")
        for warning in warnings:
            site_ref = (
                f" at site [{warning.site_gridid}]"
                if warning.site_gridid is not None
                else ""
            )
            report_lines.append(
                f"-- {warning.parameter}{site_ref}: {warning.message}"
            )
        report_lines.append("")

    if suggestions:
        report_lines.append("## SUGGESTED UPDATES")
        report_lines.append(
            f"- Suggested ({len(suggestions)}) scientific initialisation update(s)."
        )
        report_lines.append(
            "- These suggestions were not written to the updated YAML. They may be inappropriate "
            "for observed initial states, spin-up workflows, historical timezone "
            "settings, or specialist case studies."
        )
        if science_fixes == "suggest":
            report_lines.append(
                "- Re-run with --science-fixes apply to apply them to the updated YAML, or "
                "--science-fixes off to suppress scientific transformation suggestions."
            )
        for suggestion in suggestions:
            site_ref = (
                f" at site [{suggestion.site_gridid}]"
                if suggestion.site_gridid is not None
                else ""
            )
            report_lines.append(
                f"-- {suggestion.parameter}{site_ref}: {suggestion.old_value} -> "
                f"{suggestion.suggested_value} ({suggestion.message} Source: "
                f"{suggestion.source})"
            )
        report_lines.append("")

    applied_lines = []
    if adjustments:
        total_params_changed = 0
        for adjustment in adjustments:
            old_value_text = str(adjustment.old_value)
            if "temperature, tsfc, tin" in old_value_text:
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

        applied_lines.append(
            f"- Applied ({total_params_changed}) scientific update(s):"
        )
        for adjustment in adjustments:
            site_ref = (
                f" at site [{adjustment.site_gridid}]"
                if adjustment.site_gridid is not None
                else ""
            )
            applied_lines.append(
                f"-- {adjustment.parameter}{site_ref}: {adjustment.old_value} -> {adjustment.new_value} ({adjustment.reason})"
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
        applied_lines.extend(phase_a_items)

    if applied_lines:
        report_lines.append("## APPLIED UPDATES")
        report_lines.extend(applied_lines)
        report_lines.append("")

    if infos:
        report_lines.append("## INFO")
        report_lines.append(f"- Note ({len(infos)}):")
        for info in infos:
            site_ref = (
                f" at site [{info.site_gridid}]"
                if info.site_gridid is not None
                else ""
            )
            report_lines.append(f"-- {info.parameter}{site_ref}: {info.message}")
        report_lines.append("")

    if not errors and not warnings and not suggestions and not applied_lines and not infos:
        report_lines.append("## INFO")
        report_lines.append("- All scientific validation checks passed.")
        if science_fixes == "off":
            report_lines.append("- Scientific transformation suggestions were disabled.")
        report_lines.append("- Model physics parameters are consistent.")
        report_lines.append("- Geographic parameters are valid.")

    report_lines.append("")

    report_lines.append("# " + "=" * 50)

    return "\n".join(report_lines)


def print_critical_halt_message(critical_errors: List[ValidationResult]):
    """
    Print a critical halt message when Phase B detects errors that require Phase A.

    Parameters
    ----------
    critical_errors : List[ValidationResult]
        List of ERROR-level validation results that triggered the halt.

    Notes
    -----
    This function prints a clear message to the terminal, summarizing all critical
    scientific errors found during Phase B validation. It also provides guidance
    on how to resolve the issues, including running Phase A for automatic fixes.
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
        print(f"  [X] {error.parameter}{site_ref}")
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
    validation_results: List[ValidationResult],
    adjustments: List[ScientificAdjustment],
    suggestions: Optional[List[ScienceSuggestion]] = None,
    science_fixes: str = "suggest",
):
    """
    Print concise terminal output for Phase B scientific validation results.

    Parameters
    ----------
    validation_results : list of ValidationResult
        List of validation results (errors, warnings, infos, passes).
    adjustments : list of ScientificAdjustment
        List of automatic scientific adjustments applied.

    Notes
    -----
    - Errors are printed with details and guidance.
    - Warnings and info messages are summarized.
    - Adjustment count is shown if any were applied.
    - Output is formatted for clarity in terminal use.
    """
    errors = [r for r in validation_results if r.status == "ERROR"]
    warnings = [r for r in validation_results if r.status == "WARNING"]
    infos = [r for r in validation_results if r.status == "INFO"]
    suggestions = suggestions or []

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
        if suggestions and science_fixes == "suggest":
            print(f"Suggested {len(suggestions)} scientific initialisation updates")
        print("Check science_report_user.txt for details")
    else:
        print("PHASE B -- PASSED")
        if adjustments:
            print(f"Applied {len(adjustments)} automatic scientific adjustments")
        if suggestions and science_fixes == "suggest":
            print(f"Suggested {len(suggestions)} scientific initialisation updates")
        elif science_fixes == "off":
            print("Scientific transformation suggestions were disabled")

    if infos:
        for info in infos:
            site_ref = (
                f" at site [{info.site_gridid}]"
                if info.site_gridid is not None
                else ""
            )
            print(f"  Note: {info.parameter}{site_ref}: {info.message}")


def create_science_yaml_header(phase_a_performed: bool = True) -> str:
    """
    Create header for the final science-checked YAML file.

    Parameters
    ----------
    phase_a_performed : bool, optional
        Whether Phase A was performed before Phase B (default: True).

    Returns
    -------
    str
        Standardized header string for the science-checked YAML file.

    Notes
    -----
    - The header indicates that the file has been updated by the SUEWS processor.
    - It references the user-provided YAML and the existence of a detailed report.
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
    standard_yaml_file: Optional[str] = None,
    science_yaml_file: str = None,
    science_report_file: str = None,
    phase_a_report_file: str = None,
    phase_a_performed: bool = True,
    mode: str = "public",
    phase: str = "B",
    science_fixes: str = "suggest",
) -> Tuple[dict, PhaseReport]:
    """
    Main Phase B workflow: perform scientific validation and scientific adjustments.

    Parameters
    ----------
    uptodate_yaml_file : str
        Path to the Phase A output YAML file (cleaned and updated user YAML).
    user_yaml_file : str
        Path to the original user-provided YAML file.
    standard_yaml_file : str
        Path to the standard reference YAML file.
    science_yaml_file : str, optional
        Path to write the science-checked output YAML file.
    science_report_file : str, optional
        Path to write the scientific validation report.
    phase_a_report_file : str, optional
        Path to the Phase A report file, if available.
    phase_a_performed : bool, default=True
        Whether Phase A was performed before Phase B.
    mode : str, default="public"
        Output mode for the report ("public" or other).
    phase : str, default="B"
        Validation phase identifier.

    Returns
    -------
    tuple of (dict, PhaseReport)
        Final science-checked YAML configuration dictionary and the
        structured ``PhaseReport`` with all issues found. The
        ``PhaseReport`` carries a ``status`` of ``FAILED`` when
        critical errors are present (no exception is raised).

    Raises
    ------
    FileNotFoundError, ValueError, KeyError
        On unrecoverable initialisation failures (e.g. missing or
        invalid input files). The orchestrator wraps these and
        synthesises a fallback ``PhaseReport``.

    Notes
    -----
    - Runs all scientific validation and adjustment steps for Phase B.
    - Writes a detailed text report and optionally the updated YAML file.
    - Critical scientific errors are signalled via ``phase_report.has_errors``;
      the function no longer raises ``ValueError`` for that case.
    """
    science_fixes = _normalise_science_fixes(science_fixes)
    try:
        uptodate_data = _load_phase_b_input(uptodate_yaml_file)

        model_year, start_date, end_date = extract_simulation_parameters(uptodate_data)

        validation_context = ValidationContext(
            yaml_data=uptodate_data,
            start_date=start_date,
            model_year=model_year,
        )

        validation_results = RulesRegistry(
            context=validation_context
        ).run_validation()

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
            validation_results=validation_results,
            adjustments=[],  # No adjustments since we failed early
            suggestions=[],
            science_yaml_filename=science_yaml_filename,
            phase_a_report_file=phase_a_report_file,
            mode=mode,
            phase=phase,
            science_fixes=science_fixes,
        )

        # Write error report file
        if science_report_file:
            REPORT_WRITER.write(science_report_file, report_content)

        from .report_schema import PhaseReport

        issues = [r.to_issue() for r in validation_results]
        phase_report = PhaseReport(
            phase="B",
            issues=issues,
            yaml_in=uptodate_yaml_file,
            yaml_out=science_yaml_file,
            text_report_path=science_report_file,
        )
        return {}, phase_report

    critical_errors = [r for r in validation_results if r.status == "ERROR"]
    suggestions = []
    adjustments = []
    # A crashing adjustment step is reported once (the apply path re-runs the
    # same steps, so only its own run collects errors).
    adjustment_errors: List[ValidationResult] = []
    if not critical_errors:
        if science_fixes == "apply":
            science_checked_data, adjustments = run_scientific_adjustment_pipeline(
                uptodate_data,
                start_date,
                model_year,
                errors=adjustment_errors,
            )
        elif science_fixes == "suggest":
            suggestions = collect_science_suggestions(
                uptodate_data, start_date, model_year, errors=adjustment_errors
            )
            science_checked_data = deepcopy(uptodate_data)
        else:
            science_checked_data = deepcopy(uptodate_data)
    else:
        science_checked_data = deepcopy(uptodate_data)
    if adjustment_errors:
        validation_results = [*validation_results, *adjustment_errors]
        critical_errors = [*critical_errors, *adjustment_errors]
        # Nothing is written once an error is present, so do not report the
        # steps that did run as applied.
        adjustments = []

    science_yaml_filename = (
        os.path.basename(science_yaml_file) if science_yaml_file else None
    )
    report_content = create_science_report(
        validation_results=validation_results,
        adjustments=adjustments,
        suggestions=suggestions if science_fixes == "suggest" else [],
        science_yaml_filename=science_yaml_filename,
        phase_a_report_file=phase_a_report_file,
        mode=mode,
        phase=phase,
        science_fixes=science_fixes,
    )

    if science_report_file:
        REPORT_WRITER.write(science_report_file, report_content)

    from .report_schema import PhaseReport

    issues = [r.to_issue() for r in validation_results]
    issues.extend(_adjustment_to_issue(a) for a in adjustments)
    if science_fixes == "suggest":
        issues.extend(s.to_issue() for s in suggestions)

    phase_report = PhaseReport(
        phase="B",
        issues=issues,
        yaml_in=uptodate_yaml_file,
        yaml_out=science_yaml_file,
        text_report_path=science_report_file,
    )

    if critical_errors:
        print_critical_halt_message(critical_errors)
        return science_checked_data, phase_report

    print_science_check_results(
        validation_results, adjustments, suggestions, science_fixes
    )

    if science_yaml_file and not critical_errors:
        header = create_science_yaml_header(phase_a_performed)
        with open(science_yaml_file, "w", encoding="utf-8", newline="\n") as f:
            f.write(header)
            yaml.dump(
                science_checked_data, f, default_flow_style=False, sort_keys=False
            )

    return science_checked_data, phase_report


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
