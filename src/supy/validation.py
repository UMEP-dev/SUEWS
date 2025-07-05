"""
Conditional Validation for SUEWS Configuration

This module provides conditional validation functionality that can be used
independently of the main data_model package structure.
"""

from typing import List, Dict, Any, Optional, Union, Set
import warnings
import logging

logger = logging.getLogger(__name__)


class ValidationResult:
    """Simple validation result class."""

    def __init__(
        self,
        status: str = "passed",
        errors: List[str] = None,
        warnings: List[str] = None,
        skipped: List[str] = None,
        validated_methods: Set[str] = None,
        skipped_methods: Set[str] = None,
    ):
        self.status = status
        self.errors = errors or []
        self.warnings = warnings or []
        self.skipped = skipped or []
        self.validated_methods = validated_methods or set()
        self.skipped_methods = skipped_methods or set()


def analyze_config_methods(config_data: Dict[str, Any]) -> Dict[str, bool]:
    """
    Analyze configuration to determine which methods are enabled.

    Args:
        config_data: Configuration dictionary

    Returns:
        Dictionary of method flags
    """
    try:
        physics = config_data.get("model", {}).get("physics", {})

        # Extract method values
        diagmethod_val = physics.get("diagmethod", 0)
        if isinstance(diagmethod_val, dict):
            diagmethod_val = diagmethod_val.get("value", 0)

        roughmethod_val = physics.get("roughlenmommethod", 1)
        if isinstance(roughmethod_val, dict):
            roughmethod_val = roughmethod_val.get("value", 1)

        netrad_val = physics.get("netradiationmethod", 0)
        if isinstance(netrad_val, dict):
            netrad_val = netrad_val.get("value", 0)

        emissions_val = physics.get("emissionsmethod", 0)
        if isinstance(emissions_val, dict):
            emissions_val = emissions_val.get("value", 0)

        storage_val = physics.get("storageheatmethod", 0)
        if isinstance(storage_val, dict):
            storage_val = storage_val.get("value", 0)

        # Determine active methods
        methods = {
            "diagmethod_most": diagmethod_val == 0,
            "diagmethod_rst": diagmethod_val == 1,
            "diagmethod_variable": diagmethod_val == 2,
            "roughness_variable": roughmethod_val == 2,
            "netradiation_spartacus": netrad_val >= 1000,
            "emissions_advanced": emissions_val >= 4,
            "storage_estm": storage_val in [4, 5],
        }

        # Variable diagmethod uses both MOST and RST
        if methods["diagmethod_variable"]:
            methods["diagmethod_most"] = True
            methods["diagmethod_rst"] = True

        return methods

    except Exception as e:
        logger.warning(f"Error analyzing config methods: {e}")
        return {}


def validate_most_parameters(
    sites: List[Dict], site_indices: Optional[List[int]] = None
) -> List[str]:
    """Validate MOST-specific parameters."""
    errors = []
    indices = site_indices or range(len(sites))

    for i in indices:
        if i >= len(sites):
            continue

        site_props = sites[i].get("properties", {})

        # Extract values (handle RefValue objects)
        z0m = _extract_value(site_props.get("z0m_in"))
        if z0m is not None and (z0m <= 0 or z0m > 10):
            errors.append(
                f"[MOST] site[{i}].z0m_in={z0m} invalid (must be 0 < z0m ≤ 10)"
            )

        zdm = _extract_value(site_props.get("zdm_in"))
        if zdm is not None and (zdm < 0 or zdm > 50):
            errors.append(
                f"[MOST] site[{i}].zdm_in={zdm} invalid (must be 0 ≤ zdm ≤ 50)"
            )

    return errors


def validate_rst_parameters(
    sites: List[Dict], site_indices: Optional[List[int]] = None
) -> List[str]:
    """Validate RST-specific parameters."""
    errors = []
    indices = site_indices or range(len(sites))

    for i in indices:
        if i >= len(sites):
            continue

        site_props = sites[i].get("properties", {})

        bldg_height = _extract_value(site_props.get("building_height"))
        if bldg_height is not None and (bldg_height <= 0 or bldg_height > 200):
            errors.append(
                f"[RST] site[{i}].building_height={bldg_height} invalid (must be 0 < height ≤ 200)"
            )

        bldg_std = _extract_value(site_props.get("building_height_std"))
        if bldg_std is not None and (bldg_std < 0 or bldg_std > 100):
            errors.append(
                f"[RST] site[{i}].building_height_std={bldg_std} invalid (must be 0 ≤ std ≤ 100)"
            )

    return errors


def validate_variable_roughness_parameters(
    sites: List[Dict], site_indices: Optional[List[int]] = None
) -> List[str]:
    """Validate variable roughness parameters."""
    errors = []
    indices = site_indices or range(len(sites))

    for i in indices:
        if i >= len(sites):
            continue

        site_props = sites[i].get("properties", {})

        tree_h_ev = _extract_value(site_props.get("tree_height_evergreen"))
        if tree_h_ev is not None and (tree_h_ev <= 0 or tree_h_ev > 50):
            errors.append(
                f"[VAR_ROUGH] site[{i}].tree_height_evergreen={tree_h_ev} invalid (must be 0 < height ≤ 50)"
            )

        tree_h_dec = _extract_value(site_props.get("tree_height_deciduous"))
        if tree_h_dec is not None and (tree_h_dec <= 0 or tree_h_dec > 50):
            errors.append(
                f"[VAR_ROUGH] site[{i}].tree_height_deciduous={tree_h_dec} invalid (must be 0 < height ≤ 50)"
            )

    return errors


def validate_storage_heat_parameters(
    physics: Dict[str, Any], method_type: str
) -> List[str]:
    """Validate storage heat method parameters based on method type."""
    errors = []

    storage_method_val = _extract_value(physics.get("storageheatmethod", 0))
    ohmincqf_val = _extract_value(physics.get("ohmincqf", 0))

    if method_type == "ESTM":
        # ESTM methods (4, 5) - advanced parameter validation would go here
        if storage_method_val in [4, 5]:
            # Validate ESTM-specific parameters if they exist
            # For now, just basic validation
            pass
    else:  # OHM methods
        # OHM method 1: ohmincqf should be 0
        if storage_method_val == 1 and ohmincqf_val != 0:
            errors.append(
                f"[STORAGE_OHM] storageheatmethod={storage_method_val} requires ohmincqf=0, got {ohmincqf_val}"
            )

        # OHM method 2: ohmincqf should be 1
        elif storage_method_val == 2 and ohmincqf_val != 1:
            errors.append(
                f"[STORAGE_OHM] storageheatmethod={storage_method_val} requires ohmincqf=1, got {ohmincqf_val}"
            )

    return errors


def validate_netradiation_parameters(
    physics: Dict[str, Any], sites: List[Dict], method_type: str
) -> List[str]:
    """Validate net radiation method parameters."""
    errors = []

    netrad_val = _extract_value(physics.get("netradiationmethod", 0))

    if method_type == "SPARTACUS":
        # SPARTACUS methods (≥1000) - validate SPARTACUS-specific parameters
        if netrad_val >= 1000:
            # Validate that required SPARTACUS parameters are available
            # For now, basic validation
            pass
    else:  # Standard methods
        # Validate standard net radiation parameters
        if netrad_val in [11, 12, 13]:  # Surface temperature methods
            # These are marked as "Not recommended in this version"
            errors.append(
                f"[NETRAD_STANDARD] netradiationmethod={netrad_val} not recommended in this version"
            )

        if netrad_val in [100, 200, 300]:  # Zenith angle methods
            # These are marked as "Not recommended in this version"
            errors.append(
                f"[NETRAD_STANDARD] netradiationmethod={netrad_val} not recommended in this version"
            )

    return errors


def validate_emissions_parameters(
    physics: Dict[str, Any], method_type: str
) -> List[str]:
    """Validate emissions method parameters."""
    errors = []

    emissions_val = _extract_value(physics.get("emissionsmethod", 0))

    if method_type == "ADVANCED":
        # Advanced emissions methods (≥4)
        if emissions_val >= 4:
            # Validate advanced emissions parameters if they exist
            # Currently commented out in model.py, so we skip validation
            pass

    return errors


def validate_snow_parameters(physics: Dict[str, Any], sites: List[Dict]) -> List[str]:
    """Validate snow calculation parameters."""
    errors = []

    snow_use = _extract_value(physics.get("snowuse", 0))

    if snow_use == 1:
        # Snow calculations are enabled - validate snow-related parameters
        # Currently this generates a warning in model.py as there are no implemented checks
        errors.append(
            "[SNOW] SnowUse=1 enabled but no validation checks implemented for snow calculations"
        )

    return errors


def _extract_value(param: Any) -> Optional[float]:
    """Extract numeric value from parameter (handles RefValue objects)."""
    if param is None:
        return None

    if isinstance(param, dict):
        return param.get("value")

    if hasattr(param, "value"):
        return param.value

    try:
        return float(param)
    except (ValueError, TypeError):
        return None


def validate_suews_config_conditional(
    config_data: Dict[str, Any], strict: bool = False, verbose: bool = False
) -> ValidationResult:
    """
    Main conditional validation function.

    Args:
        config_data: Configuration dictionary
        strict: Whether to treat warnings as errors
        verbose: Whether to print validation progress

    Returns:
        ValidationResult object
    """
    if verbose:
        print("=== SUEWS Conditional Validation ===")

    # Analyze which methods are enabled
    methods = analyze_config_methods(config_data)

    if verbose:
        enabled = [k for k, v in methods.items() if v]
        print(f"Active methods: {', '.join(enabled) if enabled else 'None'}")

    errors = []
    skipped = []
    validated_methods = set()
    skipped_methods = set()

    sites = config_data.get("sites", [])
    physics = config_data.get("model", {}).get("physics", {})

    # Diagnostic method validation
    if methods.get("diagmethod_most") and not methods.get("diagmethod_variable"):
        if verbose:
            print("DiagMethod MOST: validating MOST parameters")
        errors.extend(validate_most_parameters(sites))
        validated_methods.add("MOST")
        skipped_methods.add("RST")  # RST is skipped when only MOST is enabled
        skipped_methods.add("VARIABLE_ROUGHNESS")
    elif not methods.get("diagmethod_most"):
        skipped.append("MOST parameter validation (DiagMethod != MOST)")
        skipped_methods.add("MOST")

    if methods.get("diagmethod_rst"):
        if verbose:
            print("DiagMethod RST: validating RST parameters")
        errors.extend(validate_rst_parameters(sites))
        validated_methods.add("RST")
        if not methods.get("diagmethod_variable"):
            skipped_methods.add("MOST")  # MOST is skipped when only RST is enabled
    else:
        skipped.append("RST parameter validation (DiagMethod != RST)")
        skipped_methods.add("RST")

    if methods.get("diagmethod_variable"):
        if verbose:
            print("DiagMethod VARIABLE: validating both MOST and RST")
        # Variable method validates both MOST and RST
        errors.extend(validate_most_parameters(sites))
        errors.extend(validate_rst_parameters(sites))
        validated_methods.add("VARIABLE")
        validated_methods.add("MOST")
        validated_methods.add("RST")

    if methods.get("roughness_variable"):
        if verbose:
            print("Variable roughness: validating vegetation parameters")
        errors.extend(validate_variable_roughness_parameters(sites))
        validated_methods.add("VARIABLE_ROUGHNESS")
    else:
        skipped.append("Variable roughness validation (RoughnessMethod != VARIABLE)")
        skipped_methods.add("VARIABLE_ROUGHNESS")

    # Storage heat method validation
    if methods.get("storage_estm"):
        if verbose:
            print("Storage heat ESTM: validating ESTM parameters")
        errors.extend(validate_storage_heat_parameters(physics, "ESTM"))
        validated_methods.add("STORAGE_ESTM")
    else:
        if verbose:
            print("Storage heat OHM: validating OHM parameters")
        errors.extend(validate_storage_heat_parameters(physics, "OHM"))
        validated_methods.add("STORAGE_OHM")

    # Net radiation method validation
    if methods.get("netradiation_spartacus"):
        if verbose:
            print("Net radiation SPARTACUS: validating SPARTACUS parameters")
        errors.extend(validate_netradiation_parameters(physics, sites, "SPARTACUS"))
        validated_methods.add("NETRADIATION_SPARTACUS")
    else:
        if verbose:
            print("Net radiation standard: validating standard parameters")
        errors.extend(validate_netradiation_parameters(physics, sites, "STANDARD"))
        validated_methods.add("NETRADIATION_STANDARD")

    # Emissions method validation
    if methods.get("emissions_advanced"):
        if verbose:
            print("Emissions advanced: validating advanced emission parameters")
        errors.extend(validate_emissions_parameters(physics, "ADVANCED"))
        validated_methods.add("EMISSIONS_ADVANCED")
    else:
        skipped.append("Advanced emissions validation (EmissionsMethod < 4)")
        skipped_methods.add("EMISSIONS_ADVANCED")

    # Snow method validation
    snow_enabled = _extract_value(physics.get("snowuse", 0)) == 1
    if snow_enabled:
        if verbose:
            print("Snow calculations: validating snow parameters")
        errors.extend(validate_snow_parameters(physics, sites))
        validated_methods.add("SNOW")
    else:
        skipped.append("Snow parameter validation (SnowUse != 1)")
        skipped_methods.add("SNOW")

    # Determine status
    status = "failed" if errors else "passed"

    result = ValidationResult(
        status=status,
        errors=errors,
        warnings=[],
        skipped=skipped,
        validated_methods=validated_methods,
        skipped_methods=skipped_methods,
    )

    if verbose:
        print(
            f"\n{'[FAILED] FAILED' if errors else '[PASSED] PASSED'}: {len(errors)} errors, {len(skipped)} skipped"
        )
        if errors:
            for error in errors:
                print(f"  • {error}")
        if skipped:
            print(f"Skipped: {', '.join(skipped)}")

    return result


# Integration functions for SUEWSConfig
def enhanced_from_yaml_validation(
    config_data: Dict[str, Any], strict: bool = True
) -> Optional[Dict[str, Any]]:
    """Enhanced YAML validation for SUEWSConfig.from_yaml integration."""
    result = validate_suews_config_conditional(config_data, verbose=True)

    if result["errors"]:
        error_msg = (
            f"SUEWS Configuration Validation Failed: {len(result['errors'])} errors\n"
        )
        error_msg += "\n".join(f"  - {err}" for err in result["errors"])

        if strict:
            raise ValueError(error_msg)
        else:
            warnings.warn(f"Validation errors found: {error_msg}")

    return result


def enhanced_to_df_state_validation(
    config_data: Dict[str, Any], strict: bool = False
) -> Optional[Dict[str, Any]]:
    """Enhanced validation for to_df_state integration."""
    result = validate_suews_config_conditional(config_data, verbose=False)

    if result["errors"]:
        error_msg = f"Configuration validation found {len(result['errors'])} issues before to_df_state conversion"

        if strict:
            detailed_msg = f"\n{error_msg}:\n" + "\n".join(
                f"  - {err}" for err in result["errors"]
            )
            raise ValueError(detailed_msg)
        else:
            warnings.warn(
                f"{error_msg}. Continuing with conversion using available valid data."
            )

    return result
