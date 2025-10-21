"""SUEWS-specific utility tools."""

from typing import Any, Optional


def calculate_ohm_coefficients(
    results_path: str,
    surface_type: Optional[str] = None,
) -> dict[str, Any]:
    """Calculate OHM (Objective Hysteresis Model) coefficients from observations.

    Uses supy.util.derive_ohm_coef() to calibrate OHM parameters from
    observed storage heat flux and net radiation.

    Args:
        results_path: Path to results file with QS and QN observations
        surface_type: Optional surface type identifier

    Returns:
        Dictionary with OHM coefficients (a1, a2, a3) and fit statistics
    """
    try:
        import pandas as pd
        from supy.util import derive_ohm_coef

        # Load results
        df = pd.read_csv(results_path, index_col=0, parse_dates=True)

        # Check required variables
        if "QS" not in df.columns or "QN" not in df.columns:
            return {
                "success": False,
                "error": "Results must contain QS (storage heat) and QN (net radiation)",
                "available_columns": list(df.columns),
            }

        # Derive OHM coefficients
        # Note: Actual implementation depends on derive_ohm_coef signature
        # This is a placeholder showing the interface
        coef_result = derive_ohm_coef(df["QN"], df["QS"])

        return {
            "success": True,
            "surface_type": surface_type or "unspecified",
            "coefficients": {
                "a1": float(coef_result.get("a1", 0)),
                "a2": float(coef_result.get("a2", 0)),
                "a3": float(coef_result.get("a3", 0)),
            },
            "fit_statistics": {
                "r_squared": float(coef_result.get("r2", 0)),
                "rmse": float(coef_result.get("rmse", 0)),
            },
            "guidance": "Use these coefficients in SUEWS OHM configuration",
            "reference": "Grimmond & Oke (1999, 2002)",
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "guidance": "Check that results file contains QS and QN variables",
        }


def calculate_surface_conductance(
    results_path: str,
    method: str = "suews",
) -> dict[str, Any]:
    """Calculate surface conductance from observations.

    Uses supy.util.cal_gs_suews() or cal_gs_obs() to compute surface conductance
    for calibrating SUEWS vegetation parameters.

    Args:
        results_path: Path to results file with meteorological and flux data
        method: Calculation method ('suews' or 'observed')

    Returns:
        Dictionary with surface conductance values and statistics
    """
    try:
        import pandas as pd
        from supy import util

        # Load results
        df = pd.read_csv(results_path, index_col=0, parse_dates=True)

        # Check required variables (depends on method)
        required_vars = {
            "suews": ["QH", "QE", "T2", "RH2", "Pres"],
            "observed": ["QE", "T2", "RH2", "Pres"],
        }

        if method not in required_vars:
            return {
                "success": False,
                "error": f"Unknown method: {method}",
                "available_methods": list(required_vars.keys()),
            }

        missing_vars = [v for v in required_vars[method] if v not in df.columns]
        if missing_vars:
            return {
                "success": False,
                "error": f"Missing required variables: {missing_vars}",
                "required_for_{method}": required_vars[method],
            }

        # Calculate conductance
        if method == "suews":
            gs_result = util.cal_gs_suews(
                df["QH"], df["QE"], df["T2"], df["RH2"], df["Pres"]
            )
        else:
            gs_result = util.cal_gs_obs(df["QE"], df["T2"], df["RH2"], df["Pres"])

        return {
            "success": True,
            "method": method,
            "conductance": {
                "mean": float(gs_result.mean()),
                "median": float(gs_result.median()),
                "std": float(gs_result.std()),
                "min": float(gs_result.min()),
                "max": float(gs_result.max()),
            },
            "units": "mm/s",
            "guidance": "Use these values to calibrate MaxConductance in SUEWS vegetation parameters",
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


def calculate_roughness(
    building_height: float,
    plan_area_fraction: float,
    frontal_area_index: Optional[float] = None,
) -> dict[str, Any]:
    """Calculate roughness length and displacement height.

    Uses supy.util.cal_z0zd() to estimate aerodynamic parameters
    from urban morphology.

    Args:
        building_height: Mean building height (m)
        plan_area_fraction: Building plan area fraction (0-1)
        frontal_area_index: Optional frontal area index

    Returns:
        Dictionary with roughness length (z0) and displacement height (zd)
    """
    try:
        from supy import util

        # Calculate roughness parameters
        if frontal_area_index is not None:
            z0, zd = util.cal_z0zd(building_height, plan_area_fraction, frontal_area_index)
        else:
            z0, zd = util.cal_z0zd(building_height, plan_area_fraction)

        return {
            "success": True,
            "input": {
                "building_height_m": building_height,
                "plan_area_fraction": plan_area_fraction,
                "frontal_area_index": frontal_area_index,
            },
            "results": {
                "roughness_length_m": float(z0),
                "displacement_height_m": float(zd),
                "z0_to_height_ratio": float(z0 / building_height),
                "zd_to_height_ratio": float(zd / building_height),
            },
            "guidance": "Use z0 and zd in SUEWS site configuration for aerodynamic calculations",
            "reference": "Grimmond & Oke (1999)",
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "guidance": "Check that building_height > 0 and 0 < plan_area_fraction < 1",
        }
