"""Energy balance analyzer for SUEWS MCP."""

from typing import Any, Dict, Optional
import pandas as pd
import numpy as np
from pathlib import Path


def calculate_energy_balance_metrics(df: pd.DataFrame) -> Dict[str, float]:
    """Calculate energy balance closure and related metrics."""

    # Essential energy balance components
    qstar = df.get("QN", df.get("Qstar", None))  # Net all-wave radiation
    qh = df.get("QH", None)  # Sensible heat flux
    qe = df.get("QE", None)  # Latent heat flux
    qs = df.get("QS", df.get("dQS", None))  # Storage heat flux
    qf = df.get("QF", None)  # Anthropogenic heat flux

    # Check if essential columns exist
    if qstar is None or qh is None or qe is None:
        return {"error": "Missing essential energy balance components"}

    # Calculate available energy
    if qf is not None:
        available_energy = qstar + qf  # Q* + QF
    else:
        available_energy = qstar

    # Calculate turbulent fluxes
    turbulent_fluxes = qh + qe

    # Energy balance residual
    if qs is not None:
        residual = available_energy - turbulent_fluxes - qs
    else:
        residual = available_energy - turbulent_fluxes

    # Calculate metrics
    metrics = {
        "mean_qstar": float(qstar.mean()),
        "mean_qh": float(qh.mean()),
        "mean_qe": float(qe.mean()),
        "mean_available_energy": float(available_energy.mean()),
        "mean_turbulent_fluxes": float(turbulent_fluxes.mean()),
        "mean_residual": float(residual.mean()),
        "residual_rmse": float(np.sqrt((residual**2).mean())),
        "energy_balance_ratio": float((turbulent_fluxes / available_energy).mean()),
        "bowen_ratio": float((qh / qe).replace([np.inf, -np.inf], np.nan).mean()),
    }

    if qs is not None:
        metrics["mean_qs"] = float(qs.mean())
        metrics["storage_fraction"] = float((qs / available_energy).mean())

    if qf is not None:
        metrics["mean_qf"] = float(qf.mean())
        metrics["qf_fraction"] = float((qf / available_energy).mean())

    # Daytime statistics (when Q* > 50 W/m²)
    daytime = qstar > 50
    if daytime.any():
        metrics["daytime_closure"] = float(
            (turbulent_fluxes[daytime] / available_energy[daytime]).mean()
        )
        metrics["daytime_bowen_ratio"] = float(
            (qh[daytime] / qe[daytime]).replace([np.inf, -np.inf], np.nan).mean()
        )

    return metrics


def diagnose_issues(metrics: Dict[str, float], site_metadata: Optional[Dict] = None) -> list:
    """Diagnose potential issues based on energy balance metrics."""
    issues = []

    # Check energy balance closure
    if "energy_balance_ratio" in metrics:
        ebr = metrics["energy_balance_ratio"]
        if ebr < 0.7:
            issues.append(
                {
                    "severity": "high",
                    "issue": "Poor energy balance closure",
                    "details": f"Energy balance ratio = {ebr:.2f} (expected 0.7-1.0)",
                    "suggestions": [
                        "Check measurement heights and footprint",
                        "Verify storage heat calculation method",
                        "Consider advection effects in heterogeneous areas",
                    ],
                }
            )
        elif ebr > 1.2:
            issues.append(
                {
                    "severity": "medium",
                    "issue": "Energy balance overclosure",
                    "details": f"Energy balance ratio = {ebr:.2f} (expected 0.7-1.0)",
                    "suggestions": [
                        "Check QF estimation method",
                        "Verify radiation measurements",
                        "Review surface energy partitioning",
                    ],
                }
            )

    # Check Bowen ratio
    if "bowen_ratio" in metrics:
        beta = metrics["bowen_ratio"]
        if site_metadata and "land_cover" in site_metadata:
            veg_fraction = site_metadata["land_cover"].get("vegetation", 0)
            if veg_fraction > 0.3 and beta > 2.0:
                issues.append(
                    {
                        "severity": "medium",
                        "issue": "High Bowen ratio for vegetated site",
                        "details": f"β = {beta:.2f} with {veg_fraction * 100:.0f}% vegetation",
                        "suggestions": [
                            "Check soil moisture conditions",
                            "Verify vegetation parameters (LAI, stomatal conductance)",
                            "Review irrigation/water availability",
                        ],
                    }
                )

    # Check storage heat fraction
    if "storage_fraction" in metrics:
        sf = metrics["storage_fraction"]
        if site_metadata and "land_cover" in site_metadata:
            urban_fraction = site_metadata["land_cover"].get("built", 0)
            if urban_fraction > 0.5 and sf < 0.1:
                issues.append(
                    {
                        "severity": "medium",
                        "issue": "Low storage heat for urban site",
                        "details": f"ΔQS/Q* = {sf:.2f} with {urban_fraction * 100:.0f}% built",
                        "suggestions": [
                            "Review OHM coefficients",
                            "Check thermal properties of urban materials",
                            "Consider using enhanced storage heat methods",
                        ],
                    }
                )

    # Check for negative latent heat
    if metrics.get("mean_qe", 0) < -10:
        issues.append(
            {
                "severity": "high",
                "issue": "Significant negative latent heat flux",
                "details": f"Mean QE = {metrics['mean_qe']:.1f} W/m²",
                "suggestions": [
                    "Check for condensation/dew formation",
                    "Verify surface wetness conditions",
                    "Review atmospheric stability calculations",
                ],
            }
        )

    return issues


async def diagnose_energy_balance(
    output_path: str, site_metadata: Optional[Dict[str, Any]] = None
) -> str:
    """Diagnose energy balance closure and identify issues."""

    try:
        # Load SUEWS output data
        path = Path(output_path)

        # Try different file formats
        if path.suffix == ".csv":
            df = pd.read_csv(path, parse_dates=["datetime"], index_col="datetime")
        elif path.suffix == ".txt":
            # SUEWS standard output format
            df = pd.read_csv(path, sep=r"\s+", parse_dates=[["Year", "DOY", "Hour", "Min"]])
            # Create datetime index
            df["datetime"] = pd.to_datetime(
                df["Year"].astype(str) + df["DOY"].astype(str), format="%Y%j"
            )
            df["datetime"] += pd.to_timedelta(df["Hour"], unit="h") + pd.to_timedelta(
                df["Min"], unit="m"
            )
            df.set_index("datetime", inplace=True)
        elif path.suffix in [".h5", ".hdf5"]:
            df = pd.read_hdf(path, key="output")
        else:
            return f"Error: Unsupported file format '{path.suffix}'"

        # Calculate metrics
        metrics = calculate_energy_balance_metrics(df)

        if "error" in metrics:
            return f"Error: {metrics['error']}"

        # Diagnose issues
        issues = diagnose_issues(metrics, site_metadata)

        # Format output
        output = "# Energy Balance Diagnosis\n\n"

        # Summary statistics
        output += "## Energy Balance Components (W/m²)\n\n"
        output += f"- **Net Radiation (Q*)**: {metrics['mean_qstar']:.1f}\n"
        output += f"- **Sensible Heat (QH)**: {metrics['mean_qh']:.1f}\n"
        output += f"- **Latent Heat (QE)**: {metrics['mean_qe']:.1f}\n"

        if "mean_qs" in metrics:
            output += f"- **Storage Heat (ΔQS)**: {metrics['mean_qs']:.1f}\n"

        if "mean_qf" in metrics:
            output += f"- **Anthropogenic Heat (QF)**: {metrics['mean_qf']:.1f}\n"

        output += f"\n- **Energy Balance Residual**: {metrics['mean_residual']:.1f}\n"
        output += f"- **Residual RMSE**: {metrics['residual_rmse']:.1f}\n\n"

        # Key ratios
        output += "## Key Ratios\n\n"
        output += f"- **Energy Balance Ratio**: {metrics['energy_balance_ratio']:.3f} "
        output += "(QH+QE)/(Q*+QF)\n"
        output += f"- **Bowen Ratio (β)**: {metrics['bowen_ratio']:.2f} (QH/QE)\n"

        if "storage_fraction" in metrics:
            output += f"- **Storage Fraction**: {metrics['storage_fraction']:.3f} (ΔQS/Q*)\n"

        if "daytime_closure" in metrics:
            output += f"\n- **Daytime Closure**: {metrics['daytime_closure']:.3f}\n"
            output += f"- **Daytime Bowen Ratio**: {metrics['daytime_bowen_ratio']:.2f}\n"

        output += "\n"

        # Issues and recommendations
        if issues:
            output += "## 🚨 Issues Detected\n\n"

            high_priority = [i for i in issues if i["severity"] == "high"]
            medium_priority = [i for i in issues if i["severity"] == "medium"]

            if high_priority:
                output += "### High Priority\n\n"
                for issue in high_priority:
                    output += f"**{issue['issue']}**\n"
                    output += f"- {issue['details']}\n"
                    output += "- Suggestions:\n"
                    for sugg in issue["suggestions"]:
                        output += f"  - {sugg}\n"
                    output += "\n"

            if medium_priority:
                output += "### Medium Priority\n\n"
                for issue in medium_priority:
                    output += f"**{issue['issue']}**\n"
                    output += f"- {issue['details']}\n"
                    output += "- Suggestions:\n"
                    for sugg in issue["suggestions"]:
                        output += f"  - {sugg}\n"
                    output += "\n"
        else:
            output += "## ✅ Energy Balance Assessment\n\n"
            output += "No significant issues detected. Energy balance closure is within acceptable range.\n\n"

        # General insights
        output += "## 💡 Insights\n\n"

        # Partitioning analysis
        qh_fraction = metrics["mean_qh"] / metrics["mean_available_energy"]
        qe_fraction = metrics["mean_qe"] / metrics["mean_available_energy"]

        output += f"- Energy partitioning: {qh_fraction * 100:.0f}% sensible, "
        output += f"{qe_fraction * 100:.0f}% latent\n"

        if metrics["bowen_ratio"] > 1:
            output += "- Site is sensible heat dominated (β > 1)\n"
        else:
            output += "- Site is latent heat dominated (β < 1)\n"

        # Time of day patterns
        output += "\n### Recommended Analysis\n\n"
        output += "1. Plot diurnal cycles of energy balance components\n"
        output += "2. Examine seasonal variations in closure\n"
        output += "3. Analyze closure vs. atmospheric stability\n"
        output += "4. Check closure during different weather conditions\n"

        return output

    except FileNotFoundError:
        return f"Error: Output file not found at '{output_path}'"
    except Exception as e:
        return f"Error analyzing energy balance: {str(e)}"
