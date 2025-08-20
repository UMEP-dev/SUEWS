"""Observation validator for SUEWS MCP."""

from typing import Any, Optional, List, Dict, Tuple
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
import warnings


def calculate_metrics(obs: pd.Series, mod: pd.Series) -> Dict[str, float]:
    """Calculate validation metrics between observations and model."""

    # Remove NaN values
    mask = ~(obs.isna() | mod.isna())
    obs_clean = obs[mask]
    mod_clean = mod[mask]

    if len(obs_clean) < 10:
        return {"error": "Insufficient data points for validation"}

    # Basic statistics
    n = len(obs_clean)
    obs_mean = obs_clean.mean()
    mod_mean = mod_clean.mean()

    # Mean Bias Error
    mbe = (mod_clean - obs_clean).mean()

    # Root Mean Square Error
    rmse = np.sqrt(((mod_clean - obs_clean) ** 2).mean())

    # Normalized RMSE
    nrmse = rmse / obs_mean if obs_mean != 0 else np.nan

    # Mean Absolute Error
    mae = np.abs(mod_clean - obs_clean).mean()

    # Coefficient of Determination (R²)
    ss_res = ((obs_clean - mod_clean) ** 2).sum()
    ss_tot = ((obs_clean - obs_mean) ** 2).sum()
    r2 = 1 - (ss_res / ss_tot) if ss_tot != 0 else np.nan

    # Pearson correlation coefficient
    correlation = obs_clean.corr(mod_clean)

    # Nash-Sutcliffe Efficiency
    nse = 1 - (ss_res / ss_tot) if ss_tot != 0 else np.nan

    # Index of Agreement (Willmott)
    numerator = ((mod_clean - obs_clean) ** 2).sum()
    denominator = ((np.abs(mod_clean - obs_mean) + np.abs(obs_clean - obs_mean)) ** 2).sum()
    d = 1 - (numerator / denominator) if denominator != 0 else np.nan

    # Systematic and unsystematic RMSE
    # Linear regression
    slope = ((obs_clean - obs_mean) * (mod_clean - mod_mean)).sum() / (
        (obs_clean - obs_mean) ** 2
    ).sum()
    intercept = mod_mean - slope * obs_mean
    mod_predicted = intercept + slope * obs_clean

    rmse_s = np.sqrt(((mod_predicted - obs_clean) ** 2).mean())  # Systematic
    rmse_u = np.sqrt(((mod_clean - mod_predicted) ** 2).mean())  # Unsystematic

    return {
        "n": n,
        "obs_mean": float(obs_mean),
        "mod_mean": float(mod_mean),
        "mbe": float(mbe),
        "rmse": float(rmse),
        "nrmse": float(nrmse),
        "mae": float(mae),
        "r2": float(r2),
        "correlation": float(correlation),
        "nse": float(nse),
        "d": float(d),
        "rmse_s": float(rmse_s),
        "rmse_u": float(rmse_u),
        "slope": float(slope),
        "intercept": float(intercept),
    }


def interpret_metrics(metrics: Dict[str, float], variable: str) -> Dict[str, str]:
    """Interpret validation metrics for different variables."""

    interpretations = {}

    # Variable-specific thresholds
    thresholds = {
        "T2": {"good_rmse": 2.0, "acceptable_rmse": 3.0, "units": "°C"},
        "RH2": {"good_rmse": 10.0, "acceptable_rmse": 15.0, "units": "%"},
        "U10": {"good_rmse": 1.5, "acceptable_rmse": 2.5, "units": "m/s"},
        "QH": {"good_rmse": 50.0, "acceptable_rmse": 75.0, "units": "W/m²"},
        "QE": {"good_rmse": 50.0, "acceptable_rmse": 75.0, "units": "W/m²"},
        "QN": {"good_rmse": 40.0, "acceptable_rmse": 60.0, "units": "W/m²"},
        "QS": {"good_rmse": 60.0, "acceptable_rmse": 100.0, "units": "W/m²"},
    }

    # Get thresholds for this variable
    var_thresholds = thresholds.get(
        variable, {"good_rmse": np.inf, "acceptable_rmse": np.inf, "units": ""}
    )

    # RMSE interpretation
    if metrics["rmse"] < var_thresholds["good_rmse"]:
        interpretations["rmse"] = "Excellent agreement"
    elif metrics["rmse"] < var_thresholds["acceptable_rmse"]:
        interpretations["rmse"] = "Good agreement"
    else:
        interpretations["rmse"] = "Poor agreement - model needs improvement"

    # Bias interpretation
    bias_percent = 100 * metrics["mbe"] / metrics["obs_mean"] if metrics["obs_mean"] != 0 else 0
    if abs(bias_percent) < 5:
        interpretations["bias"] = "Minimal bias"
    elif abs(bias_percent) < 10:
        interpretations["bias"] = "Acceptable bias"
    elif bias_percent > 10:
        interpretations["bias"] = "Model overestimates"
    else:
        interpretations["bias"] = "Model underestimates"

    # R² interpretation
    if metrics["r2"] > 0.8:
        interpretations["r2"] = "Strong correlation"
    elif metrics["r2"] > 0.6:
        interpretations["r2"] = "Moderate correlation"
    else:
        interpretations["r2"] = "Weak correlation"

    # NSE interpretation
    if metrics["nse"] > 0.75:
        interpretations["nse"] = "Very good model performance"
    elif metrics["nse"] > 0.5:
        interpretations["nse"] = "Good model performance"
    elif metrics["nse"] > 0:
        interpretations["nse"] = "Satisfactory performance"
    else:
        interpretations["nse"] = "Unsatisfactory - mean would be better predictor"

    # Systematic vs unsystematic error
    sys_ratio = metrics["rmse_s"] / metrics["rmse"] if metrics["rmse"] > 0 else 0
    if sys_ratio > 0.5:
        interpretations["error_type"] = (
            "Primarily systematic errors - check model physics/parameters"
        )
    else:
        interpretations["error_type"] = "Primarily random errors - good model structure"

    return interpretations


def analyze_temporal_patterns(
    obs_df: pd.DataFrame, mod_df: pd.DataFrame, variable: str
) -> Dict[str, Any]:
    """Analyze temporal patterns in model performance."""

    patterns = {}

    if variable not in obs_df.columns or variable not in mod_df.columns:
        return patterns

    # Calculate hourly errors
    obs_series = obs_df[variable]
    mod_series = mod_df[variable]
    errors = mod_series - obs_series

    # Diurnal pattern
    hourly_metrics = {}
    for hour in range(24):
        hour_mask = obs_df.index.hour == hour
        if hour_mask.sum() > 10:
            hour_obs = obs_series[hour_mask]
            hour_mod = mod_series[hour_mask]
            hour_metrics = calculate_metrics(hour_obs, hour_mod)
            hourly_metrics[hour] = {
                "rmse": hour_metrics["rmse"],
                "mbe": hour_metrics["mbe"],
                "n": hour_metrics["n"],
            }

    patterns["hourly"] = hourly_metrics

    # Seasonal pattern (if enough data)
    if len(obs_df) > 365 * 24:
        seasonal_metrics = {}
        for month in range(1, 13):
            month_mask = obs_df.index.month == month
            if month_mask.sum() > 100:
                month_obs = obs_series[month_mask]
                month_mod = mod_series[month_mask]
                month_metrics = calculate_metrics(month_obs, month_mod)
                seasonal_metrics[month] = {
                    "rmse": month_metrics["rmse"],
                    "mbe": month_metrics["mbe"],
                    "r2": month_metrics["r2"],
                }

        patterns["seasonal"] = seasonal_metrics

    # Error distribution
    patterns["error_distribution"] = {
        "mean": float(errors.mean()),
        "std": float(errors.std()),
        "skewness": float(errors.skew()),
        "percentiles": {
            "5th": float(errors.quantile(0.05)),
            "25th": float(errors.quantile(0.25)),
            "50th": float(errors.quantile(0.50)),
            "75th": float(errors.quantile(0.75)),
            "95th": float(errors.quantile(0.95)),
        },
    }

    return patterns


async def validate_against_observations(
    model_output: str, observations: str, variables: Optional[List[str]] = None
) -> str:
    """Validate model results against observations."""

    try:
        # Load model output
        model_path = Path(model_output)
        if model_path.suffix == ".csv":
            mod_df = pd.read_csv(model_path, parse_dates=["datetime"], index_col="datetime")
        elif model_path.suffix == ".txt":
            # SUEWS standard output format
            mod_df = pd.read_csv(model_path, sep=r"\s+")
            mod_df["datetime"] = pd.to_datetime(
                mod_df["Year"].astype(str) + mod_df["DOY"].astype(str), format="%Y%j"
            )
            mod_df["datetime"] += pd.to_timedelta(mod_df["Hour"], unit="h") + pd.to_timedelta(
                mod_df.get("Min", 0), unit="m"
            )
            mod_df.set_index("datetime", inplace=True)
        else:
            return f"Error: Unsupported model output format '{model_path.suffix}'"

        # Load observations
        obs_path = Path(observations)
        if obs_path.suffix == ".csv":
            obs_df = pd.read_csv(obs_path, parse_dates=["datetime"], index_col="datetime")
        elif obs_path.suffix == ".txt":
            # Try to parse as whitespace-separated
            obs_df = pd.read_csv(obs_path, sep=r"\s+")
            # Assume first column is datetime if not numeric
            if "datetime" in obs_df.columns:
                obs_df["datetime"] = pd.to_datetime(obs_df["datetime"])
                obs_df.set_index("datetime", inplace=True)
            else:
                # Try to construct datetime from components
                date_cols = [
                    col
                    for col in obs_df.columns
                    if col.lower() in ["year", "month", "day", "hour", "doy"]
                ]
                if "Year" in obs_df.columns and "DOY" in obs_df.columns:
                    obs_df["datetime"] = pd.to_datetime(
                        obs_df["Year"].astype(str) + obs_df["DOY"].astype(str), format="%Y%j"
                    )
                    if "Hour" in obs_df.columns:
                        obs_df["datetime"] += pd.to_timedelta(obs_df["Hour"], unit="h")
                    obs_df.set_index("datetime", inplace=True)
        else:
            return f"Error: Unsupported observation format '{obs_path.suffix}'"

        # Find common time periods
        common_times = mod_df.index.intersection(obs_df.index)
        if len(common_times) == 0:
            return "Error: No overlapping time periods between model and observations"

        # Subset to common times
        mod_df = mod_df.loc[common_times]
        obs_df = obs_df.loc[common_times]

        # Determine variables to validate
        if variables is None:
            # Find common variables
            common_vars = list(set(mod_df.columns) & set(obs_df.columns))
            # Prioritize key variables
            priority_vars = ["T2", "RH2", "U10", "QH", "QE", "QN", "QS"]
            variables = [v for v in priority_vars if v in common_vars]
            if not variables:
                variables = common_vars[:5]  # Take first 5 common variables

        # Generate validation output
        output = "# Model Validation Against Observations\n\n"
        output += f"**Time Period**: {common_times[0]} to {common_times[-1]}\n"
        output += f"**Data Points**: {len(common_times):,}\n"
        output += f"**Variables Validated**: {', '.join(variables)}\n\n"

        # Validate each variable
        for var in variables:
            if var not in mod_df.columns or var not in obs_df.columns:
                output += f"\n## {var}\n*Variable not found in both datasets*\n"
                continue

            output += f"\n## {var}\n\n"

            # Calculate metrics
            metrics = calculate_metrics(obs_df[var], mod_df[var])

            if "error" in metrics:
                output += f"*{metrics['error']}*\n"
                continue

            # Get interpretations
            interpretations = interpret_metrics(metrics, var)

            # Summary statistics
            output += "### Summary Statistics\n"
            output += f"- **Observations**: Mean = {metrics['obs_mean']:.2f}, n = {metrics['n']}\n"
            output += f"- **Model**: Mean = {metrics['mod_mean']:.2f}\n"
            output += f"- **Mean Bias**: {metrics['mbe']:.2f} ({metrics['mbe'] / metrics['obs_mean'] * 100:.1f}%)\n\n"

            # Performance metrics
            output += "### Performance Metrics\n"
            output += f"- **RMSE**: {metrics['rmse']:.2f} - *{interpretations['rmse']}*\n"
            output += f"- **MAE**: {metrics['mae']:.2f}\n"
            output += f"- **R²**: {metrics['r2']:.3f} - *{interpretations['r2']}*\n"
            output += f"- **NSE**: {metrics['nse']:.3f} - *{interpretations['nse']}*\n"
            output += f"- **d**: {metrics['d']:.3f} (Index of Agreement)\n\n"

            # Error decomposition
            output += "### Error Analysis\n"
            output += f"- **Systematic RMSE**: {metrics['rmse_s']:.2f} ({metrics['rmse_s'] / metrics['rmse'] * 100:.0f}%)\n"
            output += f"- **Unsystematic RMSE**: {metrics['rmse_u']:.2f} ({metrics['rmse_u'] / metrics['rmse'] * 100:.0f}%)\n"
            output += f"- *{interpretations['error_type']}*\n"
            output += (
                f"- **Regression**: y = {metrics['slope']:.2f}x + {metrics['intercept']:.2f}\n\n"
            )

            # Temporal patterns
            patterns = analyze_temporal_patterns(obs_df, mod_df, var)

            if "hourly" in patterns and patterns["hourly"]:
                output += "### Diurnal Performance\n"
                worst_hours = sorted(
                    patterns["hourly"].items(), key=lambda x: x[1]["rmse"], reverse=True
                )[:3]
                output += "Worst performing hours:\n"
                for hour, hour_metrics in worst_hours:
                    output += f"- {hour:02d}:00 - RMSE: {hour_metrics['rmse']:.2f}, "
                    output += f"Bias: {hour_metrics['mbe']:.2f}\n"
                output += "\n"

            if "seasonal" in patterns and patterns["seasonal"]:
                output += "### Seasonal Performance\n"
                months = [
                    "Jan",
                    "Feb",
                    "Mar",
                    "Apr",
                    "May",
                    "Jun",
                    "Jul",
                    "Aug",
                    "Sep",
                    "Oct",
                    "Nov",
                    "Dec",
                ]
                for month_num, month_metrics in patterns["seasonal"].items():
                    if month_metrics["r2"] < 0.5:  # Flag poor months
                        output += f"- **{months[month_num - 1]}**: "
                        output += f"RMSE={month_metrics['rmse']:.2f}, "
                        output += f"R²={month_metrics['r2']:.2f} ⚠️\n"

        # Overall assessment
        output += "\n## 📊 Overall Assessment\n\n"

        # Count good/poor performing variables
        good_vars = []
        poor_vars = []

        for var in variables:
            if var in mod_df.columns and var in obs_df.columns:
                metrics = calculate_metrics(obs_df[var], mod_df[var])
                if "error" not in metrics:
                    if metrics["r2"] > 0.7 and metrics["nse"] > 0.5:
                        good_vars.append(var)
                    elif metrics["r2"] < 0.5 or metrics["nse"] < 0:
                        poor_vars.append(var)

        if good_vars:
            output += f"### ✅ Well-Simulated Variables\n"
            for var in good_vars:
                output += f"- {var}\n"
            output += "\n"

        if poor_vars:
            output += f"### ⚠️ Variables Needing Improvement\n"
            for var in poor_vars:
                output += f"- {var}\n"
            output += "\n"

        # Recommendations
        output += "## 💡 Recommendations\n\n"

        # Check for systematic biases
        systematic_issues = []
        for var in variables:
            if var in mod_df.columns and var in obs_df.columns:
                metrics = calculate_metrics(obs_df[var], mod_df[var])
                if "error" not in metrics and metrics["rmse_s"] / metrics["rmse"] > 0.6:
                    systematic_issues.append(var)

        if systematic_issues:
            output += "### Address Systematic Errors\n"
            output += f"Variables with systematic biases: {', '.join(systematic_issues)}\n"
            output += "- Review input parameters and forcing data\n"
            output += "- Check model physics options\n"
            output += "- Consider site-specific calibration\n\n"

        # Variable-specific recommendations
        if "T2" in poor_vars:
            output += "### Temperature Simulation\n"
            output += "- Check surface properties (albedo, emissivity)\n"
            output += "- Review anthropogenic heat flux\n"
            output += "- Verify building morphology parameters\n\n"

        if "QH" in poor_vars or "QE" in poor_vars:
            output += "### Energy Partitioning\n"
            output += "- Review vegetation parameters\n"
            output += "- Check soil moisture initialization\n"
            output += "- Verify surface fractions\n\n"

        output += "### Next Steps\n"
        output += "1. Focus on improving poorest performing variables\n"
        output += "2. Examine error patterns (diurnal/seasonal)\n"
        output += "3. Consider parameter sensitivity analysis\n"
        output += "4. Validate against additional observation periods\n"

        return output

    except FileNotFoundError as e:
        return f"Error: File not found - {str(e)}"
    except Exception as e:
        return f"Error validating against observations: {str(e)}"
