"""Thermal comfort analyzer for SUEWS MCP."""

from typing import Dict, List, Tuple, Optional
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime


def calculate_humidex(t_air: pd.Series, rh: pd.Series) -> pd.Series:
    """Calculate Humidex (Canadian thermal comfort index)."""
    # Saturation vapor pressure (hPa)
    e_sat = 6.112 * 10 ** (7.5 * t_air / (237.7 + t_air))
    # Actual vapor pressure (hPa)
    e = e_sat * rh / 100
    # Humidex formula
    humidex = t_air + 0.5555 * (e - 10)
    return humidex


def calculate_apparent_temp(t_air: pd.Series, rh: pd.Series, ws: pd.Series) -> pd.Series:
    """Calculate Apparent Temperature (Australian BOM method)."""
    # Saturation vapor pressure (hPa)
    e_sat = 6.112 * 10 ** (7.5 * t_air / (237.7 + t_air))
    # Actual vapor pressure (hPa)
    e = e_sat * rh / 100

    # Apparent temperature formula
    at = t_air + 0.33 * e - 0.70 * ws - 4.0
    return at


def calculate_wbgt_simple(t_air: pd.Series, rh: pd.Series) -> pd.Series:
    """Calculate simplified Wet Bulb Globe Temperature."""
    # This is a simplified version without radiation
    # Full WBGT requires globe temperature measurements
    tw = (
        t_air * np.arctan(0.151977 * np.sqrt(rh + 8.313659))
        + np.arctan(t_air + rh)
        - np.arctan(rh - 1.676331)
        + 0.00391838 * rh**1.5 * np.arctan(0.023101 * rh)
        - 4.686035
    )

    # Simplified WBGT (indoor conditions, no solar load)
    wbgt = 0.7 * tw + 0.3 * t_air
    return wbgt


def calculate_pet_simple(
    t_air: pd.Series, rh: pd.Series, ws: pd.Series, tmrt: Optional[pd.Series] = None
) -> pd.Series:
    """Calculate simplified Physiological Equivalent Temperature."""
    # Simplified PET calculation
    # Full PET requires complex energy balance model

    if tmrt is None:
        # Estimate Tmrt from air temperature (simplified)
        tmrt = t_air + 2.0  # Very rough approximation

    # Simplified linear approximation
    pet = 1.07 * t_air + 0.2 * (tmrt - t_air) - 0.65 * ws + 0.0014 * rh

    return pet


def categorize_thermal_stress(metric_name: str, values: pd.Series) -> pd.Series:
    """Categorize thermal stress levels based on metric thresholds."""

    categories = {
        "humidex": [
            (20, "No discomfort"),
            (30, "Some discomfort"),
            (40, "Great discomfort"),
            (45, "Dangerous"),
            (54, "Heat stroke imminent"),
            (np.inf, "Extreme danger"),
        ],
        "apparent_temp": [
            (27, "No stress"),
            (32, "Caution"),
            (39, "Extreme caution"),
            (51, "Danger"),
            (np.inf, "Extreme danger"),
        ],
        "wbgt": [
            (18, "No stress"),
            (23, "Low stress"),
            (28, "Moderate stress"),
            (32, "High stress"),
            (np.inf, "Extreme stress"),
        ],
        "pet": [
            (4, "Very cold stress"),
            (8, "Cold stress"),
            (13, "Cool"),
            (18, "Slightly cool"),
            (23, "Comfortable"),
            (29, "Slightly warm"),
            (35, "Warm"),
            (41, "Hot stress"),
            (np.inf, "Very hot stress"),
        ],
    }

    thresholds = categories.get(metric_name, [])

    # Create categorical series
    cats = pd.Series(index=values.index, dtype=str)

    for threshold, label in thresholds:
        if metric_name == "pet":
            # PET uses both lower and upper bounds
            if threshold == 4:
                cats[values < threshold] = label
            elif threshold == np.inf:
                cats[values >= 41] = label
            else:
                prev_threshold = thresholds[thresholds.index((threshold, label)) - 1][0]
                cats[(values >= prev_threshold) & (values < threshold)] = label
        else:
            # Other metrics use only upper bounds
            if threshold == np.inf:
                cats[values >= thresholds[-2][0]] = label
            else:
                if thresholds.index((threshold, label)) == 0:
                    cats[values < threshold] = label
                else:
                    prev_threshold = thresholds[thresholds.index((threshold, label)) - 1][0]
                    cats[(values >= prev_threshold) & (values < threshold)] = label

    return cats


def analyze_period_statistics(df: pd.DataFrame, period: str) -> pd.DataFrame:
    """Filter data for specific analysis period."""

    if period == "full":
        return df
    elif period == "summer":
        # Northern hemisphere summer (Jun-Aug)
        return df[(df.index.month >= 6) & (df.index.month <= 8)]
    elif period == "winter":
        # Northern hemisphere winter (Dec-Feb)
        return df[(df.index.month == 12) | (df.index.month <= 2)]
    elif period == "daytime":
        # Daytime hours (6 AM - 8 PM)
        return df[(df.index.hour >= 6) & (df.index.hour <= 20)]
    elif period == "nighttime":
        # Nighttime hours (8 PM - 6 AM)
        return df[(df.index.hour > 20) | (df.index.hour < 6)]
    elif period == "heatwave":
        # Detect heatwave periods (3+ consecutive days with Tmax > 90th percentile)
        daily_max = df["T2"].resample("D").max()
        threshold = daily_max.quantile(0.9)
        heatwave_days = (daily_max > threshold).rolling(3).sum() >= 3
        heatwave_dates = heatwave_days[heatwave_days].index
        return df[df.index.date.isin(heatwave_dates.date)]
    else:
        return df


def identify_extreme_events(metrics: Dict[str, pd.Series]) -> List[Dict]:
    """Identify extreme thermal stress events."""

    events = []

    # Check for extreme heat events
    if "humidex" in metrics:
        dangerous = metrics["humidex"] >= 40
        if dangerous.any():
            # Find continuous periods
            changes = dangerous.diff().fillna(False)
            starts = changes[changes == True].index
            ends = changes[changes == False].index

            for i, start in enumerate(starts):
                if i < len(ends):
                    end = ends[i]
                else:
                    end = metrics["humidex"].index[-1]

                duration = (end - start).total_seconds() / 3600
                max_value = metrics["humidex"][start:end].max()

                events.append(
                    {
                        "type": "Dangerous heat (Humidex)",
                        "start": start,
                        "end": end,
                        "duration_hours": duration,
                        "max_value": max_value,
                        "severity": "high" if max_value >= 45 else "medium",
                    }
                )

    # Check for extreme WBGT
    if "wbgt" in metrics:
        high_stress = metrics["wbgt"] >= 28
        if high_stress.any():
            changes = high_stress.diff().fillna(False)
            starts = changes[changes == True].index

            for start in starts[:5]:  # Limit to first 5 events
                events.append({"type": "High WBGT stress", "start": start, "severity": "medium"})

    return events


async def interpret_thermal_comfort(output_path: str, period: str = "full") -> str:
    """Analyze thermal comfort from SUEWS outputs."""

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
        else:
            return f"Error: Unsupported file format '{path.suffix}'"

        # Check for required variables
        required_vars = ["T2", "RH2"]  # 2m temperature and relative humidity
        missing = [v for v in required_vars if v not in df.columns]
        if missing:
            return f"Error: Missing required variables: {missing}"

        # Get wind speed if available
        ws = df.get("U10", pd.Series(1.5, index=df.index))  # Default 1.5 m/s if not available

        # Get mean radiant temperature if available
        tmrt = df.get("Tmrt", None)

        # Filter to analysis period
        df_period = analyze_period_statistics(df, period)

        # Calculate thermal comfort metrics
        metrics = {
            "humidex": calculate_humidex(df_period["T2"], df_period["RH2"]),
            "apparent_temp": calculate_apparent_temp(
                df_period["T2"], df_period["RH2"], ws[df_period.index]
            ),
            "wbgt": calculate_wbgt_simple(df_period["T2"], df_period["RH2"]),
        }

        # Add PET if we have Tmrt
        if tmrt is not None:
            metrics["pet"] = calculate_pet_simple(
                df_period["T2"], df_period["RH2"], ws[df_period.index], tmrt[df_period.index]
            )

        # Categorize stress levels
        categories = {}
        for metric_name, values in metrics.items():
            categories[metric_name] = categorize_thermal_stress(metric_name, values)

        # Generate output
        output = f"# Thermal Comfort Analysis\n\n"
        output += f"**Analysis Period**: {period.title()}\n"
        output += f"**Data Points**: {len(df_period):,}\n"
        output += f"**Time Range**: {df_period.index[0]} to {df_period.index[-1]}\n\n"

        # Summary statistics
        output += "## Summary Statistics\n\n"

        # Air temperature stats
        output += f"### Air Temperature (T2)\n"
        output += f"- Mean: {df_period['T2'].mean():.1f}°C\n"
        output += f"- Range: {df_period['T2'].min():.1f} to {df_period['T2'].max():.1f}°C\n"
        output += f"- Days > 30°C: {(df_period['T2'] > 30).sum() / 24:.0f}\n"
        output += f"- Days > 35°C: {(df_period['T2'] > 35).sum() / 24:.0f}\n\n"

        # Thermal comfort metrics
        output += "## Thermal Comfort Indices\n\n"

        for metric_name, values in metrics.items():
            output += f"### {metric_name.replace('_', ' ').title()}\n"
            output += f"- Mean: {values.mean():.1f}\n"
            output += f"- Maximum: {values.max():.1f}\n"
            output += f"- 95th percentile: {values.quantile(0.95):.1f}\n"

            # Stress category distribution
            cat_counts = categories[metric_name].value_counts()
            output += "\n**Stress Level Distribution:**\n"
            for category in cat_counts.index:
                hours = cat_counts[category]
                percentage = 100 * hours / len(values)
                output += f"- {category}: {hours:,} hours ({percentage:.1f}%)\n"
            output += "\n"

        # Extreme events
        events = identify_extreme_events(metrics)
        if events:
            output += "## 🚨 Extreme Events Detected\n\n"
            for event in events[:10]:  # Show first 10 events
                output += f"**{event['type']}**\n"
                output += f"- Start: {event['start']}\n"
                if "end" in event:
                    output += f"- Duration: {event['duration_hours']:.1f} hours\n"
                if "max_value" in event:
                    output += f"- Peak value: {event['max_value']:.1f}\n"
                output += f"- Severity: {event['severity']}\n\n"

        # Health implications
        output += "## 💊 Health Implications\n\n"

        # Calculate exposure hours
        dangerous_humidex = (metrics["humidex"] >= 40).sum()
        extreme_wbgt = (metrics["wbgt"] >= 32).sum()

        if dangerous_humidex > 0:
            output += f"### Heat Stress Exposure\n"
            output += f"- **Dangerous conditions** (Humidex ≥ 40): {dangerous_humidex} hours\n"
            output += f"- Risk groups: Elderly, children, outdoor workers\n"
            output += f"- Recommendations: Limit outdoor activities, ensure hydration\n\n"

        if extreme_wbgt > 0:
            output += f"### Work Safety Concerns\n"
            output += f"- **High WBGT stress** (≥ 32°C): {extreme_wbgt} hours\n"
            output += f"- Impact: Requires work-rest cycles for outdoor labour\n"
            output += f"- Safety: Risk of heat exhaustion/stroke\n\n"

        # Comfort zone analysis
        if "pet" in metrics:
            comfortable = ((metrics["pet"] >= 18) & (metrics["pet"] <= 23)).sum()
            comfort_pct = 100 * comfortable / len(metrics["pet"])
            output += f"### Thermal Comfort Zone\n"
            output += (
                f"- Hours in comfort zone (PET 18-23°C): {comfortable:,} ({comfort_pct:.1f}%)\n"
            )
            output += f"- Implication: {100 - comfort_pct:.0f}% of time requires adaptation\n\n"

        # Recommendations
        output += "## 💡 Recommendations\n\n"

        # Period-specific recommendations
        if period == "summer" or df_period["T2"].mean() > 25:
            output += "### Hot Weather Mitigation\n"
            output += "- Increase urban greenery for cooling\n"
            output += "- Implement cool roofs and pavements\n"
            output += "- Provide shaded areas and water features\n"
            output += "- Consider misting systems in public spaces\n\n"

        if dangerous_humidex > 24:  # More than 1 day
            output += "### Public Health Measures\n"
            output += "- Establish cooling centres\n"
            output += "- Issue heat warnings\n"
            output += "- Adjust outdoor work schedules\n"
            output += "- Monitor vulnerable populations\n\n"

        # Adaptive strategies
        output += "### Climate Adaptation Strategies\n"
        mean_stress = categories["humidex"].value_counts(normalize=True)
        if "Great discomfort" in mean_stress and mean_stress["Great discomfort"] > 0.1:
            output += "- Design buildings for passive cooling\n"
            output += "- Increase urban albedo\n"
            output += "- Enhance urban ventilation corridors\n"
        else:
            output += "- Current thermal conditions are generally manageable\n"
            output += "- Focus on maintaining comfort during peak periods\n"

        return output

    except FileNotFoundError:
        return f"Error: Output file not found at '{output_path}'"
    except Exception as e:
        return f"Error analyzing thermal comfort: {str(e)}"
