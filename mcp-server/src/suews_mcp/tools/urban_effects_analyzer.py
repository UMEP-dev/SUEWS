"""Urban effects analyzer for SUEWS MCP."""

from typing import Optional, Dict, List, Tuple
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta


def calculate_uhi_intensity(urban_temp: pd.Series, rural_temp: pd.Series) -> pd.Series:
    """Calculate Urban Heat Island intensity."""
    return urban_temp - rural_temp


def analyze_diurnal_patterns(df: pd.DataFrame, variable: str) -> Dict[str, float]:
    """Analyze diurnal patterns of a variable."""
    hourly_mean = df.groupby(df.index.hour)[variable].mean()

    return {
        "mean_amplitude": float(hourly_mean.max() - hourly_mean.min()),
        "peak_hour": int(hourly_mean.idxmax()),
        "minimum_hour": int(hourly_mean.idxmin()),
        "daytime_mean": float(df.between_time("06:00", "18:00")[variable].mean()),
        "nighttime_mean": float(
            df[~df.index.to_series().between("06:00", "18:00")][variable].mean()
        ),
    }


def calculate_surface_temperature_patterns(df: pd.DataFrame) -> Dict[str, Dict]:
    """Analyze surface temperature patterns across different surfaces."""
    patterns = {}

    # Check for surface temperature columns
    surf_temp_cols = [col for col in df.columns if col.startswith("Tsurf_") or col.endswith("_T")]

    for col in surf_temp_cols:
        if col in df.columns:
            patterns[col] = {
                "mean": float(df[col].mean()),
                "max": float(df[col].max()),
                "diurnal_range": float(
                    df.groupby(df.index.date)[col].apply(lambda x: x.max() - x.min()).mean()
                ),
                "peak_timing": df.groupby(df.index.hour)[col].mean().idxmax(),
            }

    return patterns


def analyze_energy_partitioning(df: pd.DataFrame) -> Dict[str, float]:
    """Analyze how energy is partitioned in urban environment."""
    results = {}

    # Check for energy balance components
    if "QN" in df.columns and "QH" in df.columns and "QE" in df.columns:
        # Daytime positive net radiation only
        daytime_positive = df[df["QN"] > 50]

        if len(daytime_positive) > 0:
            results["bowen_ratio"] = float(
                (daytime_positive["QH"] / daytime_positive["QE"])
                .replace([np.inf, -np.inf], np.nan)
                .mean()
            )
            results["evaporative_fraction"] = float(
                (daytime_positive["QE"] / daytime_positive["QN"]).mean()
            )
            results["sensible_fraction"] = float(
                (daytime_positive["QH"] / daytime_positive["QN"]).mean()
            )

            if "QS" in df.columns:
                results["storage_fraction"] = float(
                    (daytime_positive["QS"] / daytime_positive["QN"]).mean()
                )

    return results


def identify_urban_cooling_potential(df: pd.DataFrame) -> Dict[str, float]:
    """Identify potential for urban cooling strategies."""
    potential = {}

    # Analyze evapotranspiration potential
    if "QE" in df.columns and "SMD" in df.columns:
        # Low ET during dry conditions indicates irrigation potential
        dry_conditions = df[df["SMD"] > df["SMD"].quantile(0.75)]
        wet_conditions = df[df["SMD"] < df["SMD"].quantile(0.25)]

        if len(dry_conditions) > 0 and len(wet_conditions) > 0:
            et_dry = dry_conditions["QE"].mean()
            et_wet = wet_conditions["QE"].mean()
            potential["irrigation_cooling_potential"] = float(et_wet - et_dry)

    # Analyze albedo impact
    if "Kup" in df.columns and "Kdown" in df.columns:
        current_albedo = (df["Kup"] / df["Kdown"]).replace([np.inf, -np.inf], np.nan).mean()
        # Estimate cooling from increasing albedo by 0.1
        potential["albedo_increase_cooling"] = float(
            0.1 * df["Kdown"].mean() * 0.5
        )  # Rough estimate
        potential["current_albedo"] = float(current_albedo)

    return potential


def detect_heat_stress_periods(df: pd.DataFrame) -> List[Dict]:
    """Detect periods of extreme urban heat stress."""
    stress_periods = []

    if "T2" not in df.columns:
        return stress_periods

    # Define heat stress threshold (e.g., T > 35°C)
    heat_threshold = 35
    extreme_heat = df["T2"] > heat_threshold

    if extreme_heat.any():
        # Find continuous periods
        changes = extreme_heat.astype(int).diff()
        starts = df.index[changes == 1]
        ends = df.index[changes == -1]

        # Handle edge cases
        if extreme_heat.iloc[0]:
            starts = starts.insert(0, df.index[0])
        if extreme_heat.iloc[-1]:
            ends = ends.append(pd.Index([df.index[-1]]))

        # Create stress period records
        for i in range(min(len(starts), len(ends))):
            duration = (ends[i] - starts[i]).total_seconds() / 3600
            if duration > 1:  # Only include periods > 1 hour
                stress_periods.append(
                    {
                        "start": starts[i],
                        "end": ends[i],
                        "duration_hours": duration,
                        "max_temp": float(df.loc[starts[i] : ends[i], "T2"].max()),
                        "mean_temp": float(df.loc[starts[i] : ends[i], "T2"].mean()),
                    }
                )

    return stress_periods[:10]  # Return top 10 periods


def calculate_urban_modification_factors(
    urban_df: pd.DataFrame, rural_df: Optional[pd.DataFrame] = None
) -> Dict[str, float]:
    """Calculate how urban environment modifies climate variables."""
    factors = {}

    if rural_df is not None and len(urban_df) == len(rural_df):
        # Temperature modification
        if "T2" in urban_df.columns and "T2" in rural_df.columns:
            factors["temperature_amplification"] = float((urban_df["T2"] - rural_df["T2"]).mean())
            factors["max_uhi_intensity"] = float((urban_df["T2"] - rural_df["T2"]).max())

            # Nocturnal UHI
            night_urban = urban_df.between_time("22:00", "06:00")["T2"]
            night_rural = rural_df.between_time("22:00", "06:00")["T2"]
            factors["nocturnal_uhi"] = float((night_urban - night_rural).mean())

        # Humidity modification
        if "RH2" in urban_df.columns and "RH2" in rural_df.columns:
            factors["humidity_reduction"] = float((rural_df["RH2"] - urban_df["RH2"]).mean())

        # Wind modification
        if "U10" in urban_df.columns and "U10" in rural_df.columns:
            factors["wind_reduction_factor"] = float(1 - (urban_df["U10"] / rural_df["U10"]).mean())

    return factors


async def analyze_urban_effects(output_path: str, rural_reference: Optional[str] = None) -> str:
    """Analyze urban climate effects from SUEWS results."""

    try:
        # Load urban data
        urban_path = Path(output_path)

        if urban_path.suffix == ".csv":
            urban_df = pd.read_csv(urban_path, parse_dates=["datetime"], index_col="datetime")
        elif urban_path.suffix == ".txt":
            # SUEWS standard output format
            urban_df = pd.read_csv(urban_path, sep=r"\s+")
            # Create datetime index
            urban_df["datetime"] = pd.to_datetime(
                urban_df["Year"].astype(str) + urban_df["DOY"].astype(str), format="%Y%j"
            )
            urban_df["datetime"] += pd.to_timedelta(urban_df["Hour"], unit="h") + pd.to_timedelta(
                urban_df.get("Min", 0), unit="m"
            )
            urban_df.set_index("datetime", inplace=True)
        else:
            return f"Error: Unsupported file format '{urban_path.suffix}'"

        # Load rural reference if provided
        rural_df = None
        if rural_reference:
            rural_path = Path(rural_reference)
            if rural_path.exists():
                if rural_path.suffix == ".csv":
                    rural_df = pd.read_csv(
                        rural_path, parse_dates=["datetime"], index_col="datetime"
                    )
                elif rural_path.suffix == ".txt":
                    rural_df = pd.read_csv(rural_path, sep=r"\s+")
                    rural_df["datetime"] = pd.to_datetime(
                        rural_df["Year"].astype(str) + rural_df["DOY"].astype(str), format="%Y%j"
                    )
                    rural_df["datetime"] += pd.to_timedelta(
                        rural_df["Hour"], unit="h"
                    ) + pd.to_timedelta(rural_df.get("Min", 0), unit="m")
                    rural_df.set_index("datetime", inplace=True)

        # Generate analysis output
        output = "# Urban Climate Effects Analysis\n\n"

        # Basic statistics
        output += f"**Analysis Period**: {urban_df.index[0]} to {urban_df.index[-1]}\n"
        output += f"**Data Points**: {len(urban_df):,}\n\n"

        # Temperature analysis
        if "T2" in urban_df.columns:
            output += "## Urban Temperature Characteristics\n\n"

            temp_patterns = analyze_diurnal_patterns(urban_df, "T2")
            output += f"### Diurnal Temperature Pattern\n"
            output += f"- Mean diurnal range: {temp_patterns['mean_amplitude']:.1f}°C\n"
            output += f"- Peak temperature hour: {temp_patterns['peak_hour']:02d}:00\n"
            output += f"- Minimum temperature hour: {temp_patterns['minimum_hour']:02d}:00\n"
            output += f"- Day/night difference: {temp_patterns['daytime_mean'] - temp_patterns['nighttime_mean']:.1f}°C\n\n"

        # Urban Heat Island analysis if rural reference available
        if rural_df is not None and "T2" in urban_df.columns and "T2" in rural_df.columns:
            output += "## Urban Heat Island (UHI) Analysis\n\n"

            # Calculate UHI intensity
            common_times = urban_df.index.intersection(rural_df.index)
            if len(common_times) > 0:
                uhi = urban_df.loc[common_times, "T2"] - rural_df.loc[common_times, "T2"]

                output += f"### UHI Intensity\n"
                output += f"- Mean UHI: {uhi.mean():.2f}°C\n"
                output += f"- Maximum UHI: {uhi.max():.2f}°C\n"
                output += f"- Nocturnal UHI: {uhi.between_time('22:00', '06:00').mean():.2f}°C\n"
                output += f"- Daytime UHI: {uhi.between_time('10:00', '16:00').mean():.2f}°C\n\n"

                # UHI timing
                hourly_uhi = uhi.groupby(uhi.index.hour).mean()
                output += f"### UHI Timing\n"
                output += f"- Peak UHI hour: {hourly_uhi.idxmax():02d}:00\n"
                output += f"- Minimum UHI hour: {hourly_uhi.idxmin():02d}:00\n\n"

        # Energy balance partitioning
        energy_partition = analyze_energy_partitioning(urban_df)
        if energy_partition:
            output += "## Urban Energy Balance\n\n"
            output += "### Energy Partitioning (Daytime)\n"

            if "bowen_ratio" in energy_partition:
                output += f"- Bowen ratio (β): {energy_partition['bowen_ratio']:.2f}\n"
                if energy_partition["bowen_ratio"] > 2:
                    output += "  - *High β indicates limited evapotranspiration*\n"

            if "evaporative_fraction" in energy_partition:
                output += (
                    f"- Evaporative fraction: {energy_partition['evaporative_fraction']:.2%}\n"
                )

            if "storage_fraction" in energy_partition:
                output += f"- Storage fraction: {energy_partition['storage_fraction']:.2%}\n"
                if energy_partition["storage_fraction"] > 0.4:
                    output += "  - *High storage indicates significant thermal mass*\n"

            output += "\n"

        # Surface temperature analysis
        surf_patterns = calculate_surface_temperature_patterns(urban_df)
        if surf_patterns:
            output += "## Surface Temperature Patterns\n\n"
            for surface, stats in surf_patterns.items():
                clean_name = surface.replace("Tsurf_", "").replace("_T", "").title()
                output += f"### {clean_name} Surface\n"
                output += f"- Mean temperature: {stats['mean']:.1f}°C\n"
                output += f"- Maximum: {stats['max']:.1f}°C\n"
                output += f"- Mean diurnal range: {stats['diurnal_range']:.1f}°C\n\n"

        # Urban modification factors
        if rural_df is not None:
            mod_factors = calculate_urban_modification_factors(urban_df, rural_df)
            if mod_factors:
                output += "## Urban Modification Effects\n\n"

                if "temperature_amplification" in mod_factors:
                    output += f"- Temperature amplification: +{mod_factors['temperature_amplification']:.1f}°C\n"

                if "nocturnal_uhi" in mod_factors:
                    output += f"- Nocturnal heat retention: +{mod_factors['nocturnal_uhi']:.1f}°C\n"

                if "humidity_reduction" in mod_factors:
                    output += f"- Humidity reduction: -{mod_factors['humidity_reduction']:.0f}%\n"

                if "wind_reduction_factor" in mod_factors:
                    output += (
                        f"- Wind speed reduction: {mod_factors['wind_reduction_factor']:.0%}\n"
                    )

                output += "\n"

        # Heat stress periods
        stress_periods = detect_heat_stress_periods(urban_df)
        if stress_periods:
            output += "## 🌡️ Extreme Heat Events\n\n"
            output += f"Detected {len(stress_periods)} extreme heat periods (T > 35°C):\n\n"

            for i, period in enumerate(stress_periods[:5]):  # Show top 5
                output += f"**Event {i + 1}**\n"
                output += f"- Time: {period['start']} to {period['end']}\n"
                output += f"- Duration: {period['duration_hours']:.1f} hours\n"
                output += f"- Peak temperature: {period['max_temp']:.1f}°C\n\n"

        # Cooling potential analysis
        cooling_potential = identify_urban_cooling_potential(urban_df)
        if cooling_potential:
            output += "## 💨 Urban Cooling Potential\n\n"

            if "irrigation_cooling_potential" in cooling_potential:
                output += f"### Evaporative Cooling\n"
                output += f"- Potential cooling from irrigation: {cooling_potential['irrigation_cooling_potential']:.0f} W/m²\n"
                output += "- Strategy: Increase urban vegetation and water features\n\n"

            if "current_albedo" in cooling_potential:
                output += f"### Albedo Modification\n"
                output += f"- Current albedo: {cooling_potential['current_albedo']:.2f}\n"
                output += f"- Potential cooling from +0.1 albedo: ~{cooling_potential['albedo_increase_cooling']:.0f} W/m²\n"
                output += "- Strategy: Cool roofs and light-coloured pavements\n\n"

        # Recommendations
        output += "## 💡 Urban Design Recommendations\n\n"

        # Temperature-based recommendations
        if "T2" in urban_df.columns:
            summer_mean = urban_df[urban_df.index.month.isin([6, 7, 8])]["T2"].mean()

            if summer_mean > 28:
                output += "### Heat Mitigation Strategies\n"
                output += "- **Priority**: Urgent need for cooling interventions\n"
                output += "- Increase tree canopy coverage (target 30-40%)\n"
                output += "- Implement cool/green roofs on large buildings\n"
                output += "- Create urban water features and misting systems\n"
                output += "- Design buildings for natural ventilation\n\n"

        # Energy balance recommendations
        if energy_partition and "bowen_ratio" in energy_partition:
            if energy_partition["bowen_ratio"] > 2:
                output += "### Water Balance Improvements\n"
                output += "- Increase permeable surfaces\n"
                output += "- Implement rainwater harvesting\n"
                output += "- Enhance urban irrigation systems\n"
                output += "- Create bioswales and rain gardens\n\n"

        # UHI-specific recommendations
        if rural_df is not None and "nocturnal_uhi" in mod_factors:
            if mod_factors["nocturnal_uhi"] > 3:
                output += "### Nocturnal Cooling Strategies\n"
                output += "- Enhance urban ventilation corridors\n"
                output += "- Reduce thermal mass in non-critical areas\n"
                output += "- Implement night-time ventilation in buildings\n"
                output += "- Use materials with lower heat capacity\n\n"

        # General urban climate recommendations
        output += "### Integrated Urban Climate Design\n"
        output += "1. **Green Infrastructure**: Maximise vegetation at all scales\n"
        output += "2. **Blue Infrastructure**: Integrate water for cooling\n"
        output += "3. **Grey Infrastructure**: Optimise materials and geometry\n"
        output += "4. **Planning**: Align streets with prevailing winds\n"
        output += "5. **Building Design**: Passive cooling and green facades\n"

        return output

    except FileNotFoundError:
        return f"Error: Output file not found at '{output_path}'"
    except Exception as e:
        return f"Error analyzing urban effects: {str(e)}"
