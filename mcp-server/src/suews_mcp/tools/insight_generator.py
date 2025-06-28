"""Insight report generator for SUEWS MCP."""

from typing import Dict, List, Optional, Any
import pandas as pd
import numpy as np
from pathlib import Path
import yaml
from datetime import datetime

# Import analysis functions from other tools
from .energy_balance_analyzer import calculate_energy_balance_metrics, diagnose_issues
from .thermal_comfort_analyzer import (
    calculate_humidex,
    calculate_wbgt_simple,
    categorize_thermal_stress,
)
from .urban_effects_analyzer import analyze_diurnal_patterns, analyze_energy_partitioning


def load_configuration(config_path: str) -> Dict[str, Any]:
    """Load and parse SUEWS configuration."""
    try:
        with open(config_path, "r") as f:
            config = yaml.safe_load(f)
        return config
    except Exception as e:
        return {"error": str(e)}


def analyze_simulation_context(config: Dict, research_context: str) -> Dict[str, Any]:
    """Analyze simulation setup and research context."""
    context = {
        "research_focus": research_context,
        "simulation_details": {},
        "site_characteristics": {},
        "key_parameters": {},
    }

    # Extract simulation details
    if "RunControl" in config:
        run = config["RunControl"]
        context["simulation_details"] = {
            "start_date": run.get("start_date", "Unknown"),
            "end_date": run.get("end_date", "Unknown"),
            "time_step": run.get("tstep", 300),
            "location": f"{run.get('latitude', 0):.2f}°N, {run.get('longitude', 0):.2f}°E",
        }

    # Extract site characteristics
    if "SiteCharacteristics" in config:
        site = config["SiteCharacteristics"]
        context["site_characteristics"] = {
            "mean_building_height": site.get("bldgh", 0),
            "vegetation_fraction": site.get("fr_evertr", 0)
            + site.get("fr_dectr", 0)
            + site.get("fr_grass", 0),
            "built_fraction": site.get("fr_bldg", 0) + site.get("fr_paved", 0),
            "population_density": site.get("population_density", 0),
        }

    # Extract key physics options
    if "ModelOptions" in config:
        opts = config["ModelOptions"]
        context["key_parameters"] = {
            "net_radiation_method": opts.get("NetRadiationMethod", "Unknown"),
            "storage_heat_method": opts.get("StorageHeatMethod", "Unknown"),
            "stability_method": opts.get("StabilityMethod", "Unknown"),
        }

    return context


def generate_executive_summary(
    df: pd.DataFrame,
    context: Dict[str, Any],
    energy_metrics: Dict[str, float],
    thermal_metrics: Dict[str, Any],
) -> str:
    """Generate executive summary of key findings."""

    summary = "## Executive Summary\n\n"

    # Context overview
    summary += f"This report analyzes SUEWS urban climate simulation results "
    summary += f"for {context['research_focus']}. "
    summary += f"The simulation covers the period from "
    summary += f"{context['simulation_details']['start_date']} to "
    summary += f"{context['simulation_details']['end_date']}.\n\n"

    # Key findings
    summary += "### Key Findings\n\n"

    # Temperature insights
    if "T2" in df.columns:
        t_mean = df["T2"].mean()
        t_max = df["T2"].max()
        summer_days = (df["T2"] > 30).sum() / 24

        summary += f"1. **Temperature Conditions**: Mean temperature of {t_mean:.1f}°C "
        summary += f"with maximum of {t_max:.1f}°C. "
        summary += f"Approximately {summer_days:.0f} summer days (>30°C) were simulated.\n\n"

    # Energy balance insights
    if energy_metrics:
        ebr = energy_metrics.get("energy_balance_ratio", 0)
        bowen = energy_metrics.get("bowen_ratio", 0)

        summary += f"2. **Energy Balance**: "
        if ebr > 0:
            if 0.7 <= ebr <= 1.0:
                summary += f"Good energy balance closure ({ebr:.2f}). "
            else:
                summary += f"Energy balance closure issues detected ({ebr:.2f}). "

        if bowen > 0:
            if bowen > 2:
                summary += "Site is sensible heat dominated (limited evapotranspiration).\n\n"
            else:
                summary += "Good balance between sensible and latent heat fluxes.\n\n"

    # Thermal comfort insights
    if thermal_metrics and "stress_hours" in thermal_metrics:
        stress_hours = thermal_metrics["stress_hours"]
        summary += f"3. **Thermal Comfort**: "
        summary += f"{stress_hours} hours of heat stress conditions detected. "
        summary += "See thermal comfort section for mitigation strategies.\n\n"

    # Urban characteristics
    veg_frac = context["site_characteristics"].get("vegetation_fraction", 0)
    built_frac = context["site_characteristics"].get("built_fraction", 0)

    summary += f"4. **Urban Form Impact**: "
    if built_frac > 0.7:
        summary += "Highly urbanized site with significant heat storage. "
    elif veg_frac > 0.3:
        summary += "Vegetation provides cooling benefits. "
    else:
        summary += "Mixed urban environment. "
    summary += "See urban effects section for design recommendations.\n\n"

    return summary


def generate_methodology_section(context: Dict[str, Any]) -> str:
    """Generate methodology section explaining the simulation setup."""

    method = "## Methodology\n\n"

    method += "### Simulation Setup\n\n"
    method += f"- **Location**: {context['simulation_details']['location']}\n"
    method += f"- **Period**: {context['simulation_details']['start_date']} to "
    method += f"{context['simulation_details']['end_date']}\n"
    method += f"- **Time Step**: {context['simulation_details']['time_step']}s\n\n"

    method += "### Site Characteristics\n\n"
    site = context["site_characteristics"]
    method += f"- **Mean Building Height**: {site.get('mean_building_height', 0):.1f} m\n"
    method += f"- **Vegetation Cover**: {site.get('vegetation_fraction', 0) * 100:.0f}%\n"
    method += f"- **Built Area**: {site.get('built_fraction', 0) * 100:.0f}%\n"
    method += f"- **Population Density**: {site.get('population_density', 0):.0f} people/ha\n\n"

    method += "### Model Configuration\n\n"
    params = context["key_parameters"]
    method += f"- **Radiation Scheme**: Method {params.get('net_radiation_method', 'N/A')}\n"
    method += f"- **Storage Heat**: Method {params.get('storage_heat_method', 'N/A')}\n"
    method += f"- **Stability**: Method {params.get('stability_method', 'N/A')}\n\n"

    return method


def generate_detailed_results(df: pd.DataFrame) -> str:
    """Generate detailed results section with key statistics."""

    results = "## Detailed Results\n\n"

    # Climate variables
    if "T2" in df.columns:
        results += "### Air Temperature (2m)\n"
        results += f"- Mean: {df['T2'].mean():.1f}°C\n"
        results += f"- Range: {df['T2'].min():.1f} to {df['T2'].max():.1f}°C\n"
        results += f"- Standard deviation: {df['T2'].std():.1f}°C\n\n"

    if "RH2" in df.columns:
        results += "### Relative Humidity (2m)\n"
        results += f"- Mean: {df['RH2'].mean():.0f}%\n"
        results += f"- Range: {df['RH2'].min():.0f} to {df['RH2'].max():.0f}%\n\n"

    # Energy fluxes
    energy_vars = ["QN", "QH", "QE", "QS", "QF"]
    available_energy = [v for v in energy_vars if v in df.columns]

    if available_energy:
        results += "### Energy Fluxes (W/m²)\n"
        results += "| Component | Mean | Max | Daytime Mean |\n"
        results += "|-----------|------|-----|-------------|\n"

        for var in available_energy:
            daytime = df[df["QN"] > 50][var].mean() if "QN" in df.columns else df[var].mean()
            results += f"| {var} | {df[var].mean():.0f} | {df[var].max():.0f} | {daytime:.0f} |\n"
        results += "\n"

    # Water balance
    if "SMD" in df.columns:
        results += "### Soil Moisture\n"
        results += f"- Mean deficit: {df['SMD'].mean():.1f} mm\n"
        results += f"- Maximum deficit: {df['SMD'].max():.1f} mm\n"
        dry_days = (df["SMD"] > df["SMD"].quantile(0.9)).sum() / 24
        results += f"- Dry periods: {dry_days:.0f} days\n\n"

    return results


def generate_interpretation_section(
    df: pd.DataFrame, context: Dict[str, Any], research_context: str
) -> str:
    """Generate interpretation section based on research context."""

    interp = "## Interpretation & Insights\n\n"

    # Parse research context to identify focus areas
    research_lower = research_context.lower()

    # Heat mitigation focus
    if any(word in research_lower for word in ["heat", "cooling", "mitigation", "uhi"]):
        interp += "### Urban Heat Mitigation\n\n"

        if "T2" in df.columns:
            hot_hours = (df["T2"] > 30).sum()
            extreme_hours = (df["T2"] > 35).sum()

            interp += f"The simulation shows {hot_hours} hours above 30°C "
            interp += f"and {extreme_hours} hours above 35°C. "

            if hot_hours > 1000:  # ~40 days worth
                interp += "This indicates significant heat stress requiring mitigation:\n\n"
                interp += "1. **Increase vegetation** - Target 40% canopy coverage\n"
                interp += "2. **Cool surfaces** - Implement high-albedo materials\n"
                interp += "3. **Water features** - Add fountains or misting systems\n"
                interp += "4. **Shade structures** - Design for peak sun hours\n\n"
            else:
                interp += "Heat conditions are moderate but can be improved through:\n\n"
                interp += "1. **Strategic greening** in high-traffic areas\n"
                interp += "2. **Cool pavements** in parking areas\n"
                interp += "3. **Building design** optimization\n\n"

    # Energy efficiency focus
    if any(word in research_lower for word in ["energy", "efficiency", "consumption"]):
        interp += "### Energy Implications\n\n"

        if "QF" in df.columns:
            qf_mean = df["QF"].mean()
            qf_summer = (
                df[df.index.month.isin([6, 7, 8])]["QF"].mean() if len(df) > 365 * 24 else qf_mean
            )

            interp += f"Anthropogenic heat flux averages {qf_mean:.0f} W/m², "
            interp += f"with summer values of {qf_summer:.0f} W/m². "

            if qf_summer > qf_mean * 1.2:
                interp += "Higher summer values indicate cooling demand:\n\n"
                interp += "1. **Passive cooling** - Natural ventilation design\n"
                interp += "2. **Shading** - External shading devices\n"
                interp += "3. **Insulation** - Reduce heat gain\n"
                interp += "4. **Efficient systems** - High-performance HVAC\n\n"

    # Water management focus
    if any(word in research_lower for word in ["water", "runoff", "drainage", "flood"]):
        interp += "### Water Management\n\n"

        if "Runoff" in df.columns:
            runoff_total = df["Runoff"].sum()
            rain_total = df["Rain"].sum() if "Rain" in df.columns else 0

            if rain_total > 0:
                runoff_ratio = runoff_total / rain_total
                interp += f"Runoff ratio: {runoff_ratio:.2%} of precipitation. "

                if runoff_ratio > 0.5:
                    interp += "High runoff indicates need for:\n\n"
                    interp += "1. **Green infrastructure** - Bioswales, rain gardens\n"
                    interp += "2. **Permeable surfaces** - Porous pavements\n"
                    interp += "3. **Retention** - Underground storage\n"
                    interp += "4. **Harvesting** - Rainwater collection systems\n\n"

    # Climate adaptation focus
    if any(word in research_lower for word in ["climate", "adaptation", "resilience"]):
        interp += "### Climate Resilience\n\n"
        interp += "Based on the simulation results, key adaptation priorities include:\n\n"

        # Temperature resilience
        if "T2" in df.columns and df["T2"].max() > 35:
            interp += "1. **Heat Resilience**\n"
            interp += "   - Create cooling centers and shaded pathways\n"
            interp += "   - Implement heat warning systems\n"
            interp += "   - Design for passive survivability\n\n"

        # Water resilience
        if "SMD" in df.columns and df["SMD"].max() > 100:
            interp += "2. **Drought Resilience**\n"
            interp += "   - Drought-tolerant landscaping\n"
            interp += "   - Smart irrigation systems\n"
            interp += "   - Greywater recycling\n\n"

        interp += "3. **Integrated Approach**\n"
        interp += "   - Multi-functional green infrastructure\n"
        interp += "   - Community engagement programs\n"
        interp += "   - Adaptive management strategies\n\n"

    return interp


def generate_recommendations(
    df: pd.DataFrame, context: Dict[str, Any], issues_detected: List[Dict]
) -> str:
    """Generate actionable recommendations."""

    rec = "## Recommendations\n\n"

    # Priority matrix
    rec += "### Priority Actions\n\n"

    priorities = []

    # Check for heat issues
    if "T2" in df.columns and df["T2"].max() > 35:
        priorities.append(
            {
                "priority": "HIGH",
                "action": "Implement urban cooling strategies",
                "timeline": "Immediate (0-6 months)",
                "details": "Focus on high-traffic areas and vulnerable populations",
            }
        )

    # Check for energy balance issues
    if issues_detected:
        for issue in issues_detected:
            if issue["severity"] == "high":
                priorities.append(
                    {
                        "priority": "HIGH",
                        "action": issue["issue"],
                        "timeline": "Short-term (6-12 months)",
                        "details": issue["suggestions"][0],
                    }
                )

    # Check vegetation deficit
    veg_frac = context["site_characteristics"].get("vegetation_fraction", 0)
    if veg_frac < 0.2:
        priorities.append(
            {
                "priority": "MEDIUM",
                "action": "Increase urban greenery",
                "timeline": "Medium-term (1-2 years)",
                "details": "Target 30% vegetation coverage through parks and street trees",
            }
        )

    # Format priorities
    if priorities:
        rec += "| Priority | Action | Timeline | Details |\n"
        rec += "|----------|--------|----------|---------|"
        for p in priorities:
            rec += f"\n| **{p['priority']}** | {p['action']} | {p['timeline']} | {p['details']} |"
        rec += "\n\n"

    # Detailed recommendations by category
    rec += "### Detailed Recommendations\n\n"

    # Urban design
    rec += "#### Urban Design & Planning\n"
    rec += "1. Optimize building orientation for passive cooling\n"
    rec += "2. Create green corridors aligned with wind patterns\n"
    rec += "3. Implement district-level cooling strategies\n"
    rec += "4. Design for pedestrian comfort with shading\n\n"

    # Green infrastructure
    rec += "#### Green Infrastructure\n"
    rec += "1. Establish minimum green space requirements\n"
    rec += "2. Prioritize native, drought-resistant species\n"
    rec += "3. Create multi-functional green spaces\n"
    rec += "4. Implement green walls and roofs\n\n"

    # Technology solutions
    rec += "#### Technology & Innovation\n"
    rec += "1. Smart irrigation systems with moisture sensors\n"
    rec += "2. Cool pavement technologies\n"
    rec += "3. Building energy management systems\n"
    rec += "4. Real-time urban climate monitoring\n\n"

    # Policy recommendations
    rec += "#### Policy & Governance\n"
    rec += "1. Update building codes for climate resilience\n"
    rec += "2. Incentivize green building practices\n"
    rec += "3. Develop heat action plans\n"
    rec += "4. Integrate climate considerations in planning\n\n"

    return rec


def generate_conclusions(key_findings: List[str]) -> str:
    """Generate conclusions section."""

    conc = "## Conclusions\n\n"
    conc += "This analysis of SUEWS simulation results provides valuable insights "
    conc += "into urban climate dynamics and opportunities for improvement. "
    conc += "Key conclusions include:\n\n"

    for i, finding in enumerate(key_findings, 1):
        conc += f"{i}. {finding}\n"

    conc += "\n### Next Steps\n\n"
    conc += "1. **Validation**: Compare results with observational data\n"
    conc += "2. **Sensitivity Analysis**: Test parameter variations\n"
    conc += "3. **Scenario Testing**: Evaluate proposed interventions\n"
    conc += "4. **Implementation Planning**: Develop action plans\n"
    conc += "5. **Monitoring**: Establish evaluation metrics\n\n"

    conc += "### Data Availability\n\n"
    conc += "All simulation outputs and configuration files are available "
    conc += "for further analysis. Additional scenarios can be run to "
    conc += "evaluate specific interventions or future climate conditions.\n"

    return conc


async def generate_insights_report(
    output_path: str, config_path: str, research_context: str
) -> str:
    """Generate comprehensive insights report from SUEWS results."""

    try:
        # Load simulation output
        output_p = Path(output_path)
        if output_p.suffix == ".csv":
            df = pd.read_csv(output_p, parse_dates=["datetime"], index_col="datetime")
        elif output_p.suffix == ".txt":
            df = pd.read_csv(output_p, sep=r"\s+")
            df["datetime"] = pd.to_datetime(
                df["Year"].astype(str) + df["DOY"].astype(str), format="%Y%j"
            )
            df["datetime"] += pd.to_timedelta(df["Hour"], unit="h") + pd.to_timedelta(
                df.get("Min", 0), unit="m"
            )
            df.set_index("datetime", inplace=True)
        else:
            return f"Error: Unsupported output format '{output_p.suffix}'"

        # Load configuration
        config = load_configuration(config_path)
        if "error" in config:
            return f"Error loading configuration: {config['error']}"

        # Analyze context
        context = analyze_simulation_context(config, research_context)

        # Calculate key metrics
        energy_metrics = calculate_energy_balance_metrics(df)
        issues = diagnose_issues(energy_metrics) if "error" not in energy_metrics else []

        # Calculate thermal comfort metrics
        thermal_metrics = {}
        if "T2" in df.columns and "RH2" in df.columns:
            humidex = calculate_humidex(df["T2"], df["RH2"])
            thermal_metrics["stress_hours"] = (humidex >= 40).sum()

        # Start building report
        report = f"# SUEWS Urban Climate Simulation Report\n\n"
        report += f"**Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M')}\n"
        report += f"**Research Context**: {research_context}\n\n"
        report += "---\n\n"

        # Executive summary
        report += generate_executive_summary(df, context, energy_metrics, thermal_metrics)

        # Methodology
        report += generate_methodology_section(context)

        # Detailed results
        report += generate_detailed_results(df)

        # Interpretation
        report += generate_interpretation_section(df, context, research_context)

        # Energy balance analysis
        if energy_metrics and "error" not in energy_metrics:
            report += "## Energy Balance Analysis\n\n"
            report += f"- **Closure Ratio**: {energy_metrics.get('energy_balance_ratio', 0):.2f}\n"
            report += f"- **Bowen Ratio**: {energy_metrics.get('bowen_ratio', 0):.2f}\n"
            report += f"- **Mean Residual**: {energy_metrics.get('mean_residual', 0):.0f} W/m²\n\n"

            if issues:
                report += "### Issues Detected\n\n"
                for issue in issues:
                    report += f"- **{issue['issue']}** ({issue['severity']})\n"
                    report += f"  - {issue['details']}\n\n"

        # Key findings for conclusions
        key_findings = []

        if "T2" in df.columns:
            if df["T2"].max() > 35:
                key_findings.append("Extreme heat conditions require urgent mitigation strategies")
            else:
                key_findings.append("Temperature conditions are within manageable ranges")

        if energy_metrics and "energy_balance_ratio" in energy_metrics:
            ebr = energy_metrics["energy_balance_ratio"]
            if 0.7 <= ebr <= 1.0:
                key_findings.append("Energy balance shows good model performance")
            else:
                key_findings.append("Energy balance closure needs investigation")

        veg_frac = context["site_characteristics"].get("vegetation_fraction", 0)
        if veg_frac < 0.2:
            key_findings.append("Limited vegetation coverage reduces cooling potential")
        elif veg_frac > 0.4:
            key_findings.append("Good vegetation coverage provides ecosystem services")

        # Recommendations
        report += generate_recommendations(df, context, issues)

        # Conclusions
        report += generate_conclusions(key_findings)

        # Appendices
        report += "\n---\n\n"
        report += "## Appendices\n\n"
        report += "### A. Glossary\n\n"
        report += "- **QN**: Net all-wave radiation\n"
        report += "- **QH**: Sensible heat flux\n"
        report += "- **QE**: Latent heat flux\n"
        report += "- **QS**: Storage heat flux\n"
        report += "- **QF**: Anthropogenic heat flux\n"
        report += "- **UHI**: Urban Heat Island\n"
        report += "- **Bowen Ratio**: QH/QE\n\n"

        report += "### B. Data Quality\n\n"
        report += f"- **Time Coverage**: {len(df):,} hourly records\n"
        report += f"- **Variables Available**: {len(df.columns)}\n"
        report += f"- **Missing Data**: {df.isnull().sum().sum()} values\n\n"

        report += "### C. Contact Information\n\n"
        report += "For questions about this report or the SUEWS model:\n"
        report += "- SUEWS Documentation: https://suews.readthedocs.io\n"
        report += "- Model Support: https://github.com/UMEP-dev/SUEWS\n"

        return report

    except FileNotFoundError:
        return f"Error: File not found - check paths for '{output_path}' and '{config_path}'"
    except Exception as e:
        return f"Error generating insights report: {str(e)}"
