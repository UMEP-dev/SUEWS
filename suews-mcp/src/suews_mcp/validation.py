"""Enhanced validation utilities with intelligent error suggestions.

Provides comprehensive validation for SUEWS configurations, forcing data,
and simulation parameters with detailed feedback and suggestions.
"""

import logging
from typing import Dict, List, Optional, Tuple, Any, Set
from dataclasses import dataclass, field
from pathlib import Path
import json
import yaml
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

from .error_handler import (
    ErrorContext,
    ErrorCategory,
    ErrorSeverity,
    handle_error,
    SUEWSErrorHandler,
)

logger = logging.getLogger(__name__)


@dataclass
class ValidationRule:
    """A validation rule with context."""

    name: str
    description: str
    check_function: callable
    severity: ErrorSeverity
    category: str
    suggestions: List[str] = field(default_factory=list)
    documentation_link: Optional[str] = None


@dataclass
class ValidationResult:
    """Result of validation with detailed feedback."""

    passed: bool
    total_checks: int
    passed_checks: int
    failed_checks: int
    issues: List[Dict[str, Any]]
    suggestions: List[str]
    summary: str

    def to_dict(self) -> Dict:
        """Convert to dictionary for JSON serialization."""
        return {
            "passed": self.passed,
            "total_checks": self.total_checks,
            "passed_checks": self.passed_checks,
            "failed_checks": self.failed_checks,
            "issues": self.issues,
            "suggestions": self.suggestions,
            "summary": self.summary,
        }

    def format_report(self) -> str:
        """Format validation report for display."""
        lines = [
            "\n" + "=" * 60,
            "VALIDATION REPORT",
            "=" * 60,
            f"Status: {'✅ PASSED' if self.passed else '❌ FAILED'}",
            f"Checks: {self.passed_checks}/{self.total_checks} passed",
            "",
            self.summary,
            "",
        ]

        if self.issues:
            lines.append("Issues Found:")
            lines.append("-" * 40)
            for issue in self.issues:
                severity_icon = {
                    "critical": "🔴",
                    "error": "❌",
                    "warning": "⚠️",
                    "info": "ℹ️",
                }.get(issue["severity"], "")

                lines.append(f"{severity_icon} {issue['message']}")
                if issue.get("details"):
                    lines.append(f"   Details: {issue['details']}")
                if issue.get("suggestions"):
                    for suggestion in issue["suggestions"]:
                        lines.append(f"   → {suggestion}")
            lines.append("")

        if self.suggestions:
            lines.append("General Suggestions:")
            lines.append("-" * 40)
            for suggestion in self.suggestions:
                lines.append(f"• {suggestion}")
            lines.append("")

        lines.append("=" * 60)
        return "\n".join(lines)


class ConfigurationValidator:
    """Validates SUEWS configuration files with intelligent feedback."""

    def __init__(self):
        self.rules = self._initialize_rules()
        self.error_handler = SUEWSErrorHandler()

    def _initialize_rules(self) -> List[ValidationRule]:
        """Initialize validation rules."""
        return [
            ValidationRule(
                name="surface_fractions",
                description="Surface fractions must sum to 1.0",
                check_function=self._check_surface_fractions,
                severity=ErrorSeverity.ERROR,
                category="physics",
                suggestions=[
                    "Ensure paved + buildings + evergreen + deciduous + grass + bare + water = 1.0",
                    "Check for rounding errors in fraction values",
                    "Use the normalize_fractions utility if needed",
                ],
                documentation_link="https://suews.readthedocs.io/en/latest/input_files/SUEWS_SiteInfo.html",
            ),
            ValidationRule(
                name="building_heights",
                description="Building heights must be physically reasonable",
                check_function=self._check_building_heights,
                severity=ErrorSeverity.WARNING,
                category="morphology",
                suggestions=[
                    "Typical urban building heights range from 3-50m",
                    "Check if height units are correct (meters)",
                    "Verify mean height is less than or equal to max height",
                ],
            ),
            ValidationRule(
                name="initial_conditions",
                description="Initial conditions must be within valid ranges",
                check_function=self._check_initial_conditions,
                severity=ErrorSeverity.ERROR,
                category="initialization",
                suggestions=[
                    "Temperature typically between -40 and 50°C",
                    "Relative humidity between 0 and 100%",
                    "Soil moisture between 0 and saturation",
                ],
            ),
            ValidationRule(
                name="time_settings",
                description="Time settings must be consistent",
                check_function=self._check_time_settings,
                severity=ErrorSeverity.ERROR,
                category="temporal",
                suggestions=[
                    "Ensure dt (timestep) divides evenly into 3600 seconds",
                    "Check that start_date is before end_date",
                    "Verify timezone settings are correct",
                ],
            ),
            ValidationRule(
                name="file_paths",
                description="All referenced files must exist",
                check_function=self._check_file_paths,
                severity=ErrorSeverity.CRITICAL,
                category="io",
                suggestions=[
                    "Use absolute paths for clarity",
                    "Check file permissions",
                    "Ensure paths use correct separators for your OS",
                ],
            ),
            ValidationRule(
                name="anthropogenic_heat",
                description="Anthropogenic heat settings must be reasonable",
                check_function=self._check_anthropogenic_heat,
                severity=ErrorSeverity.WARNING,
                category="energy",
                suggestions=[
                    "Typical QF values range from 0-500 W/m²",
                    "Check diurnal profiles sum to reasonable daily totals",
                    "Verify weekday/weekend patterns make sense",
                ],
            ),
            ValidationRule(
                name="vegetation_parameters",
                description="Vegetation parameters must be consistent",
                check_function=self._check_vegetation,
                severity=ErrorSeverity.WARNING,
                category="vegetation",
                suggestions=[
                    "LAI typically ranges from 0-8",
                    "Ensure seasonal patterns are appropriate for location",
                    "Check that vegetation fractions match surface fractions",
                ],
            ),
            ValidationRule(
                name="water_balance",
                description="Water balance parameters must be physical",
                check_function=self._check_water_balance,
                severity=ErrorSeverity.WARNING,
                category="hydrology",
                suggestions=[
                    "Runoff coefficients between 0 and 1",
                    "Drainage rates must be positive",
                    "Initial storage should not exceed capacity",
                ],
            ),
        ]

    def validate(self, config_path: Path, strict: bool = False) -> ValidationResult:
        """Validate a configuration file."""
        issues = []
        suggestions = set()
        passed_checks = 0

        try:
            # Load configuration
            config = self._load_config(config_path)

            # Run all validation rules
            for rule in self.rules:
                try:
                    result = rule.check_function(config)
                    if result["passed"]:
                        passed_checks += 1
                    else:
                        # Skip info level in strict mode if not critical
                        if strict and rule.severity == ErrorSeverity.INFO:
                            passed_checks += 1
                            continue

                        issues.append(
                            {
                                "rule": rule.name,
                                "message": result["message"],
                                "severity": rule.severity.value,
                                "category": rule.category,
                                "details": result.get("details", ""),
                                "suggestions": rule.suggestions,
                                "documentation": rule.documentation_link,
                            }
                        )
                        suggestions.update(rule.suggestions)

                except Exception as e:
                    logger.error(f"Error in validation rule {rule.name}: {e}")
                    issues.append(
                        {
                            "rule": rule.name,
                            "message": f"Validation rule failed: {str(e)}",
                            "severity": "error",
                            "category": rule.category,
                        }
                    )

            # Determine overall pass/fail
            critical_issues = [
                i for i in issues if i["severity"] in ["critical", "error"]
            ]
            passed = len(critical_issues) == 0

            # Create summary
            if passed:
                summary = "Configuration validation successful. Ready for simulation."
            else:
                summary = f"Configuration has {len(critical_issues)} critical issues that must be resolved."

            return ValidationResult(
                passed=passed,
                total_checks=len(self.rules),
                passed_checks=passed_checks,
                failed_checks=len(self.rules) - passed_checks,
                issues=issues,
                suggestions=list(suggestions),
                summary=summary,
            )

        except Exception as e:
            # Handle configuration loading errors
            context = ErrorContext(
                operation="configuration_validation", file_path=config_path
            )
            diagnosed = handle_error(e, "loading configuration", file_path=config_path)

            return ValidationResult(
                passed=False,
                total_checks=len(self.rules),
                passed_checks=0,
                failed_checks=len(self.rules),
                issues=[
                    {
                        "rule": "file_loading",
                        "message": diagnosed.message,
                        "severity": "critical",
                        "category": "io",
                        "suggestions": diagnosed.suggestions,
                    }
                ],
                suggestions=diagnosed.suggestions,
                summary=f"Failed to load configuration: {diagnosed.root_cause}",
            )

    def _load_config(self, config_path: Path) -> Dict:
        """Load configuration file."""
        if not config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_path}")

        with open(config_path, "r") as f:
            if config_path.suffix == ".json":
                return json.load(f)
            elif config_path.suffix in [".yml", ".yaml"]:
                return yaml.safe_load(f)
            else:
                raise ValueError(
                    f"Unsupported configuration format: {config_path.suffix}"
                )

    def _check_surface_fractions(self, config: Dict) -> Dict:
        """Check surface fractions sum to 1."""
        fractions = []
        fraction_names = [
            "fr_paved",
            "fr_bldgs",
            "fr_evetr",
            "fr_dectr",
            "fr_grass",
            "fr_bsoil",
            "fr_water",
        ]

        for name in fraction_names:
            value = config.get("surfaces", {}).get(name, 0)
            fractions.append(value)

        total = sum(fractions)
        tolerance = 0.001

        if abs(total - 1.0) < tolerance:
            return {"passed": True, "message": "Surface fractions valid"}
        else:
            return {
                "passed": False,
                "message": f"Surface fractions sum to {total:.4f}, should be 1.0",
                "details": f"Fractions: {dict(zip(fraction_names, fractions))}",
            }

    def _check_building_heights(self, config: Dict) -> Dict:
        """Check building height consistency."""
        bldg_data = config.get("morphology", {})
        h_mean = bldg_data.get("h_bldg_mean", 0)
        h_max = bldg_data.get("h_bldg_max", 0)

        issues = []
        if h_mean > h_max and h_max > 0:
            issues.append(f"Mean height ({h_mean}m) > max height ({h_max}m)")
        if h_mean < 0 or h_max < 0:
            issues.append("Negative building heights detected")
        if h_mean > 200:  # Very tall buildings
            issues.append(f"Unusually tall buildings ({h_mean}m mean)")

        if issues:
            return {
                "passed": False,
                "message": "Building height issues detected",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Building heights valid"}

    def _check_initial_conditions(self, config: Dict) -> Dict:
        """Check initial condition ranges."""
        init = config.get("initial_conditions", {})
        issues = []

        # Temperature checks
        temp = init.get("temp_c", 20)
        if temp < -50 or temp > 60:
            issues.append(f"Temperature {temp}°C outside valid range [-50, 60]")

        # Relative humidity checks
        rh = init.get("rh_pct", 50)
        if rh < 0 or rh > 100:
            issues.append(f"RH {rh}% outside valid range [0, 100]")

        # Pressure checks
        pressure = init.get("pressure_hpa", 1013)
        if pressure < 850 or pressure > 1100:
            issues.append(f"Pressure {pressure} hPa outside valid range [850, 1100]")

        if issues:
            return {
                "passed": False,
                "message": "Initial conditions outside valid ranges",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Initial conditions valid"}

    def _check_time_settings(self, config: Dict) -> Dict:
        """Check time configuration consistency."""
        time_config = config.get("time", {})
        issues = []

        # Check timestep
        dt = time_config.get("dt", 3600)
        if 3600 % dt != 0:
            issues.append(f"Timestep {dt}s doesn't divide evenly into 3600s")
        if dt < 60 or dt > 3600:
            issues.append(f"Timestep {dt}s outside recommended range [60, 3600]")

        # Check date range
        start = time_config.get("start_date")
        end = time_config.get("end_date")
        if start and end:
            try:
                start_dt = pd.to_datetime(start)
                end_dt = pd.to_datetime(end)
                if start_dt >= end_dt:
                    issues.append("Start date is after end date")
                if (end_dt - start_dt).days > 365:
                    issues.append(
                        "Simulation period > 1 year (may use excessive memory)"
                    )
            except:
                issues.append("Invalid date format")

        if issues:
            return {
                "passed": False,
                "message": "Time setting issues detected",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Time settings valid"}

    def _check_file_paths(self, config: Dict) -> Dict:
        """Check that all referenced files exist."""
        missing_files = []

        # Check forcing data path
        forcing_path = config.get("forcing", {}).get("data_path")
        if forcing_path:
            path = Path(forcing_path)
            if not path.exists():
                missing_files.append(f"Forcing data: {forcing_path}")

        # Check other file references
        for section in ["irrigation", "anthropogenic", "vegetation"]:
            section_config = config.get(section, {})
            if "file_path" in section_config:
                path = Path(section_config["file_path"])
                if not path.exists():
                    missing_files.append(f"{section}: {section_config['file_path']}")

        if missing_files:
            return {
                "passed": False,
                "message": "Referenced files not found",
                "details": "; ".join(missing_files),
            }
        return {"passed": True, "message": "All file paths valid"}

    def _check_anthropogenic_heat(self, config: Dict) -> Dict:
        """Check anthropogenic heat settings."""
        qf_config = config.get("anthropogenic", {})
        issues = []

        # Check QF magnitude
        qf_max = qf_config.get("qf_max_summer", 0)
        if qf_max > 500:
            issues.append(f"Very high QF max ({qf_max} W/m²)")
        if qf_max < 0:
            issues.append("Negative QF values")

        # Check diurnal profile if present
        profile = qf_config.get("diurnal_profile", [])
        if profile:
            if len(profile) != 24:
                issues.append(f"Diurnal profile has {len(profile)} hours, expected 24")
            if any(v < 0 for v in profile):
                issues.append("Negative values in diurnal profile")

        if issues:
            return {
                "passed": False,
                "message": "Anthropogenic heat issues detected",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Anthropogenic heat settings valid"}

    def _check_vegetation(self, config: Dict) -> Dict:
        """Check vegetation parameter consistency."""
        veg_config = config.get("vegetation", {})
        issues = []

        # Check LAI ranges
        lai_max = veg_config.get("lai_max", 5)
        if lai_max < 0 or lai_max > 10:
            issues.append(f"LAI max {lai_max} outside typical range [0, 10]")

        # Check vegetation fractions match surface fractions
        veg_frac = veg_config.get("veg_fraction", 0)
        surface_veg = sum(
            [
                config.get("surfaces", {}).get("fr_evetr", 0),
                config.get("surfaces", {}).get("fr_dectr", 0),
                config.get("surfaces", {}).get("fr_grass", 0),
            ]
        )

        if abs(veg_frac - surface_veg) > 0.01:
            issues.append(f"Vegetation fraction mismatch: {veg_frac} vs {surface_veg}")

        if issues:
            return {
                "passed": False,
                "message": "Vegetation parameter issues detected",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Vegetation parameters valid"}

    def _check_water_balance(self, config: Dict) -> Dict:
        """Check water balance parameters."""
        water_config = config.get("water", {})
        issues = []

        # Check runoff coefficients
        runoff = water_config.get("runoff_coef", {})
        for surface, coef in runoff.items():
            if coef < 0 or coef > 1:
                issues.append(f"Runoff coefficient for {surface}: {coef} outside [0,1]")

        # Check storage capacities
        capacity = water_config.get("storage_capacity", 0)
        initial = water_config.get("initial_storage", 0)
        if initial > capacity:
            issues.append(f"Initial storage ({initial}mm) > capacity ({capacity}mm)")

        if issues:
            return {
                "passed": False,
                "message": "Water balance issues detected",
                "details": "; ".join(issues),
            }
        return {"passed": True, "message": "Water balance parameters valid"}


class ForcingDataValidator:
    """Validates forcing data with intelligent feedback."""

    def __init__(self):
        self.required_columns = [
            "iy",
            "id",
            "it",
            "imin",  # Time columns
            "qn1",
            "qh",
            "qe",
            "qs",
            "qf",  # Energy fluxes
            "U",
            "RH",
            "Tair",
            "pres",  # Meteorological
            "rain",
            "kdown",
            "snow",
            "ldown",  # Precipitation and radiation
            "fcld",
            "Wuh",
            "xsmd",
            "lai",  # Additional variables
            "kdiff",
            "kdir",
            "wdir",  # Radiation components
        ]
        self.error_handler = SUEWSErrorHandler()

    def validate(self, data_path: Path) -> ValidationResult:
        """Validate forcing data file."""
        issues = []
        suggestions = set()
        passed_checks = 0
        total_checks = 0

        try:
            # Load data
            df = self._load_forcing_data(data_path)

            # Check 1: Required columns
            total_checks += 1
            missing_cols = self._check_required_columns(df)
            if not missing_cols:
                passed_checks += 1
            else:
                issues.append(
                    {
                        "message": f"Missing required columns: {missing_cols}",
                        "severity": "error",
                        "suggestions": [
                            "Check column names are correct (case-sensitive)",
                            "Refer to forcing data documentation for required columns",
                            "Use the data format converter if needed",
                        ],
                    }
                )
                suggestions.add(
                    "Ensure all required meteorological variables are present"
                )

            # Check 2: Time continuity
            total_checks += 1
            time_issues = self._check_time_continuity(df)
            if not time_issues:
                passed_checks += 1
            else:
                issues.extend(time_issues)
                suggestions.add("Use gap-filling methods for missing time periods")

            # Check 3: Physical ranges
            total_checks += 1
            range_issues = self._check_physical_ranges(df)
            if not range_issues:
                passed_checks += 1
            else:
                issues.extend(range_issues)
                suggestions.add("Apply quality control filters to remove outliers")

            # Check 4: Data completeness
            total_checks += 1
            completeness = self._check_data_completeness(df)
            if completeness["passed"]:
                passed_checks += 1
            else:
                issues.append(completeness["issue"])
                suggestions.update(completeness["suggestions"])

            # Overall assessment
            critical_count = sum(
                1 for i in issues if i.get("severity") in ["critical", "error"]
            )
            passed = critical_count == 0

            if passed:
                summary = f"Forcing data validation successful. {len(df)} timesteps ready for simulation."
            else:
                summary = (
                    f"Forcing data has {critical_count} critical issues to resolve."
                )

            return ValidationResult(
                passed=passed,
                total_checks=total_checks,
                passed_checks=passed_checks,
                failed_checks=total_checks - passed_checks,
                issues=issues,
                suggestions=list(suggestions),
                summary=summary,
            )

        except Exception as e:
            diagnosed = handle_error(e, "validating forcing data", file_path=data_path)

            return ValidationResult(
                passed=False,
                total_checks=total_checks,
                passed_checks=0,
                failed_checks=total_checks,
                issues=[
                    {
                        "message": diagnosed.message,
                        "severity": "critical",
                        "suggestions": diagnosed.suggestions,
                    }
                ],
                suggestions=diagnosed.suggestions,
                summary=f"Failed to validate forcing data: {diagnosed.root_cause}",
            )

    def _load_forcing_data(self, data_path: Path) -> pd.DataFrame:
        """Load forcing data file."""
        if not data_path.exists():
            raise FileNotFoundError(f"Forcing data file not found: {data_path}")

        # Try to read with different delimiters
        for sep in ["\t", " ", ","]:
            try:
                df = pd.read_csv(data_path, sep=sep, skipinitialspace=True)
                if len(df.columns) > 1:  # Successfully parsed
                    return df
            except:
                continue

        raise ValueError(f"Could not parse forcing data file: {data_path}")

    def _check_required_columns(self, df: pd.DataFrame) -> List[str]:
        """Check for required columns."""
        return [col for col in self.required_columns if col not in df.columns]

    def _check_time_continuity(self, df: pd.DataFrame) -> List[Dict]:
        """Check time series continuity."""
        issues = []

        if all(col in df.columns for col in ["iy", "id", "it", "imin"]):
            # Create datetime index
            try:
                df["datetime"] = pd.to_datetime(
                    df["iy"].astype(str)
                    + df["id"].astype(str).str.zfill(3)
                    + df["it"].astype(str).str.zfill(2)
                    + df["imin"].astype(str).str.zfill(2),
                    format="%Y%j%H%M",
                )

                # Check for gaps
                time_diff = df["datetime"].diff()
                expected_diff = time_diff.mode()[0]
                gaps = df[time_diff > expected_diff * 1.5]

                if not gaps.empty:
                    issues.append(
                        {
                            "message": f"Found {len(gaps)} time gaps in data",
                            "severity": "warning",
                            "suggestions": [
                                "Use interpolation for small gaps",
                                "Check data logger for failures",
                                "Consider using gap-filling from reanalysis data",
                            ],
                        }
                    )

            except Exception as e:
                issues.append(
                    {
                        "message": f"Could not parse time columns: {str(e)}",
                        "severity": "error",
                        "suggestions": ["Check time column format (iy, id, it, imin)"],
                    }
                )

        return issues

    def _check_physical_ranges(self, df: pd.DataFrame) -> List[Dict]:
        """Check if values are within physical ranges."""
        issues = []

        # Define reasonable ranges
        ranges = {
            "Tair": (-50, 60, "Temperature"),
            "RH": (0, 100, "Relative humidity"),
            "pres": (850, 1100, "Pressure"),
            "U": (0, 50, "Wind speed"),
            "rain": (0, 500, "Rainfall rate"),
            "kdown": (0, 1500, "Shortwave radiation"),
            "ldown": (50, 600, "Longwave radiation"),
        }

        for col, (vmin, vmax, name) in ranges.items():
            if col in df.columns:
                out_of_range = df[(df[col] < vmin) | (df[col] > vmax)]
                if not out_of_range.empty:
                    pct = len(out_of_range) / len(df) * 100
                    severity = "error" if pct > 10 else "warning"

                    issues.append(
                        {
                            "message": f"{name} has {len(out_of_range)} values outside [{vmin}, {vmax}] ({pct:.1f}%)",
                            "severity": severity,
                            "suggestions": [
                                f"Check units for {col}",
                                "Look for sensor calibration issues",
                                "Apply quality control filters",
                            ],
                        }
                    )

        return issues

    def _check_data_completeness(self, df: pd.DataFrame) -> Dict:
        """Check data completeness."""
        missing_pct = df.isnull().sum() / len(df) * 100
        critical_vars = ["Tair", "RH", "U", "pres", "kdown"]

        critical_missing = {
            col: pct
            for col, pct in missing_pct.items()
            if col in critical_vars and pct > 5
        }

        if critical_missing:
            return {
                "passed": False,
                "issue": {
                    "message": f"Critical variables have >5% missing data: {critical_missing}",
                    "severity": "error",
                    "suggestions": [
                        "Use gap-filling methods for critical variables",
                        "Check sensor logs for failure periods",
                        "Consider using reanalysis data to fill gaps",
                    ],
                },
                "suggestions": [
                    "Implement automated gap-filling procedures",
                    "Set up data quality monitoring",
                ],
            }

        return {"passed": True, "issue": None, "suggestions": []}


# Convenience functions
def validate_configuration(config_path: Path, strict: bool = False) -> ValidationResult:
    """Validate a SUEWS configuration file."""
    validator = ConfigurationValidator()
    return validator.validate(config_path, strict)


def validate_forcing_data(data_path: Path) -> ValidationResult:
    """Validate forcing data file."""
    validator = ForcingDataValidator()
    return validator.validate(data_path)
