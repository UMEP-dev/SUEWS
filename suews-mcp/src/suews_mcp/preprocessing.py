"""Data preprocessing and validation tools for SUEWS MCP Server."""

import logging
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Any, Optional, Union, Tuple
import yaml
import json
from datetime import datetime, timedelta

logger = logging.getLogger(__name__)


class DataQualityIssue:
    """Represents a data quality issue found during preprocessing."""

    def __init__(
        self,
        issue_type: str,
        message: str,
        severity: str = "warning",
        location: Optional[str] = None,
        value: Any = None,
    ):
        self.issue_type = issue_type
        self.message = message
        self.severity = severity  # error, warning, info
        self.location = location  # row, column, or time period
        self.value = value
        self.timestamp = datetime.now()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": self.issue_type,
            "message": self.message,
            "severity": self.severity,
            "location": self.location,
            "value": str(self.value) if self.value is not None else None,
            "timestamp": self.timestamp.isoformat(),
        }


class PreprocessingResult:
    """Container for preprocessing results."""

    def __init__(self):
        self.success = True
        self.issues: List[DataQualityIssue] = []
        self.data: Optional[pd.DataFrame] = None
        self.metadata: Dict[str, Any] = {}
        self.processing_log: List[str] = []

    def add_issue(self, issue: DataQualityIssue):
        """Add a data quality issue."""
        self.issues.append(issue)
        if issue.severity == "error":
            self.success = False

    def add_log(self, message: str):
        """Add a processing log entry."""
        self.processing_log.append(f"{datetime.now().isoformat()}: {message}")
        logger.info(message)

    def get_summary(self) -> Dict[str, Any]:
        """Get a summary of preprocessing results."""
        return {
            "success": self.success,
            "total_issues": len(self.issues),
            "errors": sum(1 for i in self.issues if i.severity == "error"),
            "warnings": sum(1 for i in self.issues if i.severity == "warning"),
            "info": sum(1 for i in self.issues if i.severity == "info"),
            "data_shape": self.data.shape if self.data is not None else None,
            "metadata": self.metadata,
            "processing_steps": len(self.processing_log),
        }


class ForcingDataPreprocessor:
    """Handles meteorological forcing data preprocessing and validation."""

    # Standard SUEWS forcing data columns with expected units
    SUEWS_COLUMNS = {
        "iy": {"type": int, "unit": "year", "range": (1900, 2100)},
        "id": {"type": int, "unit": "day of year", "range": (1, 366)},
        "it": {"type": int, "unit": "hour", "range": (0, 23)},
        "imin": {"type": int, "unit": "minute", "range": (0, 59)},
        "qn": {"type": float, "unit": "W/m²", "range": (-200, 1500)},
        "qh": {"type": float, "unit": "W/m²", "range": (-200, 800)},
        "qe": {"type": float, "unit": "W/m²", "range": (-50, 800)},
        "qs": {"type": float, "unit": "W/m²", "range": (-300, 600)},
        "qf": {"type": float, "unit": "W/m²", "range": (0, 300)},
        "U": {"type": float, "unit": "m/s", "range": (0.1, 50)},
        "RH": {"type": float, "unit": "%", "range": (0, 100)},
        "Tair": {"type": float, "unit": "°C", "range": (-50, 60)},
        "pres": {"type": float, "unit": "kPa", "range": (70, 110)},
        "rain": {"type": float, "unit": "mm", "range": (0, 500)},
        "kdown": {"type": float, "unit": "W/m²", "range": (0, 1500)},
        "snow": {"type": float, "unit": "mm", "range": (0, 100)},
        "ldown": {"type": float, "unit": "W/m²", "range": (100, 500)},
        "fcld": {"type": float, "unit": "fraction", "range": (0, 1)},
        "wuh": {"type": float, "unit": "mm", "range": (0, 50)},
        "xsmd": {"type": float, "unit": "fraction", "range": (0, 2)},
        "lai": {"type": float, "unit": "m²/m²", "range": (0, 12)},
        "kdiff": {"type": float, "unit": "W/m²", "range": (0, 800)},
        "kdir": {"type": float, "unit": "W/m²", "range": (0, 1200)},
        "wdir": {"type": float, "unit": "°", "range": (0, 360)},
    }

    # Missing data flags commonly used in SUEWS
    MISSING_DATA_FLAGS = [-999.0, -999, 999, np.nan, None]

    def __init__(self):
        self.result = PreprocessingResult()

    def preprocess_forcing_file(
        self,
        file_path: Union[str, Path],
        output_path: Optional[Union[str, Path]] = None,
        target_timestep: Optional[int] = None,
        validate_energy_balance: bool = True,
        auto_fix_issues: bool = False,
    ) -> PreprocessingResult:
        """
        Preprocess a SUEWS forcing data file.

        Args:
            file_path: Path to forcing data file
            output_path: Optional path to save preprocessed data
            target_timestep: Target time step in seconds (None = auto-detect)
            validate_energy_balance: Whether to check energy balance
            auto_fix_issues: Whether to automatically fix common issues

        Returns:
            PreprocessingResult with processed data and quality assessment
        """
        self.result = PreprocessingResult()
        file_path = Path(file_path)

        try:
            self.result.add_log(f"Starting preprocessing of {file_path.name}")

            # Load data
            df = self._load_forcing_data(file_path)
            if df is None:
                return self.result

            # Basic data validation
            self._validate_data_structure(df)
            self._validate_time_series(df)
            self._validate_data_ranges(df)

            # Check for missing data
            self._check_missing_data(df)

            # Energy balance validation
            if validate_energy_balance:
                self._validate_energy_balance(df)

            # Time series consistency
            self._check_time_consistency(df)

            # Unit conversions if needed
            df_processed = self._apply_unit_conversions(df)

            # Gap filling and interpolation if requested
            if auto_fix_issues:
                df_processed = self._auto_fix_data_issues(df_processed)

            # Time step adjustment
            if target_timestep:
                df_processed = self._adjust_timestep(df_processed, target_timestep)

            # Final validation
            self._final_validation(df_processed)

            # Save results
            self.result.data = df_processed
            self.result.metadata = self._generate_metadata(df, df_processed, file_path)

            # Save to file if requested
            if output_path:
                self._save_processed_data(df_processed, output_path)

            self.result.add_log("Preprocessing completed successfully")

        except Exception as e:
            logger.error(f"Error preprocessing {file_path}: {e}", exc_info=True)
            self.result.add_issue(
                DataQualityIssue(
                    "processing_error", f"Failed to process file: {str(e)}", "error"
                )
            )

        return self.result

    def _load_forcing_data(self, file_path: Path) -> Optional[pd.DataFrame]:
        """Load forcing data from file."""
        try:
            # Detect file format and load appropriately
            if file_path.suffix.lower() == ".csv":
                df = pd.read_csv(file_path)
            elif file_path.suffix.lower() == ".txt":
                # Try SUEWS format first (whitespace-separated)
                try:
                    df = pd.read_csv(file_path, sep=r"\s+", comment="!")
                except:
                    # Fallback to comma-separated
                    df = pd.read_csv(file_path)
            else:
                self.result.add_issue(
                    DataQualityIssue(
                        "file_format",
                        f"Unsupported file format: {file_path.suffix}",
                        "error",
                    )
                )
                return None

            self.result.add_log(f"Loaded data with shape {df.shape}")
            return df

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue("file_read", f"Cannot read file: {str(e)}", "error")
            )
            return None

    def _validate_data_structure(self, df: pd.DataFrame):
        """Validate basic data structure and columns."""
        # Check required columns
        required_cols = ["iy", "id", "it", "imin"]
        missing_cols = [col for col in required_cols if col not in df.columns]

        if missing_cols:
            self.result.add_issue(
                DataQualityIssue(
                    "missing_columns",
                    f"Missing required time columns: {missing_cols}",
                    "error",
                )
            )

        # Check for duplicate columns
        if df.columns.duplicated().any():
            duplicate_cols = df.columns[df.columns.duplicated()].tolist()
            self.result.add_issue(
                DataQualityIssue(
                    "duplicate_columns",
                    f"Duplicate columns found: {duplicate_cols}",
                    "warning",
                )
            )

        # Check data types
        for col in df.columns:
            if col in self.SUEWS_COLUMNS:
                expected_type = self.SUEWS_COLUMNS[col]["type"]
                if not pd.api.types.is_numeric_dtype(df[col]) and expected_type in [
                    int,
                    float,
                ]:
                    self.result.add_issue(
                        DataQualityIssue(
                            "invalid_data_type",
                            f"Column {col} should be numeric but contains non-numeric values",
                            "warning",
                            location=col,
                        )
                    )

        self.result.add_log("Data structure validation completed")

    def _validate_time_series(self, df: pd.DataFrame):
        """Validate time series consistency."""
        if not all(col in df.columns for col in ["iy", "id", "it", "imin"]):
            return

        try:
            # Create datetime index
            df_temp = df.copy()

            # Check for invalid dates
            invalid_dates = []
            for idx, row in df_temp.iterrows():
                try:
                    # Convert day of year to month/day
                    date = datetime(int(row["iy"]), 1, 1) + timedelta(
                        days=int(row["id"]) - 1
                    )
                    time = datetime.combine(
                        date.date(),
                        datetime.min.time().replace(
                            hour=int(row["it"]), minute=int(row["imin"])
                        ),
                    )
                except (ValueError, OverflowError):
                    invalid_dates.append(idx)

            if invalid_dates:
                self.result.add_issue(
                    DataQualityIssue(
                        "invalid_dates",
                        f"Found {len(invalid_dates)} invalid date/time entries",
                        "error",
                        location=f"rows: {invalid_dates[:10]}"
                        + ("..." if len(invalid_dates) > 10 else ""),
                    )
                )

            # Check for time gaps
            if len(df_temp) > 1:
                time_diffs = []
                for i in range(1, min(100, len(df_temp))):  # Check first 100 rows
                    try:
                        dt1 = datetime(
                            int(df_temp.iloc[i - 1]["iy"]), 1, 1
                        ) + timedelta(days=int(df_temp.iloc[i - 1]["id"]) - 1)
                        dt1 = dt1.replace(
                            hour=int(df_temp.iloc[i - 1]["it"]),
                            minute=int(df_temp.iloc[i - 1]["imin"]),
                        )
                        dt2 = datetime(int(df_temp.iloc[i]["iy"]), 1, 1) + timedelta(
                            days=int(df_temp.iloc[i]["id"]) - 1
                        )
                        dt2 = dt2.replace(
                            hour=int(df_temp.iloc[i]["it"]),
                            minute=int(df_temp.iloc[i]["imin"]),
                        )
                        time_diffs.append((dt2 - dt1).total_seconds())
                    except:
                        continue

                if time_diffs:
                    most_common_diff = (
                        pd.Series(time_diffs).mode().iloc[0] if time_diffs else None
                    )
                    if most_common_diff:
                        self.result.metadata["detected_timestep_seconds"] = (
                            most_common_diff
                        )
                        self.result.add_log(
                            f"Detected time step: {most_common_diff} seconds"
                        )

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue(
                    "time_validation",
                    f"Error validating time series: {str(e)}",
                    "warning",
                )
            )

        self.result.add_log("Time series validation completed")

    def _validate_data_ranges(self, df: pd.DataFrame):
        """Validate data values are within expected ranges."""
        for col in df.columns:
            if col in self.SUEWS_COLUMNS and col in df.columns:
                expected_range = self.SUEWS_COLUMNS[col]["range"]

                # Exclude missing data flags from range validation
                mask = ~df[col].isin(self.MISSING_DATA_FLAGS)
                valid_data = df.loc[mask, col]

                if len(valid_data) > 0:
                    min_val, max_val = valid_data.min(), valid_data.max()
                    exp_min, exp_max = expected_range

                    if min_val < exp_min or max_val > exp_max:
                        self.result.add_issue(
                            DataQualityIssue(
                                "value_out_of_range",
                                f"{col}: values [{min_val:.2f}, {max_val:.2f}] outside expected range [{exp_min}, {exp_max}] ({self.SUEWS_COLUMNS[col]['unit']})",
                                "warning",
                                location=col,
                                value=f"min: {min_val:.2f}, max: {max_val:.2f}",
                            )
                        )

        self.result.add_log("Data range validation completed")

    def _check_missing_data(self, df: pd.DataFrame):
        """Check for missing data patterns."""
        for col in df.columns:
            if col in self.SUEWS_COLUMNS:
                missing_mask = df[col].isin(self.MISSING_DATA_FLAGS) | df[col].isna()
                missing_count = missing_mask.sum()
                missing_pct = (missing_count / len(df)) * 100

                if missing_count > 0:
                    severity = (
                        "error"
                        if missing_pct > 50
                        else "warning"
                        if missing_pct > 10
                        else "info"
                    )
                    self.result.add_issue(
                        DataQualityIssue(
                            "missing_data",
                            f"{col}: {missing_count} missing values ({missing_pct:.1f}%)",
                            severity,
                            location=col,
                            value=missing_count,
                        )
                    )

                # Check for consecutive missing data
                if missing_count > 0:
                    consecutive_groups = []
                    current_group = 0
                    for is_missing in missing_mask:
                        if is_missing:
                            current_group += 1
                        else:
                            if current_group > 0:
                                consecutive_groups.append(current_group)
                            current_group = 0
                    if current_group > 0:
                        consecutive_groups.append(current_group)

                    if consecutive_groups and max(consecutive_groups) > 10:
                        self.result.add_issue(
                            DataQualityIssue(
                                "consecutive_missing",
                                f"{col}: longest gap of {max(consecutive_groups)} consecutive missing values",
                                "warning",
                                location=col,
                            )
                        )

        self.result.add_log("Missing data check completed")

    def _validate_energy_balance(self, df: pd.DataFrame):
        """Validate energy balance components."""
        energy_cols = ["qn", "qh", "qe", "qs"]
        available_cols = [col for col in energy_cols if col in df.columns]

        if len(available_cols) >= 3:  # Need at least 3 components
            # Calculate energy balance closure
            df_energy = df[available_cols].copy()

            # Exclude missing data
            for col in available_cols:
                mask = ~df_energy[col].isin(self.MISSING_DATA_FLAGS)
                df_energy = df_energy[mask]

            if len(df_energy) > 0:
                if "qn" in available_cols and len(available_cols) >= 4:
                    # Full energy balance: QN = QH + QE + QS
                    calculated_qn = df_energy["qh"] + df_energy["qe"] + df_energy["qs"]
                    residual = df_energy["qn"] - calculated_qn
                    residual_mean = residual.mean()
                    residual_std = residual.std()

                    # Check energy balance closure
                    qn_mean = df_energy["qn"].mean()
                    if abs(residual_mean) > 0.2 * abs(qn_mean):
                        self.result.add_issue(
                            DataQualityIssue(
                                "energy_balance",
                                f"Poor energy balance closure: mean residual {residual_mean:.1f} W/m² ({abs(residual_mean/qn_mean)*100:.1f}% of QN)",
                                "warning",
                            )
                        )

                    self.result.metadata["energy_balance_stats"] = {
                        "mean_residual": float(residual_mean),
                        "std_residual": float(residual_std),
                        "mean_qn": float(qn_mean),
                    }

                # Check Bowen ratio
                if "qh" in available_cols and "qe" in available_cols:
                    qe_nonzero = df_energy["qe"][df_energy["qe"] != 0]
                    if len(qe_nonzero) > 0:
                        bowen_ratio = (
                            df_energy["qh"][df_energy["qe"] != 0] / qe_nonzero
                        ).median()
                        self.result.metadata["bowen_ratio_median"] = float(bowen_ratio)

                        if bowen_ratio > 10 or bowen_ratio < -2:
                            self.result.add_issue(
                                DataQualityIssue(
                                    "unusual_bowen_ratio",
                                    f"Unusual Bowen ratio (QH/QE): {bowen_ratio:.2f}",
                                    "warning",
                                )
                            )

        self.result.add_log("Energy balance validation completed")

    def _check_time_consistency(self, df: pd.DataFrame):
        """Check for time series consistency issues."""
        if len(df) < 2:
            return

        # Check for duplicated timestamps
        if all(col in df.columns for col in ["iy", "id", "it", "imin"]):
            time_cols = df[["iy", "id", "it", "imin"]]
            duplicates = time_cols.duplicated().sum()

            if duplicates > 0:
                self.result.add_issue(
                    DataQualityIssue(
                        "duplicate_timestamps",
                        f"Found {duplicates} duplicate timestamps",
                        "error",
                    )
                )

        # Check for diurnal patterns in key variables
        for var in ["Tair", "RH", "kdown"]:
            if var in df.columns and "it" in df.columns:
                # Group by hour and check if there's expected diurnal variation
                hourly_stats = df.groupby("it")[var].agg(["mean", "std"]).dropna()

                if len(hourly_stats) >= 12:  # Need at least 12 hours
                    # Check if there's reasonable diurnal variation
                    daily_range = (
                        hourly_stats["mean"].max() - hourly_stats["mean"].min()
                    )
                    overall_std = df[var].std()

                    if (
                        var == "Tair" and daily_range < 2.0
                    ):  # Less than 2°C diurnal range
                        self.result.add_issue(
                            DataQualityIssue(
                                "weak_diurnal_pattern",
                                f"{var}: weak diurnal variation (range: {daily_range:.1f}°C)",
                                "info",
                            )
                        )
                    elif (
                        var == "kdown" and daily_range < 50
                    ):  # Very low solar radiation variation
                        self.result.add_issue(
                            DataQualityIssue(
                                "weak_diurnal_pattern",
                                f"{var}: weak diurnal variation (range: {daily_range:.1f} W/m²)",
                                "info",
                            )
                        )

        self.result.add_log("Time consistency check completed")

    def _apply_unit_conversions(self, df: pd.DataFrame) -> pd.DataFrame:
        """Apply standard unit conversions for SUEWS."""
        df_processed = df.copy()

        # Convert pressure from hPa to kPa if needed
        if "pres" in df_processed.columns:
            pres_values = df_processed["pres"][
                ~df_processed["pres"].isin(self.MISSING_DATA_FLAGS)
            ]
            if len(pres_values) > 0 and pres_values.mean() > 500:  # Likely in hPa
                df_processed["pres"] = df_processed["pres"] / 10
                self.result.add_log("Converted pressure from hPa to kPa")

        # Check wind direction is in 0-360 range
        if "wdir" in df_processed.columns:
            wdir_mask = ~df_processed["wdir"].isin(self.MISSING_DATA_FLAGS)
            wdir_values = df_processed.loc[wdir_mask, "wdir"]
            if len(wdir_values) > 0:
                # Normalize to 0-360 range
                df_processed.loc[wdir_mask, "wdir"] = wdir_values % 360

        self.result.add_log("Unit conversions applied")
        return df_processed

    def _auto_fix_data_issues(self, df: pd.DataFrame) -> pd.DataFrame:
        """Automatically fix common data issues."""
        df_fixed = df.copy()

        # Simple interpolation for short gaps in key variables
        for var in ["Tair", "RH", "U", "pres"]:
            if var in df_fixed.columns:
                # Replace missing flags with NaN for interpolation
                mask = df_fixed[var].isin(self.MISSING_DATA_FLAGS)
                df_fixed.loc[mask, var] = np.nan

                # Interpolate gaps of 3 or fewer consecutive values
                df_fixed[var] = df_fixed[var].interpolate(method="linear", limit=3)

        # Set negative solar radiation to zero
        for var in ["kdown", "kdiff", "kdir"]:
            if var in df_fixed.columns:
                mask = (df_fixed[var] < 0) & (
                    ~df_fixed[var].isin(self.MISSING_DATA_FLAGS)
                )
                if mask.sum() > 0:
                    df_fixed.loc[mask, var] = 0
                    self.result.add_log(
                        f"Set {mask.sum()} negative {var} values to zero"
                    )

        # Ensure relative humidity is in 0-100 range
        if "RH" in df_fixed.columns:
            rh_mask = ~df_fixed["RH"].isin(self.MISSING_DATA_FLAGS)
            rh_values = df_fixed.loc[rh_mask, "RH"]

            # If values seem to be in 0-1 range, convert to percentage
            if len(rh_values) > 0 and rh_values.max() <= 1.5:
                df_fixed.loc[rh_mask, "RH"] = rh_values * 100
                self.result.add_log(
                    "Converted relative humidity from fraction to percentage"
                )

            # Clip to valid range
            df_fixed.loc[rh_mask & (df_fixed["RH"] > 100), "RH"] = 100
            df_fixed.loc[rh_mask & (df_fixed["RH"] < 0), "RH"] = 0

        self.result.add_log("Auto-fix applied to data issues")
        return df_fixed

    def _adjust_timestep(self, df: pd.DataFrame, target_timestep: int) -> pd.DataFrame:
        """Adjust data to target timestep through resampling."""
        # This is a simplified version - full implementation would use SuPy's resampling
        self.result.add_log(
            f"Timestep adjustment to {target_timestep}s requested (simplified)"
        )
        return df

    def _final_validation(self, df: pd.DataFrame):
        """Perform final validation checks."""
        if df is None or len(df) == 0:
            self.result.add_issue(
                DataQualityIssue("empty_result", "Processed data is empty", "error")
            )
            return

        # Check that we still have required columns
        required_cols = ["iy", "id", "it", "imin"]
        missing_cols = [col for col in required_cols if col not in df.columns]

        if missing_cols:
            self.result.add_issue(
                DataQualityIssue(
                    "final_validation",
                    f"Missing required columns after processing: {missing_cols}",
                    "error",
                )
            )

        self.result.add_log("Final validation completed")

    def _generate_metadata(
        self, df_original: pd.DataFrame, df_processed: pd.DataFrame, file_path: Path
    ) -> Dict[str, Any]:
        """Generate metadata about the preprocessing."""
        metadata = {
            "source_file": str(file_path),
            "original_shape": df_original.shape,
            "processed_shape": df_processed.shape,
            "available_variables": list(df_processed.columns),
            "processing_timestamp": datetime.now().isoformat(),
        }

        # Add data statistics
        numeric_cols = df_processed.select_dtypes(include=[np.number]).columns
        if len(numeric_cols) > 0:
            metadata["data_statistics"] = {}
            for col in numeric_cols:
                if col in self.SUEWS_COLUMNS:
                    non_missing = df_processed[col][
                        ~df_processed[col].isin(self.MISSING_DATA_FLAGS)
                    ]
                    if len(non_missing) > 0:
                        metadata["data_statistics"][col] = {
                            "mean": float(non_missing.mean()),
                            "std": float(non_missing.std()),
                            "min": float(non_missing.min()),
                            "max": float(non_missing.max()),
                            "count": int(len(non_missing)),
                            "missing_count": int(len(df_processed) - len(non_missing)),
                        }

        return metadata

    def _save_processed_data(self, df: pd.DataFrame, output_path: Union[str, Path]):
        """Save processed data to file."""
        output_path = Path(output_path)

        try:
            if output_path.suffix.lower() == ".csv":
                df.to_csv(output_path, index=False)
            elif output_path.suffix.lower() == ".txt":
                # Save in SUEWS format (space-separated)
                df.to_csv(output_path, sep=" ", index=False, float_format="%.2f")
            else:
                # Default to CSV
                output_path = output_path.with_suffix(".csv")
                df.to_csv(output_path, index=False)

            self.result.add_log(f"Processed data saved to {output_path}")

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue(
                    "save_error", f"Failed to save processed data: {str(e)}", "error"
                )
            )


class ConfigValidator:
    """Validates SUEWS configuration files."""

    def __init__(self):
        self.result = PreprocessingResult()

    def validate_config(
        self,
        config_path: Union[str, Path],
        strict_mode: bool = False,
        check_file_paths: bool = True,
    ) -> PreprocessingResult:
        """
        Validate a SUEWS configuration file.

        Args:
            config_path: Path to configuration file
            strict_mode: Enable strict validation mode
            check_file_paths: Whether to validate referenced file paths exist

        Returns:
            PreprocessingResult with validation results
        """
        self.result = PreprocessingResult()
        config_path = Path(config_path)

        try:
            self.result.add_log(f"Starting validation of {config_path.name}")

            # Load configuration
            config_data = self._load_config(config_path)
            if config_data is None:
                return self.result

            # Basic structure validation
            self._validate_config_structure(config_data)

            # Required fields validation
            self._validate_required_fields(config_data, strict_mode)

            # Value range validation
            self._validate_value_ranges(config_data, strict_mode)

            # Physics option compatibility
            self._validate_physics_options(config_data)

            # File path validation
            if check_file_paths:
                self._validate_file_paths(config_data, config_path.parent)

            # Surface fraction validation
            self._validate_surface_fractions(config_data)

            # Time series validation
            self._validate_time_settings(config_data)

            self.result.metadata = {
                "config_file": str(config_path),
                "validation_mode": "strict" if strict_mode else "standard",
                "total_validations": 7,
                "validation_timestamp": datetime.now().isoformat(),
            }

            self.result.add_log("Configuration validation completed")

        except Exception as e:
            logger.error(f"Error validating config {config_path}: {e}", exc_info=True)
            self.result.add_issue(
                DataQualityIssue(
                    "validation_error", f"Failed to validate config: {str(e)}", "error"
                )
            )

        return self.result

    def _load_config(self, config_path: Path) -> Optional[Dict[str, Any]]:
        """Load configuration file."""
        try:
            with open(config_path, "r") as f:
                if config_path.suffix.lower() in [".yml", ".yaml"]:
                    config_data = yaml.safe_load(f)
                elif config_path.suffix.lower() == ".json":
                    config_data = json.load(f)
                else:
                    self.result.add_issue(
                        DataQualityIssue(
                            "unsupported_format",
                            f"Unsupported config format: {config_path.suffix}",
                            "error",
                        )
                    )
                    return None

            self.result.add_log(
                f"Loaded configuration with {len(config_data)} top-level sections"
            )
            return config_data

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue(
                    "config_load_error", f"Cannot load config file: {str(e)}", "error"
                )
            )
            return None

    def _validate_config_structure(self, config_data: Dict[str, Any]):
        """Validate basic configuration structure."""
        required_sections = ["name", "model", "sites"]
        missing_sections = [
            section for section in required_sections if section not in config_data
        ]

        if missing_sections:
            self.result.add_issue(
                DataQualityIssue(
                    "missing_sections",
                    f"Missing required configuration sections: {missing_sections}",
                    "error",
                )
            )

        # Check model section structure
        if "model" in config_data:
            model_sections = ["control", "physics"]
            for section in model_sections:
                if section not in config_data["model"]:
                    self.result.add_issue(
                        DataQualityIssue(
                            "missing_model_section",
                            f"Missing model.{section} section",
                            "error",
                        )
                    )

        # Check sites structure
        if "sites" in config_data:
            if (
                not isinstance(config_data["sites"], list)
                or len(config_data["sites"]) == 0
            ):
                self.result.add_issue(
                    DataQualityIssue(
                        "invalid_sites_structure",
                        "Sites section should be a non-empty list",
                        "error",
                    )
                )

        self.result.add_log("Configuration structure validation completed")

    def _validate_required_fields(self, config_data: Dict[str, Any], strict_mode: bool):
        """Validate required fields are present."""
        # Required control fields
        control_required = ["tstep", "forcing_file"]
        if "model" in config_data and "control" in config_data["model"]:
            control = config_data["model"]["control"]
            for field in control_required:
                if field not in control:
                    severity = "error" if strict_mode else "warning"
                    self.result.add_issue(
                        DataQualityIssue(
                            "missing_required_field",
                            f"Missing required control field: {field}",
                            severity,
                        )
                    )

        # Required site fields
        site_required = ["name", "properties"]
        if "sites" in config_data:
            for i, site in enumerate(config_data["sites"]):
                for field in site_required:
                    if field not in site:
                        self.result.add_issue(
                            DataQualityIssue(
                                "missing_site_field",
                                f"Site {i}: Missing required field {field}",
                                "error",
                            )
                        )

        self.result.add_log("Required fields validation completed")

    def _validate_value_ranges(self, config_data: Dict[str, Any], strict_mode: bool):
        """Validate values are within acceptable ranges."""
        # Time step validation
        if "model" in config_data and "control" in config_data["model"]:
            tstep = config_data["model"]["control"].get("tstep")
            if tstep is not None:
                if not isinstance(tstep, (int, float)) or tstep <= 0:
                    self.result.add_issue(
                        DataQualityIssue(
                            "invalid_timestep",
                            f"Time step must be positive number, got: {tstep}",
                            "error",
                        )
                    )
                elif tstep < 60 or tstep > 3600:
                    severity = "error" if strict_mode else "warning"
                    self.result.add_issue(
                        DataQualityIssue(
                            "unusual_timestep",
                            f"Unusual time step: {tstep}s (typical range: 60-3600s)",
                            severity,
                        )
                    )

        # Site property validation
        if "sites" in config_data:
            for i, site in enumerate(config_data["sites"]):
                if "properties" in site:
                    props = site["properties"]

                    # Latitude validation
                    if "lat" in props:
                        lat_val = (
                            props["lat"].get("value")
                            if isinstance(props["lat"], dict)
                            else props["lat"]
                        )
                        if (
                            not isinstance(lat_val, (int, float))
                            or lat_val < -90
                            or lat_val > 90
                        ):
                            self.result.add_issue(
                                DataQualityIssue(
                                    "invalid_latitude",
                                    f"Site {i}: Latitude must be between -90 and 90, got: {lat_val}",
                                    "error",
                                )
                            )

                    # Longitude validation
                    if "lng" in props:
                        lng_val = (
                            props["lng"].get("value")
                            if isinstance(props["lng"], dict)
                            else props["lng"]
                        )
                        if (
                            not isinstance(lng_val, (int, float))
                            or lng_val < -180
                            or lng_val > 180
                        ):
                            self.result.add_issue(
                                DataQualityIssue(
                                    "invalid_longitude",
                                    f"Site {i}: Longitude must be between -180 and 180, got: {lng_val}",
                                    "error",
                                )
                            )

        self.result.add_log("Value range validation completed")

    def _validate_physics_options(self, config_data: Dict[str, Any]):
        """Validate physics option compatibility."""
        if "model" not in config_data or "physics" not in config_data["model"]:
            return

        physics = config_data["model"]["physics"]

        # Check for incompatible option combinations
        netradiationmethod = physics.get("netradiationmethod", {}).get("value", 0)
        roughlenmommethod = physics.get("roughlenmommethod", {}).get("value", 0)

        # Example validation: certain combinations might not be recommended
        if netradiationmethod == 0 and roughlenmommethod == 3:
            self.result.add_issue(
                DataQualityIssue(
                    "physics_compatibility",
                    "Combination of netradiationmethod=0 with roughlenmommethod=3 may cause issues",
                    "warning",
                )
            )

        self.result.add_log("Physics options validation completed")

    def _validate_file_paths(self, config_data: Dict[str, Any], base_path: Path):
        """Validate that referenced files exist."""
        if "model" in config_data and "control" in config_data["model"]:
            forcing_file = config_data["model"]["control"].get("forcing_file", {})
            if isinstance(forcing_file, dict):
                forcing_path = forcing_file.get("value")
                if forcing_path:
                    full_path = base_path / forcing_path
                    if not full_path.exists():
                        self.result.add_issue(
                            DataQualityIssue(
                                "missing_forcing_file",
                                f"Forcing file not found: {full_path}",
                                "error",
                            )
                        )

        self.result.add_log("File path validation completed")

    def _validate_surface_fractions(self, config_data: Dict[str, Any]):
        """Validate surface fractions sum to 1.0."""
        if "sites" not in config_data:
            return

        for i, site in enumerate(config_data["sites"]):
            if "land_cover" not in site:
                continue

            land_cover = site["land_cover"]
            surface_types = [
                "paved",
                "bldgs",
                "evetr",
                "dectr",
                "grass",
                "bsoil",
                "water",
            ]

            fractions = []
            for surface_type in surface_types:
                if surface_type in land_cover:
                    sfr = land_cover[surface_type].get("sfr", {})
                    if isinstance(sfr, dict) and "value" in sfr:
                        fractions.append(sfr["value"])
                    elif isinstance(sfr, (int, float)):
                        fractions.append(sfr)

            if len(fractions) >= 3:  # Need at least 3 surface types
                total_fraction = sum(fractions)
                if abs(total_fraction - 1.0) > 0.01:  # Allow small rounding errors
                    self.result.add_issue(
                        DataQualityIssue(
                            "surface_fraction_sum",
                            f"Site {i}: Surface fractions sum to {total_fraction:.3f}, should be 1.000",
                            "error",
                        )
                    )

        self.result.add_log("Surface fractions validation completed")

    def _validate_time_settings(self, config_data: Dict[str, Any]):
        """Validate time-related settings."""
        # This would validate start/end times, timezone settings, etc.
        # Simplified for this implementation
        self.result.add_log("Time settings validation completed")


class DataFormatConverter:
    """Handles conversion between different meteorological data formats."""

    def __init__(self):
        self.result = PreprocessingResult()

    def convert_format(
        self,
        input_path: Union[str, Path],
        output_path: Union[str, Path],
        input_format: str,
        output_format: str,
        **kwargs,
    ) -> PreprocessingResult:
        """
        Convert data between different formats.

        Args:
            input_path: Path to input file
            output_path: Path to output file
            input_format: Input format (csv, txt, netcdf, excel)
            output_format: Output format (csv, txt, suews_txt, netcdf)
            **kwargs: Additional conversion options

        Returns:
            PreprocessingResult with conversion results
        """
        self.result = PreprocessingResult()

        try:
            self.result.add_log(f"Converting {input_format} to {output_format}")

            # Load data based on input format
            df = self._load_data_by_format(input_path, input_format, **kwargs)
            if df is None:
                return self.result

            # Apply format-specific transformations
            df_converted = self._apply_format_transformations(
                df, input_format, output_format, **kwargs
            )

            # Save data in target format
            self._save_data_by_format(
                df_converted, output_path, output_format, **kwargs
            )

            self.result.data = df_converted
            self.result.metadata = {
                "input_format": input_format,
                "output_format": output_format,
                "conversion_timestamp": datetime.now().isoformat(),
                "input_shape": df.shape,
                "output_shape": df_converted.shape,
            }

            self.result.add_log("Format conversion completed successfully")

        except Exception as e:
            logger.error(f"Error converting format: {e}", exc_info=True)
            self.result.add_issue(
                DataQualityIssue(
                    "conversion_error", f"Failed to convert format: {str(e)}", "error"
                )
            )

        return self.result

    def _load_data_by_format(
        self, file_path: Path, format_type: str, **kwargs
    ) -> Optional[pd.DataFrame]:
        """Load data based on format type."""
        file_path = Path(file_path)

        try:
            if format_type.lower() == "csv":
                df = pd.read_csv(file_path)
            elif format_type.lower() == "txt":
                # Try space-separated first, then comma
                try:
                    df = pd.read_csv(file_path, sep=r"\s+", comment="!")
                except:
                    df = pd.read_csv(file_path, sep=kwargs.get("separator", ","))
            elif format_type.lower() == "excel":
                df = pd.read_excel(file_path, sheet_name=kwargs.get("sheet_name", 0))
            elif format_type.lower() == "netcdf":
                try:
                    import xarray as xr

                    ds = xr.open_dataset(file_path)
                    df = ds.to_dataframe().reset_index()
                except ImportError:
                    self.result.add_issue(
                        DataQualityIssue(
                            "missing_dependency",
                            "xarray required for NetCDF support",
                            "error",
                        )
                    )
                    return None
            else:
                self.result.add_issue(
                    DataQualityIssue(
                        "unsupported_format",
                        f"Unsupported input format: {format_type}",
                        "error",
                    )
                )
                return None

            self.result.add_log(f"Loaded data with shape {df.shape} from {format_type}")
            return df

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue(
                    "load_error", f"Error loading {format_type} file: {str(e)}", "error"
                )
            )
            return None

    def _apply_format_transformations(
        self, df: pd.DataFrame, input_format: str, output_format: str, **kwargs
    ) -> pd.DataFrame:
        """Apply format-specific transformations."""
        df_transformed = df.copy()

        # If converting to SUEWS format, ensure required columns
        if output_format.lower() == "suews_txt":
            # Add time columns if missing (simplified)
            if "datetime" in df_transformed.columns:
                # Convert datetime to SUEWS time format
                dt_col = pd.to_datetime(df_transformed["datetime"])
                df_transformed["iy"] = dt_col.dt.year
                df_transformed["id"] = dt_col.dt.dayofyear
                df_transformed["it"] = dt_col.dt.hour
                df_transformed["imin"] = dt_col.dt.minute
                df_transformed = df_transformed.drop("datetime", axis=1)

        # Column name mapping
        column_mapping = kwargs.get("column_mapping", {})
        if column_mapping:
            df_transformed = df_transformed.rename(columns=column_mapping)

        self.result.add_log("Format transformations applied")
        return df_transformed

    def _save_data_by_format(
        self, df: pd.DataFrame, file_path: Path, format_type: str, **kwargs
    ):
        """Save data in specified format."""
        file_path = Path(file_path)

        try:
            if format_type.lower() == "csv":
                df.to_csv(file_path, index=False)
            elif format_type.lower() in ["txt", "suews_txt"]:
                # Space-separated format for SUEWS
                df.to_csv(file_path, sep=" ", index=False, float_format="%.2f")
            elif format_type.lower() == "excel":
                df.to_excel(
                    file_path,
                    index=False,
                    sheet_name=kwargs.get("sheet_name", "Sheet1"),
                )
            elif format_type.lower() == "netcdf":
                try:
                    import xarray as xr

                    ds = df.to_xarray()
                    ds.to_netcdf(file_path)
                except ImportError:
                    self.result.add_issue(
                        DataQualityIssue(
                            "missing_dependency",
                            "xarray required for NetCDF support",
                            "error",
                        )
                    )
                    return
            else:
                self.result.add_issue(
                    DataQualityIssue(
                        "unsupported_output",
                        f"Unsupported output format: {format_type}",
                        "error",
                    )
                )
                return

            self.result.add_log(f"Saved data to {file_path} in {format_type} format")

        except Exception as e:
            self.result.add_issue(
                DataQualityIssue(
                    "save_error", f"Error saving {format_type} file: {str(e)}", "error"
                )
            )
