"""
Analyze Results MCP Tool.

Wraps SuPy analysis and post-processing functionality.
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd
import numpy as np

try:
    from ..._post import resample_output
    from ...util._plot import plot_comparison, plot_rsl
except ImportError:
    # Handle imports for development/testing
    resample_output = None
    plot_comparison = None
    plot_rsl = None

from .base import MCPTool


class AnalyzeResultsTool(MCPTool):
    """
    MCP tool for analyzing SUEWS simulation results.
    
    Provides functionality for:
    - Loading and analyzing simulation output files
    - Calculating statistics and summaries
    - Resampling data to different time frequencies
    - Basic visualization and comparison
    """
    
    def __init__(self):
        super().__init__(
            name="analyze_results",
            description="Analyze SUEWS simulation results with statistics, resampling, and visualization"
        )
    
    def get_parameters(self) -> List[Dict[str, Any]]:
        """Get tool parameter definitions."""
        return [
            {
                "name": "results_path",
                "type": "string",
                "description": "Path to simulation results file (.csv, .txt, .pkl, or .parquet)",
                "required": True
            },
            {
                "name": "analysis_type",
                "type": "string", 
                "description": "Type of analysis: 'summary', 'statistics', 'energy_balance', 'water_balance', 'temporal'",
                "required": False
            },
            {
                "name": "variables",
                "type": "array",
                "description": "List of variables to analyze (if not provided, analyzes all available)",
                "required": False
            },
            {
                "name": "time_period",
                "type": "string",
                "description": "Time period for analysis: 'daily', 'monthly', 'seasonal', 'annual'",
                "required": False
            },
            {
                "name": "start_time",
                "type": "string",
                "description": "Start time for analysis period (ISO format)",
                "required": False
            },
            {
                "name": "end_time",
                "type": "string",
                "description": "End time for analysis period (ISO format)",
                "required": False
            },
            {
                "name": "comparison_path",
                "type": "string", 
                "description": "Path to comparison data file for validation/benchmarking",
                "required": False
            },
            {
                "name": "output_format",
                "type": "string",
                "description": "Output format for processed results: 'json', 'csv', 'summary'",
                "required": False
            }
        ]
    
    async def _execute(self, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Execute analysis tool."""
        # Extract and validate parameters
        results_path = self._validate_parameter(arguments, "results_path", str, required=True)
        analysis_type = arguments.get("analysis_type", "summary")
        variables = arguments.get("variables")
        time_period = arguments.get("time_period")
        start_time = arguments.get("start_time")
        end_time = arguments.get("end_time")
        comparison_path = arguments.get("comparison_path")
        output_format = arguments.get("output_format", "json")
        
        try:
            # Validate paths
            results_path_obj = self.translator.validate_file_path(results_path)
            analysis_info = {
                "results_path": str(results_path_obj),
                "analysis_type": analysis_type,
                "output_format": output_format
            }
            
            # Load simulation results
            df_results = self._load_results_file(results_path_obj)
            if df_results is None or df_results.empty:
                return self.translator.create_structured_response(
                    success=False,
                    errors=["Failed to load simulation results or file is empty"]
                )
            
            analysis_info["data_info"] = {
                "shape": df_results.shape,
                "columns": list(df_results.columns),
                "time_range": {
                    "start": df_results.index[0].isoformat(),
                    "end": df_results.index[-1].isoformat()
                }
            }
            
            # Validate and filter variables
            if variables:
                var_list = self.translator.validate_analysis_variables(variables)
                available_vars = [var for var in var_list if var in df_results.columns]
                missing_vars = [var for var in var_list if var not in df_results.columns]
                
                if missing_vars:
                    analysis_info["warnings"] = [f"Variables not found: {missing_vars}"]
                
                if not available_vars:
                    return self.translator.create_structured_response(
                        success=False,
                        errors=["None of the requested variables are available in the results"]
                    )
                
                df_analysis = df_results[available_vars]
                analysis_info["analyzed_variables"] = available_vars
            else:
                df_analysis = df_results
                analysis_info["analyzed_variables"] = list(df_results.columns)
            
            # Filter by time range
            start_ts, end_ts = self.translator.validate_time_range(start_time, end_time)
            if start_ts or end_ts:
                original_shape = df_analysis.shape
                if start_ts:
                    df_analysis = df_analysis[df_analysis.index >= start_ts]
                if end_ts:
                    df_analysis = df_analysis[df_analysis.index <= end_ts]
                
                analysis_info["time_filtering"] = {
                    "original_shape": original_shape,
                    "filtered_shape": df_analysis.shape
                }
            
            # Perform analysis based on type
            if analysis_type == "summary":
                analysis_results = self._analyze_summary(df_analysis)
            elif analysis_type == "statistics":
                analysis_results = self._analyze_statistics(df_analysis)
            elif analysis_type == "energy_balance":
                analysis_results = self._analyze_energy_balance(df_analysis)
            elif analysis_type == "water_balance":
                analysis_results = self._analyze_water_balance(df_analysis)
            elif analysis_type == "temporal":
                analysis_results = self._analyze_temporal_patterns(df_analysis, time_period)
            else:
                return self.translator.create_structured_response(
                    success=False,
                    errors=[f"Unknown analysis type: {analysis_type}"]
                )
            
            analysis_info["analysis_results"] = analysis_results
            
            # Load and compare with reference data if provided
            if comparison_path:
                comparison_results = self._perform_comparison(df_analysis, comparison_path)
                analysis_info["comparison_results"] = comparison_results
            
            # Format output based on requested format
            if output_format == "summary":
                # Create human-readable summary
                analysis_info["summary"] = self._create_text_summary(analysis_results, df_analysis)
            
            return self.translator.create_structured_response(
                success=True,
                data=analysis_info,
                message=f"Analysis completed successfully: {analysis_type}"
            )
        
        except Exception as e:
            return self.translator.create_structured_response(
                success=False,
                errors=[f"Analysis failed: {str(e)}"]
            )
    
    def _load_results_file(self, file_path: Path) -> Optional[pd.DataFrame]:
        """Load simulation results from file."""
        try:
            suffix = file_path.suffix.lower()
            
            if suffix == '.csv':
                df = pd.read_csv(file_path, index_col=0, parse_dates=True)
            elif suffix == '.txt':
                # Try space-delimited format common in SUEWS
                df = pd.read_csv(file_path, sep=r'\s+', index_col=0, parse_dates=True)
            elif suffix == '.pkl':
                df = pd.read_pickle(file_path)
            elif suffix == '.parquet':
                df = pd.read_parquet(file_path)
            else:
                return None
            
            # Ensure datetime index
            if not isinstance(df.index, pd.DatetimeIndex):
                df.index = pd.to_datetime(df.index)
            
            return df
        
        except Exception:
            return None
    
    def _analyze_summary(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Perform summary analysis."""
        summary = {
            "data_overview": {
                "n_timesteps": len(df),
                "n_variables": len(df.columns),
                "time_span_days": (df.index[-1] - df.index[0]).days,
                "frequency": self._detect_frequency(df.index)
            },
            "variable_summary": {}
        }
        
        for col in df.columns:
            if df[col].dtype in ['float64', 'float32', 'int64', 'int32']:
                summary["variable_summary"][col] = {
                    "mean": float(df[col].mean()),
                    "std": float(df[col].std()),
                    "min": float(df[col].min()),
                    "max": float(df[col].max()),
                    "missing_values": int(df[col].isna().sum())
                }
        
        return summary
    
    def _analyze_statistics(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Perform detailed statistical analysis."""
        stats = {
            "descriptive_statistics": df.describe().to_dict(),
            "correlations": {},
            "missing_data": df.isna().sum().to_dict()
        }
        
        # Calculate correlations for numeric columns
        numeric_cols = df.select_dtypes(include=[np.number]).columns
        if len(numeric_cols) > 1:
            corr_matrix = df[numeric_cols].corr()
            stats["correlations"] = corr_matrix.to_dict()
        
        return stats
    
    def _analyze_energy_balance(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Analyze energy balance components."""
        energy_vars = ["QH", "QE", "QS", "QN", "QF"]
        available_vars = [var for var in energy_vars if var in df.columns]
        
        if not available_vars:
            return {"error": "No energy balance variables found"}
        
        energy_analysis = {
            "available_variables": available_vars,
            "energy_statistics": {},
            "daily_totals": {},
            "monthly_means": {}
        }
        
        # Statistics for each component
        for var in available_vars:
            series = df[var]
            energy_analysis["energy_statistics"][var] = {
                "mean": float(series.mean()),
                "std": float(series.std()),
                "max": float(series.max()),
                "min": float(series.min()),
                "total_mj_m2": float(series.sum() * 3600 / 1e6)  # Convert W m-2 * hours to MJ m-2
            }
        
        # Daily aggregations
        df_daily = df[available_vars].resample('D').mean()
        energy_analysis["daily_totals"] = self.translator.serialize_dataframe(df_daily, max_rows=10)
        
        # Monthly aggregations
        if len(df) > 30:  # Only if we have enough data
            df_monthly = df[available_vars].resample('M').mean()
            energy_analysis["monthly_means"] = self.translator.serialize_dataframe(df_monthly, max_rows=12)
        
        # Energy balance closure if QN is available
        if "QN" in available_vars:
            energy_balance_vars = [var for var in ["QH", "QE", "QS", "QF"] if var in available_vars]
            if len(energy_balance_vars) >= 3:
                sum_components = df[energy_balance_vars].sum(axis=1)
                qn_values = df["QN"]
                residual = qn_values - sum_components
                
                energy_analysis["energy_balance_closure"] = {
                    "mean_residual": float(residual.mean()),
                    "std_residual": float(residual.std()),
                    "closure_ratio": float((sum_components / qn_values).mean())
                }
        
        return energy_analysis
    
    def _analyze_water_balance(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Analyze water balance components."""
        water_vars = ["QE", "RH2", "Rain", "Runoff", "SMD", "WU"]
        available_vars = [var for var in water_vars if var in df.columns]
        
        if not available_vars:
            return {"error": "No water balance variables found"}
        
        water_analysis = {
            "available_variables": available_vars,
            "water_statistics": {}
        }
        
        for var in available_vars:
            series = df[var]
            water_analysis["water_statistics"][var] = {
                "mean": float(series.mean()),
                "std": float(series.std()),
                "max": float(series.max()),
                "min": float(series.min())
            }
        
        return water_analysis
    
    def _analyze_temporal_patterns(self, df: pd.DataFrame, period: Optional[str]) -> Dict[str, Any]:
        """Analyze temporal patterns in the data."""
        temporal_analysis = {
            "period": period or "hourly",
            "patterns": {}
        }
        
        # Diurnal patterns (hourly averages)
        hourly_means = df.groupby(df.index.hour).mean()
        temporal_analysis["patterns"]["diurnal"] = self.translator.serialize_dataframe(hourly_means)
        
        # Seasonal patterns if we have enough data
        if len(df) > 365 * 24:  # More than a year of hourly data
            seasonal_means = df.groupby(df.index.month).mean()
            temporal_analysis["patterns"]["seasonal"] = self.translator.serialize_dataframe(seasonal_means)
        
        # Weekly patterns
        weekly_means = df.groupby(df.index.dayofweek).mean()
        temporal_analysis["patterns"]["weekly"] = self.translator.serialize_dataframe(weekly_means)
        
        # Resample to requested period if specified
        if period and resample_output:
            try:
                period_map = {
                    "daily": "D",
                    "monthly": "M", 
                    "seasonal": "Q",
                    "annual": "A"
                }
                
                if period in period_map:
                    df_resampled = resample_output(df, period_map[period])
                    temporal_analysis["resampled_data"] = self.translator.serialize_dataframe(df_resampled)
            except Exception:
                temporal_analysis["resample_error"] = f"Failed to resample to {period}"
        
        return temporal_analysis
    
    def _perform_comparison(self, df_results: pd.DataFrame, comparison_path: str) -> Dict[str, Any]:
        """Compare results with reference data."""
        try:
            comparison_path_obj = self.translator.validate_file_path(comparison_path)
            df_reference = self._load_results_file(comparison_path_obj)
            
            if df_reference is None:
                return {"error": f"Failed to load comparison data from {comparison_path}"}
            
            # Find common variables and time period
            common_vars = list(set(df_results.columns) & set(df_reference.columns))
            
            if not common_vars:
                return {"error": "No common variables found between results and reference data"}
            
            # Align time indices
            common_index = df_results.index.intersection(df_reference.index)
            
            if len(common_index) == 0:
                return {"error": "No common time period found between results and reference data"}
            
            df_results_aligned = df_results.loc[common_index, common_vars]
            df_reference_aligned = df_reference.loc[common_index, common_vars]
            
            # Calculate comparison statistics
            comparison_stats = {}
            
            for var in common_vars:
                results_vals = df_results_aligned[var]
                ref_vals = df_reference_aligned[var]
                
                # Calculate metrics
                mae = np.mean(np.abs(results_vals - ref_vals))
                rmse = np.sqrt(np.mean((results_vals - ref_vals) ** 2))
                bias = np.mean(results_vals - ref_vals)
                correlation = np.corrcoef(results_vals, ref_vals)[0, 1]
                
                comparison_stats[var] = {
                    "mae": float(mae),
                    "rmse": float(rmse),
                    "bias": float(bias),
                    "correlation": float(correlation),
                    "n_points": len(common_index)
                }
            
            return {
                "comparison_path": str(comparison_path_obj),
                "common_variables": common_vars,
                "time_overlap": {
                    "start": common_index[0].isoformat(),
                    "end": common_index[-1].isoformat(),
                    "n_points": len(common_index)
                },
                "statistics": comparison_stats
            }
        
        except Exception as e:
            return {"error": f"Comparison failed: {str(e)}"}
    
    def _detect_frequency(self, time_index: pd.DatetimeIndex) -> str:
        """Detect the frequency of the time series."""
        try:
            freq = pd.infer_freq(time_index)
            if freq:
                return freq
            
            # Fallback to calculating median difference
            time_diff = time_index[1:] - time_index[:-1]
            median_diff = time_diff.median()
            
            if median_diff <= pd.Timedelta(minutes=5):
                return "sub-hourly"
            elif median_diff <= pd.Timedelta(hours=1):
                return "hourly"
            elif median_diff <= pd.Timedelta(days=1):
                return "daily"
            else:
                return "unknown"
        except Exception:
            return "unknown"
    
    def _create_text_summary(self, analysis_results: Dict[str, Any], df: pd.DataFrame) -> str:
        """Create human-readable text summary."""
        summary_lines = [
            "SUEWS Simulation Analysis Summary",
            "=" * 40,
            f"Time Period: {df.index[0].strftime('%Y-%m-%d %H:%M')} to {df.index[-1].strftime('%Y-%m-%d %H:%M')}",
            f"Duration: {(df.index[-1] - df.index[0]).days} days",
            f"Variables: {len(df.columns)} ({', '.join(df.columns[:5])}{'...' if len(df.columns) > 5 else ''})",
            f"Timesteps: {len(df)}",
            ""
        ]
        
        if "variable_summary" in analysis_results:
            summary_lines.append("Key Variables Summary:")
            for var, stats in list(analysis_results["variable_summary"].items())[:5]:
                summary_lines.append(f"  {var}: mean={stats['mean']:.2f}, std={stats['std']:.2f}")
        
        return "\n".join(summary_lines)