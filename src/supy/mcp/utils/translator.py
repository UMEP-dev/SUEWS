"""
Parameter translation utilities for MCP tools.

Handles conversion between MCP tool parameters and SuPy function arguments.
"""

import json
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import numpy as np
import pandas as pd


class ParameterTranslator:
    """
    Translates parameters between MCP tools and SuPy functions.
    
    Handles type conversion, validation, and parameter mapping.
    """
    
    @staticmethod
    def validate_file_path(path: Union[str, Path], extensions: Optional[List[str]] = None) -> Path:
        """
        Validate and convert file path.
        
        Parameters
        ----------
        path : str or Path
            File path to validate
        extensions : list, optional
            List of allowed file extensions (e.g., ['.yaml', '.yml'])
            
        Returns
        -------
        Path
            Validated path object
            
        Raises
        ------
        FileNotFoundError
            If file doesn't exist
        ValueError
            If path is invalid or has wrong extension
        """
        if not path:
            raise ValueError("Path cannot be empty")
        
        try:
            path_obj = Path(path).expanduser().resolve()
        except Exception as e:
            raise ValueError(f"Invalid path format: {e}")
        
        if not path_obj.exists():
            raise FileNotFoundError(f"File not found: {path_obj}")
        
        if not path_obj.is_file():
            raise ValueError(f"Path is not a file: {path_obj}")
        
        # Check file extension if specified
        if extensions:
            if path_obj.suffix.lower() not in [ext.lower() for ext in extensions]:
                raise ValueError(
                    f"Invalid file extension '{path_obj.suffix}'. "
                    f"Expected one of: {extensions}"
                )
        
        return path_obj
    
    @staticmethod
    def validate_directory_path(path: Union[str, Path], create: bool = False) -> Path:
        """
        Validate directory path and optionally create it.
        
        Parameters
        ----------
        path : str or Path
            Directory path
        create : bool, optional
            Whether to create directory if it doesn't exist
            
        Returns
        -------
        Path
            Validated directory path
        """
        if not path:
            raise ValueError("Directory path cannot be empty")
        
        try:
            path_obj = Path(path).expanduser().resolve()
        except Exception as e:
            raise ValueError(f"Invalid directory path format: {e}")
        
        if create and not path_obj.exists():
            try:
                path_obj.mkdir(parents=True, exist_ok=True)
            except PermissionError:
                raise PermissionError(f"Permission denied to create directory: {path_obj}")
            except Exception as e:
                raise ValueError(f"Failed to create directory: {e}")
        elif not path_obj.exists():
            raise FileNotFoundError(f"Directory not found: {path_obj}")
        
        if not path_obj.is_dir():
            raise ValueError(f"Path is not a directory: {path_obj}")
        
        return path_obj
    
    @staticmethod
    def parse_config_updates(updates: Union[str, Dict[str, Any]]) -> Dict[str, Any]:
        """
        Parse configuration updates from string or dict.
        
        Parameters
        ----------
        updates : str or dict
            Configuration updates as JSON string or dict
            
        Returns
        -------
        dict
            Parsed configuration updates
        """
        if updates is None:
            return {}
        
        if isinstance(updates, str):
            if not updates.strip():
                return {}
            try:
                return json.loads(updates)
            except json.JSONDecodeError as e:
                raise ValueError(f"Invalid JSON in config updates: {e}")
        
        elif isinstance(updates, dict):
            return updates
        
        else:
            raise ValueError(
                f"Config updates must be JSON string or dictionary, got {type(updates)}"
            )
    
    @staticmethod
    def validate_time_range(
        start_time: Optional[str], 
        end_time: Optional[str],
        allow_same: bool = False
    ) -> tuple:
        """
        Validate time range parameters.
        
        Parameters
        ----------
        start_time : str, optional
            Start time in ISO format
        end_time : str, optional
            End time in ISO format
        allow_same : bool, optional
            Whether to allow start_time == end_time
            
        Returns
        -------
        tuple
            Validated (start_time, end_time) as pandas Timestamp objects or None
        """
        start_ts = None
        end_ts = None
        
        if start_time:
            try:
                start_ts = pd.Timestamp(start_time)
            except (ValueError, TypeError) as e:
                raise ValueError(
                    f"Invalid start_time format '{start_time}'. "
                    f"Expected ISO format (e.g., '2024-01-01T00:00:00'): {e}"
                )
        
        if end_time:
            try:
                end_ts = pd.Timestamp(end_time)
            except (ValueError, TypeError) as e:
                raise ValueError(
                    f"Invalid end_time format '{end_time}'. "
                    f"Expected ISO format (e.g., '2024-12-31T23:00:00'): {e}"
                )
        
        if start_ts and end_ts:
            if start_ts > end_ts:
                raise ValueError(
                    f"start_time ({start_ts}) must be before or equal to end_time ({end_ts})"
                )
            elif start_ts == end_ts and not allow_same:
                raise ValueError("start_time and end_time cannot be the same")
        
        return start_ts, end_ts
    
    @staticmethod
    def validate_output_format(format_name: str) -> str:
        """
        Validate output format parameter.
        
        Parameters
        ----------
        format_name : str
            Output format name
            
        Returns
        -------
        str
            Validated format name
        """
        if not format_name:
            raise ValueError("Output format cannot be empty")
        
        valid_formats = ["txt", "csv", "parquet", "pickle", "json", "nc", "netcdf"]
        format_lower = format_name.lower()
        
        if format_lower not in valid_formats:
            raise ValueError(
                f"Invalid output format '{format_name}'. "
                f"Valid formats: {', '.join(valid_formats)}"
            )
        
        return format_lower
    
    @staticmethod
    def validate_analysis_variables(
        variables: Optional[Union[str, List[str]]]
    ) -> Optional[List[str]]:
        """
        Validate analysis variables parameter.
        
        Parameters
        ----------
        variables : str, list, optional
            Variable names for analysis
            
        Returns
        -------
        list or None
            Validated list of variable names
        """
        if not variables:
            return None
        
        if isinstance(variables, str):
            # Split comma-separated string
            var_list = [v.strip() for v in variables.split(",") if v.strip()]
        elif isinstance(variables, (list, tuple)):
            var_list = list(variables)
        else:
            raise ValueError(
                f"Variables must be string or list of strings, got {type(variables)}"
            )
        
        if not var_list:
            return None
        
        # Validate variable names
        validated_vars = []
        for var in var_list:
            if not isinstance(var, str):
                raise ValueError(f"Variable name must be string, got {type(var)}: {var}")
            
            var = var.strip()
            if not var:
                continue
                
            # Allow alphanumeric, underscores, and common SUEWS variable patterns
            if not all(c.isalnum() or c in "_" for c in var):
                raise ValueError(
                    f"Invalid variable name '{var}'. "
                    f"Variable names should contain only letters, numbers, and underscores"
                )
            
            validated_vars.append(var)
        
        return validated_vars if validated_vars else None
    
    @staticmethod
    def validate_numeric_parameter(
        value: Any,
        param_name: str,
        min_val: Optional[float] = None,
        max_val: Optional[float] = None,
        allow_none: bool = False
    ) -> Optional[float]:
        """
        Validate numeric parameter.
        
        Parameters
        ----------
        value : any
            Value to validate
        param_name : str
            Parameter name for error messages
        min_val : float, optional
            Minimum allowed value
        max_val : float, optional
            Maximum allowed value
        allow_none : bool, optional
            Whether None is allowed
            
        Returns
        -------
        float or None
            Validated numeric value
        """
        if value is None:
            if allow_none:
                return None
            else:
                raise ValueError(f"{param_name} cannot be None")
        
        try:
            num_val = float(value)
        except (TypeError, ValueError):
            raise ValueError(
                f"{param_name} must be a number, got {type(value)}: {value}"
            )
        
        if min_val is not None and num_val < min_val:
            raise ValueError(
                f"{param_name} must be >= {min_val}, got {num_val}"
            )
        
        if max_val is not None and num_val > max_val:
            raise ValueError(
                f"{param_name} must be <= {max_val}, got {num_val}"
            )
        
        return num_val
    
    @staticmethod
    def create_structured_response(
        success: bool,
        data: Optional[Any] = None,
        message: Optional[str] = None,
        errors: Optional[List[str]] = None,
        warnings: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Create structured response for MCP tools.
        
        Parameters
        ----------
        success : bool
            Whether operation was successful
        data : any, optional
            Response data
        message : str, optional
            Response message
        errors : list, optional
            Error messages
        warnings : list, optional
            Warning messages
            
        Returns
        -------
        dict
            Structured response
        """
        response = {
            "success": success,
            "timestamp": pd.Timestamp.now().isoformat(),
        }
        
        if data is not None:
            response["data"] = data
        
        if message:
            response["message"] = message
        
        if errors:
            response["errors"] = errors
            response["success"] = False  # Ensure success is False if errors exist
        
        if warnings:
            response["warnings"] = warnings
        
        return response
    
    @staticmethod
    def serialize_dataframe(
        df: pd.DataFrame, 
        max_rows: int = 100,
        include_stats: bool = False
    ) -> Dict[str, Any]:
        """
        Serialize DataFrame for MCP response.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame to serialize
        max_rows : int, optional
            Maximum number of rows to include
        include_stats : bool, optional
            Whether to include basic statistics
            
        Returns
        -------
        dict
            Serialized DataFrame information
        """
        if df is None or df.empty:
            return {
                "shape": [0, 0], 
                "columns": [], 
                "data": [],
                "empty": True
            }
        
        # Limit rows for response size
        df_sample = df.head(max_rows) if len(df) > max_rows else df
        
        # Convert to serializable format
        try:
            # Handle various data types
            data = []
            for _, row in df_sample.iterrows():
                row_data = []
                for val in row:
                    if pd.isna(val):
                        row_data.append(None)
                    elif isinstance(val, (np.integer, np.floating)):
                        row_data.append(float(val))
                    elif isinstance(val, (pd.Timestamp, np.datetime64)):
                        row_data.append(pd.Timestamp(val).isoformat())
                    else:
                        row_data.append(str(val))
                data.append(row_data)
        except Exception as e:
            # Fallback to simple conversion
            data = df_sample.values.tolist()
        
        # Handle datetime index
        if isinstance(df_sample.index, pd.DatetimeIndex):
            index_data = [ts.isoformat() for ts in df_sample.index]
        else:
            index_data = df_sample.index.tolist()
        
        result = {
            "shape": list(df.shape),
            "columns": list(df.columns),
            "index": index_data,
            "data": data,
            "dtypes": {col: str(dtype) for col, dtype in df.dtypes.items()},
            "truncated": len(df) > max_rows,
            "empty": False
        }
        
        # Add basic statistics if requested
        if include_stats and not df.empty:
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            if len(numeric_cols) > 0:
                stats = {}
                for col in numeric_cols:
                    stats[col] = {
                        "mean": float(df[col].mean()),
                        "std": float(df[col].std()),
                        "min": float(df[col].min()),
                        "max": float(df[col].max())
                    }
                result["statistics"] = stats
        
        return result
    
    @staticmethod
    def map_supy_parameters(mcp_params: Dict[str, Any]) -> Dict[str, Any]:
        """
        Map MCP tool parameters to SuPy function parameters.
        
        Parameters
        ----------
        mcp_params : dict
            MCP tool parameters
            
        Returns
        -------
        dict
            Mapped SuPy parameters
        """
        # Common parameter mappings
        param_map = {
            "config_path": "path_config",
            "forcing_path": "path_forcing",
            "output_path": "path_output",
            "time_step": "tstep",
            "start_time": "start_dt",
            "end_time": "end_dt",
            "site_name": "site",
        }
        
        supy_params = {}
        for mcp_key, mcp_value in mcp_params.items():
            if mcp_value is not None:
                # Use mapped key if available, otherwise use original
                supy_key = param_map.get(mcp_key, mcp_key)
                supy_params[supy_key] = mcp_value
        
        return supy_params