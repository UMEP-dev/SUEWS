"""
SUEWSForcing - OOP wrapper for SUEWS meteorological forcing data.

Provides a structured interface for loading, validating, and manipulating
meteorological forcing data for SUEWS simulations.
"""

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd


# Variable aliases for more intuitive access
FORCING_ALIASES = {
    # Technical name -> Human-readable aliases
    "Tair": ["temperature", "air_temperature", "temp", "t_air", "ta"],
    "RH": ["relative_humidity", "humidity", "rh"],
    "pres": ["pressure", "air_pressure", "p"],
    "U": ["wind_speed", "wind", "u"],
    "kdown": ["shortwave_down", "solar_radiation", "sw_down", "k_down"],
    "ldown": ["longwave_down", "lw_down", "l_down"],
    "rain": ["precipitation", "rainfall", "precip"],
    "fcld": ["cloud_fraction", "cloud_cover", "clouds"],
    "xsmd": ["soil_moisture", "smd"],
    "qn": ["net_radiation", "qstar", "q_star"],
    "qh": ["sensible_heat", "h"],
    "qe": ["latent_heat", "le"],
    "qf": ["anthropogenic_heat"],
    "qs": ["storage_heat"],
    "snow": ["snowfall"],
    "Wuh": ["water_use", "external_water"],
    "lai": ["leaf_area_index"],
    "kdiff": ["diffuse_radiation"],
    "kdir": ["direct_radiation"],
    "wdir": ["wind_direction", "wd"],
}

# Build reverse mapping: alias -> canonical name
_ALIAS_TO_CANONICAL = {}
for canonical, aliases in FORCING_ALIASES.items():
    for alias in aliases:
        _ALIAS_TO_CANONICAL[alias.lower()] = canonical
    _ALIAS_TO_CANONICAL[canonical.lower()] = canonical


# Variable types for resampling (from _load.py)
FORCING_VAR_TYPES = {
    "iy": "time",
    "id": "time",
    "it": "time",
    "imin": "time",
    "qn": "avg",
    "qh": "avg",
    "qe": "avg",
    "qs": "avg",
    "qf": "avg",
    "U": "inst",
    "RH": "inst",
    "Tair": "inst",
    "pres": "inst",
    "rain": "sum",
    "kdown": "avg",
    "snow": "inst",
    "ldown": "avg",
    "fcld": "inst",
    "Wuh": "sum",
    "xsmd": "inst",
    "lai": "inst",
    "kdiff": "avg",
    "kdir": "avg",
    "wdir": "inst",
}

# Required columns for SUEWS forcing
REQUIRED_COLUMNS = [
    "iy",
    "id",
    "it",
    "imin",
    "qn",
    "qh",
    "qe",
    "qs",
    "qf",
    "U",
    "RH",
    "Tair",
    "pres",
    "rain",
    "kdown",
    "snow",
    "ldown",
    "fcld",
    "Wuh",
    "xsmd",
    "lai",
    "kdiff",
    "kdir",
    "wdir",
]


@dataclass
class ValidationResult:
    """Container for forcing validation results."""

    is_valid: bool
    errors: List[str]
    warnings: List[str]

    def raise_if_invalid(self):
        """Raise ValueError if forcing is invalid."""
        if not self.is_valid:
            error_msg = "; ".join(self.errors)
            raise ValueError(f"Invalid forcing data: {error_msg}")

    def __repr__(self) -> str:
        status = "Valid" if self.is_valid else "Invalid"
        n_errors = len(self.errors)
        n_warnings = len(self.warnings)
        return f"ValidationResult({status}, {n_errors} errors, {n_warnings} warnings)"


class SUEWSForcing:
    """
    Wrapper for meteorological forcing data with convenience functions.

    Provides intuitive access to forcing variables, validation, and
    manipulation methods for SUEWS meteorological input data.

    Parameters
    ----------
    data : pd.DataFrame
        Forcing DataFrame with DatetimeIndex
    source : str, optional
        Description of data source (e.g., file path)

    Examples
    --------
    Load from file:

    >>> forcing = SUEWSForcing.from_file("forcing_2023.txt")
    >>> forcing
    SUEWSForcing(2023-01-01 00:00 to 2023-12-31 23:00, 8760 timesteps @ 3600s)

    Load multiple files:

    >>> forcing = SUEWSForcing.from_file([
    ...     "forcing_2023.txt",
    ...     "forcing_2024.txt",
    ... ])

    Access variables with intuitive names:

    >>> forcing.temperature  # Same as forcing.Tair
    >>> forcing.wind_speed  # Same as forcing.U

    Case-insensitive access:

    >>> forcing.tair  # Same as forcing.Tair

    Validate forcing data:

    >>> result = forcing.validate()
    >>> if not result.is_valid:
    ...     print(result.errors)
    """

    def __init__(self, data: pd.DataFrame, source: Optional[str] = None):
        """
        Initialise SUEWSForcing with validated DataFrame.

        Parameters
        ----------
        data : pd.DataFrame
            Forcing DataFrame with DatetimeIndex
        source : str, optional
            Description of data source (e.g., file path)
        """
        self._data = data.copy()
        self._source = source
        self._validation_result: Optional[ValidationResult] = None

    # =========================================================================
    # Construction methods
    # =========================================================================

    @classmethod
    def from_file(
        cls, path: Union[str, Path, List[Union[str, Path]]], tstep_mod: int = 300
    ) -> "SUEWSForcing":
        """
        Load forcing from file(s).

        Parameters
        ----------
        path : str, Path, or list of str/Path
            Path to forcing file, or list of paths to concatenate
        tstep_mod : int, optional
            Model timestep in seconds (default 300s = 5 min)

        Returns
        -------
        SUEWSForcing
            Loaded forcing data

        Examples
        --------
        Single file:

        >>> forcing = SUEWSForcing.from_file("forcing_2023.txt")

        Multiple files:

        >>> forcing = SUEWSForcing.from_file(["2023.txt", "2024.txt"])
        """
        from .util._io import _read_forcing_impl

        # Handle list of paths
        if isinstance(path, list):
            if not path:
                raise ValueError("Empty forcing file list provided")

            dfs = []
            for p in path:
                file_path = Path(p).expanduser().resolve()
                if not file_path.exists():
                    raise FileNotFoundError(f"Forcing file not found: {file_path}")
                df = _read_forcing_impl(str(file_path), tstep_mod=tstep_mod)
                dfs.append(df)

            combined = pd.concat(dfs, axis=0).sort_index()
            # Remove any duplicates
            combined = combined[~combined.index.duplicated(keep="first")]
            return cls(combined, source=f"[{len(path)} files]")

        # Handle single path
        file_path = Path(path).expanduser().resolve()
        if not file_path.exists():
            raise FileNotFoundError(f"Forcing file not found: {file_path}")

        df = _read_forcing_impl(str(file_path), tstep_mod=tstep_mod)
        return cls(df, source=str(file_path))

    # =========================================================================
    # Core data access
    # =========================================================================

    @property
    def df(self) -> pd.DataFrame:
        """Access underlying DataFrame."""
        return self._data.copy()

    @property
    def index(self) -> pd.DatetimeIndex:
        """Datetime index of forcing data (pandas-compatible)."""
        return self._data.index

    @property
    def loc(self):
        """Label-based indexer (pandas-compatible)."""
        return self._data.loc

    @property
    def iloc(self):
        """Integer-based indexer (pandas-compatible)."""
        return self._data.iloc

    @property
    def time_range(self) -> Tuple[pd.Timestamp, pd.Timestamp]:
        """Return (start, end) timestamps."""
        return (self._data.index[0], self._data.index[-1])

    @property
    def timestep(self) -> pd.Timedelta:
        """Timestep of forcing data."""
        freq = self._data.index.freq
        if freq is not None:
            return pd.Timedelta(freq)
        # Calculate from data using median for robustness with irregular timesteps
        if len(self._data) > 1:
            diffs = self._data.index.to_series().diff().dropna()
            return diffs.median()
        return pd.Timedelta("5min")  # Default

    @property
    def timestep_seconds(self) -> int:
        """Timestep in seconds."""
        return int(self.timestep.total_seconds())

    @property
    def columns(self) -> pd.Index:
        """Column names in the forcing data."""
        return self._data.columns

    @property
    def source(self) -> Optional[str]:
        """Data source description."""
        return self._source

    def __getattr__(self, name: str) -> pd.Series:
        """
        Dynamic attribute access for variables with alias and case-insensitive support.

        Allows access like `forcing.temperature` instead of `forcing['Tair']`.
        Also supports case-insensitive access: `forcing.tair` works.
        """
        # Check if it's a known alias (case-insensitive)
        canonical = _ALIAS_TO_CANONICAL.get(name.lower())
        if canonical is not None and canonical in self._data.columns:
            return self._data[canonical]

        # Check if it's a direct column name (exact match)
        if name in self._data.columns:
            return self._data[name]

        # Case-insensitive column lookup
        name_lower = name.lower()
        for col in self._data.columns:
            if col.lower() == name_lower:
                return self._data[col]

        raise AttributeError(
            f"'{type(self).__name__}' has no attribute '{name}'. "
            f"Available columns: {list(self._data.columns)}"
        )

    def __getitem__(self, key: str) -> pd.Series:
        """Access variables by column name (case-insensitive, alias-aware)."""
        # Support alias lookup
        canonical = _ALIAS_TO_CANONICAL.get(key.lower())
        if canonical is not None and canonical in self._data.columns:
            return self._data[canonical]

        # Exact match
        if key in self._data.columns:
            return self._data[key]

        # Case-insensitive fallback
        key_lower = key.lower()
        for col in self._data.columns:
            if col.lower() == key_lower:
                return self._data[col]

        return self._data[key]  # Let pandas raise KeyError

    # =========================================================================
    # Validation
    # =========================================================================

    def validate(
        self,
        physics: Optional[Any] = None,
        raise_on_error: bool = False,
    ) -> ValidationResult:
        """
        Validate forcing data comprehensively.

        Checks:
        1. Required columns present
        2. Temporal index validity (DatetimeIndex, monotonic, no duplicates)
        3. Physical range validation for each variable
        4. Physics-specific requirements (e.g., if netradiationmethod=0, qn required)

        Parameters
        ----------
        physics : ModelPhysics, optional
            Model physics configuration for physics-aware validation
        raise_on_error : bool
            If True, raise ValueError on any validation failure

        Returns
        -------
        ValidationResult
            Validation results with errors and warnings
        """
        from ._check import check_forcing

        errors = []
        warnings_list = []

        # Use existing check_forcing logic
        physics_dict = None
        if physics is not None:
            if hasattr(physics, "model_dump"):
                physics_dict = physics.model_dump()
            elif isinstance(physics, dict):
                physics_dict = physics

        result = check_forcing(self._data, fix=False, physics=physics_dict)

        if isinstance(result, list):
            errors.extend(result)

        self._validation_result = ValidationResult(
            is_valid=len(errors) == 0,
            errors=errors,
            warnings=warnings_list,
        )

        if raise_on_error:
            self._validation_result.raise_if_invalid()

        return self._validation_result

    # =========================================================================
    # Analysis (domain-specific methods)
    # =========================================================================

    def completeness(self) -> Dict[str, float]:
        """
        Calculate data completeness for each variable.

        Returns
        -------
        dict
            Mapping variable names to completeness percentage (0-100)
        """
        time_cols = ["iy", "id", "it", "imin", "isec"]
        data_cols = [c for c in self._data.columns if c not in time_cols]

        result = {}
        for col in data_cols:
            # Count non-null and non-missing (-999) values
            valid = self._data[col].replace(-999, np.nan).notna().sum()
            total = len(self._data)
            result[col] = 100.0 * valid / total if total > 0 else 0.0

        return result

    # =========================================================================
    # Manipulation (domain-specific methods)
    # =========================================================================

    def resample(self, freq: str) -> "SUEWSForcing":
        """
        Resample to different temporal resolution.

        Uses appropriate aggregation methods for each variable type:
        - Instantaneous variables: last value
        - Average variables: mean
        - Sum variables: sum

        Parameters
        ----------
        freq : str
            Target frequency (e.g., "1H", "30min")

        Returns
        -------
        SUEWSForcing
            New forcing object at resampled frequency
        """
        resampled = self._data.copy()

        # Build aggregation dict based on variable types
        agg_dict = {}
        for col in resampled.columns:
            var_type = FORCING_VAR_TYPES.get(col, "inst")
            if var_type == "time":
                agg_dict[col] = "last"
            elif var_type == "avg":
                agg_dict[col] = "mean"
            elif var_type == "sum":
                agg_dict[col] = "sum"
            else:  # inst
                agg_dict[col] = "last"

        resampled = resampled.resample(freq, closed="right", label="right").agg(
            agg_dict
        )

        return SUEWSForcing(resampled, source=f"{self._source}@{freq}")

    def fill_gaps(self, method: str = "interpolate", **kwargs) -> "SUEWSForcing":
        """
        Fill missing values in forcing data.

        Uses appropriate handling for different variable types:
        - Sum variables (rain): filled with 0
        - Other variables: filled using specified method

        Parameters
        ----------
        method : str
            Fill method: "interpolate", "ffill", "bfill"
        **kwargs
            Additional arguments passed to the fill method

        Returns
        -------
        SUEWSForcing
            New forcing object with gaps filled
        """
        filled = self._data.copy()

        # Replace -999 with NaN for filling
        filled = filled.replace(-999, np.nan)

        # Handle sum variables (rain, Wuh) specially - fill with 0
        sum_vars = [
            col for col in filled.columns if FORCING_VAR_TYPES.get(col) == "sum"
        ]
        for var in sum_vars:
            if var in filled.columns:
                filled[var] = filled[var].fillna(0)

        # Fill other variables with specified method
        if method == "interpolate":
            filled = filled.interpolate(**kwargs)
        elif method == "ffill":
            filled = filled.ffill(**kwargs)
        elif method == "bfill":
            filled = filled.bfill(**kwargs)
        else:
            raise ValueError(f"Unknown fill method: {method}")

        return SUEWSForcing(filled, source=f"{self._source}[filled]")

    # =========================================================================
    # Export
    # =========================================================================

    def save(self, path: Union[str, Path], format: str = "suews") -> Path:
        """
        Save forcing data to file.

        Parameters
        ----------
        path : str or Path
            Output file path
        format : str, optional
            Output format: "suews" (default) or "csv"

        Returns
        -------
        Path
            Path to saved file
        """
        path = Path(path)

        if format == "suews":
            # SUEWS native text format
            self._data.to_csv(path, sep="\t", index=True)
        elif format == "csv":
            self._data.to_csv(path, index=True)
        else:
            raise ValueError(f"Unknown format: {format}. Use 'suews' or 'csv'.")

        return path

    # =========================================================================
    # Rich display
    # =========================================================================

    def __repr__(self) -> str:
        """Concise representation of forcing data."""
        start, end = self.time_range
        n_steps = len(self._data)
        tstep = self.timestep_seconds

        # Count available variables (excluding time columns)
        time_cols = ["iy", "id", "it", "imin", "isec"]
        n_vars = len([c for c in self._data.columns if c not in time_cols])

        return (
            f"SUEWSForcing({start} to {end}, "
            f"{n_steps} timesteps @ {tstep}s, {n_vars} variables)"
        )

    def _repr_html_(self) -> str:
        """HTML representation for Jupyter notebooks."""
        start, end = self.time_range
        n_steps = len(self._data)
        tstep = self.timestep_seconds

        # Get completeness info
        completeness = self.completeness()
        complete_vars = sum(1 for v in completeness.values() if v > 99)
        total_vars = len(completeness)

        html = f"""
        <div style="border: 1px solid #ccc; padding: 10px; border-radius: 5px;">
            <h4 style="margin: 0 0 10px 0;">SUEWSForcing</h4>
            <table style="border-collapse: collapse;">
                <tr><td><strong>Time range:</strong></td><td>{start} to {end}</td></tr>
                <tr><td><strong>Timestep:</strong></td><td>{tstep} seconds</td></tr>
                <tr><td><strong>Timesteps:</strong></td><td>{n_steps}</td></tr>
                <tr><td><strong>Variables:</strong></td><td>{complete_vars}/{total_vars} complete</td></tr>
                <tr><td><strong>Source:</strong></td><td>{self._source or "Unknown"}</td></tr>
            </table>
        </div>
        """
        return html

    def __len__(self) -> int:
        """Number of timesteps."""
        return len(self._data)
