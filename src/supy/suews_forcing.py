"""
SUEWSForcing - OOP wrapper for SUEWS meteorological forcing data.

Provides a structured interface for loading, validating, and analysing
meteorological forcing data for SUEWS simulations.
"""

import warnings
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd


# Variable aliases for more intuitive access
FORCING_ALIASES = {
    # Technical name -> Human-readable aliases
    "Tair": ["temperature", "air_temperature", "temp", "t_air"],
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
    "qf": ["anthropogenic_heat", "qf_obs"],
    "qs": ["storage_heat"],
    "snow": ["snowfall"],
    "Wuh": ["water_use", "external_water"],
    "lai": ["leaf_area_index"],
    "kdiff": ["diffuse_radiation", "diffuse"],
    "kdir": ["direct_radiation", "direct"],
    "wdir": ["wind_direction"],
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

    Provides intuitive access to forcing variables, validation, analysis,
    and manipulation methods for SUEWS meteorological input data.

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

    Access variables with intuitive names:

    >>> forcing.temperature  # Same as forcing.Tair
    >>> forcing.wind_speed  # Same as forcing.U

    Validate forcing data:

    >>> result = forcing.validate()
    >>> if not result.is_valid:
    ...     print(result.errors)

    Analyse data:

    >>> forcing.summary()  # Statistical summary
    >>> forcing.diurnal_pattern()  # Mean diurnal cycle

    Manipulate data:

    >>> forcing_2023 = forcing.slice("2023-01-01", "2023-12-31")
    >>> forcing_hourly = forcing.resample("1H")
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
    def from_file(cls, path: Union[str, Path], tstep_mod: int = 300) -> "SUEWSForcing":
        """
        Load forcing from a single file.

        Parameters
        ----------
        path : str or Path
            Path to forcing file
        tstep_mod : int, optional
            Model timestep in seconds (default 300s = 5 min)

        Returns
        -------
        SUEWSForcing
            Loaded forcing data
        """
        from .util._io import read_forcing

        path = Path(path).expanduser().resolve()
        if not path.exists():
            raise FileNotFoundError(f"Forcing file not found: {path}")

        df = read_forcing(str(path), tstep_mod=tstep_mod)
        return cls(df, source=str(path))

    @classmethod
    def from_files(
        cls, paths: List[Union[str, Path]], tstep_mod: int = 300
    ) -> "SUEWSForcing":
        """
        Load and concatenate forcing from multiple files.

        Parameters
        ----------
        paths : list of str or Path
            Paths to forcing files (concatenated in order)
        tstep_mod : int, optional
            Model timestep in seconds (default 300s = 5 min)

        Returns
        -------
        SUEWSForcing
            Concatenated forcing data
        """
        from .util._io import read_forcing

        if not paths:
            raise ValueError("Empty forcing file list provided")

        dfs = []
        for p in paths:
            path = Path(p).expanduser().resolve()
            if not path.exists():
                raise FileNotFoundError(f"Forcing file not found: {path}")
            df = read_forcing(str(path), tstep_mod=tstep_mod)
            dfs.append(df)

        combined = pd.concat(dfs, axis=0).sort_index()
        # Remove any duplicates
        combined = combined[~combined.index.duplicated(keep="first")]
        return cls(combined, source=f"[{len(paths)} files]")

    @classmethod
    def from_dataframe(cls, df: pd.DataFrame) -> "SUEWSForcing":
        """
        Create from existing DataFrame.

        Parameters
        ----------
        df : pd.DataFrame
            DataFrame with forcing data and DatetimeIndex

        Returns
        -------
        SUEWSForcing
            Wrapped forcing data
        """
        return cls(df, source="DataFrame")

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
    def times(self) -> pd.DatetimeIndex:
        """Datetime index of forcing data.

        .. deprecated::
            Use :attr:`index` instead for pandas-compatible access.
        """
        warnings.warn(
            "SUEWSForcing.times is deprecated, use .index instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return self._data.index

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
        # Calculate from data
        if len(self._data) > 1:
            diff = self._data.index.to_series().diff().iloc[-1]
            return diff
        return pd.Timedelta("5min")  # Default

    @property
    def timestep_seconds(self) -> int:
        """Timestep in seconds."""
        return int(self.timestep.total_seconds())

    @property
    def n_timesteps(self) -> int:
        """Number of timesteps.

        .. deprecated::
            Use ``len(forcing)`` instead.
        """
        warnings.warn(
            "SUEWSForcing.n_timesteps is deprecated, use len(forcing) instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return len(self._data)

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
        Dynamic attribute access for variables with alias support.

        Allows access like `forcing.temperature` instead of `forcing['Tair']`.
        """
        # Check if it's a known alias
        canonical = _ALIAS_TO_CANONICAL.get(name.lower())
        if canonical is not None and canonical in self._data.columns:
            return self._data[canonical]

        # Check if it's a direct column name
        if name in self._data.columns:
            return self._data[name]

        raise AttributeError(
            f"'{type(self).__name__}' has no attribute '{name}'. "
            f"Available columns: {list(self._data.columns)}"
        )

    def __getitem__(self, key: str) -> pd.Series:
        """Access variables by column name."""
        # Support alias lookup
        canonical = _ALIAS_TO_CANONICAL.get(key.lower())
        if canonical is not None and canonical in self._data.columns:
            return self._data[canonical]
        return self._data[key]

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
        warnings = []

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
            warnings=warnings,
        )

        if raise_on_error:
            self._validation_result.raise_if_invalid()

        return self._validation_result

    # =========================================================================
    # Analysis (convenience functions)
    # =========================================================================

    def summary(self) -> pd.DataFrame:
        """
        Statistical summary of all forcing variables.

        Returns
        -------
        pd.DataFrame
            Summary statistics (count, mean, std, min, max, etc.)
        """
        # Exclude time columns
        time_cols = ["iy", "id", "it", "imin", "isec"]
        data_cols = [c for c in self._data.columns if c not in time_cols]
        return self._data[data_cols].describe()

    def diurnal_pattern(self, var: Optional[str] = None) -> pd.DataFrame:
        """
        Calculate mean diurnal patterns.

        Parameters
        ----------
        var : str, optional
            Specific variable to analyse. If None, analyses all.

        Returns
        -------
        pd.DataFrame
            Hourly mean values grouped by hour of day
        """
        df = self._data.copy()
        df["hour"] = df.index.hour

        # Select columns
        time_cols = ["iy", "id", "it", "imin", "isec", "hour"]
        if var is not None:
            canonical = _ALIAS_TO_CANONICAL.get(var.lower(), var)
            cols = [canonical] if canonical in df.columns else [var]
        else:
            cols = [c for c in df.columns if c not in time_cols]

        return df.groupby("hour")[cols].mean()

    def check_gaps(self) -> pd.DataFrame:
        """
        Report gaps in forcing data.

        Returns
        -------
        pd.DataFrame
            DataFrame with gap information (start, end, duration)
        """
        idx = self._data.index
        expected_freq = self.timestep

        # Find gaps
        gaps = []
        for i in range(1, len(idx)):
            actual_diff = idx[i] - idx[i - 1]
            if actual_diff > expected_freq:
                gaps.append({
                    "gap_start": idx[i - 1],
                    "gap_end": idx[i],
                    "duration": actual_diff,
                    "missing_steps": int(actual_diff / expected_freq) - 1,
                })

        if gaps:
            return pd.DataFrame(gaps)
        else:
            return pd.DataFrame(
                columns=["gap_start", "gap_end", "duration", "missing_steps"]
            )

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
    # Manipulation
    # =========================================================================

    def slice(
        self,
        start: Optional[str] = None,
        end: Optional[str] = None,
    ) -> "SUEWSForcing":
        """
        Return a new SUEWSForcing for a time slice.

        Parameters
        ----------
        start : str, optional
            Start date/time (inclusive)
        end : str, optional
            End date/time (inclusive)

        Returns
        -------
        SUEWSForcing
            New forcing object with sliced data
        """
        sliced_data = self._data.loc[start:end]
        return SUEWSForcing(sliced_data, source=f"{self._source}[{start}:{end}]")

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
    # Plotting
    # =========================================================================

    def plot(
        self,
        var: Optional[str] = None,
        ax=None,
        **kwargs,
    ):
        """
        Quick timeseries plot.

        Parameters
        ----------
        var : str, optional
            Variable to plot. If None, plots temperature.
        ax : matplotlib.axes.Axes, optional
            Axes to plot on
        **kwargs
            Additional arguments passed to plot

        Returns
        -------
        tuple
            (fig, ax) matplotlib objects
        """
        import matplotlib.pyplot as plt

        if ax is None:
            fig, ax = plt.subplots(figsize=(12, 4))
        else:
            fig = ax.get_figure()

        # Default to temperature
        if var is None:
            var = "Tair"

        # Resolve alias
        canonical = _ALIAS_TO_CANONICAL.get(var.lower(), var)
        if canonical not in self._data.columns:
            raise ValueError(f"Variable '{var}' not found in forcing data")

        self._data[canonical].plot(ax=ax, **kwargs)
        ax.set_ylabel(canonical)
        ax.set_xlabel("Time")
        ax.set_title(f"Forcing: {canonical}")

        return fig, ax

    def plot_availability(self, ax=None, **kwargs):
        """
        Plot data availability heatmap.

        Shows which variables have data at which times.

        Parameters
        ----------
        ax : matplotlib.axes.Axes, optional
            Axes to plot on
        **kwargs
            Additional arguments passed to imshow

        Returns
        -------
        tuple
            (fig, ax) matplotlib objects
        """
        import matplotlib.pyplot as plt

        if ax is None:
            fig, ax = plt.subplots(figsize=(14, 6))
        else:
            fig = ax.get_figure()

        # Exclude time columns
        time_cols = ["iy", "id", "it", "imin", "isec"]
        data_cols = [c for c in self._data.columns if c not in time_cols]

        # Create availability matrix (1 = valid, 0 = missing)
        avail = self._data[data_cols].replace(-999, np.nan).notna().astype(int)

        # Downsample for visualisation if too many timesteps
        if len(avail) > 1000:
            avail = avail.resample("1D").mean()

        ax.imshow(avail.T, aspect="auto", cmap="RdYlGn", **kwargs)
        ax.set_yticks(range(len(data_cols)))
        ax.set_yticklabels(data_cols)
        ax.set_xlabel("Time")
        ax.set_title("Data Availability (green=available, red=missing)")

        return fig, ax

    # =========================================================================
    # Export
    # =========================================================================

    def to_dataframe(self) -> pd.DataFrame:
        """Return copy of underlying DataFrame.

        .. deprecated::
            Use :attr:`df` property instead.
        """
        warnings.warn(
            "SUEWSForcing.to_dataframe() is deprecated, use .df instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return self._data.copy()

    def to_suews_format(self, path: Union[str, Path]) -> Path:
        """
        Export in SUEWS native text format.

        Parameters
        ----------
        path : str or Path
            Output file path

        Returns
        -------
        Path
            Path to saved file
        """
        path = Path(path)

        # Generate datetime columns
        df_save = self._data.copy()

        # Format and save
        df_save.to_csv(path, sep="\t", index=True)

        return path

    def to_csv(self, path: Union[str, Path], **kwargs) -> Path:
        """
        Export to CSV format.

        Parameters
        ----------
        path : str or Path
            Output file path
        **kwargs
            Additional arguments passed to to_csv

        Returns
        -------
        Path
            Path to saved file
        """
        path = Path(path)
        self._data.to_csv(path, **kwargs)
        return path

    # =========================================================================
    # Rich display
    # =========================================================================

    def __repr__(self) -> str:
        """Concise representation of forcing data."""
        start, end = self.time_range
        n_steps = self.n_timesteps
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
        n_steps = self.n_timesteps
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
