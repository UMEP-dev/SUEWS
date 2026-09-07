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

from .data_model.forcing import FORCING_REGISTRY

# Compatibility projections retained for downstream users of these module
# constants. File aliases remain a separate registry namespace.
FORCING_ALIASES = FORCING_REGISTRY.accessor_aliases

# Build reverse mapping: alias -> canonical name
_ALIAS_TO_CANONICAL = {}
for canonical, aliases in FORCING_ALIASES.items():
    for alias in aliases:
        _ALIAS_TO_CANONICAL[alias.lower()] = canonical
    _ALIAS_TO_CANONICAL[canonical.lower()] = canonical


FORCING_VAR_TYPES = {
    variable.name: variable.temporal
    for variable in FORCING_REGISTRY.variables
    if variable.legacy_position is not None or variable.fallback == "lai"
}

# Required columns for SUEWS forcing
REQUIRED_COLUMNS = list(FORCING_REGISTRY.canonical_file_columns)


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


def _normalise_extras(
    extras: Optional[Dict[str, np.ndarray]],
) -> Dict[str, np.ndarray]:
    """Return extras as detached numpy arrays."""
    if not extras:
        return {}
    return {name: np.asarray(values).copy() for name, values in extras.items()}


def _as_forcing(data, source, extras: Optional[Dict[str, np.ndarray]] = None):
    """Wrap sliced data as SUEWSForcing, converting single-row Series to DataFrame."""
    if isinstance(data, pd.Series):
        data = data.to_frame().T
    forcing = SUEWSForcing(data, source=source)
    forcing._extras = _normalise_extras(extras)
    return forcing


class _ForcingLocIndexer:
    """Wrapper for loc indexer that returns SUEWSForcing objects."""

    def __init__(self, forcing: "SUEWSForcing"):
        self._forcing = forcing

    def __getitem__(self, key):
        """Return sliced data as SUEWSForcing."""
        return _as_forcing(
            self._forcing._data.loc[key],
            self._forcing._source,
            self._forcing._slice_extras(key, accessor="loc"),
        )


class _ForcingILocIndexer:
    """Wrapper for iloc indexer that returns SUEWSForcing objects."""

    def __init__(self, forcing: "SUEWSForcing"):
        self._forcing = forcing

    def __getitem__(self, key):
        """Return sliced data as SUEWSForcing."""
        return _as_forcing(
            self._forcing._data.iloc[key],
            self._forcing._source,
            self._forcing._slice_extras(key, accessor="iloc"),
        )


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

    Time slicing (returns new SUEWSForcing):

    >>> forcing["2012"]  # All of 2012 (if no column named "2012")
    >>> forcing["2012-01":"2012-06"]  # Slice notation is always time-based
    >>> forcing.loc["2012"]  # Always time selection (unambiguous)
    >>> forcing.iloc[:1000]  # First 1000 timesteps

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
        from .util._io import read_forcing

        # Handle list of paths
        if isinstance(path, list):
            if not path:
                raise ValueError("Empty forcing file list provided")

            dfs = []
            for p in path:
                file_path = Path(p).expanduser().resolve()
                if not file_path.exists():
                    raise FileNotFoundError(f"Forcing file not found: {file_path}")
                df = read_forcing(str(file_path), tstep_mod=tstep_mod)
                dfs.append(df)

            combined = pd.concat(dfs, axis=0).sort_index()
            # Remove any duplicates
            combined = combined[~combined.index.duplicated(keep="first")]
            df_main, extras = cls._split_per_landcover_columns(combined)
            instance = cls(df_main, source=f"[{len(path)} files]")
            instance._extras = extras
            return instance

        # Handle single path
        file_path = Path(path).expanduser().resolve()
        if not file_path.exists():
            raise FileNotFoundError(f"Forcing file not found: {file_path}")

        df = read_forcing(str(file_path), tstep_mod=tstep_mod)
        df_main, extras = cls._split_per_landcover_columns(df)
        instance = cls(df_main, source=str(file_path))
        instance._extras = extras
        return instance

    @staticmethod
    def _split_per_landcover_columns(
        df_forcing: pd.DataFrame,
    ) -> Tuple[pd.DataFrame, Dict[str, np.ndarray]]:
        """Pop whitelisted ``<var>_<surface>`` columns into an extras dict.

        Returns the kernel-facing DataFrame (with extras removed) and a
        dict mapping the lower-cased canonical name to the column values.
        The whitelist itself is owned by
        :func:`supy._load._is_per_landcover_column` so the Python and
        Rust readers stay in lock-step (gh#1372).
        """
        from ._load import _is_per_landcover_column

        extras: Dict[str, np.ndarray] = {}
        drop_cols: List[str] = []
        for col in df_forcing.columns:
            if _is_per_landcover_column(col):
                extras[col.lower()] = df_forcing[col].to_numpy()
                drop_cols.append(col)
        if drop_cols:
            df_forcing = df_forcing.drop(columns=drop_cols)
        return df_forcing, extras

    # =========================================================================
    # Core data access
    # =========================================================================

    @property
    def df(self) -> pd.DataFrame:
        """Access underlying DataFrame (copy).

        Use this when you need raw DataFrame access for advanced operations
        like column selection, complex indexing, or passing to functions
        expecting DataFrames.

        For time slicing that returns SUEWSForcing objects, prefer:
        - ``forcing["2012"]`` - subscript access
        - ``forcing.loc["2012-01":"2012-06"]`` - label-based slicing

        Returns
        -------
        pd.DataFrame
            Copy of the underlying DataFrame
        """
        return self._data.copy()

    def to_dataframe(self, include_extras: bool = False) -> pd.DataFrame:
        """Return forcing data, optionally with whitelisted extension columns."""
        df = self._data.copy()
        if include_extras and self.extras:
            df = pd.concat([df, self._extras_frame()], axis=1)
        return df

    @property
    def extras(self) -> Dict[str, np.ndarray]:
        """Per-landcover forcing columns (gh#1372).

        Maps lower-cased ``<var>_<surface>`` names to time-aligned arrays
        of length ``len(self.df)``. Empty when the file carries no
        whitelisted per-landcover columns. The kernel-facing adapter
        consumes ``lai_evetr``, ``lai_dectr`` and ``lai_grass`` when
        present; ``wuh_*`` supplies the corresponding surface-specific
        observed water-use depth to the kernel.

        Returns
        -------
        dict of str to numpy.ndarray
            Mapping from lower-cased ``<var>_<surface>`` column name to
            the corresponding time-aligned array.
        """
        return getattr(self, "_extras", {})

    def _extras_frame(self) -> pd.DataFrame:
        """Return extras as an index-aligned DataFrame for slicing/resampling."""
        if not self.extras:
            return pd.DataFrame(index=self._data.index)
        return pd.DataFrame(
            {name: np.asarray(values) for name, values in self.extras.items()},
            index=self._data.index,
        )

    def _slice_extras(
        self,
        key,
        *,
        accessor: str,
    ) -> Dict[str, np.ndarray]:
        """Slice extras with the same row indexer used for the main DataFrame.

        When the user passes a ``(rows, cols)`` tuple to ``.loc`` /
        ``.iloc`` the ``cols`` component refers to columns of the main
        forcing frame, which generally do NOT exist in the extras frame
        (extras are per-landcover columns like ``lai_evetr``,
        ``wuh_paved``). Slicing extras with the full tuple raises
        KeyError or silently mis-selects; we slice with the row
        component only so extras carry the same row subset as the main
        slice (gh#1372 review fix).
        """
        if not self.extras:
            return {}
        row_key = key[0] if isinstance(key, tuple) else key
        selected = getattr(self._extras_frame(), accessor)[row_key]
        if isinstance(selected, pd.Series):
            selected = selected.to_frame().T
        return {name: selected[name].to_numpy() for name in selected.columns}

    @property
    def index(self) -> pd.DatetimeIndex:
        """Datetime index of forcing data (pandas-compatible)."""
        return self._data.index

    @property
    def loc(self) -> _ForcingLocIndexer:
        """Label-based indexer returning SUEWSForcing objects.

        Examples
        --------
        >>> forcing.loc["2012"]  # All of 2012
        >>> forcing.loc["2012-01":"2012-06"]  # Jan-Jun 2012
        """
        return _ForcingLocIndexer(self)

    @property
    def iloc(self) -> _ForcingILocIndexer:
        """Integer-based indexer returning SUEWSForcing objects.

        All results are returned as SUEWSForcing, including single-row
        selections (e.g., ``iloc[0]`` returns a one-row SUEWSForcing,
        not a Series as standard pandas would).

        Examples
        --------
        >>> forcing.iloc[0]  # First timestep (returns SUEWSForcing)
        >>> forcing.iloc[:100]  # First 100 timesteps
        """
        return _ForcingILocIndexer(self)

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

    def __getitem__(self, key) -> Union[pd.Series, pd.DataFrame, "SUEWSForcing"]:
        """Access variables or slice by time.

        Parameters
        ----------
        key : str, list, slice, or array-like
            - String matching column name: returns pd.Series
            - List of column names: returns pd.DataFrame
            - Slice or datetime-like: returns sliced SUEWSForcing

        Returns
        -------
        pd.Series, pd.DataFrame, or SUEWSForcing
            Series for single column, DataFrame for multiple columns,
            SUEWSForcing for time slicing

        Notes
        -----
        **Priority**: Column names take precedence over time strings. If a column
        is named "2012", ``forcing["2012"]`` returns the column, not the year.

        **Disambiguation**: Use ``.loc`` for unambiguous time selection:

        - ``forcing.loc["2012"]`` - Always time selection
        - ``forcing["2012"]`` - Column if exists, else time selection

        Slice notation is always time-based: ``forcing["2012-01":"2012-06"]``

        **Return types by key type:**

        - Single column string: ``pd.Series``
        - List of column names: ``pd.DataFrame``
        - Time string or slice: ``SUEWSForcing``
        - Boolean mask: ``SUEWSForcing``

        Examples
        --------
        Column access (returns Series):

        >>> forcing["Tair"]
        >>> forcing["temperature"]  # Alias

        Multiple columns (returns DataFrame):

        >>> forcing[["Tair", "RH", "U"]]

        Time slicing (returns SUEWSForcing):

        >>> forcing["2012"]  # Time if no column named "2012"
        >>> forcing["2012-01":"2012-06"]  # Always time slicing
        >>> forcing.loc["2012"]  # Always time slicing (unambiguous)
        """
        # Handle slice objects (time slicing)
        if isinstance(key, slice):
            return _as_forcing(
                self._data.loc[key],
                self._source,
                self._slice_extras(key, accessor="loc"),
            )

        # Handle list of column names (returns DataFrame)
        if isinstance(key, list):
            return self._data[key]

        # Handle string keys
        if isinstance(key, str):
            # First check if it's a column name or alias
            canonical = _ALIAS_TO_CANONICAL.get(key.lower())
            if canonical is not None and canonical in self._data.columns:
                return self._data[canonical]

            if key in self._data.columns:
                return self._data[key]

            # Case-insensitive column lookup
            key_lower = key.lower()
            for col in self._data.columns:
                if col.lower() == key_lower:
                    return self._data[col]

            # Not a column - try as time selection (e.g., "2012")
            try:
                return _as_forcing(
                    self._data.loc[key],
                    self._source,
                    self._slice_extras(key, accessor="loc"),
                )
            except KeyError:
                raise KeyError(
                    f"'{key}' not found as column name, alias, or time index. "
                    f"Available columns: {list(self._data.columns)}"
                ) from None

        # For other types (boolean arrays, etc.), try time slicing
        return _as_forcing(
            self._data.loc[key],
            self._source,
            self._slice_extras(key, accessor="loc"),
        )

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

        result = check_forcing(
            self.to_dataframe(include_extras=True), fix=False, physics=physics_dict
        )

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

    _TIME_COLUMNS = ("iy", "id", "it", "imin", "isec")

    def _regular_timestep(self) -> pd.Timedelta:
        """Return the single spacing of the index, rejecting irregular data."""
        index = self._data.index
        if len(index) < 2:
            raise ValueError(
                "SUEWSForcing.resample needs at least two timestamps to "
                "establish the source timestep"
            )
        diffs = index.to_series().diff().dropna()
        if diffs.nunique() != 1:
            raise ValueError(
                "SUEWSForcing.resample requires a regular DatetimeIndex; "
                f"found {diffs.nunique()} distinct spacings between "
                f"{diffs.min()} and {diffs.max()}"
            )
        return pd.Timedelta(diffs.iloc[0])

    @staticmethod
    def _fixed_frequency(freq: str) -> pd.Timedelta:
        """Convert ``freq`` to a fixed duration, rejecting calendar offsets."""
        try:
            return pd.Timedelta(freq)
        except ValueError:
            pass
        try:
            return pd.Timedelta(pd.tseries.frequencies.to_offset(freq))
        except (TypeError, ValueError) as exc:
            raise ValueError(
                "SUEWSForcing.resample needs a fixed-length frequency such as "
                f"'30min', '1h' or '1D'; got {freq!r}"
            ) from exc

    @staticmethod
    def _rows_per_bin(freq: str, source: pd.Timedelta, target: pd.Timedelta) -> int:
        """Return the number of source rows per output interval."""
        if target < source:
            raise ValueError(
                f"SUEWSForcing.resample only coarsens data: {freq!r} is finer "
                f"than the current timestep of {source}. Load the file with "
                "SUEWSForcing.from_file(path, tstep_mod=...) or use "
                "supy.util._io.resample_forcing_df for physics-aware "
                "disaggregation."
            )
        if target % source != pd.Timedelta(0):
            raise ValueError(
                f"SUEWSForcing.resample target {freq!r} is not an integer "
                f"multiple of the current timestep of {source}"
            )
        return int(target / source)

    @staticmethod
    def _require_aligned_phase(
        index: pd.DatetimeIndex, freq: str, source: pd.Timedelta
    ) -> None:
        """Reject timestamps whose source intervals cannot tile the target bins.

        A row at ``t`` covers ``(t - source, t]``; the rows can only tile an
        output interval ending on the target grid if the timestamps sit on
        multiples of the source step (00:05, 00:10, ... for 5-minute data).
        Rows at 00:02, 00:07, ... would otherwise be counted as full
        coverage of ``(00:00, 00:10]`` and 00:07 reported as the 00:10
        endpoint.
        """
        offset = pd.Timedelta(index[0].value % source.value)
        if offset != pd.Timedelta(0):
            raise ValueError(
                "SUEWSForcing.resample requires timestamps on the source-step "
                f"grid so that intervals tile the {freq!r} bins; the index "
                f"starts at {index[0]}, which is {offset} past the nearest "
                f"{source} boundary. Re-label the data rather than shifting "
                "the observations."
            )

    @staticmethod
    def _aggregation_kind(column: str) -> str:
        """Map a forcing column to ``sum``, ``mean`` or ``inst`` semantics."""
        from ._load import _per_landcover_forcing_var

        if _per_landcover_forcing_var(column) == "wuh":
            return "sum"
        var_type = FORCING_VAR_TYPES.get(column, "inst")
        if var_type == "avg":
            return "mean"
        if var_type == "sum":
            return "sum"
        return "inst"

    @classmethod
    def _aggregate_bins(
        cls, masked: pd.DataFrame, freq: str, rows_per_bin: int
    ) -> pd.DataFrame:
        """Aggregate NaN-masked columns into right-labelled intervals.

        An interval is NaN unless it holds ``rows_per_bin`` rows and,
        for sums and means, every row is valid; instantaneous columns
        take the value at the interval end without NaN-skipping.
        """
        grouper = masked.resample(freq, closed="right", label="right")
        # Complete means the bin holds every source row AND its last row ends
        # exactly on the bin label, so the source intervals tile the bin.
        last_stamp = pd.Series(masked.index, index=masked.index)
        last_stamp = last_stamp.resample(freq, closed="right", label="right").last()
        complete = (grouper.size() == rows_per_bin) & (last_stamp == last_stamp.index)
        all_valid = grouper.count().eq(rows_per_bin)
        sums = grouper.sum(min_count=1)
        means = grouper.mean()

        end_pos = pd.Series(np.arange(len(masked)), index=masked.index)
        end_pos = end_pos.resample(freq, closed="right", label="right").last()
        has_end = end_pos.notna().to_numpy()
        end_vals = np.full((len(end_pos), masked.shape[1]), np.nan)
        end_vals[has_end] = masked.to_numpy()[end_pos[has_end].astype(int)]
        endpoint = pd.DataFrame(end_vals, index=end_pos.index, columns=masked.columns)

        out = pd.DataFrame(index=complete.index)
        for col in masked.columns:
            kind = cls._aggregation_kind(col)
            if kind == "sum":
                values = sums[col].where(all_valid[col])
            elif kind == "mean":
                values = means[col].where(all_valid[col])
            else:
                values = endpoint[col]
            out[col] = values.where(complete)
        return out

    @staticmethod
    def _time_columns_from_index(index: pd.DatetimeIndex) -> Dict[str, np.ndarray]:
        """Return the SUEWS temporal columns derived from ``index``."""
        return {
            "iy": index.year.to_numpy().astype("int64"),
            "id": index.dayofyear.to_numpy().astype("int64"),
            "it": index.hour.to_numpy().astype("int64"),
            "imin": index.minute.to_numpy().astype("int64"),
            "isec": index.second.to_numpy().astype("int64"),
        }

    def resample(self, freq: str) -> "SUEWSForcing":
        """
        Aggregate forcing to a coarser temporal resolution.

        Each output interval ``(t - freq, t]`` is labelled by its end
        time ``t`` (``closed="right", label="right"``), matching the
        period-ending timestamps SUEWS uses. Values are aggregated by
        variable type:

        - Accumulated variables (``rain``, ``Wuh``, ``wuh_<surface>``):
          sum over the interval.
        - Interval-average variables (radiation and other ``avg``
          columns): mean over the interval.
        - Instantaneous variables (``Tair``, ``RH``, ``U``, ``pres``,
          ``lai_<surface>`` and any unrecognised column): the value at
          the interval end.

        Missing values are preserved rather than aggregated into
        plausible numbers. ``NaN`` and the SUEWS missing sentinel (any
        value at or below ``-900``) are masked before aggregation, and an
        output interval is reported as missing (``-999``) unless

        - it is fully covered by ``freq / timestep`` source rows, and
        - every one of those rows is valid (sums and means), or the
          row at the interval end is valid (instantaneous values).

        The stricter rule is deliberate: a partial sum understates an
        accumulation, a partial mean of a diurnal variable is biased,
        and an earlier valid reading substituted for a missing endpoint
        is an invented observation. Leading or trailing intervals that
        the data only partly covers are therefore reported as missing.
        Use :meth:`fill_gaps` afterwards if gap filling is wanted.

        Temporal columns (``iy``, ``id``, ``it``, ``imin``, ``isec``)
        are rebuilt from the output index. Per-landcover extension
        columns follow the same rules.

        Timestamps must sit on the source-step grid (for example 00:05,
        00:10 for 5-minute data) so that the source intervals tile the
        output intervals exactly; offset timestamps such as 00:02, 00:07
        are rejected rather than shifted.

        Only coarsening by an integer multiple of the source timestep is
        supported. Disaggregating to a finer timestep uses the
        physics-aware distribution in
        :meth:`SUEWSForcing.from_file` (``tstep_mod``) or
        :func:`supy.util._io.resample_forcing_df`, which this method
        does not replicate.

        Parameters
        ----------
        freq : str
            Target frequency (e.g. ``"1h"``, ``"30min"``); must be an
            integer multiple of the current timestep.

        Returns
        -------
        SUEWSForcing
            New forcing object at the coarser frequency. When ``freq``
            equals the current timestep the data are returned unchanged.

        Raises
        ------
        ValueError
            If the index is irregular or offset from the source-step
            grid, ``freq`` is finer than the current timestep, or
            ``freq`` is not an integer multiple of it.
        """
        from .util._missing import from_nan, to_nan

        source_step = self._regular_timestep()
        target_step = self._fixed_frequency(freq)
        if target_step == source_step:
            unchanged = SUEWSForcing(self._data, source=self._source)
            unchanged._extras = _normalise_extras(self.extras)
            return unchanged
        rows_per_bin = self._rows_per_bin(freq, source_step, target_step)
        self._require_aligned_phase(self._data.index, freq, source_step)

        value_cols = [c for c in self._data.columns if c not in self._TIME_COLUMNS]
        frame = self._data[value_cols].astype(float)
        extras_cols: List[str] = []
        if self.extras:
            extras_df = self._extras_frame().astype(float)
            extras_cols = list(extras_df.columns)
            frame = pd.concat([frame, extras_df], axis=1)

        out = from_nan(self._aggregate_bins(to_nan(frame), freq, rows_per_bin))

        time_values = self._time_columns_from_index(out.index)
        resampled = pd.DataFrame(
            {
                col: time_values[col] if col in time_values else out[col]
                for col in self._data.columns
            },
            index=out.index,
        )

        result = SUEWSForcing(resampled, source=f"{self._source}@{freq}")
        if extras_cols:
            result._extras = {name: out[name].to_numpy() for name in extras_cols}
        return result

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

        result = SUEWSForcing(filled, source=f"{self._source}[filled]")
        result._extras = _normalise_extras(self.extras)
        return result

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
