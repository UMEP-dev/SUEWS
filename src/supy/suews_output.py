"""
SUEWSOutput - OOP wrapper for SUEWS simulation results.

Provides a structured interface for accessing and exporting SUEWS model output data.
"""

from __future__ import annotations

import copy
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import numpy as np
from packaging.version import Version
import pandas as pd

from ._filename import safe_filename_component
from ._output_metadata import OutputUnits
from .suews_checkpoint import SUEWSCheckpoint


def streamed_output_error(streamed_paths: list[str | Path]) -> RuntimeError:
    """Build the error raised when rows of a streamed run's output are requested.

    A run with ``model.control.output.stream_chunk`` set writes its output
    files during the run and keeps no output rows, so every accessor that
    would read rows raises this instead of handing back an empty frame.
    """
    paths = [Path(p) for p in streamed_paths]
    folder = paths[0].resolve().parent if paths else Path(".").resolve()
    list_parquet = sorted(
        p.name for p in paths if p.suffix == ".parquet" and "SUEWS_output" in p.name
    )
    if list_parquet:
        where = (
            f"Its Parquet output is the folder {folder / list_parquet[0]}, one "
            "file per chunk; read it back with pandas.read_parquet(folder)"
        )
    else:
        text_files = sorted(p.name for p in paths if p.suffix == ".txt")
        example = f" (for example {text_files[0]})" if text_files else ""
        where = (
            f"Its {len(text_files)} text output files are in {folder}{example}; "
            "read one back with pandas.read_csv(path, sep=r'\\s+')"
        )
    return RuntimeError(
        "This output was streamed to disk in chunks (output.stream_chunk) "
        f"and holds no rows in memory. {where}, or rerun with "
        "output.stream_chunk unset to keep the output in memory. The "
        "checkpoint, final state and kernel warnings are still available."
    )


def _copy_on_write_active() -> bool:
    """Whether pandas copy-on-write protects a shallow copy from writes.

    Always on from pandas 3. On pandas 2 it is the user's opt-in
    ``mode.copy_on_write`` option (only ``True`` counts: ``"warn"`` still
    writes through). The option is read, never set; on pandas 3 it is not
    read at all, because reading it there emits a deprecation warning.
    """
    if Version(pd.__version__).major >= 3:
        return True
    try:
        return pd.get_option("mode.copy_on_write") is True
    except (KeyError, pd.errors.OptionError):
        return False


def _frame_for_caller(df: pd.DataFrame) -> pd.DataFrame:
    """Hand *df* to a caller without copying it and without exposing it to writes.

    A deep copy on every access cost the full size of the output table, the
    largest single allocation of a long run (gh#1849). Instead:

    - Under copy-on-write, a shallow copy is enough: the first write to the
      returned frame copies the touched data, so the caller still sees an
      independent frame and *df* never changes.
    - Without copy-on-write (pandas 2 by default), the returned frame shares
      *df*'s buffer through a read-only view. Partial in-place writes
      (``.loc``/``.iloc``/``.at`` on part of a column, ``fillna``/``ffill``
      with ``inplace=True``, ...) raise ``ValueError`` instead of changing
      *df*, as do in-place writes into any part of the returned frame pandas
      hands out as a view (a column, a row slice, ``xs`` on a level), e.g.
      ``df[col] += x``. Adding, replacing or dropping whole columns and every
      operation that returns a new frame work as before. ``.copy()`` on the
      returned frame gives a writable copy.

    The axes are handed out as new ``Index`` objects, so renaming levels on the
    returned frame does not rename *df*'s. A frame whose values are not one
    shared array (several dtypes, or several blocks, e.g. after resampling)
    is returned as an independent copy, as before.
    """
    if _copy_on_write_active():
        return df.copy(deep=False)
    set_dtypes = set(df.dtypes)
    if (
        df.shape[1] == 0
        or len(set_dtypes) != 1
        or not isinstance(set_dtypes.pop(), np.dtype)
    ):
        # Several dtypes, or an extension dtype `to_numpy` would convert.
        return df.copy()
    ar_values = df.to_numpy()
    if np.may_share_memory(ar_values, df.iloc[:, 0].to_numpy()):
        # One block: `to_numpy` returned a view; make it read-only. A view of
        # the array, not the array itself, so `df` stays writable for SuPy.
        ar_values = ar_values.view()
        ar_values.flags.writeable = False
    # Otherwise `to_numpy` already built an independent array of the one dtype.
    df_caller = pd.DataFrame(
        ar_values, index=df.index.view(), columns=df.columns.view(), copy=False
    )
    if df.attrs:
        df_caller.attrs = copy.deepcopy(df.attrs)
    return df_caller


def _group_for_caller(df: pd.DataFrame, group: str) -> pd.DataFrame:
    """Hand the columns of output *group* to a caller as an independent frame.

    The group is selected from *df* itself, so the cost is group-sized
    whatever the layout of *df* (gh#1849); going through `_frame_for_caller`
    first would copy the whole table when *df* is not one block, e.g. after
    resampling. On every pandas version the returned frame is writable and
    no write to it reaches *df*:

    - Under copy-on-write the selection is handed out as a shallow copy, so
      nothing is copied until the caller writes to it.
    - Without copy-on-write (pandas 2) the group's columns are taken into a
      new array. ``df[group]`` would instead return a view of *df* or a copy
      depending on the order of the groups in *df*.

    The axes are new ``Index`` objects, so renaming levels on the returned
    frame does not rename *df*'s.
    """
    if _copy_on_write_active():
        return df[group].copy(deep=False)
    ar_positions = np.flatnonzero(df.columns.get_level_values("group") == group)
    df_group = df.take(ar_positions, axis=1)
    df_group.columns = df_group.columns.droplevel("group")
    df_group.index = df_group.index.view()
    return df_group


class SUEWSOutput:
    """
    Wrapper for SUEWS simulation results.

    Provides intuitive access to output variables and export capabilities
    for SUEWS simulation results.

    Parameters
    ----------
    df_output : pd.DataFrame
        Output DataFrame with MultiIndex columns (group, var)
    df_state_final : pd.DataFrame
        Final model state DataFrame
    config : SUEWSConfig, optional
        Configuration used for this run
    checkpoint : SUEWSCheckpoint, optional
        Typed restart checkpoint from the Rust backend
    metadata : dict, optional
        Additional metadata (timing, version, etc.)

    Examples
    --------
    Run simulation and access output:

    >>> sim = SUEWSSimulation.from_sample_data()
    >>> output = sim.run()  # Returns SUEWSOutput
    >>> output
    SUEWSOutput(2012-01-01 to 2012-12-31, 1 grid(s), 6 groups, 8760 timesteps)

    Access variables directly (case-insensitive):

    >>> output.QH  # Sensible heat flux
    >>> output.qh  # Same as above
    >>> output.get_variable("Tair", group="SUEWS")

    Access by group:

    >>> output.groups  # ['SUEWS', 'DailyState', 'snow', ...]
    >>> output.get_group("SUEWS")

    Export:

    >>> output.save("output/", format="parquet")

    Restart runs:

    >>> sim2 = SUEWSSimulation.from_checkpoint(config, output.checkpoint)
    """

    def __init__(
        self,
        df_output: pd.DataFrame,
        df_state_final: pd.DataFrame,
        config: Optional[Any] = None,
        metadata: Optional[Dict[str, Any]] = None,
        checkpoint: Optional[SUEWSCheckpoint] = None,
        kernel_warnings: Optional[pd.DataFrame] = None,
        streamed_paths: list[str | Path] | None = None,
    ):
        """
        Initialise SUEWSOutput.

        Parameters
        ----------
        df_output : pd.DataFrame
            Output DataFrame with MultiIndex columns (group, var)
        df_state_final : pd.DataFrame
            Final model state DataFrame
        config : SUEWSConfig, optional
            Configuration used for this run (for save context)
        checkpoint : SUEWSCheckpoint, optional
            Typed restart checkpoint from the Rust backend
        metadata : dict, optional
            Additional metadata (timing, version, etc.)
        kernel_warnings : pd.DataFrame, optional
            Non-fatal warnings raised by the Fortran kernel (columns
            ``grid``, ``datetime``, ``location``, ``message``)
        streamed_paths : list, optional
            Files written by a run with ``output.stream_chunk`` set. When
            given, *df_output* carries the column layout only, and every
            accessor that reads output rows raises ``RuntimeError`` naming
            the output folder instead of returning an empty frame.
        """
        self._streamed_paths = (
            [str(p) for p in streamed_paths] if streamed_paths is not None else None
        )
        # Shallow copy: a new frame object (index/columns edits on the caller's
        # frame do not leak in) that shares the values buffer. A deep copy here
        # held a second full-width output table for the life of the run
        # (gh#1849). `.df` hands the table out through `_frame_for_caller`,
        # which shares it without exposing it to writes, and the group
        # accessors through `_group_for_caller`.
        self._df_output = df_output.copy(deep=False)
        self._df_state_final = (
            df_state_final.copy() if df_state_final is not None else None
        )
        self._config = config
        self._checkpoint = checkpoint
        self._metadata = metadata or {}
        self._kernel_warnings = (
            kernel_warnings.copy()
            if kernel_warnings is not None
            else pd.DataFrame(columns=["grid", "datetime", "location", "message"])
        )

    # =========================================================================
    # Core data access
    # =========================================================================

    def _require_rows(self) -> None:
        """Raise if this output was streamed to disk and holds no rows."""
        if self._streamed_paths is not None:
            raise streamed_output_error(self._streamed_paths)

    @property
    def units(self) -> OutputUnits:
        """Read-only units keyed by column, with CF fields in ``.metadata``.

        The held rows use the model timestep; streamed output uses the
        configured output frequency. Without run context the interval is unknown.
        """
        interval_s = self._metadata.get("tstep_s")
        if self._config is not None:
            control = self._config.model.control
            value = (
                control.output.freq
                if self._streamed_paths is not None
                else control.tstep
            )
            interval_s = int(getattr(value, "value", value))
        if "resampled_to" in self._metadata:
            interval_s = int(
                pd.Timedelta(self._metadata["resampled_to"]).total_seconds()
            )
        return OutputUnits(self._df_output.columns, interval_s)

    @property
    def df(self) -> pd.DataFrame:
        """Output DataFrame, shared with this object rather than copied.

        No access copies the table (gh#1849), and no write to the returned
        frame reaches the output held here:

        - with pandas copy-on-write (always on from pandas 3), the returned
          frame behaves as an independent copy;
        - without it (pandas 2), the returned frame is a read-only view, and
          so is any part of it pandas hands out as a view (a column, a row
          slice, ``xs`` on an index level): writing into it in place (for
          example ``df.loc[mask, col] = x``, ``df[col] += x`` or
          ``df.fillna(0, inplace=True)``) raises ``ValueError``. Call
          ``output.df.copy()`` for a writable copy. Adding, replacing or
          dropping whole columns works on the view.

        ``get_group`` and group attributes (``output.SUEWS``) return a
        writable frame on every pandas version.
        """
        self._require_rows()
        return _frame_for_caller(self._df_output)

    @property
    def empty(self) -> bool:
        """Check if output is empty (pandas-compatible)."""
        self._require_rows()
        return self._df_output.empty

    @property
    def columns(self) -> pd.Index:
        """Column index of output DataFrame (pandas-compatible)."""
        return self._df_output.columns

    @property
    def index(self) -> pd.Index:
        """Row index of output DataFrame (pandas-compatible)."""
        self._require_rows()
        return self._df_output.index

    @property
    def state_final(self) -> pd.DataFrame:
        """
        Legacy final model state DataFrame.

        Prefer ``checkpoint`` for restart/continuation workflows.

        Returns
        -------
        pd.DataFrame
            Final state DataFrame ready for restart
        """
        return self._df_state_final.copy()

    @property
    def checkpoint(self) -> Optional[SUEWSCheckpoint]:
        """Typed checkpoint for restart/continuation runs."""
        return self._checkpoint

    @property
    def kernel_warnings(self) -> pd.DataFrame:
        """Non-fatal warnings raised by the Fortran kernel during this run.

        One row per recorded warning (``grid``, ``datetime``, ``location``,
        ``message``); empty for a clean run. See GH#1737.
        """
        return self._kernel_warnings.copy()

    @property
    def state_checkpoint(self) -> Optional[SUEWSCheckpoint]:
        """Alias for ``checkpoint``."""
        return self._checkpoint

    @property
    def times(self) -> pd.DatetimeIndex:
        """Datetime index of output."""
        self._require_rows()
        return self._df_output.index.get_level_values("datetime").unique()

    @property
    def time_range(self) -> tuple:
        """Return (start, end) timestamps."""
        times = self.times
        return (times.min(), times.max())

    @property
    def grids(self) -> List:
        """List of grid identifiers."""
        self._require_rows()
        return self._df_output.index.get_level_values("grid").unique().tolist()

    @property
    def groups(self) -> List[str]:
        """List of available output groups."""
        return self._df_output.columns.get_level_values("group").unique().tolist()

    @property
    def config(self) -> Optional[Any]:
        """Configuration used for this run."""
        return self._config

    @property
    def metadata(self) -> Dict[str, Any]:
        """Metadata about the simulation."""
        return self._metadata.copy()

    @property
    def loc(self):
        """Label-based indexer (pandas-compatible)."""
        self._require_rows()
        return self._df_output.loc

    @property
    def iloc(self):
        """Integer-based indexer (pandas-compatible)."""
        self._require_rows()
        return self._df_output.iloc

    def xs(self, key, axis=0, level=None, drop_level=True):
        """Cross-section from MultiIndex DataFrame (pandas-compatible)."""
        self._require_rows()
        return self._df_output.xs(key, axis=axis, level=level, drop_level=drop_level)

    def __getattr__(self, name: str) -> pd.DataFrame:
        """
        Dynamic attribute access for output variables and groups.

        Allows access like `output.QH` instead of `output.get_variable('QH')`,
        and `output.SUEWS` instead of `output.get_group('SUEWS')`.
        Supports case-insensitive access.
        """
        # Check if it's a group name first (exact match)
        all_groups = self._df_output.columns.get_level_values("group").unique()
        if name in all_groups:
            return self.get_group(name)

        # Check if it's a variable name in any group (exact match)
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if name in all_vars:
            return self.get_variable(name)

        # Case-insensitive lookup for groups
        name_lower = name.lower()
        for grp in all_groups:
            if grp.lower() == name_lower:
                return self.get_group(grp)

        # Case-insensitive lookup for variables
        for var in all_vars:
            if var.lower() == name_lower:
                return self.get_variable(var)

        raise AttributeError(
            f"'{type(self).__name__}' has no attribute '{name}'. "
            f"Available groups: {list(all_groups)}, "
            f"Available variables (first 10): {list(all_vars)[:10]}..."
        )

    def __getitem__(self, key: str) -> pd.DataFrame:
        """Access variables or groups by name (case-insensitive)."""
        # Check if it's a group name (exact match)
        if key in self.groups:
            return self.get_group(key)

        # Case-insensitive group lookup
        key_lower = key.lower()
        for grp in self.groups:
            if grp.lower() == key_lower:
                return self.get_group(grp)

        # Try as variable (exact match)
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if key in all_vars:
            return self.get_variable(key)

        # Case-insensitive variable lookup
        for var in all_vars:
            if var.lower() == key_lower:
                return self.get_variable(var)

        # Let get_variable raise appropriate error
        return self.get_variable(key)

    # =========================================================================
    # Group access
    # =========================================================================

    def get_group(self, group: str) -> pd.DataFrame:
        """
        Get all variables for an output group.

        Parameters
        ----------
        group : str
            Output group name (e.g., 'SUEWS', 'DailyState', 'snow')

        Returns
        -------
        pd.DataFrame
            All variables in the group, as a frame independent of this
            object: writing to it does not change the stored output.
        """
        self._require_rows()
        # Case-insensitive group lookup
        if group not in self.groups:
            group_lower = group.lower()
            matched_group = None
            for g in self.groups:
                if g.lower() == group_lower:
                    matched_group = g
                    break
            if matched_group is None:
                raise ValueError(
                    f"Group '{group}' not found. Available groups: {self.groups}"
                )
            group = matched_group
        return _group_for_caller(self._df_output, group)

    def get_variable(
        self,
        var_name: str,
        group: Optional[str] = None,
        grid: Optional[Union[int, str]] = None,
    ) -> pd.DataFrame:
        """
        Extract specific variable from output.

        Parameters
        ----------
        var_name : str
            Variable name (e.g., 'QH', 'QE', 'Tair')
        group : str, optional
            Output group if ambiguous
        grid : int or str, optional
            Grid filter

        Returns
        -------
        pd.DataFrame
            Time series of requested variable
        """
        self._require_rows()
        # Check if variable exists (case-insensitive)
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if var_name not in all_vars:
            # Try case-insensitive match
            var_name_lower = var_name.lower()
            matched_var = None
            for v in all_vars:
                if v.lower() == var_name_lower:
                    matched_var = v
                    break
            if matched_var is None:
                raise ValueError(
                    f"Variable '{var_name}' not found. "
                    f"Available variables (first 10): {list(all_vars)[:10]}..."
                )
            var_name = matched_var

        # Find which groups contain this variable
        matching_groups = []
        for grp in self.groups:
            try:
                _ = self._df_output.xs((grp, var_name), level=("group", "var"), axis=1)
                matching_groups.append(grp)
            except KeyError:
                continue

        if len(matching_groups) == 0:
            raise ValueError(f"Variable '{var_name}' not found in any group")

        if len(matching_groups) > 1 and group is None:
            raise ValueError(
                f"Variable '{var_name}' appears in multiple groups: "
                f"{matching_groups}. Specify group parameter."
            )

        # Handle case-insensitive group matching
        target_group = matching_groups[0]
        if group is not None:
            # Try exact match first
            if group in matching_groups:
                target_group = group
            else:
                # Try case-insensitive match
                group_lower = group.lower()
                matched = False
                for mg in matching_groups:
                    if mg.lower() == group_lower:
                        target_group = mg
                        matched = True
                        break
                if not matched:
                    raise ValueError(
                        f"Variable '{var_name}' not found in group '{group}'. "
                        f"Available groups for this variable: {matching_groups}"
                    )

        result = self._df_output.xs(
            (target_group, var_name), level=("group", "var"), axis=1
        )

        # Filter by grid if requested
        if grid is not None:
            if isinstance(grid, str):
                result = result.xs(grid, level="grid")
            else:
                result = result.iloc[:, grid]

        return result

    @property
    def available_variables(self) -> Dict[str, List[str]]:
        """Dict mapping groups to their variables."""
        result = {}
        for group in self.groups:
            try:
                vars_in_group = (
                    self._df_output[group]
                    .columns.get_level_values("var")
                    .unique()
                    .tolist()
                )
                result[group] = vars_in_group
            except KeyError:
                result[group] = []
        return result

    # =========================================================================
    # Manipulation (domain-specific)
    # =========================================================================

    def resample(self, freq: str) -> "SUEWSOutput":
        """
        Resample output to different frequency.

        Uses appropriate aggregation methods for each variable type:
        - Instantaneous variables: mean
        - Accumulated variables: sum
        - State variables: last

        Each value covers the left-open, right-closed interval
        ``(t - freq, t]`` and is labelled by its end ``t``
        (``closed="right", label="right"``); DailyState is labelled by the
        start of each day. See :ref:`timestamp_convention`.

        Parameters
        ----------
        freq : str
            Target frequency (e.g., "1H", "1D")

        Returns
        -------
        SUEWSOutput
            New output at resampled frequency
        """
        from ._post import _resample_output

        self._require_rows()
        resampled = _resample_output(self, freq)
        return SUEWSOutput(
            df_output=resampled,
            df_state_final=self._df_state_final,
            config=self._config,
            metadata={**self._metadata, "resampled_to": freq},
            checkpoint=self._checkpoint,
        )

    # =========================================================================
    # Export
    # =========================================================================

    def save(
        self,
        path: Union[str, Path] = ".",
        format: Optional[str] = None,
        freq_s: Optional[int] = None,
        groups: Optional[List[str]] = None,
    ) -> List[Path]:
        """
        Save output to files.

        Parameters
        ----------
        path : str or Path
            Output directory
        format : str, optional
            'txt' or 'parquet'. When ``None`` (the default), the format
            stored in the run configuration
            (``model.control.output.format``) is used, falling back to
            'txt' if no configuration is available. An explicit value
            always overrides the configuration.
        freq_s : int, optional
            Output frequency in seconds (default: from config or 3600)
        groups : list, optional
            Groups to save (default: ['SUEWS', 'DailyState'])

        Returns
        -------
        list
            Paths to saved files
        """
        from ._supy_module import _save_supy

        self._require_rows()
        path = Path(path)
        path.mkdir(parents=True, exist_ok=True)

        # Get defaults from config if available
        freq = freq_s or 3600
        site = ""
        output_config = None
        forcing_timestamp_reference = "local_standard_time"

        if self._config:
            try:
                if hasattr(self._config, "model"):
                    control = self._config.model.control
                    if hasattr(control, "output"):
                        output_control = control.output
                        if hasattr(output_control, "freq") and output_control.freq:
                            freq = freq_s or output_control.freq
                        output_config = output_control
                    if hasattr(control, "forcing"):
                        forcing_timestamp_reference = (
                            control.forcing.timestamp_reference
                        )
                if hasattr(self._config, "sites") and len(self._config.sites) > 0:
                    site = self._config.sites[0].name
            except AttributeError:
                pass

        site_filename = safe_filename_component(site)

        list_path_save = _save_supy(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            freq_s=int(freq),
            site=site,
            path_dir_save=str(path),
            output_config=output_config,
            output_format=format,
            save_state=False,
            forcing_timestamp_reference=forcing_timestamp_reference,
        )

        if self._checkpoint is not None:
            checkpoint_name = (
                f"{site_filename}_SUEWS_checkpoint.json"
                if site_filename
                else "SUEWS_checkpoint.json"
            )
            checkpoint_path = self._checkpoint.to_file(path / checkpoint_name)
            list_path_save.append(checkpoint_path)

        return list_path_save

    # =========================================================================
    # Rich display
    # =========================================================================

    def __repr__(self) -> str:
        """Concise representation of output data."""
        if self._streamed_paths is not None:
            return self._streamed_summary()
        start, end = self.time_range
        n_grids = len(self.grids)
        n_groups = len(self.groups)
        n_timesteps = len(self.times)

        return (
            f"SUEWSOutput({start.date()} to {end.date()}, "
            f"{n_grids} grid(s), {n_groups} groups, {n_timesteps} timesteps)"
        )

    def _repr_html_(self) -> str:
        """HTML representation for Jupyter notebooks."""
        if self._streamed_paths is not None:
            return f"<pre>{self._streamed_summary()}</pre>"
        start, end = self.time_range
        n_grids = len(self.grids)
        n_groups = len(self.groups)
        n_timesteps = len(self.times)

        # Count variables
        total_vars = sum(len(v) for v in self.available_variables.values())

        html = f"""
        <div style="border: 1px solid #ccc; padding: 10px; border-radius: 5px;">
            <h4 style="margin: 0 0 10px 0;">SUEWSOutput</h4>
            <table style="border-collapse: collapse;">
                <tr><td><strong>Time range:</strong></td><td>{start} to {end}</td></tr>
                <tr><td><strong>Grids:</strong></td><td>{n_grids}</td></tr>
                <tr><td><strong>Timesteps:</strong></td><td>{n_timesteps}</td></tr>
                <tr><td><strong>Groups:</strong></td><td>{", ".join(self.groups)}</td></tr>
                <tr><td><strong>Variables:</strong></td><td>{total_vars} total</td></tr>
            </table>
        </div>
        """
        return html

    def _streamed_summary(self) -> str:
        """One-line description of a streamed output (no rows held)."""
        folder = Path(self._streamed_paths[0]).resolve().parent
        n_text = sum(1 for p in self._streamed_paths if Path(p).suffix == ".txt")
        files = f"{n_text} text files" if n_text else "a Parquet folder"
        return (
            f"SUEWSOutput(streamed to {folder}, {files}, "
            f"{len(self.groups)} groups; no rows held in memory)"
        )

    def __len__(self) -> int:
        """Number of timesteps."""
        return len(self.times)
