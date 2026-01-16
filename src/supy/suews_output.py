"""
SUEWSOutput - OOP wrapper for SUEWS simulation results.

Provides a structured interface for accessing and exporting SUEWS model output data.
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd


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

    >>> sim2 = SUEWSSimulation.from_state(output.state_final)
    """

    def __init__(
        self,
        df_output: pd.DataFrame,
        df_state_final: pd.DataFrame,
        config: Optional[Any] = None,
        metadata: Optional[Dict[str, Any]] = None,
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
        metadata : dict, optional
            Additional metadata (timing, version, etc.)
        """
        self._df_output = df_output.copy()
        self._df_state_final = df_state_final.copy() if df_state_final is not None else None
        self._config = config
        self._metadata = metadata or {}

    # =========================================================================
    # Core data access
    # =========================================================================

    @property
    def df(self) -> pd.DataFrame:
        """Access underlying output DataFrame."""
        return self._df_output.copy()

    @property
    def empty(self) -> bool:
        """Check if output is empty (pandas-compatible)."""
        return self._df_output.empty

    @property
    def columns(self) -> pd.Index:
        """Column index of output DataFrame (pandas-compatible)."""
        return self._df_output.columns

    @property
    def state_final(self) -> pd.DataFrame:
        """
        Final model state for restart runs.

        Use with SUEWSSimulation.from_state() or from_output() to continue simulations.

        Returns
        -------
        pd.DataFrame
            Final state DataFrame ready for restart
        """
        return self._df_state_final.copy()

    @property
    def times(self) -> pd.DatetimeIndex:
        """Datetime index of output."""
        return self._df_output.index.get_level_values("datetime").unique()

    @property
    def time_range(self) -> tuple:
        """Return (start, end) timestamps."""
        times = self.times
        return (times.min(), times.max())

    @property
    def grids(self) -> List:
        """List of grid identifiers."""
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
        return self._df_output.loc

    @property
    def iloc(self):
        """Integer-based indexer (pandas-compatible)."""
        return self._df_output.iloc

    def xs(self, key, axis=0, level=None, drop_level=True):
        """Cross-section from MultiIndex DataFrame (pandas-compatible)."""
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
            All variables in the group
        """
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
        return self._df_output[group].copy()

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

        Parameters
        ----------
        freq : str
            Target frequency (e.g., "1H", "1D")

        Returns
        -------
        SUEWSOutput
            New output at resampled frequency
        """
        from ._post import resample_output

        resampled = resample_output(self._df_output, freq)
        return SUEWSOutput(
            resampled,
            self._df_state_final,
            self._config,
            {**self._metadata, "resampled_to": freq},
        )

    # =========================================================================
    # Export
    # =========================================================================

    def save(
        self,
        path: Union[str, Path] = ".",
        format: str = "parquet",
        freq_s: Optional[int] = None,
        groups: Optional[List[str]] = None,
    ) -> List[Path]:
        """
        Save output to files.

        Parameters
        ----------
        path : str or Path
            Output directory
        format : str
            'txt' or 'parquet'
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

        path = Path(path)
        path.mkdir(parents=True, exist_ok=True)

        # Get defaults from config if available
        freq = freq_s or 3600
        site = ""
        output_config = None

        if self._config:
            try:
                if hasattr(self._config, "model"):
                    control = self._config.model.control
                    if hasattr(control, "output_file"):
                        output_file = control.output_file
                        if not isinstance(output_file, str):
                            if hasattr(output_file, "freq") and output_file.freq:
                                freq = freq_s or output_file.freq
                            output_config = output_file
                if hasattr(self._config, "sites") and len(self._config.sites) > 0:
                    site = self._config.sites[0].name
            except AttributeError:
                pass

        return _save_supy(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            freq_s=int(freq),
            site=site,
            path_dir_save=str(path),
            output_config=output_config,
            output_format=format,
        )

    # =========================================================================
    # Rich display
    # =========================================================================

    def __repr__(self) -> str:
        """Concise representation of output data."""
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

    def __len__(self) -> int:
        """Number of timesteps."""
        return len(self.times)
