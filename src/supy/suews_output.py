"""
SUEWSOutput - OOP wrapper for SUEWS simulation results.

Provides a structured interface for accessing, analysing, and exporting
SUEWS model output data.
"""

import warnings
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import numpy as np
import pandas as pd


class SUEWSOutput:
    """
    Wrapper for SUEWS simulation results with analysis convenience functions.

    Provides intuitive access to output variables, analysis methods,
    and export capabilities for SUEWS simulation results.

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
    SUEWSOutput(2012-01-01 to 2012-12-31, 1 grid(s), 6 groups)

    Access variables directly:

    >>> output.QH  # Sensible heat flux
    >>> output.QE  # Latent heat flux
    >>> output.get_variable("Tair", group="SUEWS")

    Access by group:

    >>> output.groups  # ['SUEWS', 'DailyState', 'snow', ...]
    >>> output.get_group("SUEWS")

    Analysis methods:

    >>> output.energy_balance_closure()
    >>> output.diurnal_average("QH")
    >>> output.monthly_summary()

    Export:

    >>> output.save("output/", format="parquet")
    >>> output.to_netcdf("results.nc")

    Restart runs:

    >>> initial_state = output.to_initial_state()
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
        self._df_state_final = df_state_final.copy()
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

        Use with SUEWSSimulation.from_state() to continue simulations.

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
    def n_grids(self) -> int:
        """Number of grids.

        .. deprecated::
            Use ``len(output.grids)`` instead.
        """
        warnings.warn(
            "SUEWSOutput.n_grids is deprecated, use len(output.grids) instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return len(self.grids)

    @property
    def n_timesteps(self) -> int:
        """Number of output timesteps.

        .. deprecated::
            Use ``len(output.times)`` instead.
        """
        warnings.warn(
            "SUEWSOutput.n_timesteps is deprecated, use len(output.times) instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return len(self.times)

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
        """
        # Check if it's a group name first
        all_groups = self._df_output.columns.get_level_values("group").unique()
        if name in all_groups:
            return self.get_group(name)

        # Check if it's a variable name in any group
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if name in all_vars:
            return self.get_variable(name)

        # Check case-insensitively for variables
        name_lower = name.lower()
        for var in all_vars:
            if var.lower() == name_lower:
                return self.get_variable(var)

        raise AttributeError(
            f"'{type(self).__name__}' has no attribute '{name}'. "
            f"Available groups: {list(all_groups)}, "
            f"Available variables (first 10): {list(all_vars[:10])}..."
        )

    def __getitem__(self, key: str) -> pd.DataFrame:
        """Access variables or groups by name."""
        # Check if it's a group name
        if key in self.groups:
            return self.get_group(key)
        # Otherwise treat as variable
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
        if group not in self.groups:
            raise ValueError(
                f"Group '{group}' not found. Available groups: {self.groups}"
            )
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
        # Check if variable exists
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if var_name not in all_vars:
            raise ValueError(
                f"Variable '{var_name}' not found. "
                f"Available variables (first 10): {list(all_vars[:10])}..."
            )

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

        target_group = group or matching_groups[0]
        if group is not None and group not in matching_groups:
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
    # State for restart
    # =========================================================================

    def to_initial_state(self) -> pd.DataFrame:
        """
        Extract the final timestep as initial state for next simulation.

        This method handles the conversion from multi-timestep state
        to single-timestep initial state format.

        Returns
        -------
        pd.DataFrame
            Initial state ready for next simulation
        """
        df = self._df_state_final
        idx_names = list(df.index.names)

        if "datetime" in idx_names:
            datetime_level = idx_names.index("datetime")
            last_datetime = df.index.get_level_values(datetime_level).max()
            if isinstance(df.index, pd.MultiIndex):
                return df.xs(last_datetime, level="datetime").copy()
            else:
                return df.loc[[last_datetime]].copy()
        return df.copy()

    # =========================================================================
    # Analysis (convenience functions)
    # =========================================================================

    def energy_balance_closure(self, method: str = "residual") -> pd.DataFrame:
        """
        Calculate energy balance closure statistics.

        Energy balance: QN = QH + QE + QS (+ QF + dQs)

        Parameters
        ----------
        method : str
            Calculation method:
            - "residual": Calculate QN - (QH + QE + QS)
            - "ratio": Calculate (QH + QE + QS) / QN

        Returns
        -------
        pd.DataFrame
            Energy balance closure statistics
        """
        try:
            qn = self.get_variable("QN", group="SUEWS")
            qh = self.get_variable("QH", group="SUEWS")
            qe = self.get_variable("QE", group="SUEWS")
            qs = self.get_variable("QS", group="SUEWS")
        except ValueError as e:
            raise ValueError(
                f"Cannot calculate energy balance: {e}. "
                "Requires QN, QH, QE, QS variables in SUEWS group."
            )

        if method == "residual":
            # Residual: QN - (QH + QE + QS)
            residual = qn - (qh + qe + qs)
            result = pd.DataFrame({
                "QN": qn.mean(),
                "QH": qh.mean(),
                "QE": qe.mean(),
                "QS": qs.mean(),
                "Residual": residual.mean(),
                "Residual_std": residual.std(),
            })
        elif method == "ratio":
            # Closure ratio: (QH + QE + QS) / QN
            with np.errstate(divide="ignore", invalid="ignore"):
                ratio = (qh + qe + qs) / qn
                ratio = ratio.replace([np.inf, -np.inf], np.nan)
            result = pd.DataFrame({
                "Closure_ratio_mean": ratio.mean(),
                "Closure_ratio_std": ratio.std(),
            })
        else:
            raise ValueError(f"Unknown method: {method}. Use 'residual' or 'ratio'.")

        return result

    def diurnal_average(self, var: Optional[str] = None) -> pd.DataFrame:
        """
        Calculate mean diurnal cycle.

        Parameters
        ----------
        var : str, optional
            Specific variable. If None, calculates for common energy variables.

        Returns
        -------
        pd.DataFrame
            Hourly mean values grouped by hour of day
        """
        if var is not None:
            data = self.get_variable(var)
        else:
            # Default to key energy balance variables
            vars_to_use = ["QN", "QH", "QE", "QS"]
            available = self.available_variables.get("SUEWS", [])
            vars_to_use = [v for v in vars_to_use if v in available]
            data = pd.concat(
                [self.get_variable(v, group="SUEWS") for v in vars_to_use],
                axis=1,
                keys=vars_to_use,
            )

        # Extract hour and group
        df = data.copy()

        # Handle MultiIndex
        if isinstance(df.index, pd.MultiIndex):
            df = df.reset_index(level="grid", drop=True)

        df["hour"] = df.index.hour
        return df.groupby("hour").mean()

    def monthly_summary(self, var: Optional[str] = None) -> pd.DataFrame:
        """
        Monthly statistics summary.

        Parameters
        ----------
        var : str, optional
            Specific variable. If None, summarises key energy variables.

        Returns
        -------
        pd.DataFrame
            Monthly statistics
        """
        if var is not None:
            data = self.get_variable(var)
        else:
            # Default to key variables
            vars_to_use = ["QN", "QH", "QE", "QS"]
            available = self.available_variables.get("SUEWS", [])
            vars_to_use = [v for v in vars_to_use if v in available]
            data = pd.concat(
                [self.get_variable(v, group="SUEWS") for v in vars_to_use],
                axis=1,
                keys=vars_to_use,
            )

        df = data.copy()

        # Handle MultiIndex
        if isinstance(df.index, pd.MultiIndex):
            df = df.reset_index(level="grid", drop=True)

        return df.resample("M").agg(["mean", "std", "min", "max"])

    def compare_with(self, other: "SUEWSOutput") -> pd.DataFrame:
        """
        Compare two output objects.

        Useful for comparing different scenarios or model runs.

        Parameters
        ----------
        other : SUEWSOutput
            Another output object to compare with

        Returns
        -------
        pd.DataFrame
            Comparison statistics (mean difference, correlation, etc.)
        """
        # Get common variables
        self_vars = set()
        other_vars = set()
        for vars in self.available_variables.values():
            self_vars.update(vars)
        for vars in other.available_variables.values():
            other_vars.update(vars)
        common_vars = self_vars.intersection(other_vars)

        # Focus on key energy variables
        key_vars = ["QN", "QH", "QE", "QS", "Tair"]
        compare_vars = [v for v in key_vars if v in common_vars]

        results = []
        for var in compare_vars:
            try:
                self_data = self.get_variable(var)
                other_data = other.get_variable(var)

                # Align data
                common_idx = self_data.index.intersection(other_data.index)
                if len(common_idx) == 0:
                    continue

                s = self_data.loc[common_idx].values.flatten()
                o = other_data.loc[common_idx].values.flatten()

                diff = s - o
                results.append({
                    "variable": var,
                    "mean_diff": np.nanmean(diff),
                    "rmse": np.sqrt(np.nanmean(diff**2)),
                    "correlation": np.corrcoef(s, o)[0, 1],
                    "n_common": len(common_idx),
                })
            except (ValueError, KeyError):
                continue

        return pd.DataFrame(results)

    # =========================================================================
    # Manipulation
    # =========================================================================

    def resample(self, freq: str) -> "SUEWSOutput":
        """
        Resample output to different frequency.

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
    # Plotting
    # =========================================================================

    def plot_timeseries(self, var: Optional[str] = None, ax=None, **kwargs):
        """
        Quick timeseries plot.

        Parameters
        ----------
        var : str, optional
            Variable to plot. If None, plots QH.
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

        var = var or "QH"
        data = self.get_variable(var)

        # Handle MultiIndex
        if isinstance(data.index, pd.MultiIndex):
            data = data.reset_index(level="grid", drop=True)

        data.plot(ax=ax, **kwargs)
        ax.set_ylabel(var)
        ax.set_xlabel("Time")
        ax.set_title(f"SUEWS Output: {var}")

        return fig, ax

    def plot_diurnal(self, var: Optional[str] = None, ax=None, **kwargs):
        """
        Plot diurnal climatology.

        Parameters
        ----------
        var : str, optional
            Variable to plot. If None, plots energy balance components.
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
            fig, ax = plt.subplots(figsize=(10, 6))
        else:
            fig = ax.get_figure()

        diurnal = self.diurnal_average(var)
        diurnal.plot(ax=ax, **kwargs)
        ax.set_xlabel("Hour of Day")
        ax.set_ylabel("Flux (W/m²)" if var is None else var)
        ax.set_title("Diurnal Cycle")
        ax.legend()

        return fig, ax

    def plot_energy_balance(self, ax=None, **kwargs):
        """
        Standard energy balance plot (QN, QH, QE, QS).

        Parameters
        ----------
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
            fig, ax = plt.subplots(figsize=(10, 6))
        else:
            fig = ax.get_figure()

        diurnal = self.diurnal_average()
        diurnal.plot(ax=ax, **kwargs)
        ax.set_xlabel("Hour of Day")
        ax.set_ylabel("Flux (W/m²)")
        ax.set_title("Energy Balance - Diurnal Cycle")
        ax.axhline(y=0, color="k", linestyle="--", alpha=0.3)
        ax.legend()

        return fig, ax

    # =========================================================================
    # Export
    # =========================================================================

    def to_dataframe(self) -> pd.DataFrame:
        """Return copy of full output DataFrame.

        .. deprecated::
            Use :attr:`df` property instead.
        """
        warnings.warn(
            "SUEWSOutput.to_dataframe() is deprecated, use .df instead",
            DeprecationWarning,
            stacklevel=2,
        )
        return self._df_output.copy()

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

    def to_netcdf(self, path: Union[str, Path]) -> Path:
        """
        Export to NetCDF format.

        Parameters
        ----------
        path : str or Path
            Output file path

        Returns
        -------
        Path
            Path to saved file
        """
        try:
            import xarray as xr
        except ImportError as e:
            raise ImportError(
                "NetCDF export requires 'xarray' and 'netcdf4'. "
                "Install with: pip install xarray netcdf4"
            ) from e

        path = Path(path)

        # Convert to xarray Dataset
        df = self._df_output.copy()

        # Reset index for cleaner xarray conversion
        df_reset = df.reset_index()

        # Create Dataset
        ds = xr.Dataset()

        # Add coordinates
        ds["datetime"] = (["time"], df_reset["datetime"].values)
        ds["grid"] = (["grid"], df_reset["grid"].unique())

        # Add variables by group
        for group in self.groups:
            group_data = self._df_output[group]
            for var in group_data.columns.get_level_values("var").unique():
                var_data = group_data[var]
                if isinstance(var_data, pd.DataFrame):
                    var_data = var_data.iloc[:, 0]  # Take first column if multiple
                ds[f"{group}_{var}"] = (["time"], var_data.values)

        # Add metadata
        ds.attrs["source"] = "SUEWS model output"
        ds.attrs["created"] = pd.Timestamp.now().isoformat()
        if self._metadata:
            for key, val in self._metadata.items():
                if isinstance(val, (str, int, float)):
                    ds.attrs[key] = val

        # Save
        ds.to_netcdf(path)

        return path

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
