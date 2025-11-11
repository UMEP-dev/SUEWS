"""
SUEWS Simulation Class.

Modern, object-oriented interface for SUEWS urban climate model simulations.
Provides a user-friendly wrapper around the existing SuPy infrastructure.
"""

from pathlib import Path
from typing import Any, Optional, Union
import warnings

import pandas as pd

from ._run import run_supy_ser

# Import SuPy components directly
from ._supy_module import _save_supy
from .data_model import RefValue
from .data_model.core import SUEWSConfig
from .util._io import read_forcing


class SUEWSSimulation:
    """
    Simplified SUEWS simulation class for urban climate modelling.

    This class provides a clean interface for:
    - Loading and updating configuration
    - Managing forcing data
    - Running simulations
    - Saving results

    Examples
    --------
    Basic usage:
    >>> sim = SUEWSSimulation("config.yaml")
    >>> sim.update_forcing("forcing.txt")
    >>> sim.run()
    >>> sim.save("output_dir/")

    Updating configuration:
    >>> sim.update_config({"model": {"control": {"tstep": 600}}})
    >>> sim.reset()
    >>> sim.run()
    """

    def __init__(self, config: Union[str, Path, dict, Any] = None):
        """
        Initialize SUEWS simulation.

        Parameters
        ----------
        config : str, Path, dict, or SUEWSConfig, optional
            Initial configuration source:
            - Path to YAML configuration file
            - Dictionary with configuration parameters
            - SUEWSConfig object
            - None to create empty simulation
        """
        self._config = None
        self._config_path = None
        self._df_state_init = None
        self._df_forcing = None
        self._df_output = None
        self._df_state_final = None
        self._run_completed = False

        if config is not None:
            self.update_config(config)

    def update_config(self, config):
        """
        Update simulation configuration.

        Can accept full or partial configuration updates.

        Parameters
        ----------
        config : str, Path, dict, or SUEWSConfig
            Configuration source:
            - Path to YAML file
            - Dictionary with parameters (can be partial)
            - SUEWSConfig object

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Examples
        --------
        >>> sim.update_config("new_config.yaml")
        >>> sim.update_config({"model": {"control": {"tstep": 300}}})
        >>> sim.update_config("config.yaml").update_forcing("forcing.txt")
        """
        if isinstance(config, (str, Path)):
            # Load from YAML file
            config_path = Path(config).expanduser().resolve()
            if not config_path.exists():
                raise FileNotFoundError(f"Configuration file not found: {config_path}")

            # Load YAML
            self._config = SUEWSConfig.from_yaml(str(config_path))
            self._config_path = config_path

            # Convert to initial state DataFrame
            self._df_state_init = self._config.to_df_state()

            # Try to load forcing from config
            self._try_load_forcing_from_config()

        elif isinstance(config, dict):
            # Update existing config with dictionary
            if self._config is None:
                self._config = SUEWSConfig()

            # Deep update the configuration
            self._update_config_from_dict(config)

            # Regenerate state DataFrame
            self._df_state_init = self._config.to_df_state()

        else:
            # Assume it's a SUEWSConfig object
            self._config = config
            self._df_state_init = self._config.to_df_state()

        return self

    def _update_config_from_dict(self, updates: dict):
        """Apply dictionary updates to configuration."""

        def recursive_update(obj, upd):
            for key, value in upd.items():
                if hasattr(obj, key):
                    attr = getattr(obj, key)
                    if isinstance(value, dict) and hasattr(attr, "__dict__"):
                        # Recursive to read nested dictionaries
                        recursive_update(attr, value)
                    # Check whether site index or site name is provided
                    elif isinstance(attr, list):
                        site_key = list(value.keys())[0]
                        site_value = value[site_key]
                        if isinstance(site_key, int):
                            # Select site on index
                            attr = attr[site_key]
                            recursive_update(attr, site_value)
                        elif isinstance(site_key, str):
                            # Select site on name
                            attr_site = next(
                                (item for item in attr if item.name == site_key),
                                None,
                            )
                            if attr_site:
                                recursive_update(attr_site, site_value)
                            elif len(attr) == 1:
                                # Without name or index and only one site
                                attr_site = attr[0]
                                # Distinguish site name pattern from shorthand
                                # If site_key is an attribute on the site object, it's shorthand
                                if hasattr(attr_site, site_key):
                                    # Shorthand pattern: {'name': 'test'} or {'properties': {...}}
                                    recursive_update(attr_site, value)
                                else:
                                    # Site name pattern: {'NonExistent': {'gridiv': 99}}
                                    recursive_update(attr_site, site_value)
                            else:
                                # Otherwise skip these parameters
                                continue
                    else:
                        setattr(obj, key, value)

        recursive_update(self._config, updates)

    def update_forcing(self, forcing_data):
        """
        Update meteorological forcing data.

        Parameters
        ----------
        forcing_data : str, Path, list of paths, or pandas.DataFrame
            Forcing data source:
            - Path to a single forcing file
            - List of paths to forcing files (concatenated in order)
            - Path to directory containing forcing files (deprecated)
            - DataFrame with forcing data

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Examples
        --------
        >>> sim.update_forcing("forcing_2023.txt")
        >>> sim.update_forcing(["forcing_2023.txt", "forcing_2024.txt"])
        >>> sim.update_forcing(df_forcing)
        >>> sim.update_config(cfg).update_forcing(forcing).run()
        """
        if isinstance(forcing_data, RefValue):
            forcing_data = forcing_data.value
        if isinstance(forcing_data, pd.DataFrame):
            self._df_forcing = forcing_data.copy()
        elif isinstance(forcing_data, list):
            # Handle list of files
            self._df_forcing = SUEWSSimulation._load_forcing_from_list(forcing_data)
        elif isinstance(forcing_data, (str, Path)):
            forcing_path = Path(forcing_data).expanduser().resolve()
            if not forcing_path.exists():
                raise FileNotFoundError(f"Forcing path not found: {forcing_path}")
            self._df_forcing = SUEWSSimulation._load_forcing_file(forcing_path)
        else:
            raise ValueError(f"Unsupported forcing data type: {type(forcing_data)}")

        return self

    def _try_load_forcing_from_config(self):
        """Try to load forcing data from configuration if not explicitly provided."""
        if self._config is None:
            return

        try:
            if hasattr(self._config, "model") and hasattr(
                self._config.model, "control"
            ):
                forcing_file_obj = getattr(
                    self._config.model.control, "forcing_file", None
                )

                if forcing_file_obj is not None:
                    # Handle RefValue wrapper
                    if hasattr(forcing_file_obj, "value"):
                        forcing_value = forcing_file_obj.value
                    else:
                        forcing_value = forcing_file_obj

                    # Skip default placeholder value
                    if forcing_value and forcing_value != "forcing.txt":
                        # Resolve paths relative to config file if needed
                        if self._config_path:
                            forcing_value = self._resolve_forcing_paths(forcing_value)

                        self.update_forcing(forcing_value)

        except Exception as e:
            warnings.warn(f"Could not load forcing from config: {e}", stacklevel=2)

    def _resolve_forcing_paths(
        self, paths: Union[str, list[str]]
    ) -> Union[str, list[str]]:
        """Resolve forcing paths relative to config file location.

        Parameters
        ----------
        paths : str or list of str
            Path(s) to resolve. Relative paths are resolved relative to config file.

        Returns
        -------
        str or list of str
            Resolved path(s). Absolute paths are returned unchanged.
        """
        if isinstance(paths, list):
            return [self._resolve_single_path(p) for p in paths]
        else:
            return self._resolve_single_path(paths)

    def _resolve_single_path(self, path: str) -> str:
        """Resolve a single path relative to config file if it's relative.

        Parameters
        ----------
        path : str
            Path to resolve

        Returns
        -------
        str
            Resolved path. Absolute paths are returned unchanged.
        """
        if Path(path).is_absolute():
            return path
        else:
            # Relative path - resolve relative to config file location
            return str(self._config_path.parent / path)

    @staticmethod
    def _load_forcing_from_list(forcing_list: list[Union[str, Path]]) -> pd.DataFrame:
        """Load and concatenate forcing data from a list of files."""
        if not forcing_list:
            raise ValueError("Empty forcing file list provided")

        dfs = []
        for item in forcing_list:
            path = Path(item).expanduser().resolve()

            if not path.exists():
                raise FileNotFoundError(f"Forcing file not found: {path}")

            if path.is_dir():
                raise ValueError(
                    f"Directory '{path}' found in forcing file list. "
                    "Directories are not allowed in lists."
                )

            df = read_forcing(str(path))
            dfs.append(df)

        return pd.concat(dfs, axis=0).sort_index()

    @staticmethod
    def _load_forcing_file(forcing_path: Path) -> pd.DataFrame:
        """Load forcing data from file or directory."""
        if forcing_path.is_dir():
            # Issue deprecation warning for directory usage
            warnings.warn(
                f"Loading forcing data from directory '{forcing_path}' is deprecated. "
                "Please specify individual files or use a list of files instead.",
                DeprecationWarning,
                stacklevel=3,
            )

            # Find forcing files in directory
            patterns = ["*.txt", "*.csv", "*.met"]
            forcing_files = []
            for pattern in patterns:
                forcing_files.extend(sorted(forcing_path.glob(pattern)))

            if not forcing_files:
                raise FileNotFoundError(
                    f"No forcing files found in directory: {forcing_path}"
                )

            # Concatenate all files
            dfs = []
            for file in forcing_files:
                dfs.append(read_forcing(str(file)))

            return pd.concat(dfs, axis=0).sort_index()
        else:
            return read_forcing(str(forcing_path))

    def run(self, start_date=None, end_date=None, **run_kwargs) -> pd.DataFrame:
        """
        Run SUEWS simulation.

        Parameters
        ----------
        start_date : str, optional
            Start date for simulation.
        end_date : str, optional
            End date for simulation.
        run_kwargs : dict
            Additional keyword arguments passed to run_supy.
            Common options:
            - save_state: Save state at each timestep (default False)
            - chunk_day: Days per chunk for memory efficiency (default 3660)

        Returns
        -------
        pandas.DataFrame
            Simulation results with MultiIndex columns (group, variable).

        Raises
        ------
        RuntimeError
            If configuration or forcing data is missing.
        """
        # Validate inputs
        if self._df_state_init is None:
            raise RuntimeError("No configuration loaded. Use update_config() first.")
        if self._df_forcing is None:
            raise RuntimeError("No forcing data loaded. Use update_forcing() first.")

        # Run simulation
        result = run_supy_ser(
            self._df_forcing.loc[start_date:end_date],
            self._df_state_init,
            # **run_kwargs # Causes problems - requires explicit arguments
        )
        self._df_output = result[0]
        self._df_state_final = result[1]

        self._run_completed = True
        return self._df_output

    def save(self, output_path=None, **save_kwargs):
        """
        Save simulation results according to OutputConfig settings.

        Parameters
        ----------
        output_path : str or Path, optional
            Output directory path. If None, uses current directory.
        save_kwargs : dict
            Additional keyword arguments forwarded to the internal `_save_supy` helper.

        Returns
        -------
        list
            List of paths to saved files.

        Raises
        ------
        RuntimeError
            If no simulation results are available.
        """
        if not self._run_completed:
            raise RuntimeError("No simulation results available. Run simulation first.")

        # Set default path with priority: parameter > config > current directory
        if output_path is None:
            # Check if path is specified in config
            config_path = None
            try:
                output_file = self._config.model.control.output_file
                if not isinstance(output_file, str) and output_file.path:
                    config_path = output_file.path
            except AttributeError:
                pass

            output_path = Path(config_path) if config_path else Path(".")
        else:
            output_path = Path(output_path)

        # Extract parameters from config
        output_format = None
        output_config = None
        freq_s = 3600  # default hourly
        site = ""

        if self._config:
            # Get output frequency from OutputConfig if available
            if (
                hasattr(self._config, "model")
                and hasattr(self._config.model, "control")
                and hasattr(self._config.model.control, "output_file")
                and not isinstance(self._config.model.control.output_file, str)
            ):
                output_config = self._config.model.control.output_file
                if hasattr(output_config, "freq") and output_config.freq is not None:
                    freq_s = output_config.freq
                # Removed for now - can't update from YAML (TODO)
                # if hasattr(output_config, 'format') and output_config.format is not None:
                #     output_format = output_config.format

            # Get site name from first site
            if hasattr(self._config, "sites") and len(self._config.sites) > 0:
                site = self._config.sites[0].name
        if "format" in save_kwargs:  # TODO: When yaml format working, make elif
            output_format = save_kwargs["format"]

        # Use internal save helper for all formats
        list_path_save = _save_supy(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            freq_s=int(freq_s),
            site=site,
            path_dir_save=str(output_path),
            # **save_kwargs # Problematic, save_supy expects explicit arguments
            output_config=output_config,
            output_format=output_format,
        )
        return list_path_save

    def reset(self):
        """Reset simulation to initial state, clearing results.

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Examples
        --------
        >>> sim.run()
        >>> sim.reset().run()  # Re-run with same configuration
        """
        self._df_output = None
        self._df_state_final = None
        self._run_completed = False
        return self

    @classmethod
    def from_sample_data(cls):
        """Create SUEWSSimulation instance with built-in sample data.

        This factory method provides a quick way to create a simulation object
        pre-loaded with sample configuration and forcing data, ideal for tutorials,
        testing, and learning the SUEWS workflow.

        Returns
        -------
        SUEWSSimulation
            Simulation instance ready to run with sample data loaded.

        Examples
        --------
        Quick start with sample data:

        >>> from supy import SUEWSSimulation
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> results = sim.results

        This is equivalent to the functional approach:

        >>> import supy as sp
        >>> df_state, df_forcing = sp.load_sample_data()
        >>> df_output, df_final = sp.run_supy(df_forcing, df_state)

        See Also
        --------
        load_sample_data : Load sample data as DataFrames (functional approach)
        """
        from ._env import trv_supy_module
        from ._supy_module import _load_sample_data

        df_state_init, df_forcing = _load_sample_data()
        sample_config_path = Path(trv_supy_module / "sample_data" / "sample_config.yml")

        sim = cls()
        try:
            sim.update_config(sample_config_path)
        except Exception as exc:
            warnings.warn(
                f"Could not load sample configuration metadata: {exc}",
                stacklevel=2,
            )

        sim._df_state_init = df_state_init
        sim._df_forcing = df_forcing
        return sim

    def __repr__(self) -> str:
        """Concise representation showing simulation status.

        Returns
        -------
        str
            Status indicator: Complete, Ready, or Not configured

        Examples
        --------
        >>> sim = SUEWSSimulation()
        >>> sim
        SUEWSSimulation(Not configured)

        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim
        SUEWSSimulation(Ready: 1 site, 105408 timesteps)

        >>> sim.run()
        >>> sim
        SUEWSSimulation(Complete: 105408 results)
        """
        if self._run_completed:
            n_results = len(self._df_output) if self._df_output is not None else 0
            return f"SUEWSSimulation(Complete: {n_results} results)"
        elif self._df_state_init is not None and self._df_forcing is not None:
            n_sites = len(self._df_state_init)
            n_timesteps = len(self._df_forcing)
            return f"SUEWSSimulation(Ready: {n_sites} site(s), {n_timesteps} timesteps)"
        else:
            missing = []
            if self._df_state_init is None:
                missing.append("config")
            if self._df_forcing is None:
                missing.append("forcing")
            return f"SUEWSSimulation(Not configured: missing {', '.join(missing)})"

    def is_ready(self) -> bool:
        """Check if simulation is configured and ready to run.

        Returns
        -------
        bool
            True if both configuration and forcing data are loaded.

        Examples
        --------
        >>> sim = SUEWSSimulation()
        >>> sim.is_ready()
        False

        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.is_ready()
        True
        """
        return self._df_state_init is not None and self._df_forcing is not None

    def is_complete(self) -> bool:
        """Check if simulation has been run successfully.

        Returns
        -------
        bool
            True if simulation has completed and results are available.

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.is_complete()
        False

        >>> sim.run()
        >>> sim.is_complete()
        True
        """
        return self._run_completed

    def get_variable(
        self,
        var_name: str,
        group: Optional[str] = None,
        site: Optional[Union[int, str]] = None,
    ) -> pd.DataFrame:
        """Extract specific variable from simulation results.

        Convenience method to extract variables from the MultiIndex column structure
        without needing to understand the internal data layout.

        Parameters
        ----------
        var_name : str
            Variable name to extract (e.g., 'QH', 'QE', 'Tair').
        group : str, optional
            Output group name if variable appears in multiple groups.
            If None and variable is in multiple groups, raises ValueError.
        site : int or str, optional
            Site index or name. If None, returns all sites.

        Returns
        -------
        pandas.DataFrame
            DataFrame with selected variable(s), indexed by time.

        Raises
        ------
        RuntimeError
            If simulation hasn't been run yet.
        ValueError
            If variable name not found in results, or if variable is ambiguous
            (appears in multiple groups) and no group specified.

        Examples
        --------
        Extract sensible heat flux:

        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> qh = sim.get_variable("QH")
        >>> qh.plot()  # Quick visualisation

        Handle variables in multiple groups:

        >>> # If 'Tair' appears in both 'forcing' and 'output' groups:
        >>> tair = sim.get_variable("Tair", group="output")

        See Also
        --------
        results : Full simulation output DataFrame
        """
        if not self._run_completed:
            raise RuntimeError("No results available. Run simulation first.")

        if self._df_output is None:
            raise RuntimeError("Results DataFrame is None")

        # Check if variable exists in results
        all_vars = self._df_output.columns.get_level_values("var").unique()
        if var_name not in all_vars:
            raise ValueError(
                f"Variable '{var_name}' not found. "
                f"Available variables: {', '.join(all_vars[:10])}"
                + ("..." if len(all_vars) > 10 else "")
            )

        # Check if variable appears in multiple groups
        matching_groups = []
        for grp in self._df_output.columns.get_level_values("group").unique():
            try:
                # Check if this group contains the variable
                _ = self._df_output.xs((grp, var_name), level=("group", "var"), axis=1)
                matching_groups.append(grp)
            except KeyError:
                continue

        if len(matching_groups) == 0:
            # Should not happen if var_name was found above
            raise ValueError(f"Variable '{var_name}' not found in any group")

        elif len(matching_groups) > 1:
            # Variable is ambiguous - need group specification
            if group is None:
                raise ValueError(
                    f"Variable '{var_name}' appears in multiple groups: "
                    f"{', '.join(matching_groups)}. "
                    f"Specify group parameter (e.g., group='{matching_groups[0]}')"
                )
            elif group not in matching_groups:
                raise ValueError(
                    f"Variable '{var_name}' not found in group '{group}'. "
                    f"Available groups for this variable: {', '.join(matching_groups)}"
                )
            # Extract from specified group
            result = self._df_output.xs(
                (group, var_name), level=("group", "var"), axis=1
            )

        else:
            # Variable is in only one group
            if group is not None and group != matching_groups[0]:
                raise ValueError(
                    f"Variable '{var_name}' only exists in group '{matching_groups[0]}', "
                    f"not in '{group}'"
                )
            result = self._df_output.xs(var_name, level="var", axis=1)

        # Filter by site if requested
        if site is not None:
            if isinstance(site, str):
                result = result[site]
            else:
                result = result.iloc[:, site]

        return result

    @property
    def config(self) -> Optional[Any]:
        """Access to simulation configuration."""
        return self._config

    @property
    def forcing(self) -> Optional[pd.DataFrame]:
        """Access to forcing data DataFrame."""
        return self._df_forcing

    @property
    def results(self) -> Optional[pd.DataFrame]:
        """Access to simulation results DataFrame."""
        return self._df_output

    @property
    def state_init(self) -> Optional[pd.DataFrame]:
        """Initial state DataFrame for simulation.

        Returns
        -------
        pandas.DataFrame or None
            Initial state with surface characteristics and parameters.
            None if no configuration loaded.

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.state_init.shape
        (1, 1423)
        """
        return self._df_state_init

    @property
    def state_final(self) -> Optional[pd.DataFrame]:
        """Final state DataFrame after simulation.

        Available only after running simulation. Contains evolved
        state variables that can be used to continue simulation.

        Returns
        -------
        pandas.DataFrame or None
            Final state after simulation run.
            None if simulation hasn't been run yet.

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> sim.state_final is not None
        True

        See Also
        --------
        reset : Clear results and reset to initial state
        """
        return self._df_state_final
