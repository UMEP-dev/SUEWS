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
from ._supy_module import save_supy
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

        Examples
        --------
        >>> sim.update_config("new_config.yaml")
        >>> sim.update_config({"model": {"control": {"tstep": 300}}})
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

    def _update_config_from_dict(self, updates: dict):
        """Apply dictionary updates to configuration."""
        # This is a simplified version - in practice would need deep merging
        for key, value in updates.items():
            if hasattr(self._config, key):
                if isinstance(value, dict) and hasattr(
                    getattr(self._config, key), "__dict__"
                ):
                    # Recursive update for nested objects
                    for subkey, subvalue in value.items():
                        if hasattr(getattr(self._config, key), subkey):
                            setattr(getattr(self._config, key), subkey, subvalue)
                else:
                    setattr(self._config, key, value)

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

        Examples
        --------
        >>> sim.update_forcing("forcing_2023.txt")
        >>> sim.update_forcing(["forcing_2023.txt", "forcing_2024.txt"])
        >>> sim.update_forcing(df_forcing)
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
            Additional keyword arguments passed to save_supy.

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

        # Set default path
        if output_path is None:
            output_path = Path(".")
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

        # Use save_supy for all formats
        list_path_save = save_supy(
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
        """Reset simulation to initial state, clearing results."""
        self._df_output = None
        self._df_state_final = None
        self._run_completed = False

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
