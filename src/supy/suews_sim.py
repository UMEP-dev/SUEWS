"""
SUEWS Simulation Class.

Modern, object-oriented interface for SUEWS urban climate model simulations.
Provides a user-friendly wrapper around the existing SuPy infrastructure.
"""

import copy
from pathlib import Path, PurePosixPath
from typing import Any, Optional, Union
import warnings

import numpy as np
import pandas as pd

from ._run import run_supy_ser

# Import SuPy components directly
from ._supy_module import _save_supy
from .data_model import RefValue
from .data_model.core import SUEWSConfig
from .util._io import read_forcing

# Import new OOP classes
from .suews_forcing import SUEWSForcing
from .suews_output import SUEWSOutput

# Constants
DEFAULT_OUTPUT_FREQ_SECONDS = 3600  # Default hourly output frequency
DEFAULT_FORCING_FILE_PATTERNS = [
    "*.txt",
    "*.csv",
    "*.met",
]  # Valid forcing file extensions


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

    def update_config(
        self,
        config: Union[str, Path, dict, SUEWSConfig],
        auto_load_forcing: bool = True,
    ) -> "SUEWSSimulation":
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
        auto_load_forcing : bool, optional
            If True (default), automatically load forcing data specified in the
            config file. If False, forcing must be loaded explicitly using
            ``update_forcing()``.

            Set to False when:
            - You want explicit control over forcing data loading
            - Forcing file paths in config are placeholders
            - You plan to provide forcing data programmatically

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Examples
        --------
        >>> sim.update_config("new_config.yaml")
        >>> sim.update_config({"model": {"control": {"tstep": 300}}})
        >>> sim.update_config("config.yaml").update_forcing("forcing.txt")

        Explicit forcing control:

        >>> sim.update_config("config.yaml", auto_load_forcing=False)
        >>> sim.update_forcing("custom_forcing.txt")
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

            # Optionally try to load forcing from config
            if auto_load_forcing:
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

    def update_forcing(
        self, forcing_data: Union[str, Path, list, pd.DataFrame, SUEWSForcing]
    ) -> "SUEWSSimulation":
        """
        Update meteorological forcing data.

        Parameters
        ----------
        forcing_data : str, Path, list of paths, pandas.DataFrame, or SUEWSForcing
            Forcing data source:
            - Path to a single forcing file
            - List of paths to forcing files (concatenated in order)
            - Path to directory containing forcing files (deprecated)
            - DataFrame with forcing data
            - SUEWSForcing object

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Examples
        --------
        >>> sim.update_forcing("forcing_2023.txt")
        >>> sim.update_forcing(["forcing_2023.txt", "forcing_2024.txt"])
        >>> sim.update_forcing(df_forcing)
        >>> sim.update_forcing(SUEWSForcing.from_file("forcing.txt"))
        >>> sim.update_config(cfg).update_forcing(forcing).run()
        """
        if isinstance(forcing_data, RefValue):
            forcing_data = forcing_data.value
        if isinstance(forcing_data, SUEWSForcing):
            self._df_forcing = forcing_data.df.copy()
        elif isinstance(forcing_data, pd.DataFrame):
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

        Notes
        -----
        Relative paths can use '..' to reference parent directories. This is
        intentional to allow flexible file organization. Path traversal restrictions
        are not enforced since:
        1. Config files are created by the user themselves
        2. Code runs on the user's own machine
        3. No untrusted external input is involved
        """
        path_str = str(path)

        # Treat platform-native absolute paths as literal
        if Path(path_str).is_absolute() or PurePosixPath(path_str).is_absolute():
            return path_str

        # Relative path - resolve relative to config file location
        # Using resolve() handles '..' and normalizes the path
        return str((self._config_path.parent / Path(path_str)).resolve())

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
            forcing_files = []
            for pattern in DEFAULT_FORCING_FILE_PATTERNS:
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

    def run(self, start_date=None, end_date=None, **run_kwargs) -> SUEWSOutput:
        """
        Run SUEWS simulation.

        Parameters
        ----------
        start_date : str, optional
            Start date for simulation (inclusive).
        end_date : str, optional
            End date for simulation (inclusive).
        run_kwargs : dict
            **Note**: Additional keyword arguments are currently not supported
            due to underlying function signature constraints. This parameter
            is reserved for future use.

            In a future version, the following options may be supported:
            - save_state: bool - Save state at each timestep (planned)
            - chunk_day: int - Days per chunk for memory efficiency (planned)

            For now, simulations use default settings:
            - save_state=False (states not saved at each step)
            - chunk_day=3660 (approximately 10 years per chunk)

        Returns
        -------
        SUEWSOutput
            Simulation results wrapped in an OOP interface with analysis
            and plotting convenience methods. Access raw DataFrame via
            ``.to_dataframe()`` or ``.df``.

        Raises
        ------
        RuntimeError
            If configuration or forcing data is missing.

        Notes
        -----
        The simulation runs with fixed internal settings. For advanced control
        over simulation parameters, consider using the lower-level functional
        API (though it is deprecated).

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> output = sim.run()
        >>> output.QH  # Access sensible heat flux
        >>> output.diurnal_average("QH")  # Get diurnal pattern
        >>> output.to_dataframe()  # Get raw DataFrame
        """
        # Validate inputs
        if self._df_state_init is None:
            raise RuntimeError("No configuration loaded. Use update_config() first.")
        if self._df_forcing is None:
            raise RuntimeError("No forcing data loaded. Use update_forcing() first.")

        # Fall back to config values if start_date/end_date not provided
        if start_date is None and self._config is not None:
            if (
                hasattr(self._config, "model")
                and hasattr(self._config.model, "control")
                and hasattr(self._config.model.control, "start_time")
            ):
                start_date = self._config.model.control.start_time

        if end_date is None and self._config is not None:
            if (
                hasattr(self._config, "model")
                and hasattr(self._config.model, "control")
                and hasattr(self._config.model.control, "end_time")
            ):
                end_date = self._config.model.control.end_time

        # Run simulation
        result = run_supy_ser(
            self._df_forcing.loc[start_date:end_date],
            self._df_state_init,
            # **run_kwargs # Causes problems - requires explicit arguments
        )
        self._df_output = result[0]
        self._df_state_final = result[1]

        self._run_completed = True

        # Wrap results in SUEWSOutput
        return SUEWSOutput(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            config=self._config,
        )

    def run_dts(
        self, start_date=None, end_date=None, nlayer: int = 5, ndepth: int = 5
    ) -> pd.DataFrame:
        """
        Run SUEWS simulation using DTS (Derived Type Structures) interface.

        This is an experimental method that uses the f90wrap DTS interface
        directly, bypassing the df_state DataFrame intermediate layer.
        This approach provides cleaner integration with the Fortran kernel
        and may offer performance benefits for large simulations.

        Parameters
        ----------
        start_date : str, optional
            Start date for simulation (inclusive).
        end_date : str, optional
            End date for simulation (inclusive).
        nlayer : int, optional
            Number of urban canopy layers (default: 5).
        ndepth : int, optional
            Number of substrate depth levels (default: 5).

        Returns
        -------
        pandas.DataFrame
            Simulation results with MultiIndex columns (group, variable).

        Raises
        ------
        RuntimeError
            If configuration or forcing data is missing.

        Notes
        -----
        This method is experimental and may not support all features of the
        standard run() method. Use run() for production simulations.

        The DTS interface provides direct access to the Fortran kernel via
        f90wrap-generated DTS objects and accessor functions.

        Examples
        --------
        >>> sim = SUEWSSimulation("config.yaml")
        >>> sim.update_forcing("forcing.txt")
        >>> df_output = sim.run_dts()
        """
        # Validate inputs
        if self._config is None:
            raise RuntimeError("No configuration loaded. Use update_config() first.")
        if self._df_forcing is None:
            raise RuntimeError("No forcing data loaded. Use update_forcing() first.")

        # Import DTS interface components
        from .dts import (
            create_suews_config,
            create_suews_state,
            create_suews_site,
            create_suews_forcing,
            create_suews_timer,
            populate_config_from_dict,
            populate_site_from_dict,
            populate_forcing_from_row,
            populate_timer_from_datetime,
            run_supy_dts_tstep,
            bootstrap_state_from_config,
        )
        from . import _state_accessors as acc

        # Create and allocate DTS objects
        config_dts = create_suews_config()
        state_dts = create_suews_state(nlayer=nlayer, ndepth=ndepth)
        site_dts = create_suews_site(nlayer=nlayer)
        forcing_dts = create_suews_forcing()
        timer_dts = create_suews_timer()

        # Populate config from SUEWSConfig
        config_params = {}
        # Map: (DTS attribute name, physics config attribute name)
        # Note: physics config uses names without underscores
        method_attrs = [
            ('rslmethod', 'rslmethod'),
            ('storageheatmethod', 'storageheatmethod'),
            ('netradiationmethod', 'netradiationmethod'),
            ('stabilitymethod', 'stabilitymethod'),
            ('emissionsmethod', 'emissionsmethod'),
            ('ohmincqf', 'ohmincqf'),
        ]
        for dts_attr, config_attr in method_attrs:
            val = getattr(self._config.model.physics, config_attr, None)
            if val is not None:
                if hasattr(val, 'value'):
                    val = val.value
                config_params[dts_attr] = int(val)
        populate_config_from_dict(config_dts, config_params)

        # Override methods that require complex initialization not yet implemented
        # RSL z-profile needs explicit initialization - use simpler methods for now
        config_dts.rslmethod = 1         # Simple RSL (no z-profile)
        config_dts.diagmethod = 0        # No diagnostics (avoids profile interp)
        config_dts.roughlenmommethod = 1 # Simple roughness
        config_dts.stebbsmethod = 0      # No STEBBS (avoids z-profile interp)

        # Populate site from SUEWSConfig
        site_params = {}
        if self._config.sites and len(self._config.sites) > 0:
            site_cfg = self._config.sites[0]
            props = getattr(site_cfg, 'properties', None)
            if props:
                # Extract site parameters from properties
                param_map = [
                    ('lat', 'lat'),
                    ('lng', 'lon'),   # Note: lng in config -> lon in DTS
                    ('alt', 'alt'),
                    ('z', 'z'),       # Measurement height
                    ('surfacearea', 'surfacearea'),
                    ('z0m_in', 'z0m_in'),
                    ('zdm_in', 'zdm_in'),
                ]
                for cfg_attr, dts_attr in param_map:
                    val = getattr(props, cfg_attr, None)
                    if val is not None:
                        if hasattr(val, 'value'):
                            val = val.value
                        site_params[dts_attr] = float(val)

                # Handle timezone (may be string like 'UTC+0:00')
                tz = getattr(props, 'timezone', None)
                if tz:
                    if hasattr(tz, 'value'):
                        tz = tz.value
                    if isinstance(tz, str):
                        # Parse 'UTC+X:00' format
                        if 'UTC' in str(tz):
                            tz_str = str(tz).replace('UTC', '')
                            try:
                                site_params['timezone'] = float(tz_str.split(':')[0])
                            except ValueError:
                                site_params['timezone'] = 0.0
                    else:
                        site_params['timezone'] = float(tz)

            # Extract surface fractions from land_cover under properties
            lc = getattr(props, 'land_cover', None)
            if lc is not None:
                sfr_surf = []
                # Map config land cover types to sfr_surf order (SUEWS 7 surfaces)
                lc_types = [
                    ('paved', 'sfr'),      # 0: Paved
                    ('bldgs', 'sfr'),      # 1: Buildings
                    ('evetr', 'sfr'),      # 2: Evergreen trees
                    ('dectr', 'sfr'),      # 3: Deciduous trees
                    ('grass', 'sfr'),      # 4: Grass
                    ('bsoil', 'sfr'),      # 5: Bare soil
                    ('water', 'sfr'),      # 6: Water
                ]
                for lc_name, attr_name in lc_types:
                    lc_obj = getattr(lc, lc_name, None)
                    if lc_obj is not None:
                        frac = getattr(lc_obj, attr_name, None)
                        if frac is not None:
                            if hasattr(frac, 'value'):
                                frac = frac.value
                            sfr_surf.append(float(frac) if frac is not None else 0.0)
                        else:
                            sfr_surf.append(0.0)
                    else:
                        sfr_surf.append(0.0)
                if len(sfr_surf) == 7:
                    site_params['sfr_surf'] = sfr_surf

        populate_site_from_dict(site_dts, site_params)

        # Extract gsmodel from physics config
        gsmodel = 2  # Default to Ward et al. 2016
        if hasattr(self._config.model.physics, 'gsmodel'):
            gsm_val = self._config.model.physics.gsmodel
            if hasattr(gsm_val, 'value'):
                gsmodel = int(gsm_val.value)
            elif gsm_val is not None:
                gsmodel = int(gsm_val)

        # Extract conductance parameters from site properties
        cond_params = {
            'gsmodel': gsmodel,
            'g_max': 3.5,      # Default values
            'g_k': 200.0,
            'g_q_base': 0.1,
            'g_q_shape': 1.0,
            'g_t': 0.1,
            'g_sm': 0.05,
            'kmax': 1200.0,
            's1': 0.1,
            's2': 200.0,
            'th': 55.0,
            'tl': -10.0,
        }

        # Override with config values if available
        if self._config.sites and len(self._config.sites) > 0:
            site_cfg = self._config.sites[0]
            if hasattr(site_cfg, 'properties') and site_cfg.properties:
                props = site_cfg.properties
                if hasattr(props, 'conductance') and props.conductance:
                    cond = props.conductance
                    for param in ['g_max', 'g_k', 'g_q_base', 'g_q_shape',
                                  'g_t', 'g_sm', 'kmax', 's1', 's2', 'th', 'tl']:
                        val = getattr(cond, param, None)
                        if val is not None:
                            if hasattr(val, 'value'):
                                val = val.value
                            cond_params[param] = float(val)

        # Set conductance parameters using accessor
        acc.set_site_conductance(
            site_dts,
            gsmodel=cond_params['gsmodel'],
            g_max=cond_params['g_max'],
            g_k=cond_params['g_k'],
            g_q_base=cond_params['g_q_base'],
            g_q_shape=cond_params['g_q_shape'],
            g_t=cond_params['g_t'],
            g_sm=cond_params['g_sm'],
            kmax=cond_params['kmax'],
            s1=cond_params['s1'],
            s2=cond_params['s2'],
            th=cond_params['th'],
            tl=cond_params['tl'],
        )

        # Initialize roughness state with default urban values
        # These are needed for RSL calculations
        z_meas = site_params.get('z', 25.0)
        z0m_in = site_params.get('z0m_in', 1.0)
        zdm_in = site_params.get('zdm_in', 7.0)
        acc.set_roughness_state(
            state_dts,
            faibldg_use=0.3,     # Building frontal area index
            faievetree_use=0.1,  # Evergreen tree frontal area
            faidectree_use=0.1,  # Deciduous tree frontal area
            fai=0.5,             # Total frontal area index
            pai=0.3,             # Plan area index
            zh=10.0,             # Mean building height [m]
            z0m=z0m_in,          # Roughness length momentum [m]
            z0v=z0m_in * 0.1,    # Roughness length heat [m]
            zdm=zdm_in,          # Zero-plane displacement [m]
            zzd=z_meas - zdm_in, # zMeas - zdm [m]
        )

        # Set site surface properties (soil params and water limits)
        # Required for hydro state validation during bootstrap
        if self._config.sites and len(self._config.sites) > 0:
            site_cfg = self._config.sites[0]
            if hasattr(site_cfg, 'properties') and site_cfg.properties:
                land_cover = getattr(site_cfg.properties, 'land_cover', None)
                if land_cover:
                    # Build arrays for 7 surfaces
                    # Order: paved=0, bldgs=1, evetr=2, dectr=3, grass=4, bsoil=5, water=6
                    soildepth = np.zeros(7, dtype=np.float64)
                    soilstorecap = np.zeros(7, dtype=np.float64)
                    sathydraulicconduct = np.zeros(7, dtype=np.float64)
                    statelimit = np.zeros(7, dtype=np.float64)
                    wetthresh = np.zeros(7, dtype=np.float64)

                    # Default values for soil properties
                    default_soildepth = 350.0  # mm
                    default_soilstorecap = 150.0  # mm
                    default_statelimit = 10.0  # mm
                    default_wetthresh = 2.0  # mm

                    surface_order = ['paved', 'bldgs', 'evetr', 'dectr', 'grass', 'bsoil', 'water']
                    for i, surf_name in enumerate(surface_order):
                        surf = getattr(land_cover, surf_name, None)
                        if surf:
                            # Extract soil properties
                            if hasattr(surf, 'soildepth') and surf.soildepth is not None:
                                val = surf.soildepth
                                soildepth[i] = val.value if hasattr(val, 'value') else val
                            else:
                                soildepth[i] = default_soildepth

                            if hasattr(surf, 'soilstorecap') and surf.soilstorecap is not None:
                                val = surf.soilstorecap
                                soilstorecap[i] = val.value if hasattr(val, 'value') else val
                            else:
                                soilstorecap[i] = default_soilstorecap

                            if hasattr(surf, 'sathydraulicconduct') and surf.sathydraulicconduct is not None:
                                val = surf.sathydraulicconduct
                                sathydraulicconduct[i] = val.value if hasattr(val, 'value') else val

                            # Extract water limit properties
                            if hasattr(surf, 'statelimit') and surf.statelimit is not None:
                                val = surf.statelimit
                                statelimit[i] = val.value if hasattr(val, 'value') else val
                            else:
                                statelimit[i] = default_statelimit

                            if hasattr(surf, 'wetthresh') and surf.wetthresh is not None:
                                val = surf.wetthresh
                                wetthresh[i] = val.value if hasattr(val, 'value') else val
                            else:
                                wetthresh[i] = default_wetthresh
                        else:
                            # Use defaults for missing surfaces
                            soildepth[i] = default_soildepth
                            soilstorecap[i] = default_soilstorecap
                            statelimit[i] = default_statelimit
                            wetthresh[i] = default_wetthresh

                    # Set site surface properties
                    acc.set_site_soil_params(site_dts, soildepth, soilstorecap, sathydraulicconduct)
                    acc.set_site_water_limits(site_dts, statelimit, wetthresh)

                    # Set surface fractions and emissivity
                    sfr_array = np.zeros(7, dtype=np.float64)
                    emis_array = np.zeros(7, dtype=np.float64)
                    default_emis = 0.95  # Default emissivity

                    for i, surf_name in enumerate(surface_order):
                        surf = getattr(land_cover, surf_name, None)
                        if surf:
                            # Surface fraction
                            if hasattr(surf, 'sfr') and surf.sfr is not None:
                                val = surf.sfr
                                sfr_array[i] = val.value if hasattr(val, 'value') else val
                            # Emissivity
                            if hasattr(surf, 'emis') and surf.emis is not None:
                                val = surf.emis
                                emis_array[i] = val.value if hasattr(val, 'value') else val
                            else:
                                emis_array[i] = default_emis
                        else:
                            emis_array[i] = default_emis

                    acc.set_site_sfr(site_dts, sfr_array)
                    acc.set_site_emis(site_dts, emis_array)

        # Bootstrap state from config initial_states
        if self._config.sites and len(self._config.sites) > 0:
            site_cfg = self._config.sites[0]
            if hasattr(site_cfg, 'initial_states') and site_cfg.initial_states:
                bootstrap_state_from_config(
                    state_dts,
                    site_cfg.initial_states,
                    nlayer=nlayer,
                    ndepth=ndepth,
                )

        # Set non-vegetated surface albedos from land_cover properties
        # Bootstrap only sets vegetation albedos (indices 2,3,4)
        # Non-vegetated albedos (paved=0, bldgs=1, bsoil=5, water=6) come from land_cover
        if self._config.sites and len(self._config.sites) > 0:
            site_cfg = self._config.sites[0]
            if hasattr(site_cfg, 'properties') and site_cfg.properties:
                land_cover = getattr(site_cfg.properties, 'land_cover', None)
                if land_cover:
                    # Get current albedo array (has vegetation values from bootstrap)
                    # f90wrap subroutine requires output array argument
                    alb_array = np.zeros(7, dtype=np.float64)
                    acc.get_phen_state_alb(state_dts, alb_array)

                    # Non-vegetated surface indices and names
                    nonveg_surfaces = [(0, 'paved'), (1, 'bldgs'), (5, 'bsoil'), (6, 'water')]
                    default_albedo = 0.1  # Default for non-vegetated surfaces

                    for idx, surf_name in nonveg_surfaces:
                        surf = getattr(land_cover, surf_name, None)
                        if surf and hasattr(surf, 'alb') and surf.alb is not None:
                            val = surf.alb
                            alb_array[idx] = val.value if hasattr(val, 'value') else val
                        else:
                            alb_array[idx] = default_albedo

                    # Set the complete albedo array
                    acc.set_phen_state_alb(state_dts, alb_array)

                    # Extract OHM coefficients for storage heat flux calculation
                    # Using summer_dry as default; use DyOHM for dynamic selection
                    def get_ohm_coef(surf, coef_name, default=0.0):
                        """Extract OHM coefficient from surface config."""
                        if not surf or not hasattr(surf, 'ohm_coef') or surf.ohm_coef is None:
                            return default
                        ohm_coef = surf.ohm_coef
                        # Use summer_dry as default season
                        season = getattr(ohm_coef, 'summer_dry', None)
                        if season is None:
                            return default
                        val = getattr(season, coef_name, None)
                        if val is None:
                            return default
                        return val.value if hasattr(val, 'value') else val

                    # Extract coefficients for each surface
                    # Surface order: paved, bldgs, evetr, dectr, grass, bsoil, water
                    ohm_params = {}
                    for surf_name in surface_order:
                        surf = getattr(land_cover, surf_name, None)
                        ohm_params[surf_name] = {
                            'a1': get_ohm_coef(surf, 'a1', 0.5),
                            'a2': get_ohm_coef(surf, 'a2', 0.2),
                            'a3': get_ohm_coef(surf, 'a3', -10.0),
                        }

                    # Set OHM coefficients on state
                    acc.set_ohm_state_coef_surf(
                        state_dts,
                        ohm_params['bldgs']['a1'], ohm_params['bldgs']['a2'], ohm_params['bldgs']['a3'],
                        ohm_params['paved']['a1'], ohm_params['paved']['a2'], ohm_params['paved']['a3'],
                        ohm_params['evetr']['a1'], ohm_params['evetr']['a2'], ohm_params['evetr']['a3'],
                        ohm_params['dectr']['a1'], ohm_params['dectr']['a2'], ohm_params['dectr']['a3'],
                        ohm_params['grass']['a1'], ohm_params['grass']['a2'], ohm_params['grass']['a3'],
                        ohm_params['bsoil']['a1'], ohm_params['bsoil']['a2'], ohm_params['bsoil']['a3'],
                        ohm_params['water']['a1'], ohm_params['water']['a2'], ohm_params['water']['a3'],
                    )

        # Get forcing data slice and determine timestep
        forcing_df = self._df_forcing.loc[start_date:end_date]
        if len(forcing_df) < 2:
            raise RuntimeError("Insufficient forcing data for simulation")

        # Infer timestep from forcing data
        time_diff = (forcing_df.index[1] - forcing_df.index[0]).total_seconds()
        tstep_s = int(time_diff)

        # Flatten multi-index columns if present
        if isinstance(forcing_df.columns, pd.MultiIndex):
            forcing_df = forcing_df.copy()
            forcing_df.columns = ['_'.join(str(c) for c in col).strip('_') for col in forcing_df.columns]

        # Run simulation for each timestep
        results = []
        start_time = forcing_df.index[0]

        for i, (idx, row) in enumerate(forcing_df.iterrows()):
            # Calculate time since start
            dt_since_start = int((idx - start_time).total_seconds())

            # Populate timer for this timestep
            populate_timer_from_datetime(
                timer_dts,
                idx,
                tstep_s=tstep_s,
                dt_since_start=dt_since_start
            )

            # Populate forcing for this timestep
            populate_forcing_from_row(forcing_dts, row)

            # Run single timestep calculation
            state_dts, output_dict = run_supy_dts_tstep(
                timer_dts,
                config_dts,
                site_dts,
                state_dts,
                forcing_dts,
                debug=False
            )

            # Add datetime to output
            output_dict['datetime'] = idx

            # Add forcing values for reference
            output_dict['kdown'] = forcing_dts.kdown
            output_dict['temp_c'] = forcing_dts.temp_c

            results.append(output_dict)

        # Convert to DataFrame
        if results:
            df_output = pd.DataFrame(results)
            df_output.set_index('datetime', inplace=True)
            self._df_output = df_output
            self._run_completed = True
            return df_output
        else:
            raise RuntimeError("No results produced from simulation")

    def save(
        self, output_path: Optional[Union[str, Path]] = None, **save_kwargs
    ) -> list[str]:
        """
        Save simulation results according to OutputConfig settings.

        Parameters
        ----------
        output_path : str or Path, optional
            Output directory path. If None, uses current directory.
        save_kwargs : dict
            Additional keyword arguments for customising output.

            **Currently supported kwargs:**

            - **format** : str
                Output format: 'txt' (default) or 'parquet'.
                Note: This overrides config file settings.

            **Not currently supported** (due to internal constraints):

            - freq_s: Controlled by config.model.control.output_file.freq
            - site: Derived from config.sites[0].name
            - save_tstep: Not configurable via OOP interface
            - output_level: Not configurable via OOP interface

            These parameters are determined by the configuration object.
            To change them, update your configuration file or use
            ``update_config()`` before running the simulation.

        Returns
        -------
        list
            List of paths to saved files.

        Raises
        ------
        RuntimeError
            If no simulation results are available.

        Examples
        --------
        Save with default settings from config:

        >>> sim.run()
        >>> paths = sim.save()

        Save to specific directory with custom format:

        >>> sim.run()
        >>> paths = sim.save("output/", format="parquet")
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
        freq_s = DEFAULT_OUTPUT_FREQ_SECONDS
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

    def reset(self) -> "SUEWSSimulation":
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

        """
        from ._env import trv_supy_module
        from ._supy_module import _load_sample_data

        # Load core simulation data (state and forcing)
        df_state_init, df_forcing = _load_sample_data()
        sample_config_path = Path(trv_supy_module / "sample_data" / "sample_config.yml")

        sim = cls()

        # Try to load config for metadata (non-critical)
        # The actual state is set from df_state_init below, so config is optional
        try:
            sim.update_config(sample_config_path)
        except (FileNotFoundError, IOError) as exc:
            # File access issues - warn but continue
            warnings.warn(
                f"Could not load sample configuration file: {exc}\n"
                "Simulation will use data from df_state_init instead.",
                UserWarning,
                stacklevel=2,
            )
        except Exception as exc:
            # Other unexpected errors - warn but continue
            warnings.warn(
                f"Unexpected error loading sample configuration: {exc}\n"
                "Simulation will use data from df_state_init instead.",
                UserWarning,
                stacklevel=2,
            )

        # Set core simulation data (overrides any config-derived state)
        sim._df_state_init = df_state_init
        sim._df_forcing = df_forcing
        return sim

    @classmethod
    def from_state(cls, state: Union[str, Path, pd.DataFrame]):
        """Create SUEWSSimulation from saved state for continuation runs.

        Load a previously saved model state to continue simulation from where
        it left off. Useful for multi-period runs or scenario testing with
        different forcing data.

        Parameters
        ----------
        state : str, Path, or pandas.DataFrame
            State to load for continuation. Can be:

            - Path to CSV file: `df_state.csv` or `df_state_{site}.csv`
            - Path to Parquet file: `{site}_SUEWS_state_final.parquet`
            - DataFrame: `df_state_final` from previous simulation

        Returns
        -------
        SUEWSSimulation
            Simulation instance initialised with loaded state, ready for
            new forcing data and run.

        Warnings
        --------
        If the saved state was created with a different SUEWS version,
        a warning is issued about potential compatibility issues.

        Examples
        --------
        Continue from saved file:

        >>> # Period 1
        >>> sim1 = SUEWSSimulation("config.yaml")
        >>> sim1.update_forcing("forcing_2023.txt")
        >>> sim1.run()
        >>> paths = sim1.save("output/")

        >>> # Period 2 - continue from saved state
        >>> sim2 = SUEWSSimulation.from_state("output/df_state.csv")
        >>> sim2.update_forcing("forcing_2024.txt")
        >>> sim2.run()

        Continue from DataFrame directly:

        >>> # In-memory continuation without saving to file
        >>> sim1 = SUEWSSimulation.from_sample_data()
        >>> sim1.run()
        >>> df_state_final = sim1.state_final
        >>>
        >>> # Continue with new forcing
        >>> sim2 = SUEWSSimulation.from_state(df_state_final)
        >>> sim2.update_forcing("forcing_2024.txt")
        >>> sim2.run()

        Load from Parquet format:

        >>> sim2 = SUEWSSimulation.from_state(
        ...     "output/TestSite_SUEWS_state_final.parquet"
        ... )

        See Also
        --------
        save : Save simulation results and state
        reset : Clear results and reset to initial state
        state_final : Access final state from completed simulation
        """
        from ._version import __version__ as current_version

        # Load state from file or use DataFrame directly
        if isinstance(state, pd.DataFrame):
            df_state_saved = state.copy()
        elif isinstance(state, (str, Path)):
            state_path = Path(state).expanduser().resolve()

            if not state_path.exists():
                raise FileNotFoundError(f"State file not found: {state_path}")

            # Load based on file extension
            if state_path.suffix == ".csv":
                df_state_saved = pd.read_csv(
                    state_path, header=[0, 1], index_col=[0, 1], parse_dates=[0]
                )
            elif state_path.suffix == ".parquet":
                df_state_saved = pd.read_parquet(state_path)
            else:
                raise ValueError(
                    f"Unsupported state file format: {state_path.suffix}. "
                    "Expected .csv or .parquet"
                )
        else:
            raise TypeError(
                f"state must be str, Path, or DataFrame, got {type(state).__name__}"
            )

        # Extract last timestep as initial state for continuation
        idx_names = list(df_state_saved.index.names)
        if "datetime" in idx_names:
            datetime_level = idx_names.index("datetime")
            last_datetime = df_state_saved.index.get_level_values(datetime_level).max()
            if isinstance(df_state_saved.index, pd.MultiIndex):
                df_state_init = df_state_saved.xs(
                    last_datetime, level="datetime"
                ).copy()
            else:
                df_state_init = df_state_saved.loc[[last_datetime]].copy()
        else:
            # Already single-timestep state
            df_state_init = df_state_saved.copy()

        # Check version compatibility
        if ("version", "0") in df_state_saved.columns:
            saved_version = df_state_saved[("version", "0")].iloc[0]
            if saved_version != current_version:
                warnings.warn(
                    f"State was saved with SUEWS version {saved_version}, "
                    f"but current version is {current_version}. "
                    "This may cause compatibility issues.",
                    UserWarning,
                    stacklevel=2,
                )

        # Create simulation instance with loaded state
        sim = cls()
        sim._df_state_init = df_state_init

        return sim

    @classmethod
    def from_output(cls, output: SUEWSOutput) -> "SUEWSSimulation":
        """Create SUEWSSimulation from previous output for continuation runs.

        Convenience method that extracts the final state from a SUEWSOutput
        object and creates a new simulation ready for continuation.

        Parameters
        ----------
        output : SUEWSOutput
            Output object from a previous simulation run.

        Returns
        -------
        SUEWSSimulation
            Simulation instance initialised with final state from output,
            ready for new forcing data and run.

        Examples
        --------
        Seamless continuation from previous run:

        >>> # Run first period
        >>> sim1 = SUEWSSimulation("config.yaml")
        >>> sim1.update_forcing("forcing_2023.txt")
        >>> output1 = sim1.run()

        >>> # Continue from output state
        >>> sim2 = SUEWSSimulation.from_output(output1)
        >>> sim2.update_forcing("forcing_2024.txt")
        >>> output2 = sim2.run()

        See Also
        --------
        from_state : Create from saved state file or DataFrame
        SUEWSOutput.state_final : Final state property for restart
        """
        df_state_init = output.state_final
        sim = cls()
        sim._df_state_init = df_state_init
        # Deep copy config to avoid shared state issues
        if output.config is not None:
            sim._config = copy.deepcopy(output.config)

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
    def config(self) -> Optional[SUEWSConfig]:
        """Access to simulation configuration.

        Returns
        -------
        SUEWSConfig or None
            Complete SUEWS configuration object.
            None if no configuration loaded.

        See Also
        --------
        update_config : Load or update configuration
        state_init : Access initial state derived from configuration
        """
        return self._config

    @property
    def forcing(self) -> Optional[SUEWSForcing]:
        """Access to forcing data as SUEWSForcing object.

        Returns
        -------
        SUEWSForcing or None
            Meteorological forcing data wrapped in OOP interface with
            validation and analysis methods. None if no forcing loaded.

        See Also
        --------
        :ref:`df_forcing_var` : Complete forcing data structure and variable descriptions
        update_forcing : Load forcing data from files or DataFrames

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.forcing.summary()  # Get forcing statistics
        >>> sim.forcing.Tair  # Access air temperature
        >>> sim.forcing.df  # Access raw DataFrame
        """
        if self._df_forcing is None:
            return None
        return SUEWSForcing(self._df_forcing)

    @property
    def results(self) -> Optional[pd.DataFrame]:
        """Access to simulation results DataFrame (raw).

        Returns
        -------
        pandas.DataFrame or None
            Complete simulation output with all variable groups.
            None if simulation hasn't been run yet.

        See Also
        --------
        output : Access results as SUEWSOutput object with analysis methods
        :ref:`df_output_var` : Complete output data structure and variable descriptions
        get_variable : Extract specific variables from output groups
        save : Save results to files
        """
        return self._df_output

    @property
    def output(self) -> Optional[SUEWSOutput]:
        """Access to simulation results as SUEWSOutput object.

        Returns
        -------
        SUEWSOutput or None
            Simulation results wrapped in OOP interface with analysis
            and plotting convenience methods. None if simulation hasn't
            been run yet.

        See Also
        --------
        results : Access raw DataFrame directly
        run : Run simulation and return SUEWSOutput
        :ref:`df_output_var` : Complete output data structure

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> sim.output.QH  # Access sensible heat flux
        >>> sim.output.diurnal_average("QH")  # Get diurnal pattern
        >>> sim.output.energy_balance_closure()  # Analyse energy balance
        """
        if self._df_output is None:
            return None
        return SUEWSOutput(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            config=self._config,
        )

    @property
    def state_init(self) -> Optional[pd.DataFrame]:
        """Initial state DataFrame for simulation.

        Returns
        -------
        pandas.DataFrame or None
            Initial state with surface characteristics and parameters.
            None if no configuration loaded.

        See Also
        --------
        :ref:`df_state_var` : Complete state data structure and variable descriptions
        state_final : Final state after simulation
        from_state : Create simulation from existing state

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.state_init.shape
        (1, 1403)
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

        See Also
        --------
        :ref:`df_state_var` : Complete state data structure and variable descriptions
        state_init : Initial state before simulation
        reset : Clear results and reset to initial state
        from_state : Create new simulation from this final state

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> sim.state_final is not None
        True
        """
        return self._df_state_final
