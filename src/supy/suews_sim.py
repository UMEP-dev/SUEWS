"""
SUEWS Simulation Class.

Modern, object-oriented interface for SUEWS urban climate model simulations.
Provides a user-friendly wrapper around the existing SuPy infrastructure.
"""

import copy
from enum import Enum
from pathlib import Path, PurePosixPath
from typing import Any, Optional, Union
import warnings

import pandas as pd
from pydantic import BaseModel

from ._check import check_forcing
from ._env import logger_supy
from ._filename import safe_filename_component
from ._load import merge_forcing_frames
from ._provenance import (
    PROVENANCE_FORMAT_VERSION,
    TIMESTAMP_CONVENTION,
    dataframe_sha256,
    file_identity,
    json_sha256,
    now_utc_iso,
    requested_bound_to_str,
    supy_build_identity,
    timestamp_to_iso,
    write_provenance,
)
from ._run_period import resolve_run_period, slice_forcing_to_period
from ._run_rust import (
    KernelWarningLog,
    _check_rust_available,
    run_suews_rust_chunked,
)

# Import SuPy components directly
from ._supy_module import _save_supy
from .data_model import RefValue
from .data_model.core import SUEWSConfig

# Import new OOP classes
from .suews_checkpoint import SUEWSCheckpoint
from .suews_forcing import SUEWSForcing
from .suews_output import SUEWSOutput
from .util._io import prepare_dataframe_forcing, read_forcing

# Constants
DEFAULT_OUTPUT_FREQ_SECONDS = 3600  # Default hourly output frequency
DEFAULT_FORCING_FILE_PATTERNS = [
    "*.txt",
    "*.csv",
    "*.met",
]  # Valid forcing file extensions


def _validate_n_jobs(n_jobs: int) -> Optional[int]:
    """Return a Rust worker cap for a public ``n_jobs`` value."""
    if isinstance(n_jobs, bool) or not isinstance(n_jobs, int):
        raise ValueError("n_jobs must be an integer: -1, 1, or a positive value")
    if n_jobs == 0 or n_jobs < -1:
        raise ValueError("n_jobs must be -1, 1, or a positive integer")
    return None if n_jobs == -1 else n_jobs


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
            - Dictionary with a full configuration (YAML-shaped, validated
              through ``SUEWSConfig`` exactly like a YAML file)
            - SUEWSConfig object
            - None to create empty simulation
        """
        self._config = None
        self._config_path = None
        self._df_state_init = None
        self._df_forcing = None
        self._df_output = None
        self._df_state_final = None
        self._checkpoint = None
        self._kernel_warnings = KernelWarningLog()
        self._check_continuity = True
        self._run_period = None
        self._run_completed = False
        # Provenance bookkeeping (see ``_provenance.py``); private, best effort.
        self._config_identity = None
        self._forcing_sources = None
        self._run_metadata = None

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
            Configuration source. Pass a path to a YAML file, a
            ``SUEWSConfig`` object, or a dictionary with parameters. A
            dictionary is treated as a full configuration when no config is
            loaded yet, or as a partial update merged onto the existing config.
            Either way the result is re-validated through ``SUEWSConfig``, so
            enum/RefValue coercion and range checks apply exactly as for YAML
            input. Unknown keys raise ``ValueError``; list values replace the
            existing list.
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
            self._config_identity = file_identity(config_path)

            # Convert to initial state DataFrame
            self._df_state_init = self._config.to_df_state()

            # Optionally try to load forcing from config
            if auto_load_forcing:
                self._try_load_forcing_from_config()

        elif isinstance(config, dict):
            if self._config is None:
                # No existing config: treat the dict as a full configuration
                # and build it through the validated SUEWSConfig path
                # Partial dicts are only meaningful as updates to an existing
                # configuration.
                candidate = SUEWSConfig.from_dict(config)
                if not candidate.sites:
                    raise ValueError(
                        "Configuration dict defines no sites. Provide a full "
                        "configuration dict (including 'sites'), or load a "
                        "YAML file / SUEWSConfig first and then apply "
                        "partial updates via update_config()."
                    )
                self._config = candidate

                # Parity with the YAML branch: auto-load forcing declared
                # in the config. Relative paths resolve against the CWD
                # here (no file anchor); failures warn rather than raise.
                if auto_load_forcing:
                    self._try_load_forcing_from_config()
            else:
                # Merge the partial update onto the existing config, then
                # re-validate the whole configuration so enum coercion,
                # RefValue wrapping, and range checks all apply.
                # The merged dict is the user's effective input (original
                # explicitly-set fields plus this update), so from_dict's
                # default raw snapshot of it is the correct record for
                # raw-gated completeness checks.
                merged = self._merge_config_updates(self._config, config)
                self._config = SUEWSConfig.from_dict(
                    merged,
                    yaml_path=str(self._config_path) if self._config_path else None,
                )

            # Regenerate state DataFrame
            self._df_state_init = self._config.to_df_state()

        else:
            # Assume it's a SUEWSConfig object
            self._config = config
            self._config_identity = None
            self._df_state_init = self._config.to_df_state()

        return self

    @staticmethod
    def _merge_config_updates(config: SUEWSConfig, updates: dict) -> dict:
        """Merge a partial update dict onto a validated config's dump.

        Returns a plain dict ready for re-validation through
        ``SUEWSConfig.from_dict``; the configuration is never mutated in
        place, so enum coercion, RefValue wrapping, and range checks always
        apply to the merged result.

        Merge semantics:
        - dicts merge recursively into model-backed nodes; unknown field
          names raise ``ValueError`` instead of being silently dropped
        - ``{"value": ...}`` dicts merge into RefValue leaves; any other
          dict shape on a RefValue leaf replaces it wholesale (alternate
          readable input forms are re-validated, not merged)
        - lists replace wholesale
        - ``sites`` additionally accepts the established mini-language:
          ``{index: patch}``, ``{site_name: patch}``, or a single-site
          shorthand patch
        """
        # Dump only what the user explicitly set (exclude_unset): merging
        # onto a full dump would materialise pydantic defaults as if the
        # user had declared them, so conditional validators gated on
        # model_fields_set or on the raw-input snapshot (e.g. RSL/faibldg,
        # site completeness) would fire false errors on sparse configs.
        # Internal bookkeeping keys are re-stamped by from_dict; carrying
        # them through the merge would nest snapshots inside snapshots.
        base = {
            k: v
            for k, v in config.model_dump(exclude_unset=True, mode="json").items()
            if not k.startswith("_")
        }

        def normalise_legacy_input(model_cls, upd):
            """Run the model's mode='before' validators on an update dict.

            The full from_dict/from_yaml path normalises legacy input
            forms (field renames such as ``stabilitymethod`` ->
            ``stability``, the ``output_file``/``forcing_file`` lifts)
            through each model's before-validators. Partial updates must
            accept the same forms, so the patch is run through the same
            machinery before the unknown-key check.
            """
            decorators = getattr(model_cls, "__pydantic_decorators__", None)
            if decorators is None:
                return upd
            for name, decorator in decorators.model_validators.items():
                if decorator.info.mode != "before":
                    continue
                validator = getattr(model_cls, name, None)
                if validator is None:
                    continue
                result = validator(upd)
                if isinstance(result, dict):
                    upd = result
            return upd

        def merge_node(node_obj, base_dict, upd, path):
            if isinstance(node_obj, BaseModel) and not isinstance(node_obj, RefValue):
                upd = normalise_legacy_input(type(node_obj), dict(upd))
            for key, value in upd.items():
                key_path = f"{path}.{key}" if path else str(key)
                if not path and key == "sites":
                    merge_sites(base_dict, value)
                    continue
                if (
                    isinstance(node_obj, BaseModel)
                    and not isinstance(node_obj, RefValue)
                    and key not in type(node_obj).model_fields
                ):
                    raise ValueError(f"Unknown configuration key: '{key_path}'")
                attr = getattr(node_obj, key, None) if node_obj is not None else None
                if isinstance(attr, RefValue):
                    if isinstance(value, dict) and not (
                        set(value) <= set(RefValue.model_fields)
                    ):
                        # Alternate readable input form; replace wholesale
                        base_dict[key] = copy.deepcopy(value)
                        continue
                    # Scalars become {"value": ...} so the field keeps its
                    # RefValue shape (and any ref metadata) across updates
                    patch = (
                        copy.deepcopy(value)
                        if isinstance(value, dict)
                        else {"value": copy.deepcopy(value)}
                    )
                    base_val = base_dict.get(key)
                    if isinstance(base_val, dict):
                        base_val.update(patch)
                    else:
                        if "value" not in patch:
                            # Metadata-only patch with no dumped base entry
                            # (field at its default): seed the current value
                            # so re-validation does not see a value-less dict
                            seed = attr.model_dump(exclude_none=True, mode="json")
                            seed.update(patch)
                            patch = seed
                        base_dict[key] = patch
                elif isinstance(value, dict) and isinstance(attr, BaseModel):
                    base_val = base_dict.get(key)
                    if not isinstance(base_val, dict):
                        base_dict[key] = base_val = {}
                    merge_node(attr, base_val, value, key_path)
                elif (
                    isinstance(value, dict)
                    and set(value) <= set(RefValue.model_fields)
                    and "value" not in value
                    and attr is not None
                    and not isinstance(attr, (dict, list))
                ):
                    # Metadata-only RefValue-shaped patch on a bare-valued
                    # field: seed the current value before re-validation
                    current = attr.value if isinstance(attr, Enum) else attr
                    base_dict[key] = {
                        "value": copy.deepcopy(current),
                        **copy.deepcopy(value),
                    }
                else:
                    # Scalars, enums, lists, and dict input for non-model
                    # nodes replace wholesale; validation coerces the result
                    base_dict[key] = copy.deepcopy(value)

        def apply_site_patch(index, patch):
            if not isinstance(patch, dict):
                raise ValueError(
                    f"Site update for sites[{index}] must be a dict, "
                    f"got {type(patch).__name__}"
                )
            if not 0 <= index < len(config.sites):
                raise ValueError(
                    f"Site index {index} out of range "
                    f"({len(config.sites)} site(s) configured)"
                )
            merge_node(
                config.sites[index],
                base["sites"][index],
                patch,
                f"sites[{index}]",
            )

        def merge_sites(base_dict, sites_value):
            if isinstance(sites_value, list):
                # Plain list replaces the sites list wholesale
                base_dict["sites"] = copy.deepcopy(sites_value)
                return
            if not isinstance(sites_value, dict) or not sites_value:
                raise ValueError("'sites' update must be a non-empty dict or a list")
            site_names = [getattr(site, "name", None) for site in config.sites]
            for key, value in sites_value.items():
                if isinstance(key, int):
                    apply_site_patch(key, value)
                elif key in site_names:
                    apply_site_patch(site_names.index(key), value)
                elif len(config.sites) == 1:
                    if hasattr(config.sites[0], key):
                        # Single-site shorthand: the whole dict is one patch
                        apply_site_patch(0, sites_value)
                        return
                    # Unmatched site name with a single site: patch that site
                    apply_site_patch(0, value)
                elif hasattr(config.sites[0], key):
                    # Single-site shorthand used with multiple sites is
                    # ambiguous: skip rather than guess which site to patch
                    logger_supy.warning(
                        "Skipping ambiguous sites update key '%s': "
                        "single-site shorthand with multiple sites configured",
                        key,
                    )
                else:
                    # Not a site field, so it can only be a (misspelled or
                    # stale) site name: raise rather than silently no-op
                    raise ValueError(
                        f"Unknown site '{key}' in sites update. "
                        f"Configured sites: {', '.join(map(repr, site_names))}"
                    )

        # Deep-copied so the legacy-input coercions (which restructure
        # nested dicts in place) never mutate the caller's update dict.
        merge_node(config, base, copy.deepcopy(updates), "")
        return base

    def update_forcing(
        self,
        forcing_data: Union[str, Path, list, pd.DataFrame, SUEWSForcing],
        *,
        on_conflict: str = "error",
    ) -> "SUEWSSimulation":
        """
        Update meteorological forcing data.

        Parameters
        ----------
        forcing_data : str, Path, list of paths, pandas.DataFrame, or SUEWSForcing
            Forcing data source:
            - Path to a single forcing file
            - List of paths to forcing files (merged by timestamp)
            - Path to directory containing forcing files (deprecated)
            - DataFrame with forcing data
            - SUEWSForcing object
        on_conflict : {"error", "first", "last"}, optional
            Applies when several files are merged. Overlapping records that
            agree are deduplicated; records that disagree for the same
            timestamp and variable raise
            :class:`~supy.suews_forcing.ForcingConflictError` by default.
            ``"first"`` / ``"last"`` resolve them by list order (earlier /
            later file wins) and log a warning. Ignored for DataFrame and
            SUEWSForcing inputs.

        Returns
        -------
        SUEWSSimulation
            Self, for method chaining.

        Notes
        -----
        Regardless of input type, forcing data is resampled to match the
        model timestep from ``model.control.tstep`` in the configuration
        (defaulting to 300 seconds when no configuration is loaded). In-memory
        inputs (DataFrame or :class:`~supy.SUEWSForcing`) are validated as
        already being in the model-ready canonical form -- canonical column
        names, a :class:`pandas.DatetimeIndex`, and pressure in hPa -- and
        rejected with a clear error otherwise. Unlike file loading, no column
        renaming or unit conversion is applied to in-memory inputs; build them
        with :func:`supy.util.read_forcing` or
        :meth:`supy.SUEWSForcing.from_file` to guarantee that contract.

        Examples
        --------
        >>> sim.update_forcing("forcing_2023.txt")
        >>> sim.update_forcing(["forcing_2023.txt", "forcing_2024.txt"])
        >>> sim.update_forcing(df_forcing)
        >>> sim.update_forcing(SUEWSForcing.from_file("forcing.txt"))
        >>> sim.update_config(cfg).update_forcing(forcing).run()
        """
        # Get tstep from config if available, otherwise default to 300s
        tstep_mod = 300
        if self._config is not None:
            try:
                tstep_val = self._config.model.control.tstep
                tstep_mod = (
                    tstep_val.value if hasattr(tstep_val, "value") else tstep_val
                )
            except AttributeError:
                logger_supy.debug(
                    "Could not extract tstep from config; using default %ds",
                    tstep_mod,
                )

        if isinstance(forcing_data, RefValue):
            forcing_data = forcing_data.value
        if isinstance(forcing_data, SUEWSForcing):
            self._df_forcing = prepare_dataframe_forcing(
                forcing_data.to_dataframe(include_extras=True), tstep_mod=tstep_mod
            )
            self._forcing_sources = self._describe_forcing_sources(
                "in-memory", description=forcing_data._source
            )
        elif isinstance(forcing_data, pd.DataFrame):
            self._df_forcing = prepare_dataframe_forcing(
                forcing_data, tstep_mod=tstep_mod
            )
            self._forcing_sources = self._describe_forcing_sources("in-memory")
        elif isinstance(forcing_data, list):
            # Handle list of files
            self._df_forcing = SUEWSSimulation._load_forcing_from_list(
                forcing_data, tstep_mod=tstep_mod, on_conflict=on_conflict
            )
            self._forcing_sources = self._describe_forcing_sources(
                "files",
                files=[Path(item).expanduser().resolve() for item in forcing_data],
            )
        elif isinstance(forcing_data, (str, Path)):
            forcing_path = Path(forcing_data).expanduser().resolve()
            if not forcing_path.exists():
                raise FileNotFoundError(f"Forcing path not found: {forcing_path}")
            self._df_forcing = SUEWSSimulation._load_forcing_file(
                forcing_path, tstep_mod=tstep_mod, on_conflict=on_conflict
            )
            if forcing_path.is_dir():
                dir_files = []
                for pattern in DEFAULT_FORCING_FILE_PATTERNS:
                    dir_files.extend(sorted(forcing_path.glob(pattern)))
                self._forcing_sources = self._describe_forcing_sources(
                    "directory", files=dir_files, description=forcing_path.name
                )
            else:
                self._forcing_sources = self._describe_forcing_sources(
                    "files", files=[forcing_path]
                )
        else:
            raise ValueError(f"Unsupported forcing data type: {type(forcing_data)}")

        return self

    @staticmethod
    def _describe_forcing_sources(
        kind: str,
        files: Optional[list[Path]] = None,
        description: Optional[str] = None,
    ) -> dict:
        """Build the ``forcing`` block of the provenance sidecar.

        Files are identified by name, size and SHA-256 only; directories are
        not recorded, so the sidecar stays free of machine-specific paths.
        """
        record: dict = {"source": kind}
        if files:
            identities = []
            for item in files:
                try:
                    identities.append(file_identity(item))
                except OSError:
                    identities.append({"name": Path(item).name})
            record["files"] = identities
        if description:
            # A ``SUEWSForcing`` built from a file carries its path as the
            # source description; keep the file name only.
            candidate = Path(description)
            if candidate.is_file():
                record["files"] = [file_identity(candidate)]
                record["source"] = "files"
            else:
                record["description"] = description
        return record

    def _try_load_forcing_from_config(self):
        """Try to load forcing data from configuration if not explicitly provided."""
        if self._config is None:
            return

        try:
            if hasattr(self._config, "model") and hasattr(
                self._config.model, "control"
            ):
                forcing_file_obj = self._config.model.control.forcing.file

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

    def _resolve_output_path(self, path: Union[str, Path]) -> Path:
        """Resolve an output path relative to the loaded config file."""
        path_str = str(path)
        path_output = Path(path_str).expanduser()

        if path_output.is_absolute() or PurePosixPath(path_str).is_absolute():
            return path_output
        if self._config_path is not None:
            return (self._config_path.parent / path_output).resolve()
        return path_output

    @staticmethod
    def _load_forcing_from_list(
        forcing_list: list[Union[str, Path]],
        tstep_mod: int = 300,
        on_conflict: str = "error",
    ) -> pd.DataFrame:
        """Load and merge forcing data from a list of files.

        Overlapping timestamps are reconciled by
        :func:`supy._load.merge_forcing_frames`: identical records are
        deduplicated, conflicting observations raise unless ``on_conflict``
        selects a precedence.
        """
        if not forcing_list:
            raise ValueError("Empty forcing file list provided")

        dfs = []
        sources = []
        for item in forcing_list:
            path = Path(item).expanduser().resolve()

            if not path.exists():
                raise FileNotFoundError(f"Forcing file not found: {path}")

            if path.is_dir():
                raise ValueError(
                    f"Directory '{path}' found in forcing file list. "
                    "Directories are not allowed in lists."
                )

            df = read_forcing(str(path), tstep_mod=tstep_mod, on_conflict=on_conflict)
            dfs.append(df)
            sources.append(str(path))

        return merge_forcing_frames(dfs, sources, on_conflict=on_conflict)

    @staticmethod
    def _load_forcing_file(
        forcing_path: Path, tstep_mod: int = 300, on_conflict: str = "error"
    ) -> pd.DataFrame:
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

            # Merge all files, reconciling overlapping timestamps
            dfs = [
                read_forcing(str(file), tstep_mod=tstep_mod, on_conflict=on_conflict)
                for file in forcing_files
            ]
            return merge_forcing_frames(
                dfs, [str(f) for f in forcing_files], on_conflict=on_conflict
            )
        else:
            return read_forcing(
                str(forcing_path), tstep_mod=tstep_mod, on_conflict=on_conflict
            )

    def run(
        self,
        start_date=None,
        end_date=None,
        chunk_day: int = 3660,
        n_jobs: int = -1,
        clip_to_forcing: bool = False,
        **run_kwargs,
    ) -> SUEWSOutput:
        """
        Run SUEWS simulation using the Rust bridge backend.

        Parameters
        ----------
        start_date : str, date or Timestamp, optional
            Start of the simulation period. Defaults to
            ``model.control.start_time``, then to the first forcing row.
            A date-only value (``"2012-01-01"``) names a calendar day:
            forcing rows are stamped at the end of each interval, so the run
            starts with the first row after that day's midnight. A value with
            a time component is an exact row timestamp and is inclusive.
        end_date : str, date or Timestamp, optional
            End of the simulation period. Defaults to
            ``model.control.end_time``, then to the last forcing row.
            A date-only value (``"2012-12-31"``) runs through the end of that
            day, i.e. up to and including the row stamped at the following
            midnight. A value with a time component is inclusive.
        clip_to_forcing : bool, optional
            The loaded forcing must cover the requested period; otherwise
            ``run()`` raises ``ValueError`` rather than running on whatever
            overlap exists. Pass ``True`` to run on the overlap only; the
            requested and actual periods are then logged as a warning. A
            request with no overlap at all always raises.
        chunk_day : int, optional
            Chunk size in days for splitting long simulations, by default 3660
            (~10 years). Smaller values reduce peak memory at a small overhead
            cost.
        n_jobs : int, optional
            Parallel worker control for multi-grid runs. ``-1`` (default) uses
            Rayon default parallelism, ``1`` forces serial execution, and
            values greater than ``1`` cap Rayon to that many threads.

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
        ValueError
            If the forcing does not cover the requested period and
            ``clip_to_forcing`` is False, or if the request and the forcing
            do not overlap at all.

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> output = sim.run()
        >>> output.QH  # Access sensible heat flux
        >>> output.diurnal_average("QH")  # Get diurnal pattern
        >>> output.to_dataframe()  # Get raw DataFrame
        """
        # Handle deprecated backend kwarg
        backend = run_kwargs.pop("backend", None)
        if backend is not None and backend != "rust":
            raise ValueError(
                f"The '{backend}' backend has been removed. "
                f"Only the 'rust' backend is available. "
                f"Remove the backend parameter or use backend='rust'."
            )
        validate_forcing = run_kwargs.pop("_validate_forcing", True)

        max_workers = _validate_n_jobs(n_jobs)
        serial_mode = n_jobs == 1

        _check_rust_available()

        # Validate inputs
        if self._df_state_init is None:
            raise RuntimeError("No configuration loaded. Use update_config() first.")
        if self._df_forcing is None:
            raise RuntimeError("No forcing data loaded. Use update_forcing() first.")
        if self._config is None:
            # Backward-compatible path: allow runs initialised from df_state
            # (legacy functional workflows and continuation tests).
            try:
                self._config = SUEWSConfig.from_df_state(self._df_state_init)
            except Exception:
                # Some legacy state CSVs carry an extra unnamed index level
                # (e.g. MultiIndex [('grid', None)]). Strip to grid only.
                df_state_for_cfg = self._df_state_init.copy()
                if isinstance(df_state_for_cfg.index, pd.MultiIndex):
                    if "grid" in df_state_for_cfg.index.names:
                        grid_vals = df_state_for_cfg.index.get_level_values("grid")
                    else:
                        grid_vals = df_state_for_cfg.index.get_level_values(0)
                    df_state_for_cfg.index = pd.Index(grid_vals, name="grid")
                self._config = SUEWSConfig.from_df_state(df_state_for_cfg)

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

        # Resolve the requested period under interval-end semantics and
        # reject forcing that does not cover it (gh#1268). This runs
        # regardless of _validate_forcing so no public path can skip it.
        period = resolve_run_period(start_date, end_date, self._df_forcing.index)
        tstep_cfg = self._config.model.control.tstep
        tstep_cfg = int(getattr(tstep_cfg, "value", tstep_cfg))
        df_forcing_slice, self._run_period = slice_forcing_to_period(
            self._df_forcing,
            period,
            clip_to_forcing=clip_to_forcing,
            tstep=tstep_cfg,
        )
        run_metadata = self._start_run_metadata(
            start_date, end_date, df_forcing_slice, n_jobs, chunk_day
        )

        # A checkpoint continuation must pick up exactly one timestep after
        # the checkpointed period; this runs regardless of _validate_forcing.
        if self._checkpoint is not None:
            self._validate_continuation_forcing(df_forcing_slice)

        if validate_forcing:
            # Validate forcing data, including physics-specific forcing requirements
            # (e.g. laimethod=0 requires populated effective observed-LAI sources).
            physics_dict = None
            if self._config is not None and hasattr(self._config, "model"):
                physics = getattr(self._config.model, "physics", None)
                if physics is not None and hasattr(physics, "model_dump"):
                    physics_dict = physics.model_dump(mode="python")
                # Cross-check physics path against forcing columns.
                # Helper is silent on success; raises ValueError on mismatch.
                if physics is not None:
                    from .data_model.core.forcing_validation import (
                        validate_forcing_columns_against_physics,
                    )

                    validate_forcing_columns_against_physics(df_forcing_slice, physics)
            list_issues = check_forcing(df_forcing_slice, physics=physics_dict)
            if isinstance(list_issues, list) and len(list_issues) > 0:
                issues_summary = (
                    list_issues[:3] if len(list_issues) > 3 else list_issues
                )
                suffix = (
                    f" (and {len(list_issues) - 3} more)"
                    if len(list_issues) > 3
                    else ""
                )
                raise ValueError(f"Invalid forcing data: {issues_summary}{suffix}")
        else:
            # The retired runner always enforced observed-LAI completeness,
            # even when its general ``check_input`` switch was false.
            lai_method = getattr(self._config.model.physics, "leaf_area_index", None)
            if lai_method is None:
                lai_method = getattr(self._config.model.physics, "laimethod", None)
            if hasattr(lai_method, "value"):
                lai_method = lai_method.value
            if lai_method is not None and int(lai_method) == 0:
                from ._check import _check_observed_lai_nonneg

                lai_issues = []
                if _check_observed_lai_nonneg(df_forcing_slice, lai_issues):
                    raise RuntimeError(lai_issues[0])

        # Preserve observed-soil-moisture preprocessing from the retired
        # DataFrame runner on the single canonical execution path.
        soil_moisture_deficit = getattr(
            self._config.model.physics,
            "soil_moisture_deficit",
            None,
        )
        if soil_moisture_deficit is None:
            soil_moisture_deficit = getattr(
                self._config.model.physics,
                "smdmethod",
                None,
            )
        if hasattr(soil_moisture_deficit, "value"):
            soil_moisture_deficit = soil_moisture_deficit.value
        if soil_moisture_deficit is not None and int(soil_moisture_deficit) > 0:
            from .util._forcing import convert_observed_soil_moisture

            df_forcing_slice = convert_observed_soil_moisture(
                df_forcing_slice.copy(),
                self._df_state_init,
            )

        # Snapshot the identity of the inputs as they are handed to the
        # kernel, so a later update_config/update_forcing without a rerun
        # cannot relabel this run's output at save time.
        run_metadata["inputs"] = self._snapshot_input_identity(df_forcing_slice)

        # Run simulation via Rust bridge
        initial_state_json_by_grid = (
            self._checkpoint.grid_states if self._checkpoint is not None else None
        )
        df_output, dict_state_json, kernel_warnings = run_suews_rust_chunked(
            config=self._config,
            df_forcing=df_forcing_slice,
            chunk_day=chunk_day,
            serial_mode=serial_mode,
            max_workers=max_workers,
            initial_state_json_by_grid=initial_state_json_by_grid,
        )
        self._df_output = df_output
        # Surface non-fatal kernel warnings (physics fallbacks) that were
        # previously discarded inside the Fortran layer (GH#1737).
        self._kernel_warnings = kernel_warnings
        kernel_warnings.log(logger_supy)
        self._checkpoint = (
            SUEWSCheckpoint.from_grid_states(
                dict_state_json,
                last_timestamp=df_forcing_slice.index.max(),
            )
            if dict_state_json
            else None
        )
        # The checkpoint now belongs to this run, so a further run() on the
        # same instance is an implicit continuation and is always checked.
        self._check_continuity = True

        # Keep a legacy DFState-shaped final state for compatibility only.
        from ._version import __version__

        df_state_final = self._df_state_init.copy()
        df_state_final[("version", "0")] = __version__
        self._df_state_final = df_state_final

        run_metadata["ended_at"] = now_utc_iso()
        self._run_metadata = run_metadata
        self._run_completed = True

        # Wrap results in SUEWSOutput
        return SUEWSOutput(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            config=self._config,
            checkpoint=self._checkpoint,
            kernel_warnings=self._kernel_warnings.to_frame(),
        )

    def save(
        self, output_path: Optional[Union[str, Path]] = None, **save_kwargs
    ) -> list[str]:
        """
        Save simulation results according to OutputControl settings.

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
            - **command** : str
                Command line that produced the run (recorded in the
                ``provenance.json`` sidecar; set by ``suews run``).

            **Not currently supported** (due to internal constraints):

            - freq_s: Controlled by config.model.control.output.freq
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

        if self._df_state_final is None:
            raise NotImplementedError(
                "save() is not yet supported for the Rust backend. "
                "Access results directly via sim.output.df_output"
            )

        # Set default path with priority: parameter > config > current directory
        if output_path is None:
            # Check if dir is specified in config
            config_dir = None
            try:
                output_control = self._config.model.control.output
                if output_control.dir:
                    config_dir = output_control.dir
            except AttributeError:
                pass

            output_path = (
                self._resolve_output_path(config_dir) if config_dir else Path(".")
            )
        else:
            output_path = Path(output_path)

        # Extract parameters from config
        output_format = None
        output_config = None
        forcing_timestamp_reference = "local_standard_time"
        freq_s = DEFAULT_OUTPUT_FREQ_SECONDS
        site = ""

        if self._config:
            # Get output frequency from OutputControl if available
            if (
                hasattr(self._config, "model")
                and hasattr(self._config.model, "control")
                and hasattr(self._config.model.control, "output")
            ):
                output_config = self._config.model.control.output
                if hasattr(output_config, "freq") and output_config.freq is not None:
                    freq_s = output_config.freq
                # Removed for now - can't update from YAML (TODO)
                # if hasattr(output_config, 'format') and output_config.format is not None:
                #     output_format = output_config.format
                control = self._config.model.control
                if hasattr(control, "forcing"):
                    forcing_timestamp_reference = control.forcing.timestamp_reference

            # Get site name from first site
            if hasattr(self._config, "sites") and len(self._config.sites) > 0:
                site = self._config.sites[0].name
        site_filename = safe_filename_component(site)
        if "format" in save_kwargs:  # TODO: When yaml format working, make elif
            output_format = save_kwargs["format"]

        # Use internal save helper for all formats
        list_path_save = _save_supy(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            freq_s=int(freq_s),
            site=site,
            path_dir_save=str(output_path),
            # **save_kwargs # The shared save backend expects explicit arguments
            output_config=output_config,
            output_format=output_format,
            save_state=False,
            forcing_timestamp_reference=forcing_timestamp_reference,
        )

        if self._checkpoint is not None:
            checkpoint_name = (
                f"{site_filename}_SUEWS_checkpoint.json"
                if site_filename
                else "SUEWS_checkpoint.json"
            )
            checkpoint_path = self._checkpoint.to_file(output_path / checkpoint_name)
            list_path_save.append(checkpoint_path)

        # Provenance sidecar: written last so it can list what was saved.
        # Report the format that was actually written: the YAML
        # ``output.format`` applies when no ``format`` kwarg is given.
        written_format = (
            "parquet"
            if any(Path(p).suffix == ".parquet" for p in list_path_save)
            else "txt"
        )
        payload = self._build_provenance(
            list_path_save,
            output_format=written_format,
            freq_s=int(freq_s),
            forcing_timestamp_reference=forcing_timestamp_reference,
            output_config=output_config,
            command=save_kwargs.get("command"),
        )
        list_path_save.append(write_provenance(output_path, payload))

        return list_path_save

    def _start_run_metadata(
        self, start_date, end_date, df_forcing_slice: pd.DataFrame, n_jobs, chunk_day
    ) -> dict:
        """Record the requested and actual period plus run options.

        ``requested`` is what ``run()`` was asked for (explicit arguments or
        the configuration's ``start_time`` / ``end_time``); ``actual`` is the
        forcing slice that was simulated. Both are kept so a reader can see
        when a request was clipped to the available forcing.

        When the period-coverage check has already published its result as
        ``self._run_period`` (keys ``requested_start``, ``requested_end``,
        ``requested_start_raw``, ``requested_end_raw``, ``actual_start``,
        ``actual_end``, ``clipped``, ``policy``), that record is the single
        source of truth and is consumed as is; this method never writes it.
        """
        index = df_forcing_slice.index
        tstep_s = None
        if self._config is not None:
            try:
                tstep_val = self._config.model.control.tstep
                tstep_s = int(getattr(tstep_val, "value", tstep_val))
            except (AttributeError, TypeError, ValueError):
                tstep_s = None

        run_period = getattr(self, "_run_period", None)
        if isinstance(run_period, dict) and run_period:
            requested = {
                "start": requested_bound_to_str(run_period.get("requested_start")),
                "end": requested_bound_to_str(run_period.get("requested_end")),
                "start_raw": requested_bound_to_str(
                    run_period.get("requested_start_raw")
                ),
                "end_raw": requested_bound_to_str(run_period.get("requested_end_raw")),
            }
            actual_start = run_period.get("actual_start")
            actual_end = run_period.get("actual_end")
            clipped = run_period.get("clipped")
            policy = run_period.get("policy")
        else:
            requested = {
                "start": requested_bound_to_str(start_date),
                "end": requested_bound_to_str(end_date),
            }
            actual_start = index.min() if len(index) else None
            actual_end = index.max() if len(index) else None
            clipped = None
            policy = None

        return {
            "started_at": now_utc_iso(),
            "ended_at": None,
            "requested": requested,
            "actual": {
                "start": timestamp_to_iso(actual_start),
                "end": timestamp_to_iso(actual_end),
                "n_timesteps": int(len(index)),
            },
            "clipped": clipped,
            "policy": policy,
            "tstep_s": tstep_s,
            "n_jobs": int(n_jobs),
            "chunk_day": int(chunk_day),
            "continued_from_checkpoint": self._checkpoint is not None,
        }

    def _snapshot_input_identity(self, df_forcing_slice: pd.DataFrame) -> dict:
        """Describe the configuration and forcing exactly as run.

        Source files are identified by name, size and SHA-256 (captured when
        they were loaded); ``effective_sha256`` hashes the in-memory
        configuration and the model-ready forcing slice that the kernel
        received, which is what a later reader should compare against.
        """
        config_block: dict = {"source": "in-memory"}
        if self._config_identity is not None:
            config_block = {"source": "file", **self._config_identity}
        if self._config is not None:
            schema_version = getattr(self._config, "schema_version", None)
            config_block["schema_version"] = (
                str(schema_version) if schema_version is not None else None
            )
            config_block["sites"] = [
                str(getattr(site, "name", ""))
                for site in getattr(self._config, "sites", [])
            ]
            try:
                config_block["effective_sha256"] = json_sha256(
                    self._config.model_dump(mode="json")
                )
            except Exception:  # pragma: no cover - never let hashing fail a run
                config_block["effective_sha256"] = None
        if self._df_state_init is not None:
            index = self._df_state_init.index
            grid_level = (
                index.get_level_values("grid")
                if isinstance(index, pd.MultiIndex) and "grid" in index.names
                else index
            )
            config_block["grids"] = [int(g) for g in grid_level]

        forcing_block = dict(self._forcing_sources or {"source": "unknown"})
        forcing_block["effective_sha256"] = dataframe_sha256(df_forcing_slice)
        forcing_block["effective_n_rows"] = int(len(df_forcing_slice))
        return {"config": config_block, "forcing": forcing_block}

    def _build_provenance(
        self,
        list_path_save: list,
        *,
        output_format: str,
        freq_s: int,
        forcing_timestamp_reference,
        output_config,
        command: Optional[str],
    ) -> dict:
        """Assemble the ``provenance.json`` payload for :meth:`save`.

        Input identities come from the snapshot taken by :meth:`run`, never
        from the simulation's current attributes.
        """
        run_metadata = dict(self._run_metadata or {})
        inputs = run_metadata.get("inputs") or {}
        config_block = inputs.get("config") or {"source": "unknown"}
        forcing_block = inputs.get("forcing") or {"source": "unknown"}

        output_reference = "follow"
        if output_config is not None:
            ref = getattr(output_config, "timestamp_reference", None)
            if ref is not None:
                output_reference = str(getattr(ref, "value", ref))
        forcing_reference = str(
            getattr(forcing_timestamp_reference, "value", forcing_timestamp_reference)
        )

        checkpoint_names = [
            Path(p).name
            for p in list_path_save
            if Path(p).name.endswith("_checkpoint.json")
            or Path(p).name == "SUEWS_checkpoint.json"
        ]
        output_names = [
            Path(p).name for p in list_path_save if Path(p).name not in checkpoint_names
        ]

        period = {
            "requested": run_metadata.get("requested"),
            "actual": run_metadata.get("actual"),
            "clipped": run_metadata.get("clipped"),
            "policy": run_metadata.get("policy"),
            "tstep_s": run_metadata.get("tstep_s"),
        }
        run_block = {
            "interface": "cli" if command else "python",
            "command": command,
            "started_at": run_metadata.get("started_at"),
            "ended_at": run_metadata.get("ended_at"),
            "n_jobs": run_metadata.get("n_jobs"),
            "chunk_day": run_metadata.get("chunk_day"),
            "continued_from_checkpoint": run_metadata.get(
                "continued_from_checkpoint", False
            ),
        }
        return {
            "format_version": PROVENANCE_FORMAT_VERSION,
            "generator": "supy.SUEWSSimulation.save",
            "created_at": now_utc_iso(),
            **supy_build_identity(),
            "config": config_block,
            "forcing": forcing_block,
            "period": period,
            "timestamps": {
                "convention": TIMESTAMP_CONVENTION,
                "forcing_reference": forcing_reference,
                "output_reference": output_reference,
            },
            "run": run_block,
            "output": {
                "format": output_format,
                "frequency_s": freq_s,
                "files": output_names,
                "checkpoint": checkpoint_names[0] if checkpoint_names else None,
            },
        }

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
        self._checkpoint = None
        self._kernel_warnings = KernelWarningLog()
        self._check_continuity = True
        self._run_period = None
        self._run_completed = False
        self._run_metadata = None
        return self

    def _validate_continuation_forcing(self, df_forcing: pd.DataFrame) -> None:
        """Require continuation forcing to start one timestep after the checkpoint.

        Forcing timestamps mark the end of each interval, so the first row of
        a continuation must sit exactly one model timestep after the
        checkpoint's ``last_timestamp``. Overlaps and gaps are rejected with
        distinct messages; ``check_continuity=False`` on ``from_checkpoint``
        or ``continue_from`` skips the check for the next run only.
        """
        if not self._check_continuity or df_forcing.empty:
            return
        if not isinstance(df_forcing.index, pd.DatetimeIndex):
            # check_forcing reports a non-datetime index with its own message.
            return

        checkpoint = self._checkpoint
        if checkpoint.last_timestamp is None:
            raise ValueError(
                "Checkpoint has no last_timestamp metadata, so the continuation "
                "forcing cannot be checked for continuity. Use a checkpoint "
                "produced by SUEWSSimulation.run() or save(), or pass "
                "check_continuity=False to from_checkpoint()/continue_from() "
                "to skip the check."
            )

        tstep_val = self._config.model.control.tstep
        tstep = int(tstep_val.value if hasattr(tstep_val, "value") else tstep_val)
        last = pd.Timestamp(checkpoint.last_timestamp)
        first = df_forcing.index[0]
        if (last.tzinfo is None) != (first.tzinfo is None):
            raise ValueError(
                f"Checkpoint last_timestamp {last} and continuation forcing "
                f"start {first} differ in timezone awareness; supply forcing "
                "on the same clock as the checkpointed run."
            )

        expected = last + pd.Timedelta(seconds=tstep)
        if first == expected:
            return
        remedy = (
            f"Expected the first forcing timestamp to be {expected} "
            f"(checkpoint last_timestamp {last} + tstep {tstep} s)."
        )
        if first <= last:
            raise ValueError(
                f"Continuation forcing starts at {first}, which overlaps the "
                f"checkpointed period ending at {last}. {remedy} Call reset() "
                "to run from the initial state again, or pass "
                "check_continuity=False to from_checkpoint()/continue_from() "
                "to deliberately re-run a period from the checkpointed state."
            )
        raise ValueError(
            f"Continuation forcing starts at {first}, leaving a gap after the "
            f"checkpointed period ending at {last}. {remedy} Supply forcing "
            f"that starts at {expected}, or pass check_continuity=False to "
            "from_checkpoint()/continue_from() to skip the check."
        )

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

        sample_config_path = trv_supy_module / "sample_data" / "sample_config.yml"
        return cls(sample_config_path)

    @staticmethod
    def _coerce_checkpoint(
        checkpoint: Union[str, Path, SUEWSCheckpoint],
    ) -> SUEWSCheckpoint:
        if isinstance(checkpoint, SUEWSCheckpoint):
            return checkpoint
        if isinstance(checkpoint, (str, Path)):
            return SUEWSCheckpoint.from_file(checkpoint)
        raise TypeError(
            "checkpoint must be a SUEWSCheckpoint, str, or Path, "
            f"got {type(checkpoint).__name__}"
        )

    @classmethod
    def from_checkpoint(
        cls,
        config: Union[str, Path, dict, SUEWSConfig],
        checkpoint: Union[str, Path, SUEWSCheckpoint],
        check_continuity: bool = True,
    ) -> "SUEWSSimulation":
        """Create a continuation simulation from YAML config and checkpoint.

        Parameters
        ----------
        config : str, Path, dict or SUEWSConfig
            Configuration used for the checkpointed run.
        checkpoint : str, Path or SUEWSCheckpoint
            Typed checkpoint, or the path of a checkpoint JSON file.
        check_continuity : bool, optional
            When ``True`` (default) the next ``run()`` requires the forcing to
            start exactly one model timestep after the checkpoint's
            ``last_timestamp`` and rejects overlaps and gaps. Set ``False`` to
            deliberately re-run a period from the checkpointed state, for
            example when cycling the same forcing for spin-up.
        """
        if config is None:
            raise ValueError(
                "Checkpoint continuation requires a YAML/SUEWSConfig "
                "configuration as well as the checkpoint."
            )

        sim = cls()
        if not isinstance(config, (str, Path, dict)):
            config = copy.deepcopy(config)
        sim.update_config(config, auto_load_forcing=False)
        return sim.continue_from(checkpoint, check_continuity=check_continuity)

    def continue_from(
        self,
        checkpoint: Union[str, Path, SUEWSCheckpoint],
        check_continuity: bool = True,
    ) -> "SUEWSSimulation":
        """Use a typed checkpoint as the initial runtime state.

        ``check_continuity`` has the same meaning as in ``from_checkpoint``:
        it governs whether the next ``run()`` requires the forcing to start
        one model timestep after the checkpoint's ``last_timestamp``.
        """
        if self._config is None:
            raise RuntimeError(
                "Checkpoint continuation requires a loaded configuration. "
                "Use SUEWSSimulation.from_checkpoint(config, checkpoint) or "
                "call update_config() before continue_from()."
            )

        checkpoint_value = self._coerce_checkpoint(checkpoint)
        checkpoint_value.validate_for_continuation()
        self._checkpoint = checkpoint_value
        self._check_continuity = bool(check_continuity)
        self._df_output = None
        self._df_state_final = None
        self._kernel_warnings = KernelWarningLog()
        self._run_completed = False
        return self

    @classmethod
    def from_state(cls, state: Union[str, Path, pd.DataFrame]):
        """Create SUEWSSimulation from legacy DFState.

        This compatibility adapter reads older DFState CSV/parquet/DataFrame
        artifacts. New continuation workflows should use
        ``from_checkpoint(config, checkpoint)``.

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
        Legacy import from saved file:

        >>> # Period 1
        >>> sim1 = SUEWSSimulation("config.yaml")
        >>> sim1.update_forcing("forcing_2023.txt")
        >>> sim1.run()
        >>> paths = sim1.save("output/")

        >>> # Period 2 - legacy DFState continuation
        >>> sim2 = SUEWSSimulation.from_state("output/df_state.csv")
        >>> sim2.update_forcing("forcing_2024.txt")
        >>> sim2.run()

        Legacy import from DataFrame directly:

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
        from_checkpoint : Create continuation from YAML config and checkpoint
        save : Save simulation results and checkpoint
        reset : Clear results and reset to initial state
        checkpoint : Access typed checkpoint from completed simulation
        """
        warnings.warn(
            "SUEWSSimulation.from_state() is a legacy DFState compatibility "
            "adapter. Use SUEWSSimulation.from_checkpoint(config, checkpoint) "
            "for restart/continuation runs.",
            DeprecationWarning,
            stacklevel=2,
        )

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
                    state_path,
                    header=[0, 1],
                    index_col=0,
                    parse_dates=True,
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
            Simulation instance initialised with checkpoint from output,
            ready for new forcing data and run.

        Examples
        --------
        Seamless continuation from previous run:

        >>> # Run first period
        >>> sim1 = SUEWSSimulation("config.yaml")
        >>> sim1.update_forcing("forcing_2023.txt")
        >>> output1 = sim1.run()

        >>> # Continue from output checkpoint
        >>> sim2 = SUEWSSimulation.from_output(output1)
        >>> sim2.update_forcing("forcing_2024.txt")
        >>> output2 = sim2.run()

        See Also
        --------
        from_checkpoint : Create from YAML config and checkpoint
        SUEWSOutput.checkpoint : Typed checkpoint property for restart
        """
        if output.checkpoint is not None:
            if output.config is None:
                raise RuntimeError(
                    "SUEWSOutput has a checkpoint but no configuration. "
                    "Use SUEWSSimulation.from_checkpoint(config, checkpoint)."
                )
            return cls.from_checkpoint(
                copy.deepcopy(output.config),
                output.checkpoint,
            )

        warnings.warn(
            "SUEWSSimulation.from_output() is falling back to legacy DFState. "
            "Prefer SUEWSSimulation.from_checkpoint(config, output.checkpoint).",
            DeprecationWarning,
            stacklevel=2,
        )
        return cls.from_state(output.state_final)

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
        df_main, extras = SUEWSForcing._split_per_landcover_columns(self._df_forcing)
        forcing = SUEWSForcing(df_main)
        forcing._extras = extras
        return forcing

    @property
    def results(self) -> Optional[pd.DataFrame]:
        """Access to simulation results DataFrame (raw).

        .. deprecated:: 2025.1
            Use ``output = sim.run()`` to get a ``SUEWSOutput`` object,
            then ``output.df`` for the raw DataFrame if needed.

        Returns
        -------
        pandas.DataFrame or None
            Complete simulation output with all variable groups.
            None if simulation hasn't been run yet.

        See Also
        --------
        output : Access results as SUEWSOutput object with analysis methods
        :ref:`output_variable_reference` : Complete output variable reference
        get_variable : Extract specific variables from output groups
        save : Save results to files
        """
        warnings.warn(
            "sim.results is deprecated and will be removed in version 2026.1. "
            "Use 'output = sim.run()' to get a SUEWSOutput "
            "object, then 'output.df' for the raw DataFrame if needed.",
            DeprecationWarning,
            stacklevel=2,
        )
        return self._df_output

    @property
    def output(self) -> Optional[SUEWSOutput]:
        """Access to simulation results as SUEWSOutput object.

        .. note::
            Preferred pattern is ``output = sim.run()`` which returns the
            same ``SUEWSOutput`` object. This property is provided for
            convenience when re-accessing results after simulation.

        Returns
        -------
        SUEWSOutput or None
            Simulation results wrapped in OOP interface with analysis
            and plotting convenience methods. None if simulation hasn't
            been run yet.

        See Also
        --------
        run : Run simulation and return SUEWSOutput (preferred)
        :ref:`output_variable_reference` : Complete output variable reference

        Examples
        --------
        Preferred pattern - capture return value:

        >>> sim = SUEWSSimulation.from_sample_data()
        >>> output = sim.run()  # Capture output
        >>> output.QH  # Access sensible heat flux

        Alternative - use property after run:

        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> sim.output.QH  # Re-access via property
        """
        if self._df_output is None:
            return None
        return SUEWSOutput(
            df_output=self._df_output,
            df_state_final=self._df_state_final,
            config=self._config,
            checkpoint=self._checkpoint,
            kernel_warnings=self._kernel_warnings.to_frame(),
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
        checkpoint : Typed checkpoint after simulation
        from_checkpoint : Create simulation from checkpoint

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.state_init.shape
        (1, 1403)
        """
        return self._df_state_init

    @property
    def state_final(self) -> Optional[pd.DataFrame]:
        """Legacy final state DataFrame after simulation.

        Prefer ``checkpoint`` for restart/continuation workflows.

        Returns
        -------
        pandas.DataFrame or None
            Legacy final state after simulation run.
            None if simulation hasn't been run yet.

        See Also
        --------
        :ref:`df_state_var` : Complete state data structure and variable descriptions
        state_init : Initial state before simulation
        reset : Clear results and reset to initial state
        checkpoint : Typed restart artifact
        from_checkpoint : Create new simulation from checkpoint

        Examples
        --------
        >>> sim = SUEWSSimulation.from_sample_data()
        >>> sim.run()
        >>> sim.state_final is not None
        True
        """
        return self._df_state_final

    @property
    def checkpoint(self) -> Optional[SUEWSCheckpoint]:
        """Typed checkpoint produced by the most recent run."""
        return self._checkpoint

    @property
    def kernel_warnings(self) -> pd.DataFrame:
        """Non-fatal warnings raised by the Fortran kernel in the last run.

        One row per recorded warning with columns ``grid``, ``datetime``,
        ``location`` and ``message``. Empty when the run was clean. These
        mark physics fallbacks (for example SPARTACUS flat-tile substitution
        or EHC leaving QS at zero) that used to be silent (GH#1737).
        """
        return self._kernel_warnings.to_frame()

    @property
    def state_checkpoint(self) -> Optional[SUEWSCheckpoint]:
        """Alias for ``checkpoint``."""
        return self._checkpoint
