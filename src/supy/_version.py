# version info for supy

try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files
from ._env import trv_supy_module
import json
import sys
from ._version_scm import __version__, __version_tuple__


import pandas as pd


def show_version(mode="simple", as_json=False):
    """
    Display SUEWS version and system dependency information.

    This function prints version information for SuPy and optionally for system
    dependencies. Output can be displayed as plain text or exported as JSON.

    Parameters
    ----------
    mode : str, optional
        Display mode for version information. Options are:

        - 'simple' (default): Print only the SuPy version number
        - 'full': Print SuPy version and complete system dependency information

    as_json : bool or str, optional
        Controls JSON output format. Options are:

        - False (default): Display as plain text
        - True: Print version information as JSON to stdout
        - str: Path to JSON file where version information will be saved

        When a file path is provided, the function appends SuPy version
        information to pandas system information in the specified file.

    Returns
    -------
    None
        This function prints to stdout or writes to file; it does not return a value.

    Raises
    ------
    ValueError
        If mode is not 'simple' or 'full'.

    Examples
    --------
    Display simple version information:

    >>> import supy as sp
    >>> sp.show_version()
    2025.6.2.dev99

    Display full version and system information:

    >>> sp.show_version(mode="full")
    SUEWS VERSION: 2025.6.2.dev99
    -------------

    SYSTEM DEPENDENCY
    [System dependency information follows...]

    Export version information to JSON:

    >>> sp.show_version(as_json="version_info.json")

    Notes
    -----
    The full mode leverages pandas.show_versions() to display comprehensive
    information about installed dependencies, which is useful for debugging
    and issue reporting.

    When using JSON output with a file path, the function reads existing pandas
    version information and prepends SuPy-specific version data.

    See Also
    --------
    pandas.show_versions : Display pandas version and dependency information
    """
    dict_info_supy = {}
    dict_info_supy["supy"] = __version__

    if as_json:
        if as_json is True:
            print(json.dumps(dict_info_supy, indent=2))
            pd.show_versions(as_json=as_json)
        else:
            from pathlib import Path

            assert isinstance(as_json, str)  # needed for mypy
            pd.show_versions(as_json=as_json)
            path_json = Path(as_json)
            ser_json = pd.read_json(path_json, typ="series", convert_dates=False)
            ser_info_supy = pd.Series(dict_info_supy)
            ser_json = pd.concat([ser_info_supy, ser_json], axis=0)
            ser_json.to_json(path_json, orient="index")
    else:
        if mode == "simple":
            version_text = f"{__version__}"
            print(version_text)
        elif mode == "full":
            version_text = f"SUEWS VERSION: {__version__}"
            print(version_text)
            print("-" * len(version_text) + "\n")
            print("SYSTEM DEPENDENCY")
            pd.show_versions()
        else:
            raise ValueError(f"Invalid mode: {mode}")
