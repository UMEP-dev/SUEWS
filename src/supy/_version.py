# version info for supy

try:
    from importlib.resources import files
except ImportError:
    # backport for python < 3.9
    from importlib_resources import files
# from .supy_driver import __version__ as sd_ver
from ._env import trv_supy_module
import json
import sys
from ._version_scm import __version__, __version_tuple__


import pandas as pd

# ser_ver = pd.read_json(
#     path_supy_module / "supy_version.json", typ="series", convert_dates=False
# )
# __version__ = "-".join(
#     list(
#         filter(
#             None,
#             [
#                 ser_ver.version,
#                 ser_ver.iter,
#                 ser_ver.git_commit,
#             ],
#         )
#     )
# )
# __version_driver__ = sd_ver


def show_version(mode="simple", as_json=False):
    """print `supy` and `supy_driver` version information."""
    dict_info_supy = {}
    dict_info_supy["supy"] = __version__
    # dict_info_supy["supy_driver"] = __version_driver__
    # dict_info['system'] = {'platform':sys.platform,'python_version':sys.version}

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
        print(f"SuPy version: {__version__}")
        print("-------------")
        # print(f"supy_driver: {__version_driver__}")
        if mode == "full":
            print("\n=================")
            print("SYSTEM DEPENDENCY")
            pd.show_versions()
