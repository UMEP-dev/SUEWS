"""Version helpers for the SUEWS MCP package."""

from __future__ import annotations

try:
    from ._version_scm import __version__, __version_tuple__, version, version_tuple
except ModuleNotFoundError:
    try:
        import supy

        __version__ = supy.__version__
    except Exception:
        __version__ = "0+unknown"

    version = __version__
    __version_tuple__ = version_tuple = ()
