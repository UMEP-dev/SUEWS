# Lazy imports for fast CLI startup
# Only import specific CLI tools when actually accessed

import importlib as _importlib

__all__ = [
    "SUEWS",
    "compare_runs_cmd",
    "convert_table_cmd",
    "diagnose_run_cmd",
    "schema_cli_main",
    "summarise_output_cmd",
    "validate_config_main",
]

_lazy_cache = {}

# Submodules exposed through ``from supy.cmd import <name>``. Listed
# explicitly so static checks (e.g. test_api_surface.py's hasattr probe)
# resolve them without requiring the consumer to first import the
# fully-qualified module path.
_LAZY_SUBMODULES = frozenset({"rust_bridge", "suews_cli"})


def __getattr__(name):
    """Lazy attribute loader for CLI commands."""
    if name in _lazy_cache:
        return _lazy_cache[name]

    if name == "SUEWS":
        from .SUEWS import SUEWS

        _lazy_cache[name] = SUEWS
        return _lazy_cache[name]

    if name == "convert_table_cmd":
        from .table_converter import convert_table_cmd

        _lazy_cache[name] = convert_table_cmd
        return _lazy_cache[name]

    if name == "validate_config_main":
        from .validate_config import main as validate_config_main

        _lazy_cache[name] = validate_config_main
        return _lazy_cache[name]

    if name == "schema_cli_main":
        try:
            from .schema_cli import main as schema_cli_main

            _lazy_cache[name] = schema_cli_main
            return _lazy_cache[name]
        except Exception:
            _lazy_cache[name] = None
            return None

    if name == "summarise_output_cmd":
        from .summarise_output import summarise_output_cmd

        _lazy_cache[name] = summarise_output_cmd
        return _lazy_cache[name]

    if name == "compare_runs_cmd":
        from .compare_runs import compare_runs_cmd

        _lazy_cache[name] = compare_runs_cmd
        return _lazy_cache[name]

    if name == "diagnose_run_cmd":
        from .diagnose_run import diagnose_run_cmd

        _lazy_cache[name] = diagnose_run_cmd
        return _lazy_cache[name]

    if name in _LAZY_SUBMODULES:
        module = _importlib.import_module(f"{__name__}.{name}")
        _lazy_cache[name] = module
        return module

    raise AttributeError(f"module 'supy.cmd' has no attribute {name!r}")


# to_yaml is used internally by table_converter but not exposed as a command
