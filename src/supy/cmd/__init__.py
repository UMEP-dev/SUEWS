# ruff: noqa: F822, RUF067
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

_LAZY_ATTRS = {
    "SUEWS": ("SUEWS", "SUEWS"),
    "compare_runs_cmd": ("compare_runs", "compare_runs_cmd"),
    "convert_table_cmd": ("table_converter", "convert_table_cmd"),
    "diagnose_run_cmd": ("diagnose_run", "diagnose_run_cmd"),
    "schema_cli_main": ("schema_cli", "main"),
    "summarise_output_cmd": ("summarise_output", "summarise_output_cmd"),
    "validate_config_main": ("validate_config", "main"),
}

# Submodules exposed through ``from supy.cmd import <name>``. Listed
# explicitly so static checks (e.g. test_api_surface.py's hasattr probe)
# resolve them without requiring the consumer to first import the
# fully-qualified module path.
_LAZY_SUBMODULES = frozenset({"rust_bridge", "suews_cli"})


def __getattr__(name):
    """Lazy attribute loader for CLI commands."""
    if name in _lazy_cache:
        return _lazy_cache[name]

    if name in _LAZY_ATTRS:
        module_name, attr_name = _LAZY_ATTRS[name]
        try:
            module = _importlib.import_module(f"{__name__}.{module_name}")
            value = getattr(module, attr_name)
        except Exception:
            if name != "schema_cli_main":
                raise
            value = None
        _lazy_cache[name] = value
        return _lazy_cache[name]

    if name in _LAZY_SUBMODULES:
        module = _importlib.import_module(f"{__name__}.{name}")
        _lazy_cache[name] = module
        return module

    raise AttributeError(f"module 'supy.cmd' has no attribute {name!r}")


# to_yaml is used internally by table_converter but not exposed as a command
