# Lazy imports for fast CLI startup
# Only import specific CLI tools when actually accessed

__all__ = ["SUEWS", "convert_table_cmd", "validate_config_main", "schema_cli_main"]

_lazy_cache = {}


def __getattr__(name):
    """Lazy attribute loader for CLI commands and submodules."""
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

    # Fallback: allow `from supy.cmd import <submodule>` for any module file
    # under cmd/ without enumerating each one explicitly. Used by suews_cli,
    # rust_bridge, json_envelope, and the future init/inspect/diagnose/compare
    # subcommand modules.
    import importlib

    try:
        module = importlib.import_module(f".{name}", __name__)
    except ImportError as exc:
        raise AttributeError(
            f"module 'supy.cmd' has no attribute {name!r}"
        ) from exc
    _lazy_cache[name] = module
    return module


# to_yaml is used internally by table_converter but not exposed as a command
