# Lazy imports for fast CLI startup
# Only import specific CLI tools when actually accessed

__all__ = ["SUEWS", "convert_table_cmd", "validate_config_main", "schema_cli_main"]

_lazy_cache = {}


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

    raise AttributeError(f"module 'supy.cmd' has no attribute {name!r}")


# to_yaml is used internally by table_converter but not exposed as a command
