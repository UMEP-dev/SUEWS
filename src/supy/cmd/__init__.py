from .SUEWS import SUEWS
from .table_converter import convert_table_cmd
from .validate_config import main as validate_config_main

# Optional CLI (not officially released yet). Avoid breaking import if missing.
try:
    from .schema_cli import main as schema_cli_main  # type: ignore
except Exception:  # pragma: no cover - optional tool
    schema_cli_main = None  # type: ignore

# to_yaml is used internally by table_converter but not exposed as a command
