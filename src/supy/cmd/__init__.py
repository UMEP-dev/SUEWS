from .SUEWS import SUEWS
from .table_converter import convert_table_cmd
from .validate_config import main as validate_config_main

# Optional CLI modules (not officially released yet). Avoid breaking import if missing.
try:
    from .schema_cli import main as schema_cli_main  # type: ignore
except Exception:  # pragma: no cover - optional tool
    schema_cli_main = None  # type: ignore

try:
    from .json_output import JSONOutput, ErrorCode, ValidationError  # type: ignore
except Exception:  # pragma: no cover - optional module
    JSONOutput = None  # type: ignore
    ErrorCode = None  # type: ignore
    ValidationError = None  # type: ignore

# to_yaml is used internally by table_converter but not exposed as a command
