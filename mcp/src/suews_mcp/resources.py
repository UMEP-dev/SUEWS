"""Resource helpers for the SUEWS MCP server."""

from __future__ import annotations

from functools import lru_cache
from importlib.resources import files
import json
from pathlib import Path
import subprocess

from .tools import ALLOWED_COMMANDS, run_allowlisted_command

HELP_TIMEOUT_S = 10.0


@lru_cache(maxsize=16)
def schema_json(version: str = "current") -> str:
    """Return a generated SUEWS JSON Schema resource."""
    from supy.data_model.schema.publisher import generate_json_schema  # noqa: PLC0415
    from supy.data_model.schema.version import (  # noqa: PLC0415
        CURRENT_SCHEMA_VERSION,
        SCHEMA_VERSIONS,
    )

    resolved_version = CURRENT_SCHEMA_VERSION if version == "current" else version
    if resolved_version not in SCHEMA_VERSIONS:
        raise ValueError(f"unknown SUEWS schema version: {version}")

    payload = {
        "version": resolved_version,
        "schema": generate_json_schema(version=resolved_version),
    }
    return json.dumps(payload, indent=2, ensure_ascii=False)


def sample_config() -> str:
    """Return the packaged SUEWS sample YAML configuration."""
    return (
        files("supy")
        .joinpath("sample_data")
        .joinpath("sample_config.yml")
        .read_text(encoding="utf-8")
    )


def yaml_config_docs() -> str:
    """Return concise YAML configuration guidance for AI clients."""
    return """# SUEWS YAML configuration

YAML is the recommended modern SUEWS configuration format. Start from a known
sample or template, validate before editing, and treat legacy namelist inputs as
migration sources rather than the preferred authoring format.

Useful CLI commands:
- `suews-validate --dry-run --format json config.yml`
- `suews-validate config.yml` for the full validation pipeline
- `suews-convert -i RunControl.nml -o config.yml` for legacy migration

Docs: https://docs.suews.io/
"""


def forcing_data_docs() -> str:
    """Return concise forcing-data guidance for AI clients."""
    return """# SUEWS forcing data

Forcing data must provide a continuous local-time meteorological time series
with required time columns and meteorological variables. Validate forcing through
the SUEWS validator before running simulations, and separate missing columns,
time-axis problems, unit problems, and physically suspicious values.

Important checks:
- Required columns are present.
- Time stamps are continuous and monotonic.
- Wind speed remains positive enough for atmospheric calculations.
- Radiation, humidity, pressure, rainfall, and temperature ranges are plausible.
- Optional missing values use SUEWS conventions rather than arbitrary blanks.

Docs: https://docs.suews.io/
"""


def cli_help(command: str, root: Path) -> str:
    """Return allowlisted CLI help text for a SUEWS command."""
    if command not in ALLOWED_COMMANDS:
        raise ValueError(f"command is not allowlisted: {command}")
    try:
        proc = run_allowlisted_command(
            command,
            ["--help"],
            cwd=root,
            timeout_s=HELP_TIMEOUT_S,
        )
    except subprocess.TimeoutExpired as exc:
        stdout = exc.stdout or ""
        stderr = exc.stderr or ""
        return (
            f"# {command} --help timed out after {HELP_TIMEOUT_S:g} seconds\n\n"
            f"{stdout}\n{stderr}".strip()
        )
    return "\n".join(
        part for part in (proc.stdout.strip(), proc.stderr.strip()) if part
    )
