"""``suews inspect`` — surface the high-level shape of a SUEWS YAML config.

The command loads the config via ``SUEWSConfig.from_yaml`` (the same
validator pipeline used by ``suews validate``) and emits a compact
overview: per-site coordinates, surface-cover fractions, forcing summary,
and any warnings encountered while introspecting nested values.

This is a read-only command — it never writes files and never mutates
the config. It also avoids triggering the full forcing load (the config
already records the forcing file path) so that ``inspect`` stays fast
even on configs that reference multi-GB forcing archives.
"""

from __future__ import annotations

from contextlib import nullcontext as _nullcontext
from pathlib import Path
import sys
from typing import Any, Dict, List, Optional

import click

from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso, silent_supy_logger

# Names of surface-cover sub-objects on ``LandCover`` — kept as a fixed list
# so the inspector works deterministically and reports missing surfaces as
# warnings rather than silently dropping them.
_SURFACE_NAMES: tuple[str, ...] = (
    "paved",
    "bldgs",
    "evetr",
    "dectr",
    "grass",
    "bsoil",
    "water",
)


def _ref_value(value: Any) -> Any:
    """Unwrap a ``RefValue``-like dict / object, else return verbatim.

    Handles four shapes:
    - ``RefValue`` instance with ``.value`` attribute
    - dict carrying a ``"value"`` key (post-``model_dump``)
    - bare scalar
    - None / missing
    """
    if value is None:
        return None
    if hasattr(value, "value"):
        return value.value
    if isinstance(value, dict) and "value" in value:
        return value["value"]
    return value


def _site_summary(site_data: Dict[str, Any]) -> Dict[str, Any]:
    """Extract ``{name, lat, lng, alt}`` from a dumped site dict."""
    properties = site_data.get("properties", {}) or {}
    return {
        "name": site_data.get("name"),
        "lat": _ref_value(properties.get("lat")),
        "lng": _ref_value(properties.get("lng")),
        "alt": _ref_value(properties.get("alt")),
    }


def _surface_fractions(
    site_data: Dict[str, Any], list_warnings: List[str]
) -> Dict[str, Optional[float]]:
    """Return a ``{surface_name: sfr}`` dict for one site.

    Missing surfaces are reported as warnings and rendered as ``None`` in
    the result so the JSON shape stays stable for callers (MCP / Skill).
    """
    properties = site_data.get("properties", {}) or {}
    land_cover = properties.get("land_cover", {}) or {}
    out: Dict[str, Optional[float]] = {}
    site_name = site_data.get("name", "<unnamed>")
    for surf in _SURFACE_NAMES:
        block = land_cover.get(surf)
        if block is None:
            list_warnings.append(
                "site '%s': land_cover.%s missing — treated as 0." % (site_name, surf)
            )
            out[surf] = None
            continue
        sfr_raw = (
            block.get("sfr") if isinstance(block, dict) else getattr(block, "sfr", None)
        )
        sfr_value = _ref_value(sfr_raw)
        if sfr_value is None:
            list_warnings.append(
                "site '%s': land_cover.%s.sfr missing." % (site_name, surf)
            )
        out[surf] = float(sfr_value) if isinstance(sfr_value, (int, float)) else None
    return out


def _resolve_forcing_path(path_value: str, path_config_dir: Path) -> Path:
    """Resolve a forcing path relative to the inspected config."""
    path_forcing = Path(path_value)
    if not path_forcing.is_absolute():
        path_forcing = path_config_dir / path_forcing
    return path_forcing


def _peek_forcing_header(path_value: str, path_config_dir: Path) -> Dict[str, Any]:
    """Return a cheap header summary for one forcing file path."""
    summary: Dict[str, Any] = {
        "file": path_value,
        "n_columns": None,
        "columns": [],
    }
    path_forcing = _resolve_forcing_path(path_value, path_config_dir)
    if path_forcing.exists() and path_forcing.is_file():
        try:
            with path_forcing.open("r", encoding="utf-8") as handle:
                header = handle.readline().strip()
            if header:
                cols = header.split()
                summary["n_columns"] = len(cols)
                summary["columns"] = cols
        except OSError:
            # Treat unreadable forcing as unknown rather than fatal.
            pass
    return summary


def _forcing_summary(model_data: Dict[str, Any], path_config_dir: Path) -> Dict[str, Any]:
    """Extract a forcing file summary from ``model.control``.

    We deliberately do NOT open the forcing file — that's an I/O cost we
    don't want on every inspect call. Column count / column names are
    therefore reported only when the file exists and is small enough to
    peek at cheaply (we sniff the first line).
    """
    control = model_data.get("control", {}) or {}
    # Post-gh#1372 shape: model.control.forcing.file. Legacy shape
    # (model.control.forcing_file) is accepted as a fallback so this
    # command stays useful on user YAMLs that have not yet been migrated.
    forcing_block = control.get("forcing")
    if isinstance(forcing_block, dict) and "file" in forcing_block:
        forcing_raw = forcing_block.get("file")
    else:
        forcing_raw = control.get("forcing_file")
    forcing_value = _ref_value(forcing_raw)

    summary: Dict[str, Any] = {
        "file": forcing_value,
        "n_columns": None,
        "columns": [],
    }
    if isinstance(forcing_value, str):
        summary.update(_peek_forcing_header(forcing_value, path_config_dir))
    elif isinstance(forcing_value, list):
        list_files = [
            _peek_forcing_header(item, path_config_dir)
            for item in forcing_value
            if isinstance(item, str)
        ]
        summary["files"] = list_files
        list_columns = [
            item["columns"]
            for item in list_files
            if item["n_columns"] is not None and item["columns"]
        ]
        if (
            list_files
            and len(list_columns) == len(list_files)
            and all(cols == list_columns[0] for cols in list_columns)
        ):
            summary["columns"] = list_columns[0]
            summary["n_columns"] = len(list_columns[0])
    return summary


def _build_text_message(data: Dict[str, Any], list_warnings: List[str]) -> str:
    lines = [
        "Configuration: %s" % data.get("config_path"),
        "Schema version: %s" % data.get("schema_version"),
        "",
        "Sites (%d):" % len(data.get("sites", [])),
    ]
    for site in data.get("sites", []):
        lines.append(
            "  - %s @ (lat=%s, lng=%s, alt=%s)"
            % (site.get("name"), site.get("lat"), site.get("lng"), site.get("alt"))
        )
    lines.append("")
    lines.append("Surface cover fractions (first site):")
    for surf, frac in (data.get("surface_cover_fraction") or {}).items():
        lines.append("  %-7s %s" % (surf, frac))
    forcing = data.get("forcing_summary") or {}
    lines.append("")
    lines.append("Forcing file: %s" % forcing.get("file"))
    if forcing.get("n_columns") is not None:
        lines.append("  columns: %d" % forcing["n_columns"])
    if list_warnings:
        lines.append("")
        lines.append("Warnings:")
        for warn in list_warnings:
            lines.append("  - %s" % warn)
    return "\n".join(lines)


@click.command(
    name="inspect",
    short_help="Inspect a SUEWS YAML configuration.",
    help=(
        "Show a compact overview of a SUEWS YAML config: per-site coordinates, "
        "surface-cover fractions, forcing file summary. Read-only — never "
        "modifies the config. Use --format json for structured output."
    ),
)
@click.argument(
    "config_path",
    type=click.Path(exists=True, dir_okay=False, file_okay=True),
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
def inspect_config_cmd(config_path: str, output_format: str) -> None:
    """Implementation of ``suews inspect``."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    list_args = sys.argv[1:]
    if list_args and list_args[0] == "inspect":
        list_args = list_args[1:]
    command = " ".join(["suews", "inspect"] + list_args)

    path_config = Path(config_path)

    try:
        from ..data_model.core.config import SUEWSConfig
    except ImportError as exc:  # pragma: no cover - dev-env edge case
        message = "Failed to import SUEWSConfig: %s" % exc
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"config_path": str(path_config)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    log_silencer = silent_supy_logger() if json_mode else _nullcontext()
    try:
        with log_silencer:
            config = SUEWSConfig.from_yaml(str(path_config))
    except Exception as exc:  # noqa: BLE001 - surface validator output verbatim
        message = "Failed to load config: %s: %s" % (type(exc).__name__, exc)
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"config_path": str(path_config)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    list_warnings: List[str] = []
    config_dict = config.model_dump(mode="python")

    list_sites_raw = config_dict.get("sites", []) or []
    list_sites = [_site_summary(site) for site in list_sites_raw]

    surface_cover_fraction: Dict[str, Optional[float]] = {}
    if list_sites_raw:
        surface_cover_fraction = _surface_fractions(list_sites_raw[0], list_warnings)
        if len(list_sites_raw) > 1:
            list_warnings.append(
                "Multiple sites detected — surface_cover_fraction reports the "
                "first site only ('%s')." % list_sites_raw[0].get("name")
            )

    model_data = config_dict.get("model", {}) or {}
    forcing_summary = _forcing_summary(model_data, path_config.resolve().parent)

    schema_version = config_dict.get("schema_version")

    data: Dict[str, Any] = {
        "config_path": str(path_config),
        "schema_version": schema_version,
        "sites": list_sites,
        "surface_cover_fraction": surface_cover_fraction,
        "forcing_summary": forcing_summary,
        "derived": {},
        "warnings": list_warnings,
    }

    if json_mode:
        Envelope.success(
            data=data,
            command=command,
            warnings=list_warnings or None,
            started_at=started_at,
        ).emit()
    else:
        click.echo(_build_text_message(data, list_warnings))
