"""``suews://schema/{version}`` resource."""

from __future__ import annotations

from typing import Any

from ..backend import SUEWSMCPError, run_suews_cli


def read_schema_resource(version: str = "current") -> dict[str, Any]:
    """Return the JSON-Schema-like dump of the SUEWS YAML grammar.

    Parameters
    ----------
    version
        Schema version. ``"current"`` selects the version currently shipped
        with the installed supy. Other values are passed through to
        ``suews schema export --version``.
    """
    args = ["export"]
    if version and version != "current":
        args += ["--version", version]
    try:
        return run_suews_cli("schema", args)
    except SUEWSMCPError as exc:
        return {
            "status": "error",
            "data": {},
            "errors": [{"message": str(exc)}],
            "warnings": [],
            "meta": {"command": f"suews schema export --version {version}"},
        }
