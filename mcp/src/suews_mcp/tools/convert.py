"""``convert_config`` MCP tool — upgrade legacy inputs to the current YAML."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope


def convert_config(
    input_path: str,
    output_path: str,
    project_root: Optional[str] = None,
    from_version: Optional[str] = None,
    debug_dir: Optional[str] = None,
    no_profile_validation: bool = False,
) -> dict[str, Any]:
    """Convert any supported SUEWS input into a current-schema YAML.

    Parameters
    ----------
    input_path
        Source file: ``RunControl.nml`` for tables, ``*.csv``/``*.pkl`` for
        a ``df_state`` snapshot, or ``*.yml``/``*.yaml`` for an older YAML
        config.
    output_path
        Destination YAML path under the project root.
    project_root
        Override the per-session project root for this call.
    from_version
        Source version. For ``.nml`` inputs pick a table release (e.g.
        ``2024a``); for ``.yml`` inputs pass a supy release tag (e.g.
        ``2026.1.28``) or a schema version. Auto-detected when ``None``.
    debug_dir
        Optional directory to keep intermediate conversion files
        (table/df_state paths only). Ignored for YAML upgrades.
    no_profile_validation
        Disable automatic profile validation and creation of missing
        profiles (table/df_state paths only).
    """
    root = ProjectRoot(project_root)
    try:
        in_path = root.resolve(input_path)
        out_path = root.resolve(output_path)
        debug_resolved: Optional[str] = None
        if debug_dir:
            debug_resolved = str(root.resolve(debug_dir))
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(
            str(exc), command=f"suews convert -i {input_path} -o {output_path}"
        )

    args = ["-i", str(in_path), "-o", str(out_path)]
    if from_version:
        args += ["-f", from_version]
    if debug_resolved:
        args += ["-d", debug_resolved]
    if no_profile_validation:
        args.append("--no-profile-validation")

    try:
        return run_suews_cli("convert", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews convert -i {in_path} -o {out_path}"
        )
