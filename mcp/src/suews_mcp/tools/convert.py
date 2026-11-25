"""``convert_config`` MCP tool — upgrade legacy inputs to the current YAML."""

from __future__ import annotations

from pathlib import Path
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
    """**Use this when the user has a legacy SUEWS input** (RunControl
    namelist, table CSV, or an older YAML version)
    and wants the current schema. Do not call this when starting a
    new case — `init_case` is the right entry point. Pair with
    `validate_config` afterwards to check the converted YAML
    (gh#1407).

    Parameters
    ----------
    input_path
        Source file: ``RunControl.nml`` for tables, ``*.csv`` for
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
    try:
        root = ProjectRoot(project_root)
        in_path = root.resolve(input_path)
        if any(
            path.suffix.lower() in {".pkl", ".pickle"}
            for path in (Path(input_path), in_path)
        ):
            raise SUEWSMCPSandboxError(
                "Pickle inputs are not permitted by convert_config; use CSV or YAML."
            )
        if in_path.suffix.lower() == ".nml":
            root.check_runcontrol(in_path, versioned=bool(from_version))
        elif in_path.suffix.lower() in {".yaml", ".yml"}:
            root.check_config(in_path)
        out_path = root.resolve(output_path)
        debug_resolved: Optional[str] = None
        if debug_dir:
            debug_resolved = str(root.check_tree(debug_dir))
    except (SUEWSMCPSandboxError, OSError) as exc:
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
