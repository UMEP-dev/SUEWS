"""``init_case`` MCP tool — scaffold a new SUEWS case from a template."""

from __future__ import annotations

from typing import Any, Optional

from ..backend import (
    ProjectRoot,
    SUEWSMCPError,
    SUEWSMCPSandboxError,
    run_suews_cli,
)
from .validate import _error_envelope

#: Template names accepted by ``suews init --template``.
ALLOWED_TEMPLATES: frozenset[str] = frozenset({
    "simple-urban",
    "multi-site",
    "spartacus",
    "teaching-demo",
})


def init_case(
    target_dir: str,
    project_root: Optional[str] = None,
    template: str = "simple-urban",
) -> dict[str, Any]:
    """Initialise a new SUEWS case directory by copying a packaged template.

    Parameters
    ----------
    target_dir
        Directory under the project root that should receive the template.
        Created if missing; existing config files are not overwritten.
    project_root
        Override the per-session project root for this call.
    template
        One of :data:`ALLOWED_TEMPLATES`. Currently only ``simple-urban`` is
        shipped; other names are accepted by the CLI but rejected with a
        structured error envelope.
    """
    if template not in ALLOWED_TEMPLATES:
        return _error_envelope(
            f"Template {template!r} is not in the allow-list "
            f"({sorted(ALLOWED_TEMPLATES)})",
            command=f"suews init {target_dir} --template {template}",
        )

    root = ProjectRoot(project_root)
    try:
        path = root.resolve(target_dir)
    except SUEWSMCPSandboxError as exc:
        return _error_envelope(str(exc), command=f"suews init {target_dir}")

    args = [str(path), "--template", template]
    try:
        return run_suews_cli("init", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews init {path} --template {template}"
        )
