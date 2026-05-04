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


def _agent_next_steps(target_dir: str, config_path: str) -> list[str]:
    """Return the MCP-tool-call follow-on steps an agent should take
    after a successful ``init_case`` (gh#1408).

    The CLI's own ``next_steps`` is shell-command form ("Edit X", "suews
    validate X", "suews run X") — useful at a terminal, but it does not
    tell an MCP agent which *MCP tool* to call next. In the EGU26
    poster trace, after a successful ``init_case`` the agent issued 11
    `query_knowledge` calls before timing out, never editing the YAML
    or calling ``validate_config``. Imperative MCP-tool-call phrasing
    here is what closes that loop.
    """
    return [
        f"open and edit `{config_path}` in your editor — set "
        "`sites[0].properties` (lat / lng / alt / timezone / surfacearea) "
        "and adjust `land_cover.<surface>.sfr` to the user's fractions",
        f"call `mcp__suews__inspect_config` on `{config_path}` to confirm "
        "the active fields and surface fractions before any further edits",
        f"call `mcp__suews__validate_config` on `{config_path}` after each "
        "edit cycle to catch missing critical-physics or site-level fields "
        "early",
    ]

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
    """**Call this first** when the user asks for a SUEWS configuration
    "from scratch" or for a new case. Returns a minimal valid YAML
    plus the path to it, so you avoid reconstructing the schema
    field-by-field from `query_knowledge` (gh#1407, gh#1408).

    The returned envelope's ``data`` carries:

    - ``data.recommendation`` — the single highest-priority MCP-tool
      call the agent should make next (typically "open and edit the
      scaffolded YAML"). Acts as a one-line agent guidance signal that
      does not require scanning the ``next_steps`` array (gh#1408).
    - ``data.next_steps`` — full ordered list of imperative MCP-tool
      calls (edit -> ``inspect_config`` -> ``validate_config``).
      Replaces the CLI's shell-command-form ``next_steps`` so an agent
      does not have to translate ``suews validate <path>`` into the
      corresponding MCP tool call.
    - ``data.files_created``, ``data.target_dir``, ``data.template``,
      ``data.schema_version`` — preserved verbatim from the CLI
      envelope.

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
        envelope = run_suews_cli("init", args, project_root=root.root)
    except SUEWSMCPError as exc:
        return _error_envelope(
            str(exc), command=f"suews init {path} --template {template}"
        )

    # Promote agent-actionable follow-on (gh#1408). The CLI's
    # `data.next_steps` is shell-command form; replace it with
    # MCP-tool-call form and add a `recommendation` field with the
    # single highest-priority next move so the agent does not need to
    # parse the array to know what to do next.
    data = envelope.get("data") or {}
    files_created = data.get("files_created") or []
    config_path = next(
        (p for p in files_created if str(p).endswith(".yml")),
        f"{path}/sample_config.yml",
    )
    agent_steps = _agent_next_steps(target_dir=str(path), config_path=str(config_path))
    data = {
        **data,
        # Single pointer the agent can act on without scanning the array.
        "recommendation": agent_steps[0],
        # Replace the shell-form list with MCP-tool-call-form list.
        "next_steps": agent_steps,
    }
    envelope = {**envelope, "data": data}
    return envelope
