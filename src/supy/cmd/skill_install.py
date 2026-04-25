"""``suews skill install`` — copy the bundled SUEWS Skill into the user's
``~/.claude/skills/suews/`` so MCP-aware AI clients pick it up globally.

Phase-1 implementation locates the Skill source by walking up from the
installed ``supy`` package to find a sibling ``.claude/skills/suews/`` —
i.e. it works for editable installs out of the SUEWS repo. A future
iteration will bundle the Skill inside the supy wheel under
``supy/_skills/`` so the same command works for non-editable installs.
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path

import click

from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso


def _locate_skill_source() -> Path | None:
    """Find the canonical Skill folder shipped with this checkout."""
    try:
        import supy

        path_supy = Path(supy.__file__).resolve().parent
    except ImportError:
        return None

    for candidate in (path_supy, *path_supy.parents):
        path_skill = candidate / ".claude" / "skills" / "suews"
        if path_skill.exists() and (path_skill / "SKILL.md").exists():
            return path_skill
    return None


def _user_skill_target(global_install: bool) -> Path:
    home = Path.home()
    if global_install:
        return home / ".claude" / "skills" / "suews"
    # Project-local install: under the current working directory.
    return Path.cwd() / ".claude" / "skills" / "suews"


@click.command(
    name="install",
    short_help="Install the SUEWS Skill into ~/.claude/skills/suews/",
    help=(
        "Copy (or symlink) the bundled SUEWS Skill so MCP-aware AI clients "
        "pick up the procedural workflow rules.\n\n"
        "By default installs into the current project's `.claude/skills/`. "
        "Use --global to install into ~/.claude/skills/."
    ),
)
@click.option(
    "--global/--local",
    "global_install",
    default=False,
    help="Install globally under ~/.claude/skills/ instead of the cwd.",
)
@click.option(
    "--symlink",
    is_flag=True,
    default=False,
    help="Create a symlink to the source Skill instead of copying.",
)
@click.option(
    "--force",
    is_flag=True,
    default=False,
    help="Overwrite an existing Skill at the destination.",
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format.",
)
def skill_install_cmd(
    global_install: bool,
    symlink: bool,
    force: bool,
    output_format: str,
) -> None:
    """Install the SUEWS Skill into the user's Claude skill directory."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "skill", "install"] + sys.argv[1:])

    def _emit_error(msg: str, exit_code: int = EXIT_USER_ERROR) -> None:
        if json_mode:
            Envelope.error(
                errors=[msg], command=command, started_at=started_at
            ).emit()
        else:
            click.secho(msg, fg="red", err=True)
        sys.exit(exit_code)

    src = _locate_skill_source()
    if src is None:
        _emit_error(
            "Could not locate the SUEWS Skill source (.claude/skills/suews/). "
            "Phase-1 'skill install' only works from editable installs of the "
            "SUEWS repo. A future release will bundle the Skill in the wheel."
        )

    target = _user_skill_target(global_install)

    if target.exists():
        if not force:
            _emit_error(
                f"Skill already installed at {target}. Use --force to overwrite."
            )
        if target.is_symlink() or target.is_file():
            target.unlink()
        else:
            shutil.rmtree(target)

    target.parent.mkdir(parents=True, exist_ok=True)

    if symlink:
        target.symlink_to(src)
    else:
        shutil.copytree(src, target)

    if json_mode:
        Envelope.success(
            data={
                "source": str(src),
                "target": str(target),
                "method": "symlink" if symlink else "copy",
                "global": global_install,
            },
            command=command,
            started_at=started_at,
        ).emit()
    else:
        click.secho(
            f"Installed SUEWS Skill: {src} -> {target} "
            f"({'symlink' if symlink else 'copy'})",
            fg="green",
        )


@click.group(
    name="skill",
    help="Manage the SUEWS Skill (procedural workflow rules for AI clients).",
)
def skill_group() -> None:
    """Top-level ``suews skill`` group."""


skill_group.add_command(skill_install_cmd)
