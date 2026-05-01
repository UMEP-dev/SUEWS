"""``suews init`` -- scaffold a new SUEWS YAML case from a template.

Wave-3 ships a single template (``simple-urban``), backed by the
``sample_data/sample_config.yml`` already vendored with supy. The other
template names accepted by ``--template`` (``multi-site``,
``teaching-demo``, ``spartacus``) are reserved but not yet shipped: the
command rejects them with a clear "not yet shipped" envelope so users
get an actionable signal rather than a confusing fallback.
"""

from __future__ import annotations

from pathlib import Path
import shutil
import sys

import click

from ..data_model.schema.version import CURRENT_SCHEMA_VERSION
from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso

# Mapping of template name -> (relative path under ``src/supy/sample_data``,
# whether it ships in this wave). Only ``simple-urban`` is shipped today.
_TEMPLATES: dict[str, tuple[str, bool]] = {
    "simple-urban": ("sample_config.yml", True),
    "multi-site": ("sample_config.yml", False),
    "teaching-demo": ("sample_config.yml", False),
    "spartacus": ("sample_config.yml", False),
}

# Companion files to copy alongside the YAML when the source template
# references them. Keep this small -- the canonical contract is the YAML;
# anything else is a convenience for out-of-the-box runs.
_COMPANION_FILES: tuple[str, ...] = ("Kc_2012_data_60.txt",)


def _locate_sample_data() -> Path:
    """Return the path to the bundled ``sample_data`` directory."""
    return Path(__file__).resolve().parent.parent / "sample_data"


def _build_text_message(
    path_output_dir: Path,
    list_files_created: list[str],
    schema_version: str,
    yaml_name: str,
) -> str:
    lines = [
        f"Initialised SUEWS case at {path_output_dir}",
        "Files written:",
    ]
    for path_str in list_files_created:
        lines.append(f"  - {path_str}")
    lines.append(f"Schema version: {schema_version}")
    lines.append("Next steps:")
    lines.append(f"  1. Edit {path_output_dir}/{yaml_name}")
    lines.append(f"  2. suews validate {path_output_dir}/{yaml_name}")
    lines.append(f"  3. suews run {path_output_dir}/{yaml_name}")
    return "\n".join(lines)


def _copy_companion_files(path_sample_dir: Path, path_target_dir: Path) -> list[str]:
    """Copy packaged companion files and return the created paths."""
    list_files_created: list[str] = []
    for name in _COMPANION_FILES:
        path_src = path_sample_dir / name
        if not path_src.exists():
            continue
        path_dst = path_target_dir / name
        if path_dst.exists():
            continue
        shutil.copy2(path_src, path_dst)
        list_files_created.append(str(path_dst))
    return list_files_created


@click.command(
    name="init",
    short_help="Initialise a new SUEWS case from a template.",
    help=(
        "Initialise a new SUEWS case directory by copying a packaged template "
        "into TARGET_DIR. The directory is created if missing; existing "
        "config files are not overwritten. Currently only the 'simple-urban' "
        "template is shipped; the other names are reserved and rejected with "
        "a structured error envelope."
    ),
)
@click.argument(
    "target_dir",
    type=click.Path(file_okay=False, dir_okay=True, writable=False, resolve_path=False),
    required=True,
)
@click.option(
    "--template",
    type=click.Choice(sorted(_TEMPLATES.keys()), case_sensitive=False),
    default="simple-urban",
    show_default=True,
    help="Template to scaffold from.",
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
def init_case_cmd(target_dir: str, template: str, output_format: str) -> None:
    """Implement ``suews init``."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    # Use the full argv so the recorded command matches what the user
    # actually typed; ``sys.argv[0]`` is the entry-point script name
    # (``suews`` for the dispatcher, ``suews init`` for direct module use).
    command = " ".join(sys.argv)
    template_key = template.lower()
    path_relative, is_shipped = _TEMPLATES[template_key]

    if not is_shipped:
        message = (
            f"Template '{template_key}' is reserved but not yet shipped. "
            "Only 'simple-urban' is currently available."
        )
        _emit_error(
            message,
            command=command,
            data={"template": template_key, "target_dir": target_dir},
            started_at=started_at,
            json_mode=json_mode,
        )
        sys.exit(EXIT_USER_ERROR)

    path_sample = _locate_sample_data() / path_relative
    if not path_sample.exists():
        message = f"Sample template not found at {path_sample}."
        _emit_error(
            message,
            command=command,
            data={"template": template_key, "target_dir": target_dir},
            started_at=started_at,
            json_mode=json_mode,
        )
        sys.exit(EXIT_USER_ERROR)

    path_target_dir = Path(target_dir)
    path_target_dir.mkdir(parents=True, exist_ok=True)

    yaml_name = path_sample.name
    path_target_yaml = path_target_dir / yaml_name
    if path_target_yaml.exists():
        message = (
            f"Refusing to overwrite existing {path_target_yaml}. Remove it or "
            "pick a fresh target directory."
        )
        _emit_error(
            message,
            command=command,
            data={"template": template_key, "target_dir": str(path_target_dir)},
            started_at=started_at,
            json_mode=json_mode,
        )
        sys.exit(EXIT_USER_ERROR)

    shutil.copy2(path_sample, path_target_yaml)
    list_files_created = [str(path_target_yaml)]

    # Companion files: copied only if the bundled template references them
    # implicitly (e.g. the simple-urban YAML names ``Kc_2012_data_60.txt`` as
    # its forcing file).
    list_files_created.extend(
        _copy_companion_files(_locate_sample_data(), path_target_dir)
    )

    list_next_steps = [
        f"Edit {path_target_yaml}",
        f"suews validate {path_target_yaml}",
        f"suews run {path_target_yaml}",
    ]

    if json_mode:
        Envelope.success(
            data={
                "target_dir": str(path_target_dir),
                "template": template_key,
                "files_created": list_files_created,
                "schema_version": CURRENT_SCHEMA_VERSION,
                "next_steps": list_next_steps,
            },
            command=command,
            started_at=started_at,
        ).emit()
    else:
        click.echo(
            _build_text_message(
                path_target_dir,
                list_files_created,
                CURRENT_SCHEMA_VERSION,
                yaml_name,
            )
        )


def _emit_error(
    message: str,
    *,
    command: str,
    data: dict,
    started_at: str,
    json_mode: bool,
) -> None:
    """Surface a user-facing error in either text or JSON envelope form."""
    if json_mode:
        Envelope.error(
            errors=[message],
            command=command,
            data=data,
            started_at=started_at,
        ).emit()
    else:
        click.secho(message, fg="red", err=True)


if __name__ == "__main__":  # pragma: no cover
    init_case_cmd()
