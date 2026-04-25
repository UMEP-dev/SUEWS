"""``suews init`` — scaffold a new SUEWS YAML case from a template.

Phase-1 ships a single template (``simple-urban``), backed by the
``sample_data/sample_config.yml`` already vendored with supy. The other
three template names accepted by ``--template`` (``multi-site``,
``teaching-demo``, ``spartacus``) are reserved but not yet shipped: the
command rejects them with a clear "not yet shipped" envelope so users
get an actionable signal rather than a confusing fallback.
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path
from typing import Tuple

import click

from .json_envelope import EXIT_USER_ERROR, Envelope, _now_iso

# Mapping of template name -> (relative path under ``src/supy/sample_data``,
# whether it ships in Phase 1). Only ``simple-urban`` is shipped.
_TEMPLATES: dict[str, Tuple[str, bool]] = {
    "simple-urban": ("sample_config.yml", True),
    "multi-site": ("sample_config.yml", False),
    "teaching-demo": ("sample_config.yml", False),
    "spartacus": ("sample_config.yml", False),
}


def _locate_sample_data() -> Path:
    """Return the path to the bundled ``sample_data`` directory."""
    import supy

    return Path(supy.__file__).resolve().parent / "sample_data"


def _build_text_message(
    path_output_dir: Path, list_files_created: list[str], schema_version: str
) -> str:
    lines = [
        "Initialised SUEWS case at %s" % path_output_dir,
        "Files written:",
    ]
    for path_str in list_files_created:
        lines.append("  - %s" % path_str)
    lines.append("Schema version: %s" % schema_version)
    lines.append("Next steps:")
    lines.append("  1. Edit %s/sample_config.yml" % path_output_dir)
    lines.append("  2. suews validate %s/sample_config.yml" % path_output_dir)
    lines.append("  3. suews run %s/sample_config.yml --output runs/" % path_output_dir)
    return "\n".join(lines)


@click.command(
    name="init",
    short_help="Initialise a new SUEWS case from a template.",
    help=(
        "Initialise a new SUEWS case directory by copying a template config "
        "into --output. Phase-1 only ships the 'simple-urban' template; the "
        "other names are reserved and will be rejected with a structured error."
    ),
)
@click.option(
    "--template",
    type=click.Choice(sorted(_TEMPLATES.keys()), case_sensitive=False),
    default="simple-urban",
    show_default=True,
    help="Template to scaffold from.",
)
@click.option(
    "--output",
    "-o",
    "output_dir",
    type=click.Path(file_okay=False, dir_okay=True),
    required=True,
    help="Destination directory (created if missing).",
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
def init_case_cmd(template: str, output_dir: str, output_format: str) -> None:
    """Implementation of ``suews init``."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "init"] + sys.argv[1:])
    template_key = template.lower()
    template_info = _TEMPLATES[template_key]
    path_relative, is_shipped = template_info

    if not is_shipped:
        message = (
            "Template '%s' is reserved but not yet shipped in Phase 1. "
            "Only 'simple-urban' is currently available." % template_key
        )
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"template": template_key, "output_dir": output_dir},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    path_sample = _locate_sample_data() / path_relative
    if not path_sample.exists():
        message = "Sample template not found at %s." % path_sample
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"template": template_key, "output_dir": output_dir},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    path_output_dir = Path(output_dir)
    path_output_dir.mkdir(parents=True, exist_ok=True)

    path_target_yaml = path_output_dir / "sample_config.yml"
    if path_target_yaml.exists():
        message = (
            "Refusing to overwrite existing %s. Remove it or pick a fresh "
            "--output directory." % path_target_yaml
        )
        if json_mode:
            Envelope.error(
                errors=[message],
                command=command,
                data={"template": template_key, "output_dir": str(path_output_dir)},
                started_at=started_at,
            ).emit()
        else:
            click.secho(message, fg="red", err=True)
        sys.exit(EXIT_USER_ERROR)

    shutil.copy2(path_sample, path_target_yaml)
    list_files_created = [str(path_target_yaml)]

    # Copy companion forcing file when present so the template runs
    # out-of-the-box. The simple-urban template references
    # 'Kc_2012_data_60.txt' as its forcing.
    path_forcing = _locate_sample_data() / "Kc_2012_data_60.txt"
    if path_forcing.exists():
        path_target_forcing = path_output_dir / path_forcing.name
        if not path_target_forcing.exists():
            shutil.copy2(path_forcing, path_target_forcing)
            list_files_created.append(str(path_target_forcing))

    # Schema version comes from the canonical module — never re-derive.
    from ..data_model.schema.version import CURRENT_SCHEMA_VERSION

    list_next_steps = [
        "Edit %s" % path_target_yaml,
        "suews validate %s" % path_target_yaml,
        "suews run %s --output %s/runs --format json" % (path_target_yaml, path_output_dir),
    ]

    if json_mode:
        Envelope.success(
            data={
                "output_dir": str(path_output_dir),
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
            _build_text_message(path_output_dir, list_files_created, CURRENT_SCHEMA_VERSION)
        )
