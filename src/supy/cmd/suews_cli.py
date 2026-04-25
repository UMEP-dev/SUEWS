"""Unified ``suews`` CLI dispatcher.

Wires the existing per-command Click implementations as subcommands of a
single ``suews`` Click group. This is the canonical entry point that the
SUEWS MCP server and downstream tooling depend on; the legacy hyphenated
console scripts (``suews-run``, ``suews-convert``, ``suews-validate``,
``suews-schema``) remain as deprecated thin aliases.

Hard rule: this module never reimplements physics, validation, schema, or
run logic. It only routes.
"""

from __future__ import annotations

import sys

import click

# Each existing entry point is reused unchanged. Importing the Click commands
# here keeps the dispatcher small and ensures behaviour is identical across
# the new and legacy invocation styles.
from .SUEWS import SUEWS as _run_cmd
from .compare_runs import compare_runs_cmd as _compare_cmd
from .diagnose_run import diagnose_run_cmd as _diagnose_cmd
from .init_case import init_case_cmd as _init_cmd
from .inspect_config import inspect_config_cmd as _inspect_cmd
from .schema_cli import cli as _schema_cli
from .skill_install import skill_group as _skill_group
from .summarise_output import summarise_output_cmd as _summarise_cmd
from .table_converter import convert_table_cmd as _convert_cmd
from .validate_config import cli as _validate_cli


@click.group(
    help=(
        "SUEWS unified command-line interface.\n\n"
        "Use `suews <subcommand> --help` to see options for each subcommand.\n"
        "All subcommands emit a standard JSON envelope when invoked with "
        "`--format json` (where supported)."
    ),
    context_settings={"help_option_names": ["-h", "--help"]},
)
def cli() -> None:
    """Top-level ``suews`` group."""


# ---------------------------------------------------------------------------
# Subcommands: register the existing Click implementations under stable names.
# ---------------------------------------------------------------------------

cli.add_command(_validate_cli, name="validate")
cli.add_command(_schema_cli, name="schema")
cli.add_command(_convert_cmd, name="convert")
cli.add_command(_run_cmd, name="run")

# Phase-1 gap-fill commands. Names follow the architecture plan:
# init / inspect / diagnose / compare / summarise.
cli.add_command(_init_cmd, name="init")
cli.add_command(_inspect_cmd, name="inspect")
cli.add_command(_diagnose_cmd, name="diagnose")
cli.add_command(_compare_cmd, name="compare")
cli.add_command(_summarise_cmd, name="summarise")

# Skill management group: `suews skill install [...]`.
cli.add_command(_skill_group, name="skill")


@cli.command(
    name="rust",
    context_settings={
        "ignore_unknown_options": True,
        "allow_extra_args": True,
        "help_option_names": [],  # let -h / --help fall through to the Rust binary
    },
    help="Forward arguments to the bundled SUEWS Rust CLI.",
)
@click.pass_context
def _rust(ctx: click.Context) -> None:
    """Pass-through to the bundled Rust binary (``supy.bin.suews``)."""
    from . import rust_bridge

    rust_bridge.main(argv=list(ctx.args))


# ---------------------------------------------------------------------------
# Back-compat aliases for the legacy hyphenated entry points.
#
# Each shim writes a single deprecation line to stderr and forwards to the
# underlying Click command. We deliberately do NOT use the project-wide
# ``_warn_functional_deprecation`` helper here — that one targets Python
# function deprecations and the warnings filter system. CLI entry points
# need a hard, always-visible stderr line because shells redirect stdout
# for piping into tools like ``jq``.
# ---------------------------------------------------------------------------


_DEPRECATION_TEMPLATE = (
    "DEPRECATED: '{legacy}' is deprecated and will be removed in a future release. "
    "Use '{replacement}' instead."
)


def _emit_deprecation(legacy: str, replacement: str) -> None:
    sys.stderr.write(_DEPRECATION_TEMPLATE.format(legacy=legacy, replacement=replacement))
    sys.stderr.write("\n")
    sys.stderr.flush()


def run_alias() -> None:
    """Deprecated alias for ``suews run``."""
    _emit_deprecation("suews-run", "suews run")
    _run_cmd()


def convert_alias() -> None:
    """Deprecated alias for ``suews convert``."""
    _emit_deprecation("suews-convert", "suews convert")
    _convert_cmd()


def validate_alias() -> None:
    """Deprecated alias for ``suews validate``."""
    _emit_deprecation("suews-validate", "suews validate")
    _validate_cli()


def schema_alias() -> None:
    """Deprecated alias for ``suews schema``."""
    _emit_deprecation("suews-schema", "suews schema")
    _schema_cli()


def main() -> None:
    """Console-script entry point for ``suews``."""
    cli()


if __name__ == "__main__":  # pragma: no cover
    main()
