"""CLI commands for the packaged SUEWS knowledge pack."""

from __future__ import annotations

from pathlib import Path
import sys

import click

from ..knowledge import build_pack, load_manifest, query_pack
from .json_envelope import EXIT_OK, EXIT_USER_ERROR, Envelope


@click.group(help="Build and query the versioned SUEWS source-evidence pack.")
def knowledge_group() -> None:
    """Knowledge-pack command group."""


@knowledge_group.command(name="build")
@click.option(
    "--output",
    "output_dir",
    type=click.Path(file_okay=False, path_type=Path),
    required=True,
    help="Directory where manifest.json and chunks.jsonl.gz will be written.",
)
@click.option(
    "--repo-root",
    type=click.Path(file_okay=False, path_type=Path),
    default=Path("."),
    show_default=True,
    help="Repository checkout to package.",
)
@click.option("--git-sha", help="Explicit Git SHA when building outside a Git checkout.")
@click.option("--format", "output_format", type=click.Choice(["text", "json"]), default="text")
def build_cmd(
    output_dir: Path,
    repo_root: Path,
    git_sha: str | None,
    output_format: str,
) -> None:
    """Build a source-evidence knowledge pack from the current checkout."""
    command = "suews knowledge build"
    try:
        manifest = build_pack(
            repo_root=repo_root,
            output_dir=output_dir,
            git_sha=git_sha,
            command=command,
        )
    except Exception as exc:
        if output_format == "json":
            Envelope.error(
                [{"message": str(exc), "code": "knowledge_build_failed"}],
                command=command,
            ).emit()
            raise SystemExit(EXIT_USER_ERROR) from exc
        raise click.ClickException(str(exc)) from exc

    data = {
        "manifest_path": str(output_dir / "manifest.json"),
        "chunks_path": str(output_dir / "chunks.jsonl.gz"),
        "manifest": manifest,
    }
    if output_format == "json":
        Envelope.success(data=data, command=command).emit()
    else:
        click.echo(f"Built SUEWS knowledge pack: {data['manifest_path']}")
    raise SystemExit(EXIT_OK)


@knowledge_group.command(name="manifest")
@click.option(
    "--pack",
    "pack_dir",
    type=click.Path(file_okay=False, path_type=Path),
    help="Knowledge-pack directory. Defaults to the installed pack.",
)
@click.option("--format", "output_format", type=click.Choice(["text", "json"]), default="text")
def manifest_cmd(pack_dir: Path | None, output_format: str) -> None:
    """Show the knowledge-pack manifest."""
    command = "suews knowledge manifest"
    try:
        manifest = load_manifest(pack_dir)
    except Exception as exc:
        if output_format == "json":
            Envelope.error(
                [{"message": str(exc), "code": "knowledge_manifest_failed"}],
                command=command,
            ).emit()
            raise SystemExit(EXIT_USER_ERROR) from exc
        raise click.ClickException(str(exc)) from exc

    if output_format == "json":
        Envelope.success(data={"manifest": manifest}, command=command).emit()
    else:
        click.echo(f"SUEWS knowledge pack @ {manifest.get('git_sha', 'unknown')}")
        click.echo(f"chunks: {manifest.get('chunk_count', 'unknown')}")
        click.echo(f"content_hash: {manifest.get('content_hash', 'unknown')}")
    raise SystemExit(EXIT_OK)


@knowledge_group.command(name="query")
@click.argument("question")
@click.option(
    "--pack",
    "pack_dir",
    type=click.Path(file_okay=False, path_type=Path),
    help="Knowledge-pack directory. Defaults to the installed pack.",
)
@click.option("--limit", type=int, default=5, show_default=True)
@click.option("--scope", help="Restrict to a content_type such as fortran, rust, python_api, schema.")
@click.option("--format", "output_format", type=click.Choice(["text", "json"]), default="text")
def query_cmd(
    question: str,
    pack_dir: Path | None,
    limit: int,
    scope: str | None,
    output_format: str,
) -> None:
    """Retrieve cited source evidence from the knowledge pack."""
    command = "suews knowledge query"
    try:
        result = query_pack(question, pack_dir=pack_dir, limit=limit, scope=scope)
    except Exception as exc:
        if output_format == "json":
            Envelope.error(
                [{"message": str(exc), "code": "knowledge_query_failed"}],
                command=command,
            ).emit()
            raise SystemExit(EXIT_USER_ERROR) from exc
        raise click.ClickException(str(exc)) from exc

    if output_format == "json":
        Envelope.success(data=result, command=command).emit()
    else:
        for match in result["matches"]:
            sys.stdout.write(
                f"{match['score']:>4} {match['repo_path']}:{match['line_start']}-{match['line_end']}\n"
            )
            if match.get("symbol"):
                sys.stdout.write(f"     {match['symbol']}\n")
            sys.stdout.write(f"     {match['github_url']}\n")
    raise SystemExit(EXIT_OK)
