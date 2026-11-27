"""CLI commands for discovering, exporting, publishing, and validating SUEWS data contracts."""

from __future__ import annotations

import json
from pathlib import Path
import sys

import click

from ..data_model.contracts import (
    CONTRACT_INTERFACES,
    DEFAULT_BASE_URL,
    ContractError,
    ContractPublicationError,
    export_contract,
    get_contract_info,
    load_contract_artefact_bytes,
    publish_contracts_site,
    sha256_digest,
    validate_published_contracts,
)
from .json_envelope import EXIT_OK, EXIT_USER_ERROR, Envelope, _now_iso

_INTERFACE_CHOICE = click.Choice(list(CONTRACT_INTERFACES), case_sensitive=False)
_FORMAT_CHOICE = click.Choice(["text", "json"], case_sensitive=False)
_SHOW_ARTEFACT_CHOICE = click.Choice(
    ["catalogue", "schema", "manifest"], case_sensitive=False
)
_EXPORT_ARTEFACT_CHOICE = click.Choice(
    ["catalogue", "schema", "manifest", "bundle"], case_sensitive=False
)


def _fail(
    message: str,
    *,
    code: str,
    command: str,
    output_format: str,
    started_at: str,
) -> None:
    if output_format.lower() == "json":
        Envelope.error(
            [{"message": message, "code": code}],
            command=command,
            started_at=started_at,
        ).emit()
        raise SystemExit(EXIT_USER_ERROR)
    raise click.ClickException(message)


@click.group(
    help=(
        "Inspect, export, publish, and validate versioned SUEWS forcing and "
        "output data contracts."
    )
)
def contracts_group() -> None:
    """Data-contract command group."""


@contracts_group.command(name="info")
@click.argument("interface", type=_INTERFACE_CHOICE, required=False, default=None)
@click.option(
    "--base-url",
    default=DEFAULT_BASE_URL,
    show_default=True,
    help="Base URL used to build published contract links.",
)
@click.option(
    "--format",
    "output_format",
    type=_FORMAT_CHOICE,
    default="text",
    show_default=True,
    help="Output format. Choices: text, json.",
)
def info_cmd(
    interface: str | None,
    base_url: str,
    output_format: str,
) -> None:
    """List published forcing and output contract versions and URLs."""
    started_at = _now_iso()
    command = "suews contracts info"
    try:
        info = get_contract_info(
            interface.lower() if interface is not None else None,
            base_url=base_url,
        )
    except ContractError as exc:
        _fail(
            str(exc),
            code="contract_info_failed",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    if output_format.lower() == "json":
        Envelope.success(data=info, command=command, started_at=started_at).emit()
        raise SystemExit(EXIT_OK)

    click.echo(f"SUEWS Data Contracts ({info['contracts_url']})")
    for iface_name, iface_data in info["interfaces"].items():
        click.echo("")
        click.echo(
            f"[{iface_name}] current: {iface_data['current_version']} "
            f"({iface_data['version_count']} version(s))"
        )
        click.echo(f"  latest catalogue: {iface_data['latest_urls']['catalogue']}")
        click.echo(f"  latest schema   : {iface_data['latest_urls']['schema']}")
        click.echo(f"  latest manifest : {iface_data['latest_urls']['manifest']}")
        click.echo("  versions:")
        for entry in iface_data["versions"]:
            marker = "*" if entry["is_current"] else " "
            click.echo(f"  {marker} {entry['version']}  {entry['digest']}")
    raise SystemExit(EXIT_OK)


@contracts_group.command(name="show")
@click.argument("interface", type=_INTERFACE_CHOICE)
@click.option(
    "--version",
    "contract_version",
    default=None,
    help="Contract version to inspect. Defaults to the current version.",
)
@click.option(
    "--artefact",
    type=_SHOW_ARTEFACT_CHOICE,
    default="catalogue",
    show_default=True,
    help="Contract artefact to inspect. Choices: catalogue, schema, manifest.",
)
@click.option(
    "--format",
    "output_format",
    type=_FORMAT_CHOICE,
    default="text",
    show_default=True,
    help="Output format. Choices: text, json.",
)
def show_cmd(
    interface: str,
    contract_version: str | None,
    artefact: str,
    output_format: str,
) -> None:
    """Inspect a forcing or output contract catalogue, schema, or manifest."""
    started_at = _now_iso()
    command = "suews contracts show"
    iface = interface.lower()
    art = artefact.lower()
    try:
        resolved, filename, raw_bytes = load_contract_artefact_bytes(
            iface,  # type: ignore[arg-type]
            artefact=art,  # type: ignore[arg-type]
            version=contract_version,
        )
    except ContractError as exc:
        _fail(
            str(exc),
            code="contract_show_failed",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    payload = json.loads(raw_bytes.decode("utf-8"))
    digest = sha256_digest(raw_bytes)
    data = {
        "interface": iface,
        "version": resolved,
        "artefact": art,
        "file": filename,
        "digest": digest,
        "payload": payload,
    }

    if output_format.lower() == "json":
        Envelope.success(data=data, command=command, started_at=started_at).emit()
        raise SystemExit(EXIT_OK)

    click.echo(f"SUEWS {iface} contract v{resolved} ({filename})")
    click.echo(f"  digest: {digest}")
    if art == "catalogue":
        variables = payload.get("variables", [])
        click.echo(f"  variables: {len(variables)}")
        if iface == "output":
            groups = payload.get("groups", [])
            group_names = ", ".join(str(g.get("group", "")) for g in groups)
            click.echo(f"  groups ({len(groups)}): {group_names}")
        else:
            rules = payload.get("requirement_rules", [])
            click.echo(f"  requirement_rules: {len(rules)}")
            click.echo(
                f"  timestamp_reference: {payload.get('timestamp_reference', '')}"
            )
    elif art == "schema":
        click.echo(f"  title: {payload.get('title', '')}")
        click.echo(f"  $schema: {payload.get('$schema', '(implicit Draft 2020-12)')}")
        defs = payload.get("$defs", {})
        click.echo(f"  definitions: {len(defs)}")
    else:
        click.echo(f"  interface: {payload.get('interface', '')}")
        for name, file_digest in sorted((payload.get("files") or {}).items()):
            click.echo(f"  {name}: {file_digest}")
    raise SystemExit(EXIT_OK)


@contracts_group.command(name="export")
@click.argument("interface", type=_INTERFACE_CHOICE)
@click.option(
    "--version",
    "contract_version",
    default=None,
    help="Contract version to export. Defaults to the current version.",
)
@click.option(
    "--artefact",
    type=_EXPORT_ARTEFACT_CHOICE,
    default="catalogue",
    show_default=True,
    help="Artefact to export. Choices: catalogue, schema, manifest, bundle.",
)
@click.option(
    "--output",
    "-o",
    "output_path",
    type=click.Path(path_type=Path),
    default=None,
    help=(
        "Destination file path, or destination directory for bundle exports. "
        "Omit to write a single artefact to stdout."
    ),
)
@click.option(
    "--format",
    "output_format",
    type=_FORMAT_CHOICE,
    default="text",
    show_default=True,
    help=(
        "Status output format when writing to --output, or envelope mode on stdout. "
        "Choices: text, json."
    ),
)
def export_cmd(
    interface: str,
    contract_version: str | None,
    artefact: str,
    output_path: Path | None,
    output_format: str,
) -> None:
    """Export a forcing or output contract artefact or release bundle."""
    started_at = _now_iso()
    command = "suews contracts export"
    iface = interface.lower()
    art = artefact.lower()

    if art == "bundle" and output_path is None:
        _fail(
            "--output is required when exporting a full contract bundle",
            code="contract_export_missing_output",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    if output_path is None:
        try:
            resolved, filename, raw_bytes = load_contract_artefact_bytes(
                iface,  # type: ignore[arg-type]
                artefact=art,  # type: ignore[arg-type]
                version=contract_version,
            )
        except ContractError as exc:
            _fail(
                str(exc),
                code="contract_export_failed",
                command=command,
                output_format=output_format,
                started_at=started_at,
            )
        if output_format.lower() == "json":
            Envelope.success(
                data={
                    "interface": iface,
                    "version": resolved,
                    "artefact": art,
                    "file": filename,
                    "digest": sha256_digest(raw_bytes),
                    "payload": json.loads(raw_bytes.decode("utf-8")),
                },
                command=command,
                started_at=started_at,
            ).emit()
        else:
            stdout_buf = getattr(sys.stdout, "buffer", None)
            if stdout_buf is not None:
                stdout_buf.write(raw_bytes)
                stdout_buf.flush()
            else:
                sys.stdout.write(raw_bytes.decode("utf-8"))
                sys.stdout.flush()
        raise SystemExit(EXIT_OK)

    try:
        result = export_contract(
            iface,  # type: ignore[arg-type]
            output_path,
            version=contract_version,
            artefact=art,  # type: ignore[arg-type]
        )
    except (ContractError, OSError) as exc:
        _fail(
            str(exc),
            code="contract_export_failed",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    if output_format.lower() == "json":
        Envelope.success(data=result, command=command, started_at=started_at).emit()
    elif art == "bundle":
        click.echo(
            f"[OK] Exported {iface} contract v{result['version']} bundle to {result['output_dir']}"
        )
    else:
        click.echo(
            f"[OK] Exported {iface} contract v{result['version']} {art} "
            f"to {result['path']} ({result['digest']})"
        )
    raise SystemExit(EXIT_OK)


@contracts_group.command(name="publish")
@click.option(
    "--output-dir",
    "-o",
    "output_dir",
    type=click.Path(file_okay=False, path_type=Path),
    required=True,
    help="Target contracts directory (for example _site/contracts).",
)
@click.option(
    "--base-url",
    default=DEFAULT_BASE_URL,
    show_default=True,
    help="Base URL recorded in generated index.json files.",
)
@click.option(
    "--preserve-from",
    type=click.Path(exists=True, file_okay=False, path_type=Path),
    default=None,
    help="Existing contracts directory whose historical releases are preserved.",
)
@click.option(
    "--validate/--no-validate",
    default=True,
    show_default=True,
    help="Validate schemas, digests, latest aliases, and links after publishing.",
)
@click.option(
    "--format",
    "output_format",
    type=_FORMAT_CHOICE,
    default="text",
    show_default=True,
    help="Output format. Choices: text, json.",
)
def publish_cmd(
    output_dir: Path,
    base_url: str,
    preserve_from: Path | None,
    validate: bool,
    output_format: str,
) -> None:
    """Materialise versioned and latest forcing/output contracts for static hosting."""
    started_at = _now_iso()
    command = "suews contracts publish"
    try:
        report = publish_contracts_site(
            output_dir,
            base_url=base_url,
            preserve_from=preserve_from,
            validate=validate,
        )
    except (ContractError, ContractPublicationError, OSError) as exc:
        _fail(
            str(exc),
            code="contract_publish_failed",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    if output_format.lower() == "json":
        Envelope.success(data=report, command=command, started_at=started_at).emit()
    else:
        click.echo(f"[OK] Published contracts to {report['output_dir']}")
        for iface, details in report["interfaces"].items():
            click.echo(
                f"  - {iface}: current={details['current_version']}, "
                f"versions={len(details['versions'])}, "
                f"preserved={len(details['preserved_versions'])}"
            )
        if report["validated"] and report["validation"] is not None:
            val = report["validation"]
            click.echo(
                f"[OK] Validated {val['html_files_checked']} HTML page(s) and "
                f"{val['relative_links_checked']} relative link(s)"
            )
    raise SystemExit(EXIT_OK)


@contracts_group.command(name="validate")
@click.argument(
    "contracts_dir",
    type=click.Path(exists=True, file_okay=False, path_type=Path),
)
@click.option(
    "--base-url",
    default=None,
    help="Optional base URL that published index.json URLs must match.",
)
@click.option(
    "--format",
    "output_format",
    type=_FORMAT_CHOICE,
    default="text",
    show_default=True,
    help="Output format. Choices: text, json.",
)
def validate_cmd(
    contracts_dir: Path,
    base_url: str | None,
    output_format: str,
) -> None:
    """Validate a built contracts directory for schemas, digests, and links."""
    started_at = _now_iso()
    command = "suews contracts validate"
    try:
        report = validate_published_contracts(contracts_dir, base_url=base_url)
    except (ContractError, ContractPublicationError, OSError) as exc:
        _fail(
            str(exc),
            code="contract_validate_failed",
            command=command,
            output_format=output_format,
            started_at=started_at,
        )

    if output_format.lower() == "json":
        Envelope.success(data=report, command=command, started_at=started_at).emit()
    else:
        click.echo(f"[OK] Validated contracts directory: {report['contracts_dir']}")
        for iface, versions in report["interfaces"].items():
            click.echo(f"  - {iface}: {', '.join(versions)}")
        click.echo(
            f"  - HTML pages: {report['html_files_checked']}, "
            f"relative links: {report['relative_links_checked']}"
        )
    raise SystemExit(EXIT_OK)
