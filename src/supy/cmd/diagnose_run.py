"""``suews diagnose`` — run diagnostic checks against a finished SUEWS run.

This subcommand is a thin CLI wrapper over ``supy.diagnostics.check_run``;
the heavy lifting lives there so that MCP tools and downstream callers
can use the same checks without re-implementing them.
"""

from __future__ import annotations

import json
from pathlib import Path
import sys
from typing import Any

import click

from ..diagnostics import check_run
from .json_envelope import Envelope, _now_iso


def _summarise(list_results: list[Any]) -> dict[str, int]:
    counts = {"n_pass": 0, "n_warn": 0, "n_fail": 0}
    for res in list_results:
        sev = res.severity
        if sev == "pass":
            counts["n_pass"] += 1
        elif sev == "warning":
            counts["n_warn"] += 1
        elif sev == "fail":
            counts["n_fail"] += 1
    return counts


def _build_recommendations(list_results: list[Any]) -> list[str]:
    """Map check failures to short, actionable recommendations."""
    list_recommendations: list[str] = []
    for res in list_results:
        if res.passed:
            continue
        if res.name == "provenance_present":
            list_recommendations.append(
                "Preserve the run command, configuration path, and git commit "
                "in a provenance.json sidecar before archiving the run."
            )
        elif res.name == "output_files_present":
            list_recommendations.append(
                "Confirm the run actually finished: check stderr / Fortran log "
                "for errors before diagnosing."
            )
        elif res.name == "nan_proportion":
            list_recommendations.append(
                "Inspect forcing-file gaps (Tair / Kdown / RH) and rerun once "
                "gaps are filled."
            )
        elif res.name == "energy_balance_closure":
            list_recommendations.append(
                "Review storage_heat / emissions physics options and check "
                "land-cover fractions sum to 1.0."
            )
    return list_recommendations


def _build_text_message(data: dict[str, Any]) -> str:
    summary = data["summary"]
    lines = [
        f"Run directory: {data['run_dir']}",
        "",
        (
            f"Checks: {summary['n_pass']} pass / "
            f"{summary['n_warn']} warn / {summary['n_fail']} fail"
        ),
    ]
    for check in data["checks"]:
        lines.append(
            f"  [{check['severity'].upper()}] {check['name']} - {check['message']}"
        )
    if data.get("recommendations"):
        lines.append("")
        lines.append("Recommendations:")
        for rec in data["recommendations"]:
            lines.append(f"  - {rec}")
    return "\n".join(lines)


@click.command(
    name="diagnose",
    short_help="Diagnose a finished SUEWS run directory.",
    help=(
        "Read the contents of a SUEWS run directory and run a battery of "
        "Phase-1 checks (provenance present, output files present, NaN "
        "proportion in QH/QE/QN, energy-balance closure). Use --format json "
        "for structured output consumable by MCP tooling."
    ),
)
@click.argument(
    "run_dir",
    type=click.Path(exists=True, file_okay=False, dir_okay=True),
)
@click.option(
    "--format",
    "output_format",
    type=click.Choice(["text", "json"], case_sensitive=False),
    default="text",
    show_default=True,
    help="Output format. 'json' emits the standard SUEWS envelope on stdout.",
)
def diagnose_run_cmd(run_dir: str, output_format: str) -> None:
    """Diagnose a saved SUEWS run directory."""
    started_at = _now_iso()
    json_mode = output_format.lower() == "json"
    command = " ".join(["suews", "diagnose", *sys.argv[1:]])

    path_run_dir = Path(run_dir)

    list_results = check_run(path_run_dir)
    list_check_dicts = [res.to_dict() for res in list_results]
    summary = _summarise(list_results)
    list_recommendations = _build_recommendations(list_results)

    data: dict[str, Any] = {
        "run_dir": str(path_run_dir),
        "checks": list_check_dicts,
        "summary": summary,
        "recommendations": list_recommendations,
    }

    list_warnings = [res.message for res in list_results if res.severity == "warning"]

    # Sidecar: write diagnostics.json next to the run output. Required by the
    # gh#1361 acceptance criteria. Failure to write (e.g. read-only mount)
    # degrades to an envelope warning rather than a hard error so 'diagnose'
    # remains usable on CI artefact trees.
    if path_run_dir.is_dir():
        path_sidecar = path_run_dir / "diagnostics.json"
        try:
            path_sidecar.write_text(
                json.dumps(data, indent=2, ensure_ascii=False),
                encoding="utf-8",
            )
        except OSError as exc:
            list_warnings.append(f"could not write diagnostics.json sidecar: {exc}")

    if json_mode:
        # Map a hard 'fail' onto the envelope error channel so MCP tooling can
        # short-circuit; warnings remain structured but non-fatal.
        list_errors = [res.message for res in list_results if res.severity == "fail"]
        if list_errors:
            Envelope.error(
                errors=list_errors,
                command=command,
                data=data,
                warnings=list_warnings or None,
                started_at=started_at,
            ).emit()
        else:
            Envelope.success(
                data=data,
                command=command,
                warnings=list_warnings or None,
                started_at=started_at,
            ).emit()
    else:
        click.echo(_build_text_message(data))
