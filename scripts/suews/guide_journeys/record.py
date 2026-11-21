"""Durable JSON records for successful and failed reader journeys."""

import json
from pathlib import Path


def write(path: Path, record: dict) -> None:
    """Atomically replace a JSON record, with full process output retained."""
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".json.tmp")
    temporary.write_text(
        json.dumps(record, indent=2, ensure_ascii=True) + "\n", encoding="utf-8"
    )
    temporary.replace(path)


def new_record(manifest: dict, revision: str, journey: dict, guide_text: str) -> dict:
    """Populate the evidence schema even if environment setup fails early."""
    return {
        "guide_path": manifest["guide"],
        "revision_run": revision,
        "revision_reviewed": manifest["envelope"]["reviewed_revision"],
        "journey_id": journey["id"],
        "choice_ids": journey.get("choices", []),
        "guide_text": guide_text,
        "supy_version": None,
        "wheel_sha256": None,
        "input_sha256s": {},
        "environment_preflight": None,
        "package_preflight": None,
        "wheelhouse": {},
        "installed_help": {},
        "steps": [],
        "path_outcomes": [],
        "status": "unrun",
        "first_unrun_consumer": None,
        "seconds": None,
        "failure": None,
    }


def unrun_step(spec: dict) -> dict:
    """Keep all evidence fields present for instructions that were never run."""
    return {
        "id": spec["id"],
        "kind": spec["kind"],
        "probe_kind": spec.get("probe_kind"),
        "source_lines": spec["source_lines"],
        "literal_command": spec["command"],
        "reviewed_revision": spec.get("reviewed_revision"),
        "span_sha256": spec.get("span_sha256"),
        "status": "unrun",
        "promises": [],
        "before": {},
        "after": {},
        "stdout": "",
        "stderr": "",
        "warnings": [],
        "exit_status": None,
    }
