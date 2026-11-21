"""Validate reviewed bindings and fail closed on stale or unbound instructions."""

from pathlib import Path

import yaml

from .extractor import Document, digest, shell_spans


def load(path: Path) -> dict:
    """Read a journey manifest without constructing arbitrary Python objects."""
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if data.get("format") != 1:
        raise ValueError("Unsupported journey manifest format")
    for journey in data["journeys"]:
        for step in journey["steps"]:
            if "probe" not in step and not step.get("promises"):
                raise ValueError(f"Documented step has no promises: {step['id']}")
        promises = journey.get("outcomes", []) + [
            p for step in journey["steps"] for p in step.get("promises", [])
        ]
        for promise in promises:
            if promise["kind"] == "creates" and not promise.get("delta"):
                raise ValueError("A creates promise requires a content delta")
    return data


def check(document: Document, manifest: dict, current_revision: str) -> list[str]:
    """Return all stale bindings, with reviewed and current revisions attached."""
    errors = []
    bindings = manifest["steps"] + manifest["conditions"] + manifest["exclusions"]
    declared = {entry["anchor"] for entry in bindings}
    if declared != set(document.anchors):
        errors.append(f"Anchor inventory differs: {declared ^ set(document.anchors)}")
    if digest(document.envelope.text) != manifest["envelope"]["sha256"]:
        errors.append("Section envelope changed")
    condition_ids = {entry["id"] for entry in manifest["conditions"]}
    for step in manifest["steps"]:
        if not set(step.get("conditions", [])).issubset(condition_ids):
            errors.append(f"Unknown condition on {step['id']}")
    for entry in bindings:
        if not entry.get("reviewed_revision"):
            errors.append(f"Missing reviewed revision: {entry['id']}")
        try:
            if digest(document.span(entry).text) != entry["sha256"]:
                errors.append(f"Span changed: {entry['id']}")
        except (KeyError, ValueError, IndexError) as error:
            errors.append(f"Binding {entry['id']}: {error}")
    for node, kind in document.candidates():
        owners = [
            key
            for key, value in document.anchors.items()
            if value is node or (kind == "inline" and node.parent is value)
        ]
        entries = [
            entry
            for entry in manifest["steps"] + manifest["exclusions"]
            if entry["anchor"] in owners
        ]
        if kind == "inline":
            covered = any(entry.get("inline") == node.rawsource for entry in entries)
        elif any(entry.get("kind") == "example-data" for entry in entries):
            covered = True
        else:
            covered = {entry.get("command") for entry in entries} == set(
                range(
                    len(shell_spans(document.raw(node).text, document.raw(node).first))
                )
            )
        if not covered:
            errors.append(f"Unbound {kind}: {node.rawsource}")
    reviewed = manifest["envelope"]["reviewed_revision"]
    return [
        f"{error} (reviewed {reviewed}; current {current_revision})" for error in errors
    ]


def step_coverage(document: Document, manifest: dict) -> dict:
    """Count extracted instructions separately from those carrying a binding."""
    documented = bound = 0
    for node, kind in document.candidates():
        owners = {
            key
            for key, value in document.anchors.items()
            if value is node or (kind == "inline" and node.parent is value)
        }
        exclusions = [e for e in manifest["exclusions"] if e["anchor"] in owners]
        if any(
            e.get("kind") == "example-data"
            or (kind == "inline" and e.get("inline") == node.rawsource)
            for e in exclusions
        ):
            continue
        entries = [e for e in manifest["steps"] if e["anchor"] in owners]
        if kind == "inline":
            documented += 1
            bound += any(e.get("inline") == node.rawsource for e in entries)
        else:
            raw = document.raw(node)
            spans = shell_spans(raw.text, raw.first)
            documented += len(spans)
            bound += sum(
                any(e.get("command") == i for e in entries) for i in range(len(spans))
            )
    return {"documented_steps": documented, "bound_steps": bound}
