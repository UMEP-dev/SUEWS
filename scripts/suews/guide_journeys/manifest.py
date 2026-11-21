"""Validate reviewed bindings and fail closed on stale or unbound instructions."""

from copy import deepcopy
from pathlib import Path

import yaml

from .extractor import Document, digest, shell_spans


def validate_probes(steps: list[dict], documented_ids: set[str]) -> None:
    """Keep probes terminal and their identities separate from documented steps."""
    seen = set()
    for index, step in enumerate(steps):
        probe = "probe" in step or step.get("kind") == "probe"
        if probe and step["id"] in documented_ids:
            raise ValueError(f"Probe id collides with documented step: {step['id']}")
        if step["id"] in seen:
            raise ValueError(f"Duplicate step id: {step['id']}")
        seen.add(step["id"])
        if probe and index != len(steps) - 1:
            raise ValueError("A probe may run only as the last step of a journey")
        if probe and step.get("probe_kind") not in {"required", "diagnostic"}:
            raise ValueError("A probe must declare required or diagnostic probe_kind")


def validate_commands(steps: list[dict]) -> None:
    """Accept assertions and source coordinates, never documented command text."""
    allowed = {"id", "promises", "exit_status", "command_index", "consumes"}
    for step in steps:
        if "probe" not in step and set(step) - allowed:
            raise ValueError(
                "Documented steps cannot supply command text or unknown fields"
            )


def validate_order(document: Document, manifest: dict, journey: dict) -> None:
    """Require source order and a bound condition for every omitted instruction."""
    selected = [s["id"] for s in journey["steps"] if "probe" not in s]
    if not selected:
        return
    bindings = {s["id"]: s for s in manifest["steps"]}
    positions = [document.span(bindings[key]).first for key in selected]
    if positions != sorted(positions):
        raise ValueError("Documented steps must run in source order")
    skipped = set(bindings) - set(selected)
    skips = journey.get("skips", {})
    conditions = {c["id"]: c for c in manifest["conditions"]}
    if set(skips) != skipped:
        raise ValueError("Every skipped step must name its branch condition")
    for key, condition in skips.items():
        if condition not in conditions or condition not in bindings[key].get(
            "conditions", []
        ):
            raise ValueError(f"Unknown branch condition for skipped step: {key}")
        document.span(conditions[condition])


def load(path: Path) -> dict:
    """Read a journey manifest without constructing arbitrary Python objects."""
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if data.get("format") != 1:
        raise ValueError("Unsupported journey manifest format")
    for binding in data["steps"]:
        if "command" in binding and type(binding["command"]) is not int:
            raise ValueError("A source command index cannot supply command text")
    for journey in data["journeys"]:
        validate_commands(journey["steps"])
        validate_probes(journey["steps"], {s["id"] for s in data["steps"]})
        for step in journey["steps"]:
            if "probe" not in step and not step.get("promises"):
                raise ValueError(f"Documented step has no promises: {step['id']}")
        promises = journey.get("outcomes", []) + [
            p for step in journey["steps"] for p in step.get("promises", [])
        ]
        for promise in promises:
            if promise.get("delta") == "expected_delta":
                promise["delta"] = deepcopy(data["expected_delta"])
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
