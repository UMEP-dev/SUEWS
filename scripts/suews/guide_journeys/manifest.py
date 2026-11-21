"""Validate reviewed bindings and fail closed on stale or unbound instructions."""

from copy import deepcopy
import hashlib
from pathlib import Path

import yaml

from .extractor import Document, digest


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
    positions = [
        (
            document.span(bindings[key]).first,
            bindings[key].get("statement", bindings[key].get("command", 0)),
        )
        for key in selected
    ]
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
    raw = path.read_text(encoding="utf-8")
    data = yaml.safe_load(raw)
    if data.get("binding_mode", "committed") not in {"committed", "fresh"}:
        raise ValueError("Unsupported binding mode")
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
    data["_manifest_text"] = raw
    data["_manifest_sha256"] = hashlib.sha256(raw.encode("utf-8")).hexdigest()
    return data


def check(document: Document, manifest: dict, current_revision: str) -> list[str]:
    """Return all stale bindings, with reviewed and current revisions attached."""
    errors = []
    bindings = manifest["steps"] + manifest["conditions"] + manifest["exclusions"]
    fresh = manifest.get("binding_mode", "committed") == "fresh"
    if fresh:
        errors.extend(check_fresh(document, manifest, current_revision))
    else:
        declared = {entry.get("anchor") for entry in bindings}
        if declared != set(document.anchors):
            errors.append(
                f"Anchor inventory differs: {declared ^ set(document.anchors)}"
            )
    if digest(document.envelope.text) != manifest["envelope"]["sha256"]:
        errors.append("Section envelope changed")
    condition_ids = {entry["id"] for entry in manifest["conditions"]}
    for step in manifest["steps"]:
        if not set(step.get("conditions", [])).issubset(condition_ids):
            errors.append(f"Unknown condition on {step['id']}")
    for entry in bindings:
        if not fresh and not entry.get("reviewed_revision"):
            errors.append(f"Missing reviewed revision: {entry['id']}")
        try:
            if digest(document.span(entry).text) != entry["sha256"]:
                errors.append(f"Span changed: {entry['id']}")
        except (KeyError, ValueError, IndexError) as error:
            errors.append(f"Binding {entry['id']}: {error}")
    errors.extend(unbound_instructions(document, manifest))
    errors.extend(check_install_exclusions(document, manifest))
    if fresh:
        return [f"{error} (extracted; current {current_revision})" for error in errors]
    reviewed = manifest["envelope"]["reviewed_revision"]
    return [
        f"{error} (reviewed {reviewed}; current {current_revision})" for error in errors
    ]


def step_coverage(document: Document, manifest: dict) -> dict:
    """Count extracted instructions separately from those carrying a binding."""
    documented = bound = 0
    for node, kind in document.candidates():
        exclusions = [e for e in manifest["exclusions"] if document.owns(e, node, kind)]
        if any(
            e.get("kind") in {"example-data", "installation"}
            or (kind == "inline" and e.get("inline") == node.rawsource)
            for e in exclusions
        ):
            continue
        entries = [e for e in manifest["steps"] if document.owns(e, node, kind)]
        if kind == "inline":
            documented += 1
            bound += any(e.get("inline") == node.rawsource for e in entries)
        else:
            spans = document.statements(node)
            documented += len(spans)
            bound += sum(
                any(
                    e.get(
                        "statement"
                        if document.language(node) == "python"
                        else "command"
                    )
                    == i
                    for e in entries
                )
                for i in range(len(spans))
            )
    return {"documented_steps": documented, "bound_steps": bound}


def check_fresh(document: Document, manifest: dict, revision: str) -> list[str]:
    """Require an extraction identity, never a fabricated review stamp."""
    errors = []
    if manifest.get("source_blob") != document.source_blob:
        errors.append("Page git blob changed")
    stamp = manifest.get("extraction_stamp", {})
    if (
        stamp.get("revision") != revision
        or not stamp.get("extracted_at")
        or stamp.get("extractor") != "rst-spans-v1"
    ):
        errors.append("Missing or mismatched extraction stamp")
    if contains_review(manifest):
        errors.append("Fresh bindings cannot carry a reviewed field")
    for entry in manifest["steps"] + manifest["conditions"] + manifest["exclusions"]:
        if "anchor" in entry or "node" not in entry:
            errors.append("Fresh bindings require node coordinates")
    return errors


def contains_review(value: object) -> bool:
    """Reject review claims anywhere in a fresh manifest."""
    if isinstance(value, dict):
        return any(
            "reviewed" in str(key) or contains_review(item)
            for key, item in value.items()
        )
    return isinstance(value, list) and any(contains_review(item) for item in value)


def unbound_instructions(document: Document, manifest: dict) -> list[str]:
    """Report every instruction missing from the declared steps and exclusions."""
    errors = []
    for node, kind in document.candidates():
        entries = [
            entry
            for entry in manifest["steps"] + manifest["exclusions"]
            if document.owns(entry, node, kind)
        ]
        if kind == "inline":
            covered = any(entry.get("inline") == node.rawsource for entry in entries)
        elif any(
            entry.get("kind") in {"example-data", "installation"} for entry in entries
        ):
            covered = True
        else:
            covered = {
                entry.get(
                    "statement" if document.language(node) == "python" else "command"
                )
                for entry in entries
            } == set(range(len(document.statements(node))))
        if not covered:
            errors.append(f"Unbound {kind}: {node.rawsource}")
    return errors


def check_install_exclusions(document: Document, manifest: dict) -> list[str]:
    """Require install exclusions to select the actual setup instruction."""
    errors = []
    for entry in manifest["exclusions"]:
        if entry.get("kind") != "installation":
            continue
        install = manifest.get("installation", {})
        coordinates = {k: v for k, v in install.items() if k not in {"guide", "title"}}
        try:
            if (
                install.get("guide") != manifest["guide"]
                or install.get("title") != manifest["title"]
                or document.span(coordinates) != document.span(entry)
            ):
                errors.append("Installation exclusion does not match setup binding")
        except (ValueError, KeyError, IndexError):
            errors.append("Installation exclusion has no valid setup binding")
    return errors
