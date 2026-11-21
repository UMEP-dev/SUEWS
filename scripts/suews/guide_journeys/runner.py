"""Command-line runner; every invocation supplies its document revision explicitly."""

import argparse
from dataclasses import asdict
import json
from pathlib import Path
import subprocess
import time

from . import behaviour, inventory
from .environment import execute, isolated_python, prepare
from .extractor import Document, shell_spans
from .manifest import check, load
from .oracle import evaluate, require_provenance, sha256, snapshot
from .record import new_record, unrun_step, write
from .replays import assess_replay
from .summary import summarise

FORCING_SETUP = """
from importlib.resources import files
from pathlib import Path
import subprocess
import hashlib,json
resource = files('supy') / 'sample_data' / 'Kc_2012_data_60.txt'
data = resource.read_bytes()
Path('Kc_2012_data_60.txt').write_bytes(data)
print(json.dumps({'resource':'supy/sample_data/Kc_2012_data_60.txt',
                  'sha256':hashlib.sha256(data).hexdigest()}))
"""


def resolve_steps(document: Document, manifest: dict, journey: dict) -> list[dict]:
    """Resolve documented spans; only explicitly labelled probes carry commands."""
    bindings = {entry["id"]: entry for entry in manifest["steps"]}
    result = []
    for entry in journey["steps"]:
        spec = dict(entry)
        if "probe" in spec:
            spec.update(kind="probe", source_lines=None, command=spec["probe"])
        else:
            binding = bindings[entry["id"]]
            span = document.span(binding)
            spec.update(
                kind="documented",
                source_lines=[span.first, span.last],
                command=span.text,
                consumes=binding["consumes"],
                reviewed_revision=binding["reviewed_revision"],
                span_sha256=binding["sha256"],
            )
        result.append(spec)
    return result


def historical_steps(text: str, manifest: dict, journey: dict) -> list[dict]:
    """Use pinned historical block spans without modifying the vendored guide."""
    history = manifest["historical"]
    document = Document(text, manifest["title"])
    block = document.candidates()[history["block"]][0]
    raw = document.raw(block)
    spans = shell_spans(raw.text, raw.first + history["source_first_line"] - 1)
    return [
        dict(
            entry,
            command=spans[entry["command_index"]].text,
            source_lines=[
                spans[entry["command_index"]].first,
                spans[entry["command_index"]].last,
            ],
            kind="documented",
            reviewed_revision=history["revision"],
        )
        for entry in journey["steps"]
    ]


def run_steps(
    specs: list[dict], work: Path, env: dict, pinned: dict
) -> tuple[list[dict], str | None]:
    """Stop on the first failed promise and retain each later consumer as unrun."""
    certified = dict(pinned)
    records = []
    stopped = False
    consumer = None
    missing = set()
    for spec in specs:
        record = unrun_step(spec)
        records.append(record)
        if stopped:
            if consumer is None and missing.intersection(spec.get("consumes", [])):
                consumer = spec["id"]
            continue
        before = snapshot(work)
        record["before"] = before
        try:
            require_provenance(spec.get("consumes", []), certified, before)
        except ValueError as error:
            record.update(status="failed", stderr=str(error), after=before)
            stopped = True
            continue
        process = execute(spec["command"], work, env)
        after = snapshot(work)
        record.update(process, after=after)
        promises = [evaluate(p, before, after, work, process) for p in spec["promises"]]
        record["promises"] = promises
        expected_exit = spec.get("exit_status", 0)
        record["exit_assertion"] = {
            "expected": expected_exit,
            "observed": process["exit_status"],
        }
        passed = process["exit_status"] == expected_exit and all(
            p["passed"] for p in promises
        )
        record["status"] = "passed" if passed else "failed"
        if passed:
            for promise in spec["promises"]:
                if promise["kind"] == "creates":
                    certified[promise["path"]] = after[promise["path"]]
        else:
            stopped = True
            missing = {
                p["expected"]["path"]
                for p in promises
                if not p["passed"] and p["expected"]["kind"] == "creates"
            }
    return records, consumer


def setup_inputs(root: Path, manifest: dict, journey: dict, work: Path) -> dict:
    """Copy only checksum-pinned fixtures; never certify a planted stale output."""
    inputs = {}
    for target, fixture_id in journey["inputs"].items():
        fixture = manifest["fixtures"][fixture_id]
        source = root / fixture["path"]
        if sha256(source) != fixture["sha256"]:
            raise ValueError(f"Fixture checksum mismatch: {fixture_id}")
        (work / target).write_bytes(source.read_bytes())
        inputs[target] = fixture["sha256"]
    return inputs


def inspect_installed(work: Path, env: dict) -> dict:
    """Gather the installed CLI inventory and independent migration-effect audit."""
    return {
        "inventory": isolated_python(inventory.inspection_program(), work, env),
        "schema_info": execute("suews schema info", work, env),
        "behaviour_audit": isolated_python(behaviour.inspection_program(), work, env),
    }


def run_journey(
    root: Path,
    manifest: dict,
    document: Document,
    journey: dict,
    args: argparse.Namespace,
) -> dict:
    """Install, execute and record one independent reader path."""
    text = (root / manifest["guide"]).read_text(encoding="utf-8")
    if journey.get("historical"):
        history = manifest["historical"]
        path = root / history["path"]
        if sha256(path) != history["sha256"]:
            raise ValueError("Historical guide checksum mismatch")
        text = path.read_text(encoding="utf-8")
        specs = historical_steps(text, manifest, journey)
    else:
        specs = resolve_steps(document, manifest, journey)
    record = new_record(manifest, args.revision, journey, text)
    if journey.get("historical"):
        record.update(
            guide_path=history["path"],
            revision_reviewed=history["revision"],
            revision_run=history["revision"],
        )
    record["wheel_sha256"] = sha256(args.wheel)
    started = time.monotonic()
    record["steps"] = [unrun_step(spec) for spec in specs]
    try:
        for fixture_id in journey["inputs"].values():
            fixture = manifest["fixtures"][fixture_id]
            record["input_sha256s"][fixture_id] = sha256(root / fixture["path"])
            if record["input_sha256s"][fixture_id] != fixture["sha256"]:
                raise ValueError(
                    f"Fixture checksum mismatch before installation: {fixture_id}"
                )
        env, installation = prepare(
            args.output / journey["id"], args.wheel, args.wheelhouse, args.pip_version
        )
        record.update(installation)
        record["supy_version"] = installation["package_preflight"]["supy_version"]
        work = args.output / journey["id"] / "work"
        record["input_sha256s"] = setup_inputs(root, manifest, journey, work)
        if journey.get("plant_stale"):
            record["planted_file"] = {
                "path": "site.migrated.yml",
                "sha256": record["input_sha256s"]["site.migrated.yml"],
            }
        record["forcing_setup"] = isolated_python(FORCING_SETUP, work, env)
        if journey["id"] == "tagged-dev11":
            record.update(inspect_installed(work, env))
        record["input_sha256s"]["Kc_2012_data_60.txt"] = record["forcing_setup"][
            "sha256"
        ]
        initial = snapshot(work)
        pinned = {
            name: initial[name]
            for name in journey["inputs"]
            if name != "site.migrated.yml"
        }
        pinned["Kc_2012_data_60.txt"] = initial["Kc_2012_data_60.txt"]
        for command in ("suews schema migrate", "suews validate", "suews convert"):
            record["installed_help"][command] = execute(command + " --help", work, env)
        record["steps"], record["first_unrun_consumer"] = run_steps(
            specs, work, env, pinned
        )
        final = snapshot(work)
        last = next(
            (s for s in reversed(record["steps"]) if s["status"] != "unrun"), {}
        )
        record["path_outcomes"] = [
            evaluate(p, initial, final, work, last) for p in journey.get("outcomes", [])
        ]
        passed = all(s["status"] == "passed" for s in record["steps"])
        passed &= all(p["passed"] for p in record["path_outcomes"])
        record["status"] = "passed" if passed else "failed"
        record["accepted"] = passed
        if "expected_failure" in journey:
            assess_replay(record, journey, specs[1], work, env)
    except (ValueError, OSError, RuntimeError, subprocess.SubprocessError) as error:
        record["status"] = "failed"
        record["failure"] = f"{type(error).__name__}: {error}"
    record["seconds"] = time.monotonic() - started
    write(args.output / (journey["id"] + ".json"), record)
    return record


def main() -> int:
    """Run source checks or every manifest journey, with evidence outside checkout."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--manifest", type=Path, default=Path("test/docs/journeys/transition_guide.yml")
    )
    parser.add_argument("--revision", required=True)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--wheel", type=Path)
    parser.add_argument("--wheelhouse", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--pip-version", default="26.3")
    parser.add_argument(
        "--journey", action="append", help="Run only this journey ID; repeatable"
    )
    args = parser.parse_args()
    root = Path.cwd().resolve()
    manifest = load(args.manifest)
    text = (root / manifest["guide"]).read_text(encoding="utf-8")
    document = Document(text, manifest["title"])
    stale = check(document, manifest, args.revision)
    if args.check:
        print(
            json.dumps(
                {
                    "stale": stale,
                    "steps": []
                    if stale
                    else [asdict(document.span(s)) for s in manifest["steps"]],
                },
                indent=2,
            )
        )
        return bool(stale)
    if not all((args.wheel, args.wheelhouse, args.output)):
        parser.error("--wheel, --wheelhouse and --output are required for journeys")
    args.output = args.output.resolve()
    args.wheel = args.wheel.resolve()
    args.wheelhouse = args.wheelhouse.resolve()
    if args.output.is_relative_to(root):
        parser.error("Journey output must be outside the checkout")
    if args.output.exists():
        parser.error(
            "Use a fresh output directory; previous evidence is never overwritten"
        )
    args.output.mkdir(parents=True)
    if stale:
        record = new_record(manifest, args.revision, {"id": "staleness"}, text)
        record.update(status="stale", failure=stale)
        write(args.output / "staleness.json", record)
        return 1
    selected = set(args.journey or [j["id"] for j in manifest["journeys"]])
    if selected - {j["id"] for j in manifest["journeys"]}:
        parser.error("Unknown journey ID")
    records = []
    for journey in manifest["journeys"]:
        if journey["id"] in selected:
            record = run_journey(root, manifest, document, journey, args)
            records.append(record)
            print(f"{journey['id']}: {record['status']}", flush=True)
    summary = summarise(manifest, records, stale)
    write(args.output / "summary.json", summary)
    print(json.dumps(summary, indent=2, ensure_ascii=True))
    inventory_stale = any(
        domain["option_names"]["stale"]
        or domain["option_names"]["missed"]
        or any(values["stale"] for values in domain["enumerated_values"].values())
        for domain in summary.get("inventory", {}).values()
        if isinstance(domain, dict)
    )
    return (
        any(not r.get("accepted", False) for r in records)
        or inventory_stale
        or bool(summary.get("schema_versions", {}).get("unsupported"))
        or summary.get("behaviour_audit", {}).get("passed") is False
    )


if __name__ == "__main__":
    raise SystemExit(main())
