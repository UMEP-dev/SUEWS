"""Command-line runner; every invocation supplies its document revision explicitly."""

import argparse
from contextlib import ExitStack
from dataclasses import asdict
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

import yaml

from . import behaviour, inventory
from .environment import execute, isolated_python, prepare
from .extractor import Document, digest, shell_spans
from .fixtures import fixture_bytes
from .manifest import (
    check,
    load,
    step_coverage,
    validate_commands,
    validate_order,
    validate_probes,
)
from .oracle import evaluate, require_provenance, sha256, snapshot
from .python_session import PythonSession
from .record import new_record, unrun_step, write
from .replays import assess_replay
from .state_replay import observe_replay
from .summary import failed_summary, summarise

FORCING_SETUP = """
from importlib.resources import files
from pathlib import Path
import hashlib,json,sys
spec = json.loads(sys.argv[1])
resource = files(spec['package']).joinpath(*spec['path'].split('/'))
data = resource.read_bytes()
Path(spec['target']).write_bytes(data)
print(json.dumps({'resource':spec['package']+'/'+spec['path'],
                  'sha256':hashlib.sha256(data).hexdigest()}))
"""


def read_page(root: Path, revision: str, path: str) -> str:
    """Read committed page bytes at the stated revision, never the working tree."""
    return subprocess.check_output(
        ["git", "show", f"{revision}:{path}"], cwd=root
    ).decode("utf-8")


class StaleInstallation(ValueError):
    """The installation instruction no longer matches its reviewed hash."""


def resolve_install(root: Path, revision: str, binding: dict) -> dict:
    """Extract and checksum the install instruction at the selected revision."""
    allowed = {
        "guide",
        "title",
        "block",
        "node",
        "anchor",
        "command",
        "span_type",
        "sha256",
    }
    if set(binding) - allowed or type(binding["command"]) is not int:
        raise ValueError("Install binding cannot supply executable replacement text")
    text = read_page(root, revision, binding["guide"])
    document = Document(text, binding["title"])
    coordinates = {
        key: value
        for key, value in binding.items()
        if key not in {"guide", "title", "block"}
    }
    if "block" in binding:
        if (
            "node" in binding
            or "anchor" in binding
            or type(binding["block"]) is not int
            or binding["block"] < 0
        ):
            raise ValueError(
                "Install binding requires one nonnegative node or block coordinate"
            )
        blocks = [node for node, kind in document.candidates() if kind == "block"]
        node = blocks[binding["block"]]
        coordinates["node"] = next(
            i
            for i, candidate in enumerate(document.source_nodes())
            if candidate is node
        )
    span = document.span(coordinates)
    if span.span_type != "shell":
        raise ValueError("Installation requires shell source language")
    if digest(span.text) != binding["sha256"]:
        raise StaleInstallation("Installation source changed")
    return {
        "command": span.text,
        "source_lines": [span.first, span.last],
        "guide": binding["guide"],
        "guide_text": text,
        "source_blob": document.source_blob,
        "sha256": binding["sha256"],
    }


def resolve_steps(document: Document, manifest: dict, journey: dict) -> list[dict]:
    """Resolve documented spans; only explicitly labelled probes carry commands."""
    bindings = {entry["id"]: entry for entry in manifest["steps"]}
    validate_commands(journey["steps"])
    validate_probes(journey["steps"], set(bindings))
    validate_order(document, manifest, journey)
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
                span_type=span.span_type,
                source_path=manifest["guide"],
                consumes=binding["consumes"],
                reviewed_revision=binding.get("reviewed_revision"),
                span_sha256=binding["sha256"],
            )
        result.append(spec)
    return result


def historical_steps(text: str, manifest: dict, journey: dict) -> list[dict]:
    """Use pinned historical block spans without modifying the vendored guide."""
    history = manifest["historical"]
    validate_commands(journey["steps"])
    document = Document(text, manifest["title"])
    block = document.candidates()[history["block"]][0]
    raw = document.raw(block)
    spans = shell_spans(raw.text, raw.first + history["source_first_line"] - 1)
    indices = [entry["command_index"] for entry in journey["steps"]]
    if any(index < 0 or index >= len(spans) for index in indices):
        raise IndexError("Historical command index is outside the source block")
    if indices != list(range(len(spans))):
        raise ValueError("Historical steps must run in source order without omissions")
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
    session = PythonSession(work, env)
    try:
        return execute_steps(specs, work, env, pinned, session)
    finally:
        session.close()


def execute_steps(
    specs: list[dict], work: Path, env: dict, pinned: dict, session: PythonSession
) -> tuple[list[dict], str | None]:
    """Keep observations out of any reader with documented steps still to run."""
    validate_probes(specs, {s["id"] for s in specs if s["kind"] == "documented"})
    last_documented = max(
        (i for i, spec in enumerate(specs) if spec["kind"] == "documented"),
        default=-1,
    )
    with ExitStack() as stack:
        seed = None
        if any(
            p["kind"] in {"memory", "physical"}
            for spec in specs[:last_documented]
            for p in spec["promises"]
        ):
            temporary = stack.enter_context(tempfile.TemporaryDirectory())
            seed = Path(temporary) / "initial"
            shutil.copytree(work, seed)
        return execute_reader_steps(
            specs, work, env, pinned, session, last_documented, seed
        )


def execute_reader_steps(
    specs: list[dict],
    work: Path,
    env: dict,
    pinned: dict,
    session: PythonSession,
    last_documented: int,
    seed: Path | None,
) -> tuple[list[dict], str | None]:
    """Execute source spans and route early state promises to independent replays."""
    certified = dict(pinned)
    records = []
    stopped = False
    consumer = None
    missing = set()
    state_failed = False
    for index, spec in enumerate(specs):
        record = unrun_step(spec)
        records.append(record)
        if stopped:
            if consumer is None and (
                missing.intersection(spec.get("consumes", []))
                or (state_failed and spec["kind"] == "documented")
            ):
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
        process = (
            session.execute(spec)
            if spec.get("span_type") == "python"
            else execute(spec["command"], work, env)
        )
        after = snapshot(work)
        record.update(process, after=after)
        promises = [
            observe_replay(p, specs[: index + 1], seed, work, env)
            if index < last_documented and p["kind"] in {"memory", "physical"}
            else evaluate(p, before, after, work, process, session)
            for p in spec["promises"]
        ]
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
        if passed and spec["kind"] == "documented":
            for promise in spec["promises"]:
                if promise["kind"] == "creates":
                    certified[promise["path"]] = after[promise["path"]]
        elif not passed:
            stopped = True
            state_failed = any(
                not p["passed"] and p["expected"]["kind"] in {"memory", "physical"}
                for p in promises
            )
            missing = {
                p["expected"]["path"]
                for p in promises
                if not p["passed"] and p["expected"]["kind"] == "creates"
            }
    return records, consumer


def journey_passed(steps: list[dict], outcomes: list[dict]) -> bool:
    """Decide acceptance from documented steps and required probes only."""
    return all(
        s["status"] == "passed"
        for s in steps
        if not (s["kind"] == "probe" and s.get("probe_kind") == "diagnostic")
    ) and all(p["passed"] for p in outcomes)


def run_diagnostic(spec: dict, work: Path, env: dict) -> dict:
    """Observe a terminal diagnostic without changing the reader's outcome files."""
    try:
        with tempfile.TemporaryDirectory() as temporary:
            copy = Path(temporary) / "work"
            shutil.copytree(work, copy)
            records, _ = run_steps([spec], copy, env, snapshot(copy))
            return records[0]
    except (ValueError, OSError, RuntimeError, subprocess.SubprocessError) as error:
        record = unrun_step(spec)
        record.update(status="failed", stderr=str(error))
        return record


def run_required_probe(
    probe: dict,
    steps: list[dict],
    work: Path,
    env: dict,
    pinned: dict,
    session: PythonSession,
) -> dict:
    """Retain the reader's stop boundary and certificates for a terminal probe."""
    if not all(step["status"] == "passed" for step in steps):
        return unrun_step(probe)
    certified = dict(pinned)
    for step in steps:
        for promise in step["promises"]:
            expected = promise["expected"]
            if expected["kind"] == "creates":
                path = expected["path"]
                certified[path] = step["after"][path]
    records, _ = execute_steps([probe], work, env, certified, session)
    return records[0]


def setup_inputs(root: Path, manifest: dict, journey: dict, work: Path) -> dict:
    """Copy only checksum-pinned fixtures; never certify a planted stale output."""
    inputs = {}
    for target, fixture_id in journey["inputs"].items():
        fixture = manifest["fixtures"][fixture_id]
        data = fixture_bytes(root, manifest["fixtures"], fixture_id)
        (work / target).write_bytes(data)
        inputs[target] = fixture["sha256"]
    return inputs


def inspect_installed(
    work: Path, env: dict, manifest: dict, document: Document
) -> dict:
    """Gather the installed CLI inventory and independent migration-effect audit."""
    result = {}
    spec = None
    if manifest.get("behaviour_audit"):
        spec = dict(manifest["behaviour_audit"])
        binding = next(
            (s for s in manifest["steps"] if s["id"] == spec["command_step"]), None
        )
        if binding is None:
            raise ValueError(f"Unknown audit command step: {spec['command_step']}")
        span = document.span(binding)
        spec.update(command=span.text, source_lines=[span.first, span.last])
    if manifest.get("cli_inspection"):
        result["inventory"] = isolated_python(inventory.inspection_program(), work, env)
        result["schema_info"] = execute("suews schema info", work, env)
    if spec is None:
        return result
    result["behaviour_audit"] = isolated_python(
        behaviour.inspection_program(),
        work,
        env,
        [json.dumps([spec, manifest["expected_delta"]])],
    )
    return result


def setup_optional(
    manifest: dict,
    journey: dict,
    work: Path,
    env: dict,
    document: Document,
    record: dict,
) -> list[str]:
    """Run only the migration preparation and inspections the manifest requests."""
    pinned = []
    if forcing := manifest.get("forcing_resource"):
        record["forcing_setup"] = isolated_python(
            FORCING_SETUP, work, env, [json.dumps(forcing)]
        )
        record["input_sha256s"][forcing["target"]] = record["forcing_setup"]["sha256"]
        pinned.append(forcing["target"])
    selected = dict(manifest)
    for key in ("cli_inspection", "behaviour_audit"):
        if journey["id"] != manifest.get(key, {}).get("journey"):
            selected.pop(key, None)
    if selected.get("cli_inspection") or selected.get("behaviour_audit"):
        record.update(inspect_installed(work, env, selected, document))
    for command in manifest.get("cli_inspection", {}).get("commands", []):
        record["installed_help"][command] = execute(command + " --help", work, env)
    return pinned


def bind_journey(
    root: Path, manifest: dict, document: Document, journey: dict, record: dict
) -> list[dict]:
    """Resolve source commands while retaining their separate reviewed revision."""
    text = document.text
    if journey.get("historical"):
        history = manifest["historical"]
        record.update(guide_path=history["path"], revision_reviewed=history["revision"])
        text = read_page(root, record["revision_run"], history["path"])
        if hashlib.sha256(text.encode("utf-8")).hexdigest() != history["sha256"]:
            raise ValueError("Historical guide checksum mismatch")
        record["guide_text"] = text
        specs = historical_steps(text, manifest, journey)
    else:
        record["guide_text"] = text
        specs = resolve_steps(document, manifest, journey)
    return specs


def execute_journey(
    root: Path,
    manifest: dict,
    document: Document,
    journey: dict,
    args: argparse.Namespace,
    record: dict,
) -> None:
    """Populate installation, step and outcome observations for one journey."""
    specs = bind_journey(root, manifest, document, journey, record)
    record["steps"] = [unrun_step(spec) for spec in specs]
    record["wheel_sha256"] = sha256(args.wheel)
    for fixture_id in journey["inputs"].values():
        fixture = manifest["fixtures"][fixture_id]
        fixture_bytes(root, manifest["fixtures"], fixture_id)
        record["input_sha256s"][fixture_id] = fixture["sha256"]
    install_source = resolve_install(root, args.revision, manifest["installation"])
    record["installation_source"] = install_source
    env, installation = prepare(
        args.output / journey["id"],
        args.wheel,
        args.wheelhouse,
        args.pip_version,
        install_source["command"],
    )
    record.update(installation)
    record["supy_version"] = installation["package_preflight"]["supy_version"]
    work = args.output / journey["id"] / "work"
    record["input_sha256s"] = setup_inputs(root, manifest, journey, work)
    if journey.get("plant_stale"):
        record["planted_file"] = {
            "path": journey["plant_stale"],
            "sha256": record["input_sha256s"][journey["plant_stale"]],
        }
    setup_paths = setup_optional(manifest, journey, work, env, document, record)
    initial = snapshot(work)
    pinned = {
        name: initial[name]
        for name in journey["inputs"]
        if name != journey.get("plant_stale")
    }
    pinned.update({name: initial[name] for name in setup_paths})
    probe = specs[-1] if specs and specs[-1]["kind"] == "probe" else None
    session = PythonSession(work, env)
    try:
        record["steps"], record["first_unrun_consumer"] = execute_steps(
            specs[:-1] if probe else specs, work, env, pinned, session
        )
        final = snapshot(work)
        last = next(
            (s for s in reversed(record["steps"]) if s["status"] != "unrun"), {}
        )
        record["path_outcomes"] = [
            evaluate(p, initial, final, work, last, session)
            for p in journey.get("outcomes", [])
        ]
        if probe and probe["probe_kind"] == "required":
            record["steps"].append(
                run_required_probe(probe, record["steps"], work, env, pinned, session)
            )
    finally:
        session.close()
    if probe and probe["probe_kind"] == "diagnostic":
        record["steps"].append(run_diagnostic(probe, work, env))
    passed = journey_passed(record["steps"], record["path_outcomes"])
    record["status"] = "passed" if passed else "failed"
    record["accepted"] = passed
    if "expected_failure" in journey:
        consumer = next(
            s for s in specs if s["id"] == journey["expected_failure"]["consumer"]
        )
        assess_replay(record, journey, consumer, work, env)


def run_journey(
    root: Path,
    manifest: dict,
    document: Document,
    journey: dict,
    args: argparse.Namespace,
) -> dict:
    """Install, execute and record one independent reader path."""
    record = new_record(manifest, args.revision, journey, "")
    started = time.monotonic()
    try:
        execute_journey(root, manifest, document, journey, args, record)
    except StaleInstallation as error:
        record.update(status="stale", accepted=False, failure=str(error))
    except (
        ValueError,
        OSError,
        RuntimeError,
        KeyError,
        IndexError,
        TypeError,
        yaml.YAMLError,
        subprocess.SubprocessError,
        StopIteration,
    ) as error:
        record["status"] = "failed"
        record["accepted"] = False
        record["failure"] = f"{type(error).__name__}: {error}"
    record["seconds"] = time.monotonic() - started
    write(args.output / (journey["id"] + ".json"), record)
    return record


def arguments() -> argparse.Namespace:
    """Parse arguments and reserve a fresh evidence directory."""
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
    if not args.check:
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
    return args


def bind_run(root: Path, args: argparse.Namespace) -> tuple:
    """Resolve the requested revision and validate the selected source bindings."""
    manifest = load(args.manifest)
    args.revision = (
        subprocess
        .check_output(
            ["git", "rev-parse", "--verify", f"{args.revision}^{{commit}}"],
            cwd=root,
        )
        .decode("ascii")
        .strip()
    )
    text = read_page(root, args.revision, manifest["guide"])
    document = Document(text, manifest["title"])
    stale = check(document, manifest, args.revision)
    try:
        resolve_install(root, args.revision, manifest["installation"])
    except StaleInstallation as error:
        stale.append(str(error))
    manifest["source_blob"] = document.source_blob
    selected = set(args.journey or [j["id"] for j in manifest["journeys"]])
    if selected - {j["id"] for j in manifest["journeys"]}:
        raise ValueError("Unknown journey ID")
    return manifest, text, document, stale, selected


def main() -> int:
    """Run source checks or every manifest journey, with evidence outside checkout."""
    args = arguments()
    root = Path.cwd().resolve()
    try:
        manifest, text, document, stale, selected = bind_run(root, args)
    except (
        ValueError,
        OSError,
        KeyError,
        IndexError,
        TypeError,
        yaml.YAMLError,
        subprocess.SubprocessError,
    ) as error:
        record = new_record(
            {"guide": str(args.manifest), "envelope": {"reviewed_revision": None}},
            args.revision,
            {"id": "setup"},
            "",
        )
        record.update(
            status="failed", accepted=False, failure=f"{type(error).__name__}: {error}"
        )
        if not args.check:
            write(args.output / "setup.json", record)
        print(json.dumps(record, indent=2))
        return 1
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
    if stale:
        record = new_record(manifest, args.revision, {"id": "staleness"}, text)
        record.update(status="stale", failure=stale)
        write(args.output / "staleness.json", record)
        return 1
    (args.output / "manifest.yml").write_text(
        manifest["_manifest_text"], encoding="utf-8"
    )
    records = []
    for journey in manifest["journeys"]:
        if journey["id"] in selected:
            record = run_journey(root, manifest, document, journey, args)
            records.append(record)
            print(f"{journey['id']}: {record['status']}", flush=True)
    summary = summarise(manifest, records, stale, step_coverage(document, manifest))
    write(args.output / "summary.json", summary)
    print(json.dumps(summary, indent=2, ensure_ascii=True))
    return failed_summary(summary)


if __name__ == "__main__":
    raise SystemExit(main())
