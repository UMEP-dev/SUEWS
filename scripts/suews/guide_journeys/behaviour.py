"""Explicit audit of software branches and effects beyond reader coverage."""

import inspect


def removed_paths(before: object, after: object, prefix: str = "") -> list[str]:
    """Find observed removals independently of the planted or expected fields."""
    removed = []
    if isinstance(before, dict) and isinstance(after, dict):
        for key, value in before.items():
            path = f"{prefix}/{key}".lstrip("/")
            if key not in after:
                removed.append(path)
            else:
                removed.extend(removed_paths(value, after[key], path))
    elif isinstance(before, list) and isinstance(after, list):
        for index, value in enumerate(before):
            path = f"{prefix}/{index}".lstrip("/")
            if index >= len(after):
                removed.append(path)
            else:
                removed.extend(removed_paths(value, after[index], path))
    return removed


def migration_case(source: dict, spec: dict, delta: dict) -> tuple[dict, dict]:
    """Plant audit zeros and build the separately declared hand-written expectation."""
    from copy import deepcopy  # noqa: PLC0415 - isolated wheel program

    case = deepcopy(source)
    case["schema_version"] = spec["from_version"]
    cover = case["sites"][0]["properties"]["land_cover"]
    deletions = []
    for effect in spec["undocumented_effects"]:
        for surface in effect["surfaces"]:
            field = effect["field"].format(surface=surface)
            cover[surface]["waterdist"][field] = deepcopy(effect["value"])
            deletions.append((surface, field))
    expected = deepcopy(case)
    expected.update(delta["set"])
    for surface, field in deletions:
        del expected["sites"][0]["properties"]["land_cover"][surface]["waterdist"][
            field
        ]
    for path in delta["remove"]:
        parts = path.split("/")
        parent = expected
        for key in parts[:-1]:
            parent = parent[int(key)] if isinstance(parent, list) else parent[key]
        del parent[parts[-1]]
    return case, expected


def cli_migration(case: dict, expected: dict, spec: dict) -> dict:
    """Run the actual guide command on the same planted input in a fresh directory."""
    import hashlib  # noqa: PLC0415 - isolated wheel program
    import json  # noqa: PLC0415
    from pathlib import Path  # noqa: PLC0415
    import shlex  # noqa: PLC0415
    import subprocess  # noqa: PLC0415

    import yaml  # noqa: PLC0415

    work = Path("behaviour-audit")
    work.mkdir(exist_ok=False)
    source = work / spec["source"]
    source.write_text(yaml.safe_dump(case, sort_keys=False), encoding="utf-8")
    before = hashlib.sha256(source.read_bytes()).hexdigest()
    output = work / spec["output"]
    absent_before = not output.exists()
    result = subprocess.run(
        shlex.split(spec["command"]),
        cwd=work,
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    observed = (
        yaml.safe_load(output.read_text(encoding="utf-8")) if output.is_file() else None
    )
    after = hashlib.sha256(source.read_bytes()).hexdigest()
    return {
        "literal_command": spec["command"],
        "source_lines": spec["source_lines"],
        "argv": result.args,
        "exit_status": result.returncode,
        "stdout": result.stdout,
        "stderr": result.stderr,
        "input_sha256_before": before,
        "input_sha256_after": after,
        "output_absent_before": absent_before,
        "output_sha256": hashlib.sha256(output.read_bytes()).hexdigest()
        if output.is_file()
        else None,
        "observed": observed,
        "passed": result.returncode == 0
        and absent_before
        and before == after
        and json.dumps(expected, sort_keys=True)
        == json.dumps(observed, sort_keys=True),
    }


def audit(spec: dict, delta: dict) -> dict:
    """Compare API and documented CLI output with the same independent expectation."""
    from copy import deepcopy  # noqa: PLC0415 - isolated wheel program
    import json  # noqa: PLC0415
    from pathlib import Path  # noqa: PLC0415

    import yaml  # noqa: PLC0415

    from supy.data_model.configuration.migration import SchemaMigrator  # noqa: PLC0415
    from supy.data_model.core.type import SurfaceType  # noqa: PLC0415

    source = yaml.safe_load(Path(spec["source"]).read_text(encoding="utf-8"))
    migrator = SchemaMigrator()
    partition = {}
    for name, branch in spec["partition"].items():
        detected = migrator.auto_detect_version(branch["indicator"])
        partition[name] = {
            "detected": detected,
            "expected": branch["expected"],
            "passed": detected == branch["expected"],
            "migration_coverage": branch["migration_coverage"],
        }
    case, expected = migration_case(source, spec, delta)
    observed = migrator.migrate(
        deepcopy(case), from_version=spec["from_version"], to_version=spec["to_version"]
    )
    cli = cli_migration(case, expected, spec)
    undocumented = {
        channel: sorted(set(removed_paths(case, value)) - set(delta["remove"]))
        for channel, value in {"api": observed, "cli": cli["observed"]}.items()
    }
    api_passed = json.dumps(expected, sort_keys=True) == json.dumps(
        observed, sort_keys=True
    )
    return {
        "surface_enum_available": [surface.value for surface in SurfaceType],
        "input_version_partition": partition,
        "undocumented_deletions": [
            {
                "path": path,
                "guide_coverage": "missed",
                "observed_through": [
                    channel for channel, paths in undocumented.items() if path in paths
                ],
            }
            for path in sorted(set(undocumented["api"]) | set(undocumented["cli"]))
        ],
        "input": case,
        "expected": expected,
        "observed": observed,
        "api_passed": api_passed,
        "cli": cli,
        "passed": api_passed
        and cli["passed"]
        and all(p["passed"] for p in partition.values()),
    }


def inspection_program() -> str:
    """Capture production logging separately from the audit's structured evidence."""
    return (
        "import contextlib,io,json,sys\n"
        + "\n".join(
            inspect.getsource(f)
            for f in (removed_paths, migration_case, cli_migration, audit)
        )
        + "\nlog=io.StringIO()\nwith contextlib.redirect_stdout(log):\n    result=audit(*json.loads(sys.argv[1]))\n"
        + "result['stdout']=log.getvalue()\nprint(json.dumps(result))"
    )
