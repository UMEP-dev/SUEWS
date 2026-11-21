"""Explicit audit of software branches and effects beyond reader coverage."""

import inspect


def audit(spec: dict, delta: dict) -> dict:
    """Probe the public migrator with hand-written deltas, independently of its handler."""
    from copy import deepcopy  # noqa: PLC0415 - executed inside the isolated wheel venv
    import json  # noqa: PLC0415 - executed inside the isolated wheel venv
    from pathlib import Path  # noqa: PLC0415 - executed inside the isolated wheel venv

    import yaml  # noqa: PLC0415 - executed inside the isolated wheel venv

    from supy.data_model.configuration.migration import (  # noqa: PLC0415
        SchemaMigrator,
    )
    from supy.data_model.core.type import SurfaceType  # noqa: PLC0415

    source = yaml.safe_load(Path(spec["source"]).read_text(encoding="utf-8"))
    migrator = SchemaMigrator()
    partition = {}
    for name, branch in spec["partition"].items():
        partition[name] = {
            "detected": migrator.auto_detect_version(branch["indicator"]),
            "expected": branch["expected"],
            "migration_coverage": branch["migration_coverage"],
        }
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
    observed = migrator.migrate(
        case, from_version=spec["from_version"], to_version=spec["to_version"]
    )
    return {
        "surface_enum_available": [surface.value for surface in SurfaceType],
        "input_version_partition": partition,
        "undocumented_deletions": [
            {"surface": s, "field": f, "guide_coverage": "missed"} for s, f in deletions
        ],
        "expected": expected,
        "observed": observed,
        "passed": json.dumps(expected, sort_keys=True)
        == json.dumps(observed, sort_keys=True),
    }


def inspection_program() -> str:
    """Capture production logging separately from the audit's structured evidence."""
    return (
        "import contextlib,io,json,sys\n"
        + inspect.getsource(audit)
        + "\nlog=io.StringIO()\nwith contextlib.redirect_stdout(log):\n    result=audit(*json.loads(sys.argv[1]))\n"
        + "result['stdout']=log.getvalue()\nprint(json.dumps(result))"
    )
