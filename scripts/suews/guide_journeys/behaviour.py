"""Explicit audit of software branches and effects beyond reader coverage."""

import inspect


def audit() -> dict:
    """Probe the public migrator with hand-written deltas, independently of its handler."""
    from copy import deepcopy  # noqa: PLC0415 - executed inside the isolated wheel venv
    import json  # noqa: PLC0415 - executed inside the isolated wheel venv
    from pathlib import Path  # noqa: PLC0415 - executed inside the isolated wheel venv

    import yaml  # noqa: PLC0415 - executed inside the isolated wheel venv

    from supy.data_model.configuration.migration import (  # noqa: PLC0415
        SchemaMigrator,
    )
    from supy.data_model.core.type import SurfaceType  # noqa: PLC0415

    source = yaml.safe_load(Path("site.yml").read_text(encoding="utf-8"))
    migrator = SchemaMigrator()
    partition = {}
    for name, indicator in {
        "explicit_older": {"schema_version": "2026.6.dev11"},
        "explicit_current": {"schema_version": "2026.6.dev12"},
        "explicit_newer": {"schema_version": "2099.1"},
        "legacy_version": {"version": "old"},
        "legacy_config_version": {"config_version": "old"},
        "no_indicator": {},
    }.items():
        partition[name] = {
            "detected": migrator.auto_detect_version(indicator),
            "migration_coverage": "missed"
            if name in {"explicit_newer", "legacy_version", "legacy_config_version"}
            else "journeys",
        }
    case = deepcopy(source)
    case["schema_version"] = "2026.6.dev11"
    cover = case["sites"][0]["properties"]["land_cover"]
    deletions = []
    for surface in ("paved", "bldgs", "evetr", "dectr", "grass", "bsoil"):
        field = "to_" + surface
        cover[surface]["waterdist"][field] = {
            "value": 0.0,
            "ref": {"desc": "audit provenance", "ID": "audit"},
        }
        deletions.append((surface, field))
    for surface in ("evetr", "dectr", "grass", "bsoil"):
        cover[surface]["waterdist"]["to_runoff"] = 0.0
        deletions.append((surface, "to_runoff"))
    expected = deepcopy(case)
    expected["schema_version"] = "2026.6.dev12"
    for surface, field in deletions:
        del expected["sites"][0]["properties"]["land_cover"][surface]["waterdist"][
            field
        ]
    # The pinned dev11 journey example has one additional documented drop.
    expected["sites"][0]["properties"]["land_cover"]["paved"]["waterdist"].pop(
        "to_soilstore", None
    )
    observed = migrator.migrate(
        case, from_version="2026.6.dev11", to_version="2026.6.dev12"
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
        "import contextlib,io,json\n"
        + inspect.getsource(audit)
        + "\nlog=io.StringIO()\nwith contextlib.redirect_stdout(log):\n    result=audit()\n"
        + "result['stdout']=log.getvalue()\nprint(json.dumps(result))"
    )
