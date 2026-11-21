"""Report execution separately from guide coverage and software behaviour."""

import re

from .inventory import compare, compare_values, coverage


def summarise(manifest: dict, records: list[dict], stale: list[str]) -> dict:
    """Summarise preserved observations without counting probes as guide instructions."""
    result = coverage(manifest, records, stale)
    documented = {step["id"] for step in manifest["steps"]}
    result["executed_steps"] = sorted(documented.intersection(result["executed_steps"]))
    result["executed_step_count"] = len(result["executed_steps"])
    result["choice_counts"] = {
        status: sum(c["status"] == status for c in result["choices"])
        for status in ("exercised", "missed", "stale", "excluded")
    }
    result["documented_choices"] = len(result["choices"])
    result["journeys"] = [
        {
            "id": r["journey_id"],
            "normal_status": r["status"],
            "accepted": r.get("accepted", r["status"] == "passed"),
            "replay_passed": r.get("diagnostic_replay", {}).get("passed"),
        }
        for r in records
    ]
    evidence = next(
        (
            record
            for record in records
            if all(
                key in record for key in ("inventory", "schema_info", "behaviour_audit")
            )
        ),
        None,
    )
    if evidence is None:
        result["behaviour_audit"] = {
            "status": "unrun",
            "reason": "Select tagged-dev11 to run the installed software audit.",
        }
        result["inventory"] = {"status": "unrun"}
        return result
    result["inventory"] = compare(evidence["inventory"], manifest["inventory"])
    unrestricted_coverage(result["inventory"], records, evidence["inventory"])
    info = evidence["schema_info"]
    versions = re.findall(r"(?m)^\s*(?:\u2192\s*)?v([^:\s]+):", info["stdout"])
    result["schema_versions"] = {
        "documented": manifest["schema_versions"],
        "available": versions,
        "unsupported": sorted(set(manifest["schema_versions"]) - set(versions)),
        "source": "installed suews schema info",
        "exit_status": info["exit_status"],
    }
    audit = evidence["behaviour_audit"]
    result["schema_enumerations"] = {
        "surface": compare_values(audit["surface_enum_available"], ["paved", "bldgs"])
    }
    result["behaviour_audit"] = {
        key: value
        for key, value in audit.items()
        if key not in {"expected", "observed", "stdout"}
    }
    result["argument_exclusions"] = manifest["argument_exclusions"]
    return result


def unrestricted_coverage(
    inventory: dict, records: list[dict], installed: dict
) -> None:
    """Attach execution witnesses for unrestricted values, using installed aliases."""
    for command, domains in inventory.items():
        prefix = (
            ["suews", "schema", "migrate"]
            if command == "migrate"
            else ["suews", command]
        )
        for option, coverage_item in domains["unrestricted"].items():
            aliases = installed[command]["options"][option]["aliases"]
            witnesses = option_witnesses(records, prefix, aliases)
            coverage_item["exercised"] = {
                value: witnesses[value]
                for value in coverage_item["documented"]
                if value in witnesses
            }
            coverage_item["missed"] = [
                value for value in coverage_item["documented"] if value not in witnesses
            ]


def option_witnesses(
    records: list[dict], prefix: list[str], aliases: list[str]
) -> dict:
    """Find literal option values in commands that actually ran."""
    witnesses = {}
    for record in records:
        for step in record["steps"]:
            argv = step.get("argv", [])
            if argv[: len(prefix)] != prefix:
                continue
            for index, token in enumerate(argv[:-1]):
                if token in aliases:
                    witnesses.setdefault(argv[index + 1], []).append(
                        record["journey_id"] + ":" + step["id"]
                    )
    return witnesses
