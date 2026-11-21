"""Report execution separately from guide coverage and software behaviour."""

import re

from .inventory import compare, compare_values, coverage


def summarise(
    manifest: dict, records: list[dict], stale: list[str], step_counts: dict
) -> dict:
    """Summarise preserved observations without counting probes as guide instructions."""
    result = coverage(manifest, records, stale, step_counts)
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
    versions = schema_versions(info["stdout"])
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
            if step.get("status") == "unrun":
                continue
            argv = step.get("argv", [])
            if argv[: len(prefix)] != prefix:
                continue
            for index, token in enumerate(argv):
                option, separator, value = token.partition("=")
                if option in aliases:
                    if not separator and index + 1 < len(argv):
                        value = argv[index + 1]
                    if value:
                        witnesses.setdefault(value, []).append(
                            record["journey_id"] + ":" + step["id"]
                        )
    return witnesses


def schema_versions(stdout: str) -> list[str]:
    """Read the public version list, excluding headings and current-version prose."""
    return re.findall(r"(?m)^\s*(?:\u2192\s*)?v([^:\s]+):", stdout)


def failed_summary(summary: dict) -> bool:
    """Fail required inventory evidence, including unexercised free-form values."""
    inventory_stale = any(
        domain["option_names"]["stale"]
        or domain["option_names"]["missed"]
        or any(values["stale"] for values in domain["enumerated_values"].values())
        or any(values["missed"] for values in domain["unrestricted"].values())
        for domain in summary.get("inventory", {}).values()
        if isinstance(domain, dict)
    )
    return bool(
        any(not r["accepted"] for r in summary["journeys"])
        or inventory_stale
        or summary.get("schema_versions", {}).get("unsupported")
        or summary.get("schema_versions", {}).get("exit_status", 0)
        or summary.get("behaviour_audit", {}).get("passed") is False
    )
