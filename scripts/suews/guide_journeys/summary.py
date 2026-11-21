"""Report execution separately from guide coverage and software behaviour."""

import re

from .inventory import compare, compare_values, coverage, historical_journeys


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
    audit_spec = manifest.get("behaviour_audit")
    cli_spec = manifest.get("cli_inspection")
    result["behaviour_audit"] = {"status": "unrun" if audit_spec else "not_requested"}
    if audit_spec:
        result["behaviour_audit"]["reason"] = (
            f"Select {audit_spec['journey']} to run the installed software audit."
        )
    result["inventory"] = {"status": "unrun" if cli_spec else "not_requested"}
    cli = next((r for r in records if "inventory" in r), None)
    if cli is not None:
        summarise_cli(result, manifest, records, cli)
    evidence = next((r for r in records if "behaviour_audit" in r), None)
    if evidence is not None:
        summarise_audit(result, manifest, evidence["behaviour_audit"])
    return result


def summarise_cli(
    result: dict, manifest: dict, records: list[dict], evidence: dict
) -> None:
    """Compare a requested CLI inventory with its documented choices."""
    result["inventory"] = compare(evidence["inventory"], manifest["inventory"])
    unrestricted_coverage(
        result["inventory"],
        records,
        evidence["inventory"],
        historical_journeys(manifest),
    )
    info = evidence["schema_info"]
    versions = schema_versions(info["stdout"])
    result["schema_versions"] = {
        "documented": manifest["schema_versions"],
        "available": versions,
        "unsupported": sorted(set(manifest["schema_versions"]) - set(versions)),
        "source": "installed suews schema info",
        "exit_status": info["exit_status"],
    }


def summarise_audit(result: dict, manifest: dict, audit: dict) -> None:
    """Keep optional behaviour observations separate from reader coverage."""
    result["schema_enumerations"] = {
        "surface": compare_values(
            audit["surface_enum_available"], manifest["schema_enumerations"]["surface"]
        )
    }
    result["behaviour_audit"] = {
        key: value
        for key, value in audit.items()
        if key not in {"input", "expected", "observed", "stdout", "cli"}
    }
    result["behaviour_audit"]["cli"] = {
        key: value
        for key, value in audit["cli"].items()
        if key not in {"observed", "stdout", "stderr"}
    }
    result["argument_exclusions"] = manifest["argument_exclusions"]


def unrestricted_coverage(
    inventory: dict, records: list[dict], installed: dict, historical: set[str]
) -> None:
    """Attach execution witnesses for unrestricted values, using installed aliases.

    Earlier-guide replays are never witnesses for the corrected guide.
    """
    records = [r for r in records if r["journey_id"] not in historical]
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
    """Find literal option values in documented commands that actually ran."""
    witnesses = {}
    for record in records:
        for step in record["steps"]:
            if step.get("status") == "unrun" or step.get("kind") != "documented":
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
        or summary.get("inventory", {}).get("status") == "unrun"
        or (
            summary.get("behaviour_audit", {}).get("status") != "not_requested"
            and summary.get("behaviour_audit", {}).get("passed") is not True
        )
    )
