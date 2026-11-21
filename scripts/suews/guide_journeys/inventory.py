"""Separate option names, enumerated values and unrestricted reader inputs."""

import inspect


def installed_commands() -> dict:
    """Inspect public Click parameters, excluding hidden compatibility switches."""
    import click  # noqa: PLC0415 - inspection runs inside the installed wheel venv

    from supy.cmd.schema_cli import cli as schema  # noqa: PLC0415
    from supy.cmd.table_converter import convert_table_cmd  # noqa: PLC0415
    from supy.cmd.validate_config import cli as validate  # noqa: PLC0415

    result = {}
    for name, command in {
        "migrate": schema.commands["migrate"],
        "validate": validate,
        "convert": convert_table_cmd,
    }.items():
        context = click.Context(command)
        params = {}
        arguments = []
        for param in command.get_params(context):
            if isinstance(param, click.Argument):
                arguments.append({"name": param.name, "multiple": param.nargs == -1})
            elif not param.hidden:
                canonical = next(
                    (o for o in param.opts if o.startswith("--")), param.opts[0]
                )
                params[canonical] = {
                    "aliases": param.opts + param.secondary_opts,
                    "values": list(param.type.choices)
                    if isinstance(param.type, click.Choice)
                    else None,
                    "default": param.to_info_dict()["default"],
                    "default_origin": "installed software",
                }
        result[name] = {
            "options": params,
            "arguments": arguments,
            "help": command.get_help(context),
        }
    return result


def inspection_program() -> str:
    """Return the same public inspection function for the isolated wheel process."""
    return (
        "import json\n"
        + inspect.getsource(installed_commands)
        + "\nprint(json.dumps(installed_commands()))"
    )


def compare(installed: dict, declared: dict) -> dict:
    """Compare names and Choice values both ways, never unrestricted literals."""
    result = {}
    for name, actual in installed.items():
        expected = declared[name]
        documented = expected["documented"]
        excluded = expected["excluded"]
        actual_names = set(actual["options"])
        declared_names = set(documented) | set(excluded)
        domains = {
            "option_names": {
                "missed": sorted(actual_names - declared_names),
                "stale": sorted(declared_names - actual_names),
                "excluded": excluded,
            },
            "enumerated_values": {},
            "unrestricted": {},
            "software_defaults": {},
        }
        for option, parameter in actual["options"].items():
            domains["software_defaults"][option] = parameter["default"]
            values = parameter["values"]
            mentioned = documented.get(option, [])
            if values is not None:
                domains["enumerated_values"][option] = {
                    "documented": mentioned,
                    "missed": sorted(set(values) - set(mentioned)),
                    "stale": sorted(set(mentioned) - set(values)),
                    "exclusion_reason": excluded.get(option),
                }
            elif mentioned:
                domains["unrestricted"][option] = {
                    "documented": mentioned,
                    "comparison": "journey evidence, not literal presence in help",
                }
        result[name] = domains
    return result


def historical_journeys(manifest: dict) -> set[str]:
    """Journeys that replay the earlier guide and so are never corrected-guide evidence."""
    return {j["id"] for j in manifest["journeys"] if j.get("historical")}


def coverage(
    manifest: dict, records: list[dict], stale: list[str], step_counts: dict
) -> dict:
    """Keep exercised single choices distinct from dependent combinations."""
    historical = historical_journeys(manifest)
    choices = []
    for choice in manifest["choices"]:
        eligible = (
            records
            if choice.get("historical")
            else [r for r in records if r["journey_id"] not in historical]
        )
        witnesses = choice_witnesses(manifest, choice, eligible)
        exercised = sorted({w["journey_id"] for w in witnesses})
        status = (
            "stale"
            if stale
            else "excluded"
            if choice.get("reason")
            else "exercised"
            if exercised
            else "missed"
        )
        choices.append(
            dict(choice, status=status, exercised_by=exercised, witnesses=witnesses)
        )
    combinations = {"exercised": [], "untested": []}
    for combination in manifest["combinations"]:
        members = [
            next(c for c in manifest["choices"] if c["id"] == key)
            for key in combination["choices"]
        ]
        witnesses = []
        for record in records:
            if record["journey_id"] not in combination.get("journeys", []):
                continue
            if record["journey_id"] in historical and not combination.get("historical"):
                continue
            observed = [choice_witnesses(manifest, c, [record]) for c in members]
            if observed and all(observed):
                witnesses.extend(w for group in observed for w in group)
        exercised = sorted({w["journey_id"] for w in witnesses})
        combinations["exercised" if exercised else "untested"].append(
            dict(combination, exercised_by=exercised, witnesses=witnesses)
        )
    executed = {
        s["id"]
        for r in records
        if r["journey_id"] not in historical
        for s in r["steps"]
        if s["kind"] == "documented" and s.get("exit_status") is not None
    }
    return {
        **step_counts,
        "executed_steps": sorted(executed),
        "choices": [c for c in choices if not c.get("historical")],
        "historical_choices": [c for c in choices if c.get("historical")],
        "combinations": {
            k: [c for c in v if not c.get("historical")]
            for k, v in combinations.items()
        },
        "historical_combinations": {
            k: [c for c in v if c.get("historical")] for k, v in combinations.items()
        },
        "promise_edits": manifest["promise_edits"],
        "durations": {
            r["journey_id"]: {
                "journey_seconds": r["seconds"],
                "install_seconds": r.get("installation", {}).get("seconds"),
            }
            for r in records
        },
    }


def compare_values(available: list[str], documented: list[str]) -> dict:
    """Compare an actual enumeration in both directions, never free-form strings."""
    return {
        "documented": documented,
        "missed": sorted(set(available) - set(documented)),
        "stale": sorted(set(documented) - set(available)),
    }


def observed_condition(condition: dict, step: dict, manifest: dict) -> dict | None:
    """Match a declared selector against process arguments or the pre-step snapshot."""
    kind = condition["kind"]
    if kind == "option":
        argv = step.get("argv", [])
        for index, token in enumerate(argv):
            name, separator, value = token.partition("=")
            if name in condition["names"]:
                if not separator and index + 1 < len(argv):
                    value = argv[index + 1]
                if value == condition["value"]:
                    return {"kind": kind, "name": name, "value": value}
    elif kind == "fixture":
        path = condition["path"]
        actual = step.get("before", {}).get(path, {}).get("sha256")
        expected = manifest["fixtures"][condition["fixture"]]["sha256"]
        if actual == expected:
            return {"kind": kind, "path": path, "sha256": actual}
    elif kind == "directory":
        path = condition["path"]
        before = step.get("before", {})
        present = path in before
        if present == condition["present"]:
            return {
                "kind": kind,
                "path": path,
                "present": present,
                "entry": before.get(path),
            }
    elif kind == "omitted_option":
        argv = step.get("argv", [])
        if argv and not any(t.split("=", 1)[0] in condition["names"] for t in argv):
            return {"kind": kind, "absent": condition["names"], "argv": argv}
    else:
        raise ValueError(f"Unsupported witness condition: {kind}")
    return None


def choice_witnesses(manifest: dict, choice: dict, records: list[dict]) -> list[dict]:
    """Credit only a documented process that ran with the selected input condition."""
    selector = choice.get("witness")
    if selector is None:
        return []
    result = []
    for record in records:
        if record["journey_id"] not in choice.get("journeys", []):
            continue
        for step in record["steps"]:
            if (
                step["id"] not in selector["steps"]
                or step.get("kind") != "documented"
                or step.get("exit_status") is None
            ):
                continue
            observed = observed_condition(selector["condition"], step, manifest)
            if observed is not None:
                result.append({
                    "journey_id": record["journey_id"],
                    "step_id": step["id"],
                    "observed_condition": observed,
                    "promises_satisfied": all(
                        p["passed"] for p in step.get("promises", [])
                    ),
                })
    return result
