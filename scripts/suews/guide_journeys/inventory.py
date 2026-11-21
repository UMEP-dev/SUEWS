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


def coverage(manifest: dict, records: list[dict], stale: list[str]) -> dict:
    """Keep exercised single choices distinct from dependent combinations."""
    attempted = {
        r["journey_id"]
        for r in records
        if any(s["status"] != "unrun" for s in r["steps"])
    }
    choices = []
    for choice in manifest["choices"]:
        exercised = sorted(attempted.intersection(choice.get("journeys", [])))
        status = (
            "stale"
            if stale
            else "excluded"
            if choice.get("reason")
            else "exercised"
            if exercised
            else "missed"
        )
        choices.append(dict(choice, status=status, exercised_by=exercised))
    combinations = {"exercised": [], "untested": []}
    for combination in manifest["combinations"]:
        exercised = sorted(attempted.intersection(combination.get("journeys", [])))
        combinations["exercised" if exercised else "untested"].append(
            dict(combination, exercised_by=exercised)
        )
    executed = {
        s["id"]
        for r in records
        for s in r["steps"]
        if s["kind"] == "documented" and s["status"] != "unrun"
    }
    return {
        "documented_steps": len(manifest["steps"]),
        "bound_steps": len(manifest["steps"]),
        "executed_steps": sorted(executed),
        "choices": choices,
        "combinations": combinations,
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
