"""Workflow prompts exposed by the SUEWS MCP server."""

from __future__ import annotations

COMMON_SAFETY = """Always validate before edits. Do not invent SUEWS parameter
values; state assumptions and ask when scientific judgement is required.
Distinguish schema errors, forcing-data errors, and physical plausibility
warnings. Prefer minimal patches. Never overwrite files unless the user has
explicitly approved that exact destructive action."""

PROMPTS = {
    "review_config_before_run": f"""Review a SUEWS configuration before a model run.

{COMMON_SAFETY}

Workflow:
1. Read the configuration and identify schema version, sites, forcing paths,
   dates, physics options, surface fractions, and output settings.
2. Run `validate_config`.
3. Summarise blocking errors first, then warnings and suspicious values.
4. Suggest the smallest reproducible next step.
""",
    "debug_failed_suews_run": f"""Debug a failed SUEWS run.

{COMMON_SAFETY}

Workflow:
1. Inspect the reported error and locate the config and run directory.
2. Run `validate_config` before changing anything.
3. Check file paths, forcing time axis, required variables, schema version,
   land-cover fractions, temporal resolution, and output directory state.
4. Explain whether the failure is structural, input-data related, or physically
   suspicious.
5. Propose a minimal fix and a command the user can rerun.
""",
    "migrate_namelist_to_yaml": f"""Migrate a legacy SUEWS namelist case to YAML.

{COMMON_SAFETY}

Workflow:
1. Treat namelist input as deprecated legacy material.
2. Use the SUEWS converter rather than manually recreating parameters.
3. Validate the generated YAML through `validate_config`.
4. Explain migration warnings, renamed fields, and any required manual review.
5. Preserve original files and never overwrite the legacy case.
""",
    "prepare_new_suews_case": f"""Prepare a new SUEWS case.

{COMMON_SAFETY}

Workflow:
1. Start from a packaged sample or known-good template.
2. Ask for site-specific scientific inputs instead of inventing them.
3. Validate the draft configuration with `validate_config`.
4. Check forcing availability, dates, units, surface fractions, and physics
   choices before suggesting a run command.
5. Record assumptions needed for reproducibility.
""",
    "compare_scenarios": f"""Compare SUEWS scenario outputs.

{COMMON_SAFETY}

Workflow:
1. Confirm both scenarios are reproducible and identify their configs,
   provenance, forcing, and output periods.
2. Validate configs before interpreting differences.
3. Compare like-for-like periods, variables, and temporal aggregation.
4. Separate numerical differences from physical interpretation.
5. Report uncertainty, assumptions, and the smallest additional check needed.
""",
}


def get_prompt(name: str) -> str:
    """Return a named SUEWS workflow prompt."""
    try:
        return PROMPTS[name]
    except KeyError as exc:
        raise ValueError(f"unknown SUEWS prompt: {name}") from exc
