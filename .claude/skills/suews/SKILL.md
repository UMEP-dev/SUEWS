---
name: suews
description: Use whenever helping with SUEWS or SuPy — preparing or migrating YAML configurations, running urban-climate simulations, validating inputs, diagnosing failed or suspicious runs, comparing scenarios, interpreting energy/water balance outputs, or reviewing simulation setups before publication. Prefers structured CLI/MCP calls (suews validate, suews diagnose, validate_config tool) over guessing parameters or rewriting files.
---

# SUEWS workflow

When helping with SUEWS or SuPy, follow this rule set. The Skill teaches *when*
and *why* to call which tool — actual execution lives in the unified `suews`
CLI and the `suews-mcp` server. Never re-implement validation, schema
checking, or simulation logic in conversation.

1. Prefer YAML configuration. Treat namelist as legacy and only touch it
   during migration via `suews convert`.
2. Never invent parameter values. If a value is missing, ask the user or
   reuse a sample from `suews://examples/{name}` or
   `src/supy/sample_data/sample_config.yml`.
3. Always validate before editing. Call the `validate_config` MCP tool, or
   run `suews validate <config> --format json`. Read the structured output,
   not the prose. Pretty-printed terminal output is for humans; you want the
   JSON envelope (`{status, data, errors, warnings, meta}`).
4. Use structured JSON outputs only. If a CLI lacks `--format json`, flag
   it as a CLI gap rather than scrape prose.
5. Failure-diagnosis order: schema/version → file paths → forcing time
   axis → required forcing variables → land-cover fractions sum to 1 →
   temporal resolution → output directory writable. Stop at the first
   failure and explain it before moving on.
6. Suggest the smallest safe patch. Single-key edits, never wholesale
   rewrites.
7. Never overwrite user files without explicit consent. Prefer creating
   `*.suews-suggested.yml` next to the original.
8. Distinguish four error classes in any reply:
   schema errors / missing input data / unit problems / physically
   suspicious values. Tag each finding with one of these.
9. After running, always read `provenance.json` and `diagnostics.json`
   before claiming success. The MCP resources `suews://runs/{id}/provenance`
   and `suews://runs/{id}/diagnostics` are the canonical way.
10. For publication, route through the `review_config_before_publication`
    MCP prompt. Do not invent a checklist — see
    `references/publication-review-checklist.md`.

## Procedural recipes

These are the standard moves; expand by reading the referenced files.

- **New case from scratch**: `suews init --template simple-urban -o demo`,
  edit, then `suews validate demo/config.yml --format json`. See
  `references/forcing-checklist.md` before adding forcing.
- **Failed run**: `suews validate` → `suews inspect` → `suews diagnose`.
  Read `references/common-errors.md` for the most frequent diagnoses.
- **Namelist → YAML migration**: `suews convert -i RunControl.nml -o config.yml`,
  then validate. See `references/migration-namelist-to-yaml.md`.
- **Output interpretation**: read `references/output-interpretation.md` for
  energy balance, storage heat, anthropogenic heat, Bowen ratio.
- **Scenario comparison**: `suews compare <baseline> <scenario>` — see
  `references/output-interpretation.md` for which metrics to read first.
