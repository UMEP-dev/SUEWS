# Fresh-site setup — the end-to-end workflow

The full procedure for taking a first-time SUEWS user from a place or site idea
to a validated, diagnosed, clearly qualified run. This is **part of the `/suews`
skill** — the SKILL.md rules (honesty about assumed defaults, parameter
importance, authorised data sources, plain language) all apply here; this file
is the step-by-step orchestration and the user-facing readiness judgement.

Use it whenever a user wants to run SUEWS for a new site, city, neighbourhood,
address, coordinates, campus, or scenario, or says things like "simulate this
place", "make me a SUEWS case for London", "I've never used SUEWS", or "can we
get a reasonable result for this site?".

The aim is not to make the agent sound confident — it is to make the workflow
**auditable**: every value either comes from the user, a SUEWS template, a cited
data source, or an explicitly marked assumption.

## Operating Principles

1. Never invent site parameters silently.
2. Never claim a result is site-specific if forcing data, land cover, or key
   site metadata came from a template or proxy.
3. Start from `init_case`; do not reconstruct a YAML config field by field
   from memory or from `query_knowledge`.
4. Use structured MCP/CLI envelopes, not prose output, for validation and
   diagnosis.
5. Keep edits small and explain why each one is needed.
6. Seed the assumption ledger from `assess_readiness`, then maintain it from the
   first starter config to the final report.
7. Separate "the run completed" from "the result is scientifically usable".
8. Speak plain language to the user. Define any unavoidable technical term
   (YAML, schema, forcing) the first time; avoid developer jargon such as
   "scaffold" (say "create a starter configuration"). Follows the SKILL's
   plain-language rule (rule 11).

## Readiness Levels

Use exactly one readiness level in the final report.

| Level | Name | Meaning |
| --- | --- | --- |
| 0 | incomplete | The case cannot run or fails critical validation. |
| 1 | demo | The case runs, but uses sample/template forcing or major defaults. Teaching only. |
| 2 | screening | The case runs with plausible site metadata and forcing/proxy data. Useful for exploration, not validation. |
| 3 | decision-support | High-impact assumptions are sourced or confirmed, diagnostics pass, and remaining uncertainties are explicit. |
| 4 | publication-ready | Forcing, land cover, site metadata, parameter choices, provenance, diagnostics, and evaluation evidence are complete. |

Default to the lower level when evidence is mixed. A successful run with
template forcing is Level 1, even if diagnostics pass.

Anchor the level to `assess_readiness` (Workflow step 5) rather than judging it
from scratch:

- `ready: false`, or any high-impact item still in `assumed_defaults` -> at most
  Level 1 (demo). A freshly created starter case sits here by construction.
- High-impact assumptions resolved (location, forcing, and land cover are the
  user's, not sample defaults) with validation and diagnostics passing ->
  Level 2-3, per the upgrade rules below.
- `checklist_for_a_meaningful_run` is the concrete to-do list that moves a case
  up the levels; surface it to the user.

## Assumption Ledger

The ledger is **seeded by `assess_readiness`** (Workflow step 5), not built from
scratch — its `assumed_defaults` become the initial high-impact rows, with each
item's `risk` copied into `notes`. Track every non-user-provided value or
decision in this shape. Keep the ledger in the conversation. If the agent has file access and is creating a new case
directory, also write it next to the config as `assumptions.yml` or
`assumptions.json`.

```yaml
- field_or_decision: sites[0].properties.lat
  value: 51.5074
  source_type: derived_from_location
  source_detail: geocoded from "London, UK"
  confidence: medium
  impact: high
  confirmation_needed: true
  notes: Coordinates should be replaced with exact site coordinates.
```

Allowed `source_type` values:

- `user_provided`
- `observed_data`
- `documented_source`
- `derived_from_location`
- `suews_template`
- `sample_data`
- `agent_assumption`
- `not_set`

Allowed `confidence` values: `high`, `medium`, `low`.
Allowed `impact` values: `high`, `medium`, `low`.

Mark `confirmation_needed: true` for any high-impact value that is not
`user_provided`, `observed_data`, or a strong `documented_source`.

High-impact categories include:

- coordinates, timezone, altitude, and surface area
- forcing data source, time range, and timestep
- land-cover fractions
- vegetation properties, irrigation, water use, and anthropogenic heat
- model physics options and output variables used for interpretation

## Workflow

### 0. Onboarding -- install SUEWS and the MCP

Do this once, before anything else. A bare `pip install supy` gives the `suews`
CLI but **not** the MCP, so check for both.

**Detect** (if both succeed, skip to step 1):

```bash
python -c "import supy"        # the runtime + the `suews` CLI
python -c "import suews_mcp"   # the MCP server (provides the `suews-mcp` command)
```

**Install** -- with the user's consent, since it changes their environment.
First settle the package manager: **ask the user, or detect** (a `uv.lock` or
`uv` on `PATH` -> uv; otherwise pip). The MCP package depends on `supy`, so
installing the MCP pulls the runtime too. `suews-mcp` is **not yet on PyPI**
(only `supy` is), so install it from the repo:

```bash
# pip:
pip install "git+https://github.com/UMEP-dev/SUEWS.git#subdirectory=mcp"
# uv (drop-in for pip, into the active venv):
uv pip install "git+https://github.com/UMEP-dev/SUEWS.git#subdirectory=mcp"
# uv-managed project: `uv add` with the same git+...#subdirectory=mcp reference.
# From a local clone:           pip install ./SUEWS/mcp   (or uv pip install -e ./SUEWS/mcp)
# Runtime/CLI only (no MCP):    pip install supy          (or uv pip install supy)
# Once suews-mcp is published:  pip install suews-mcp     (or uv pip install suews-mcp)
```

For Claude Code users, the whole AI layer (this skill + the MCP registration +
commands) can instead be installed as a **Claude Code plugin** (`claude plugin
install <git-url>`); the plugin still needs the `pip`/`uv` runtime install above
because it cannot ship the compiled Fortran extension.

**Register the MCP with the chat client**, pointing `--root` at the case
directory (the sandbox the tools are allowed to touch):

- Claude Code: `claude mcp add suews -- suews-mcp --root <case-dir>`
- Claude Desktop / Cursor / other: add to the client's MCP config --
  `{"mcpServers": {"suews": {"command": "suews-mcp", "args": ["--root", "<case-dir>"]}}}`

**Verify** with one smoke call: `read_knowledge_manifest` should return the
schema version, suews version, and git commit. If it errors, the MCP is not
wired up -- fix that before proceeding.

### 1. Intake

Identify the user's requested place, time period, purpose, and available data.
Ask at most three questions if the workflow cannot proceed safely. Prioritise:

1. Exact site or coordinates.
2. Simulation period and timestep.
3. Whether local meteorological forcing or land-cover data are available.

Also clarify the **surface context**: a region name ("US Midwest", "Arizona")
is not the same as an urban site there -- confirm whether the user means an
urban, suburban, or rural setup before choosing a template, and state the
implication (the Midwest and an urban Midwest site behave very differently).

If the user only gives a place name, proceed only as a demo or screening setup
and say that exact site data will be needed for stronger conclusions.

### 2. Establish Evidence Surface

Call these before making substantive claims:

1. `mcp__suews__read_knowledge_manifest` to record the knowledge pack version.
2. `mcp__suews__list_docs` to discover relevant docs.
3. Read relevant docs resources, usually:
   - `forcing-data`
   - `configuration-yaml`
   - `tutorial-setup-own-site`
   - `output-variables`
   - `parameterisations`

Use `search_schema` for field lookup. Use `query_knowledge` for physics,
algorithm, or parameter semantics.

### 3. Create the Starter Configuration

Call `mcp__suews__init_case` with a fresh target directory and the
`simple-urban` template (currently the only template that ships; the other
names are reserved and rejected by `init`).

Immediately add ledger entries for all template-provided values that remain in
the config. The template is a useful starting point, not evidence that those
values represent the user's site. Never present this starter configuration to
the user as "ready" -- step 5 attaches its honest readiness verdict first.

### 4. Inspect Before Editing

Call `mcp__suews__inspect_config` on the starter YAML. Use the response to
identify active site fields, land-cover blocks, forcing paths, and physics
options. Do not edit from memory.

### 5. Assess Readiness and Seed the Ledger

Call `mcp__suews__assess_readiness` on the starter YAML (it takes
`config_path`, plus `project_root` when the config sits outside the default
sandbox). This is the dedicated fresh-user tool: it compares the config against
the bundled sample and returns, under `data`:

- `assumed_defaults` — every site-defining value still carrying a sample
  default, **each with a `risk` string** and its **energy-balance `role`**
  (e.g. land cover weights albedo->QN, materials->QS, conductance->QE).
- `looks_customised` — values that already appear to be the user's.
- `checklist_for_a_meaningful_run` — the ordered list of what to fix.
- `parameter_importance` — the energy-balance importance ladder (albedo first;
  QH and surface temperature are model outputs you never set).
- `ready` — a boolean overall judgement.

**Seed the assumption ledger directly from `assumed_defaults`** — one ledger
entry per item, copying its `risk` into `notes`, setting
`source_type: suews_template` (or `sample_data` for forcing), and
`confirmation_needed: true` for the high-impact ones. Do not hand-reconstruct
the ledger from memory when this tool has already enumerated the assumptions;
then refine each entry as the user supplies real values (step 6).

### 6. Edit Known Inputs Only

Apply the smallest config edits needed to reflect user-provided or documented
inputs. For each edit:

1. Record the source in the assumption ledger.
2. Record confidence and impact.
3. Mark whether user confirmation is still needed.
4. Avoid wholesale rewrites.

If a necessary high-impact value is unknown, leave the template value only for
demo/screening workflows and mark it as such. Do not present it as a real site
parameter.

**Forcing the user lacks.** If they have no forcing file, point them at a source
and record it in the ledger -- see `references/forcing-checklist.md`
("Where to get forcing data"): ERA5 single point via the CDS dataset
`reanalysis-era5-single-levels-timeseries` is the default. Reanalysis is a
proxy, not site observations -- say so. For site-descriptor data (land cover,
morphology, population) use only the **authorised registry** in
`references/data-sources.md` (GLAMOUR for building morphology, ESA WorldCover
for land cover, GEDI, GHSL, and the SUEWS-database for parameter values) --
never name other GIS datasets from memory.

**Context shifts change properties, not just land cover.** When the region or
scenario changes (e.g. London -> Arizona, or "add green infrastructure"), follow
the SKILL's *Parameter importance & context shifts* section (`../SKILL.md`):
the importance order is the energy balance itself -- reconsider radiative and
morphology properties (**albedo first**, then emissivity, QF, QS materials, QE
vegetation/water), not only the land-cover fractions. For every high-impact
property you could not source, add a ledger entry with
`confirmation_needed: true` -- never leave it silently. Close the step with an
explicit "adjusted vs left-unadjusted" list to the user.

### 7. Validate Loop

Call `mcp__suews__validate_config` after each edit cycle. Classify findings as:

- schema errors
- missing input data
- unit problems
- physically suspicious values

Fix blocking validation errors before attempting a run. If validation warnings
remain, carry them into the final report and readiness judgement.

### 8. Run Path

Current MCP surface note: this server exposes post-run tools, but the public
MCP tool list does not expose a `run_config` tool. If a `run_config` MCP tool
is added later, use it. If working in a local development agent with shell
access, run the unified CLI. The `run` subcommand takes only the positional
config file (no `--format` flag — that envelope option exists on
`validate` / `inspect` / `diagnose`, not on `run`):

```bash
suews run <config.yml>
```

If neither path is available, stop after validation and report:

```text
Validated-ready, not run: the MCP surface needs a run_config tool or local CLI access.
```

Do not fake run outputs.

### 9. Post-Run Checks

After a real run:

1. Read `suews://runs/{run_id}/provenance`.
2. Call `mcp__suews__diagnose_run`.
3. Read `suews://runs/{run_id}/diagnostics`.
4. Call `mcp__suews__summarise_run`.
5. Use `mcp__suews__compare_runs` only when there is a baseline, scenario, or
   observation dataset.

Do not claim success until provenance and diagnostics have been checked.

## Final Report

Use this structure.

```markdown
## Outcome
Run status: completed | validated-ready | blocked
Readiness: Level N - name

## What Was Created
- Case directory:
- Config:
- Run directory:

## Evidence Used
- Knowledge pack:
- Docs/resources:
- User-provided inputs:

## Assumption Ledger
| Field or decision | Value | Source | Confidence | Impact | Needs confirmation |
| --- | --- | --- | --- | --- | --- |

## Validation and Diagnostics
- Validation:
- Provenance:
- Diagnostics:
- Summary outputs:

## Interpretation Boundary
State exactly what the result can and cannot be used for at this readiness level.

## Next Best Upgrades
List the few inputs that would most improve readiness.
```

## Readiness Upgrade Rules

Move from Level 1 to Level 2 only when forcing and site metadata are no longer
pure template/sample data.

Move from Level 2 to Level 3 only when high-impact assumptions are either
confirmed by the user or backed by documented sources.

Move from Level 3 to Level 4 only when there is evaluation evidence against
observations or a publication-quality justification for why evaluation is not
possible.

## Failure Modes

If the user wants "reasonable results" but provides no site data, be helpful but
firm: create a demo or screening case and clearly label it. The right tone is
"we can make a useful first pass, and here is exactly where it stops being
evidence."

If the agent is tempted to search general web data for land cover or forcing,
pause and explain the source quality issue. Use external data only when the
user asks for it or provides the source, and still record it in the ledger.

If the knowledge pack or schema lookup does not cover a parameter, say it is a
knowledge-surface gap. Do not fill it from general model knowledge.
