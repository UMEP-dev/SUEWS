"""Server instructions and MCP prompts for the SUEWS MCP.

These carry the *procedural* guidance — the honesty contract, the energy-balance
parameter logic, the authorised data sources, and the fresh-site / evaluation
workflows — **inside the MCP itself**. That makes the guidance travel to every
MCP client (Claude Desktop, Codex, Cursor, …), not only to Claude Code where it
also ships as the `/suews` skill. Claude Code consumes the skill; everyone else
gets the same contract via `serverInfo.instructions` and these prompts.
"""

from __future__ import annotations

# Injected into the MCP `initialize` handshake (FastMCP `instructions=`), so the
# host model sees the contract before any tool call.
SERVER_INSTRUCTIONS = """\
The SUEWS MCP exposes read-only / write-locked tools over a sandboxed project
root for the SUEWS urban land-surface model. Work to this contract:

- HONESTY: ground every claim in tool evidence (schema field, doc slug,
  file:line, and the provenance the tools return — git commit, schema version).
  Never invent parameter values or numbers; if the tools lack evidence, say so.
- FRESH USERS: call `assess_readiness` before trusting a config — a freshly
  scaffolded case is the bundled KCL/London sample, not the user's site. Surface
  the assumed-defaults list (with each item's energy-balance role) before any run.
- PARAMETER IMPORTANCE follows the energy balance QN + QF = QS + QE + QH, where
  QH is the residual and surface temperature is iterated to convergence. Set
  parameters in that order: albedo first (net shortwave = (1-albedo)*Kdown),
  then emissivity (net longwave; low-effort, sample default acceptable), QF,
  material/thermal properties (QS), vegetation + water (QE). Never set QH or
  surface temperature — they are model outputs. Energy-balance closure is
  automatic, so it is NOT a validation check.
- DATA SOURCES: recommend only the authorised set — ERA5 / ERA5-Land (forcing),
  GLAMOUR (building morphology), ESA WorldCover (land cover), GEDI (canopy),
  GHSL / GHS-POP (population), and the SUEWS-database (parameter values). Flag
  any other GIS dataset as general / unverified rather than asserting it.
- PLAIN LANGUAGE: define jargon on first use; avoid developer slang ("scaffold"
  -> "create a starter configuration").

Use the prompts `fresh_site_setup`, `parameter_importance` and `evaluate_results`
for the full step-by-step procedures.
"""


def fresh_site_setup() -> str:
    """End-to-end procedure to set up and run SUEWS for a new site, honestly."""
    return """\
Set up and run SUEWS for a new site, keeping every value auditable.

0. Onboarding — ensure `supy` (CLI) and `suews_mcp` (MCP) are installed; a bare
   `pip install supy` does NOT include the MCP. Smoothest for Claude Code / Codex
   is the `suews` plugin: its bundled `.mcp.json` spawns the server via `uvx`,
   self-bootstrapping both (only `uv` is required). Explicit alternative:
   `pip`/`uv install "git+https://github.com/UMEP-dev/SUEWS.git#subdirectory=mcp"`
   installs both.
1. Intake — get the exact site/coordinates, period/timestep, and whether the
   user has local forcing or land-cover data. Clarify urban vs rural (a region
   name is not an urban site there).
2. Evidence — `read_knowledge_manifest`, `list_docs`, then read `forcing-data`,
   `tutorial-setup-own-site`, `output-variables`, `parameterisations`.
3. Create the starter config — `init_case` (template `simple-urban`). The
   template is a starting point, not the user's site.
4. `inspect_config` — see active fields; do not edit from memory.
5. `assess_readiness` — lists which site-defining values are still the sample's
   defaults, each with its energy-balance role and risk, plus a checklist and a
   parameter-importance ladder. Seed an assumption ledger from it.
6. Edit known inputs only (smallest edits). On a region/scenario shift,
   reconsider parameters in energy-balance order (albedo first), and for every
   high-impact property you cannot source, say you left it at the sample value
   and why — never silently. Recommend data only from the authorised registry.
7. `validate_config` after each edit; fix errors before running.
8. Run — `suews run <config.yml>` (no `--format` flag on `run`).
9. Post-run — read provenance + diagnostics, `summarise_run`, and `compare_runs`
   for scenario vs baseline.

Report a single readiness level: 1 demo (template forcing/defaults) / 2 screening
(plausible site + proxy data) / 3 decision-support (high-impact assumptions
sourced or confirmed, diagnostics pass) / 4 publication (complete + evaluated).
State explicitly what the result can and cannot be used for.
"""


def parameter_importance() -> str:
    """Which SUEWS parameters matter most, derived from the energy balance."""
    return """\
SUEWS closes the surface energy balance every timestep, QN + QF = QS + QE + QH,
iterating surface temperature to convergence and taking QH as the residual. So
the balance ALWAYS closes (closure is not a validation check), and QH and
surface temperature are OUTPUTS you never set. Importance is the order of the
terms:

1. QN net shortwave — ALBEDO FIRST. Net shortwave = (1-albedo)*Kdown; albedo
   scales the largest input, varies strongly by surface/region, highest leverage.
   Set land-cover fractions (the weights) with it.
2. QN net longwave — EMISSIVITY, low effort. Uses the iterated surface
   temperature (which you cannot set). Common materials ~0.90-0.97, so the
   sample default is acceptable if you have no better value.
3. QF anthropogenic heat — population density, energy-use profiles. Matters
   where large (dense / cold / hot cities); negligible rural.
4. QS storage heat — material/thermal properties (thickness, heat capacity,
   conductivity) via OHM (QS = a1*QN + a2*dQN/dt + a3).
5. QE latent heat — the energy<->water hinge: surface conductance (gsModel),
   LAI, soil-moisture deficit, surface water; irrigation / soil store / runoff
   set the water available to evaporate.
6. QH sensible heat & surface temperature — OUTPUTS. Get 1-5 right and these
   follow; never tune them.

For parameter VALUES, use the SUEWS-database (authorised). On a context shift,
end with an explicit "adjusted vs left-unadjusted" list.
"""


def evaluate_results() -> str:
    """How to evaluate a SUEWS run against observations, without circularity."""
    return """\
Evaluating a SUEWS run for a region:

- Three checks, not one: (a) are the INPUTS regionally representative
  (`assess_readiness` — is the config yours or the sample's?); (b) are the
  regional physics drivers right (snow, anthropogenic heat, irrigation,
  phenology); (c) are the OUTPUTS plausible.
- Compare against the modelled T2 OUTPUT, NOT the forcing Tair INPUT — comparing
  the model to the air temperature you fed in is circular. `T2` is "air
  temperature at 2 m" in the SUEWS output group.
- Use `compare_runs` (run vs observations CSV) with `variables=T2` (its default
  is QH,QE,QN), reporting RMSE / bias / Pearson r; align timezone, units and
  frequency first. Run `diagnose_run` before concluding.
- Energy-balance CLOSURE is NOT an independent validation check — SUEWS solves
  QH as the residual, so the balance closes by construction. Validate against
  independent observations instead.
- State the readiness level and what the numbers can/cannot support (e.g. a run
  on reanalysis/proxy forcing is screening-grade, not site-validated).
"""
