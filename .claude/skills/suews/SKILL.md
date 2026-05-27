---
name: suews
description: Use whenever helping with SUEWS or SuPy -- setting up a new site from scratch for a fresh user (with an honest readiness check of which values are still assumed sample defaults), preparing or migrating YAML configurations, running urban-climate simulations, validating inputs, diagnosing failed or suspicious runs, comparing scenarios, interpreting energy/water balance outputs, or reviewing simulation setups before publication. Prefers structured CLI/MCP calls (suews validate, suews diagnose, assess_readiness, validate_config tool) over guessing parameters or rewriting files.
---

# SUEWS workflow

When helping with SUEWS or SuPy, follow this rule set. The Skill teaches *when*
and *why* to call which tool -- actual execution lives in the unified `suews`
CLI and the `suews-mcp` server. Never re-implement validation, schema
checking, or simulation logic in conversation.


> **CLI naming note**: this Skill describes the unified `suews <subcommand>`
> CLI (`suews validate`, `suews inspect`, `suews diagnose`, `suews convert`,
> `suews compare`, `suews init`). On installations that ship the legacy
> hyphenated entry points only (`suews-validate`, `suews-convert`,
> `suews-schema`, `suews-run`), substitute the hyphenated form -- the
> structured JSON envelope and exit semantics are the same. When in doubt,
> run `suews --help` and pick whichever surface the wheel exposes.

> **Prerequisite -- the CLI and the MCP must be installed.** A bare
> `pip install supy` gives the `suews` CLI but **not** the MCP (`suews-mcp` is
> not yet on PyPI). Smoothest for Claude Code / Codex: install the **`suews`
> plugin** -- its bundled `.mcp.json` spawns the server via `uvx`, which
> self-bootstraps `suews-mcp` (and `supy`) on first use, so no separate install
> is needed (only `uv` on the machine). Otherwise run the onboarding in the
> fresh-site setup workflow (`references/fresh-site-setup.md`, Step 0), whose
> explicit path is `pip install "git+https://github.com/UMEP-dev/SUEWS.git#subdirectory=mcp"`
> (or `uv pip install ...`), pulling `supy` in, then registers the MCP with the
> chat client.

## Two tracks: meet the user where they are

- **Fresh / quick-start user** ("I have SUEWS, set me up a site fast"): they do
  not yet know what data SUEWS needs. Hand-hold, and **be honest about assumed
  defaults** -- a freshly scaffolded case *is* the bundled KCL/London sample, so
  its location, surface mix and forcing are placeholders, not theirs. Follow the
  **fresh-site setup workflow** (`references/fresh-site-setup.md`) -- the full
  end-to-end procedure (onboarding -> intake -> create starter config ->
  assess_readiness -> edit -> validate -> run -> diagnose), with readiness levels
  and an auditable assumption ledger. The quick-start recipe below is its summary.
- **Advanced user**: drive the tools directly -- `search_schema`,
  `query_knowledge`, `convert_config`, `compare_runs`, `diagnose_run` -- and lean
  on provenance. They want precision and citations, not hand-holding.

**Honesty rule for assumed defaults.** Whenever a user scaffolds, or hands you, a
config that may carry sample defaults, call **`assess_readiness`** and tell them
plainly which site-defining values are still the sample's (location, land-cover
fractions, forcing) and the risk of each. Never let a fresh user run the sample's
London setup believing it is their site. `validate_config` checks the config is
*valid*; `assess_readiness` checks it is *theirs*.

1. Prefer YAML configuration. Treat namelist as legacy and only touch it
   during migration via `suews convert`.
2. Never invent parameter values. If a value is missing, ask the user or
   reuse a sample from `suews://examples/{name}` or
   `src/supy/sample_data/sample_config.yml`.
3. Always validate before editing. Call the `validate_config` MCP tool, or
   run `suews validate --format json <config>` (the `--format` flag must come
   *before* the file path -- after it, click parses it as a file). Read the structured output,
   not the prose. Pretty-printed terminal output is for humans; you want the
   JSON envelope (`{status, data, errors, warnings, meta}`).
4. Use structured JSON outputs only. If a CLI lacks `--format json`, flag
   it as a CLI gap rather than scrape prose.
5. Failure-diagnosis order: schema/version -> file paths -> forcing time
   axis -> required forcing variables -> land-cover fractions sum to 1 ->
   temporal resolution -> output directory writable. Stop at the first
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
    MCP prompt. Do not invent a checklist -- see
    `references/publication-review-checklist.md`.
11. Plain language for fresh users. In user-facing output, prefer plain
    language; when a technical term is unavoidable (YAML, schema, forcing,
    `init`), define it in a few words the first time you use it. Avoid inherited
    developer jargon -- say "create a starter configuration", not "scaffold a
    case".
12. Data sources: authorised only. When recommending where to get data,
    recommend **only** the sources in `references/data-sources.md` (ERA5 /
    ERA5-Land for forcing; GLAMOUR for building morphology; ESA WorldCover /
    Copernicus for land cover; GEDI for canopy; GHSL / GHS-POP for population).
    For any other tool or dataset (OSM, OS MasterMap, ad-hoc LiDAR, WorldPop,
    manual UMEP route), do not assert it -- flag it as general / unverified and
    point back to the authorised list.

## Model choice

The MCP **amplifies a capable model; it does not rescue a weak one.** Use a
frontier model (Opus / Sonnet class) with the MCP -- these orchestrate the tools
and cite evidence. Weak models (Haiku class, or free-tier GPT-instant) under-call
the tools and can be *confidently wrong* rather than admitting uncertainty; do
not judge the MCP's value on them, and steer fresh users to a capable model.

## Parameter importance & context shifts

Do not present parameters as a flat list. Their importance is **derived from
the model's nature**: SUEWS closes the surface energy balance every timestep --
`QN + QF = QS + QE + QH` -- by iterating surface temperature to convergence and
taking **QH as the residual** (`suews_ctrl_driver.f95:3201`). Two facts follow,
and they set the whole logic:

- The balance **always closes**, so "it ran and balanced" says nothing about
  whether the *partition* is right. **Closure is not a validation check.**
- **QH and surface temperature are outputs the model solves -- you never set
  them.** You get them right only by getting the terms they derive from right.
  So parameter importance is simply the order of the energy-balance terms:

1. **Net radiation `QN` -- the dominant input.**
   - **Albedo first.** Net shortwave is `(1 - albedo)*Kdown`
     (`suews_phys_narp.f95:414`): albedo scales the largest input directly, so a
     wrong albedo makes `QN` -- and the whole partition -- wrong. It varies
     strongly by surface and region (arid surfaces are far brighter than London
     brick), so it is the highest-leverage value and the first to check.
   - **Emissivity second, but low effort.** Net longwave uses surface emissivity
     and the *iterated* surface temperature (`suews_phys_narp.f95:402-407`). You
     cannot set surface temperature -- the model solves it. You set
     **emissivity**, but common materials sit ~0.90-0.97, so if you have no
     better value the **sample default is acceptable** -- flag it, don't agonise.
2. **Anthropogenic heat `QF` -- the other input.** Added straight into available
   energy (`suews_ctrl_driver.f95:2618`). Matters where large (dense / cold-winter
   / hot-summer cities); negligible rural. Set its drivers (population density,
   energy-use profiles) to your context.
3. **Storage heat `QS` -- the first sink.** OHM: `QS = a1*QN + a2*dQN/dt + a3`
   (`suews_phys_ohm.f95:653`), driven by the **material/thermal properties**
   (thickness, heat capacity, conductivity) of walls/roofs/roads. After `QN` and
   `QF`, this is the next term to get right.
4. **Available energy `QN + QF - QS`, split into `QE` and `QH`.**
   - **`QE` (latent) -- the energy<->water hinge.** Penman-Monteith via surface
     conductance (`gsModel`, Jarvi 2011 / Ward 2016), modulated by **LAI,
     soil-moisture deficit and surface water state** (`suews_phys_evap.f95`,
     `suews_phys_resist.f95:122`). This is where the **water balance** enters:
     irrigation, soil store and runoff set how much water is available to
     evaporate. Vegetation + water parameters live here.
   - **`QH` (sensible) -- the residual.** Whatever `QE` leaves. There is no `QH`
     parameter; get `QN`, `QS`, `QE` right and `QH` (and the 2 m temperature you
     evaluate) follows.
   - **Morphology** (building height, frontal-area index, roughness) sets the
     *aerodynamic resistance* that moves heat/moisture off the surface, and
     couples to the forcing-height `z` rule (see `references/forcing-checklist.md`).

**The importance ladder is the energy balance itself:** albedo (net SW) ->
land-cover fractions (the mix every term is weighted over) -> emissivity
(net LW, default-OK) -> QF drivers -> material/thermal (QS) -> vegetation + water
(QE). For *parameter values*, the authorised source is the SUEWS-database (see
`references/data-sources.md`).

On a **context shift** (new region/scenario), walk this ladder; for each
high-impact term you cannot source, **say you left it at the sample value and
why -- never silently** (Sue Grimmond, gh#1384 Q2/Q5). Example: "I changed the
land-cover fractions for Arizona but left building albedo at London's value --
in a hot, arid city it should be higher; please confirm or supply it." End with
an explicit "adjusted vs left-unadjusted" list keyed to these terms.

## Procedural recipes

These are the standard moves; expand by reading the referenced files.

- **Fresh-user quick start (set up a site fast)** -- summary of the full
  workflow in `references/fresh-site-setup.md` (read it for the readiness levels,
  assumption ledger and final-report structure): `init_case` (create starter) ->
  **`assess_readiness`** (honestly lists which values are still the bundled
  sample's defaults -- location, land-cover fractions, forcing -- each with its
  energy-balance role, the risk, and a checklist) -> guide the user to set
  location / timezone / land-cover fractions / forcing -> `inspect_config` to
  confirm -> `validate_config` -> `suews run`. **Always surface the
  assumed-defaults list before any run** so the user is never silently running
  the sample's site.
- **New case from scratch (CLI form)**: `suews init --template simple-urban -o demo`,
  edit, then `suews validate --format json demo/config.yml`. See
  `references/forcing-checklist.md` before adding forcing.
- **Failed run**: `suews validate` -> `suews inspect` -> `suews diagnose`.
  Read `references/common-errors.md` for the most frequent diagnoses.
- **Namelist -> YAML migration**: `suews convert -i RunControl.nml -o config.yml`,
  then validate. See `references/migration-namelist-to-yaml.md`.
- **Output interpretation**: read `references/output-interpretation.md` for
  energy balance, storage heat, anthropogenic heat, Bowen ratio.
- **Scenario comparison**: `suews compare <baseline> <scenario>` -- see
  `references/output-interpretation.md` for which metrics to read first.
