# Agent Workspace Directory

Shared governance, path-scoped rules, skills, review subagents, hooks, and validation scripts for coding agents working in SUEWS (`AGENTS.md` is the canonical root entry point; `CLAUDE.md` is a thin `@AGENTS.md` compatibility loader).

## Directory Structure

```text
.claude/
|-- rules/                 # Standing conventions (unscoped + path-conditional)
|   |-- 00-project-essentials.md
|   |-- autonomous-workflow.md
|   |-- code-design.md
|   |-- dependency-safety.md
|   |-- labels.md
|   |-- naming-convention.md
|   |-- physics-change-evidence.md
|   |-- review-convergence.md
|   |-- work-sizing.md
|   |-- changelog/format.md
|   |-- ci/conventions.md
|   |-- docs/{conventions,release-docs-sanity,bib-topic-tags}.md
|   |-- fortran/{conventions,error-reporting}.md
|   |-- python/{conventions,api-approach,config-patterns,deprecation,schema-versioning}.md
|   |-- rust/{conventions,bridge-contract}.md
|   `-- tests/{patterns,verification-methodology}.md
|
|-- agents/                # Review subagents dispatched by fix-issue / audit-pr
|   |-- docs-reviewer.md
|   |-- fortran-error-state-reviewer.md
|   `-- schema-migration-checker.md
|
|-- hooks/                 # Claude Code hooks (configured in .claude/settings.json)
|   |-- session-start.sh          # SessionStart: remote container setup
|   |-- validate-tutorial.sh      # PostToolUse: tutorial syntax check
|   |-- check-doc-ascii.sh        # PostToolUse: docs/source/ ASCII gate
|   |-- check-bib-tags.sh         # PostToolUse: .bib topic-tag gate
|   `-- pre-commit-check.sh       # PreToolUse: quality gate on git commit
|
|-- skills/                # Invoked on demand via /skill-name
|   |-- fix-issue/         # Workflow: issue -> PR-ready branch
|   |-- triage-issue/      # Workflow: issue governance (audit/rewrite/split)
|   |-- triage-pr/         # Workflow: PR disposition router
|   |-- audit-pr/          # Workflow: comprehensive PR review
|   |-- split-pr/          # Workflow: carve oversized PR into stacked series
|   |-- queue-pr/          # Workflow: PR coordination and merge-queue preflight
|   |-- prep-release/      # Workflow: release pre-flight + tag
|   |-- republish-docs/    # Workflow: move release docs tag to clean anchor
|   |-- lint-code/         # Operation: static style and contract linters
|   |-- verify-build/      # Operation: build graph and packaging checks
|   |-- audit-docs/        # Operation: docs ASCII + bib topic-tag checks
|   |-- log-changes/       # Operation: CHANGELOG management
|   `-- suews/             # User-facing simulation skill (bundled into plugins/suews/)
|
|-- reference/
|   `-- templates/         # Commit message and review templates
|
|-- scripts/               # Instruction-integrity validation and local git hook helpers
|   |-- validate-claude-md.py
|   |-- test_validate_agents_md.py
|   |-- pre-commit-hook.sh
|   `-- setup-claude-protection.sh
|
`-- README.md              # This file
```

## Authority Hierarchy and Rule-Authoring Principles

When repository instructions or notes disagree, precedence runs from executable checks down to historical rationale:

1. **Executable CI gates and lint scripts** (`scripts/lint/*`, `scripts/suews/*`, `.claude/scripts/validate-claude-md.py`, `.github/workflows/*`).
2. **Standing rules** (`AGENTS.md` and `.claude/rules/**/*.md`).
3. **Skills and review subagents** (`.claude/skills/**` and `.claude/agents/*.md`).
4. **Developer reference notes** (`dev-ref/*.md`, including `dev-ref/CODEBASE_AUDIT_PROTOCOL.md` for whole-repository sweeps and `dev-ref/RUST_BRIDGE_AUDIT.md` for dated seam ledgers).

Follow five rules whenever adding or revising `.claude/` governance files:

1. **Encode recurring invariants in scripts, not prose tables.** If a rule can be checked mechanically (e.g. saved Fortran locals, `encoding="utf-8"`, manifest parity, rule-index sync), add or extend a script in `scripts/lint/` or `.claude/scripts/validate-claude-md.py` instead of pasting code tables into `.claude/rules/`.
2. **Anchor on `path::symbol`, never bare line numbers.** Source line numbers drift every week; cite `src/suews/src/suews_ctrl_driver.f95::SUEWS_update_outputLine` or a `git grep` recipe instead.
3. **One canonical home per rule.** State each policy once in its owning `.claude/rules/` file and cross-link from other rules, skills, and `dev-ref/` notes rather than duplicating tables.
4. **Keep `AGENTS.md` and `.claude/rules/` in 1:1 sync.** Every `.claude/rules/**/*.md` file must appear in `AGENTS.md` under `## Shared rule files` with the exact same `paths:` globs (or `all work.` when unscoped) so Claude Code auto-loading and Codex/Gemini explicit loading never diverge. `python .claude/scripts/validate-claude-md.py` enforces this in CI.
5. **Plain ASCII and British English everywhere.** All rules, skills, subagents, hooks, and scripts must stay strictly within ASCII (`0x00..0x7F`) and use British English prose.

## Concepts

### Rules (Auto-Loaded in Claude Code, Indexed in `AGENTS.md`)

Rules in `.claude/rules/` with `paths:` YAML frontmatter load only when working on matching paths:

```yaml
---
paths:
  - src/suews/**/*.f9*
  - src/suews/**/*.F90
  - src/suews_bridge/c_api/**/*.f9*
---
```

Rules without `paths:` frontmatter load on every task (`all work.` in `AGENTS.md`).
Keep unscoped rules concise because they consume context on every session.

### Review Subagents (`.claude/agents/`)

Focused reviewers dispatched by `fix-issue` and `audit-pr`:

- `docs-reviewer` - Checks user-facing doc drift against code changes and enforces `docs/` house style.
- `fortran-error-state-reviewer` - Verifies `set_supy_error` / `supy_error_flag()` propagation, `modState%errorstate%report`, `INTENT(OUT)` sentinels, and zero `SAVE` / `STOP` / stdout `WRITE` in changed Fortran.
- `schema-migration-checker` - Verifies `CURRENT_SCHEMA_VERSION`, `SCHEMA_VERSIONS`, `yaml_upgrade.py` handlers, `sample_config.yml` + `sample_config_short.yml`, and schema docs when `src/supy/data_model/` changes.

Subagent frontmatter uses `color:` (never `colour:`) and omits `model:` so subagents inherit the caller's configured model tier.

### Skills (On-Demand)

Skills perform multi-step workflows or atomic checks when invoked via `/skill-name`:

- **Workflow skills (orchestrators)**: `/fix-issue`, `/triage-issue`, `/triage-pr`, `/audit-pr`, `/split-pr`, `/queue-pr`, `/prep-release`, `/republish-docs`.
- **Operation skills (building blocks)**: `/lint-code`, `/verify-build`, `/audit-docs`, `/log-changes`.
- **User-facing plugin skill**: `/suews` (packaged into `plugins/suews/` via `make plugin`, verified by `test/mcp/test_packaging_manifests.py`).

For whole-repository or subsystem audits at a pinned SHA (as distinct from `/audit-pr` diff review), follow `dev-ref/CODEBASE_AUDIT_PROTOCOL.md`.

## Skill, Subagent, and Hook Relationships

```text
fix-issue (super-workflow: issue -> PR-ready)
  |-- triage-issue                     (readiness gate / governance)
  |-- lint-code / verify-build         (static + build operations)
  |-- fortran-error-state-reviewer     (when Fortran changes)
  |-- schema-migration-checker         (when data_model / sample_config changes)
  |-- docs-reviewer                    (when user-facing surface changes)
  |-- log-changes                      (CHANGELOG)
  `-- audit-pr                         (PR review convergence loop)
        `-- split-pr (if oversized) -- queue-pr (merge-queue preflight)

prep-release (release workflow)
  |-- verify-build / lint-code         (pre-flight operations)
  |-- docs-reviewer                    (release docs pass)
  `-- log-changes                      (CHANGELOG)

republish-docs (docs workflow)
  `-- docs-sync + release-docs-anchor  (move tag to a clean anchor)

pre-commit-check.sh (PreToolUse on git commit)
  |-- ruff check + ruff format --check (staged Python)
  |-- fprettify --diff                 (staged Fortran)
  |-- build-registration check         (new .f95 in src/supy/meson.build, src/suews/Makefile, build.rs)
  |-- RST heading depth check          (staged .rst)
  |-- CHANGELOG ### DD Mon YYYY + tag  (staged CHANGELOG.md additions)
  |-- validate-claude-md.py            (staged AGENTS.md / CLAUDE.md / .claude / audit protocol)
  `-- make test-smoke                  (staged Python or Fortran)
```

## Hook Infrastructure

Configured in `.claude/settings.json`:

- **SessionStart** (`session-start.sh`): Sets up system compilers, Rust availability check, `.venv`, build prerequisites, and editable install in remote web containers (`CLAUDE_CODE_REMOTE=true`).
- **PostToolUse on Edit/Write**:
  - `validate-tutorial.sh`: Syntax-checks edited `docs/source/tutorials/*.py` files.
  - `check-doc-ascii.sh`: Blocks non-ASCII characters in `docs/source/**/*.{py,rst,md}`.
  - `check-bib-tags.sh`: Validates topic tags on `docs/source/assets/refs/*.bib` via `.claude/skills/audit-docs/scripts/audit.py`.
- **PreToolUse on Bash** (`pre-commit-check.sh`): Runs staged-file quality checks when `git commit` is invoked (skipping files brought in unchanged during a clean merge).

## Validation

Run the shared instruction validator locally after editing `AGENTS.md`, `CLAUDE.md`, `.claude/`, or `dev-ref/CODEBASE_AUDIT_PROTOCOL.md`:

```bash
python -m unittest discover -s .claude/scripts -p test_validate_agents_md.py
python .claude/scripts/validate-claude-md.py
```

## Git Policy

- **Commit**: All shared rules, agents, hooks, skills, templates, and scripts.
- **Ignore**: `settings.local.json`, `AGENTS.md.backup`, `.claude/snapshots/`, and temporary scratch files.
