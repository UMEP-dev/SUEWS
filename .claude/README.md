# Claude Code Workspace Directory

This directory contains all Claude Code-specific documentation, plans, and configuration for SUEWS development.

## Directory Structure

```
.claude/
├── rules/                 # AUTO-LOADED conventions (path-conditional)
│   ├── 00-project-essentials.md    # Always loaded
│   ├── fortran/
│   │   └── conventions.md          # When editing src/suews/**/*.f9*
│   ├── python/
│   │   ├── conventions.md          # When editing src/supy/**/*.py
│   │   └── config-patterns.md      # When editing src/supy/**/*.py
│   ├── docs/
│   │   └── conventions.md          # When editing docs/**/*
│   ├── tests/
│   │   └── patterns.md             # When editing test*/**/*.py
│   ├── changelog/
│   │   └── format.md               # When editing CHANGELOG.md
│   └── ci/
│       └── conventions.md          # When editing .github/workflows/**/*
│
├── hooks/                 # Claude Code hooks (auto-triggered)
│   ├── session-start.sh          # SessionStart: environment setup
│   ├── validate-tutorial.sh      # PostToolUse: tutorial validation
│   └── pre-commit-check.sh       # PreToolUse: quality gate on git commit
│
├── skills/                # Invoked via /skill-name (workflow + operation tiers)
│   │                      # -- Workflow skills (orchestrators) --
│   ├── fix-issue/         # Super-workflow: issue -> PR-ready branch
│   ├── triage-issue/      # Issue governance (audit/rewrite/split)
│   ├── triage-pr/         # PR disposition router (advance/defer/close/...)
│   ├── audit-pr/          # Comprehensive PR review
│   ├── split-pr/          # Carve an oversized PR into a stacked series
│   ├── queue-pr/          # PR coordination and merge-queue preflight
│   ├── prep-release/      # Release pre-flight + tag
│   ├── republish-docs/    # Republish/revise released docs (clean anchor)
│   │                      # -- Operation skills (building blocks) --
│   ├── lint-code/         # Code style (references rules/)
│   ├── verify-build/      # Build config checks
│   ├── sync-docs/         # Doc-code content consistency
│   ├── audit-docs/        # Docs sanity check (non-ASCII + bib tags)
│   ├── log-changes/       # CHANGELOG management
│   │                      # -- User-facing (published as the suews plugin) --
│   └── suews/             # Run/validate/analyse simulations
│
├── reference/             # Templates and static reference
│   └── templates/         # Reusable templates
│
├── scripts/               # Infrastructure
│   ├── validate-claude-md.py
│   ├── pre-commit-hook.sh
│   └── setup-claude-protection.sh
│
└── README.md              # This file
```

## Concepts

### Rules (Auto-Loaded)

Rules in `.claude/rules/` are **automatically loaded** when Claude Code starts a session.

**Path-conditional loading**: Rules with `paths:` frontmatter only load when working with matching files:
```yaml
---
paths:
  - src/suews/**/*.f9*
---
```

**Always loaded**: Rules without `paths:` frontmatter (like `00-project-essentials.md`) load for every session.

### Skills (On-Demand)

Skills perform specific workflows when invoked via `/skill-name`. Each skill has:
- `SKILL.md` - Main content with frontmatter (name, description)
- Optional subdirectories for references

**Key difference from rules**: Skills are invoked explicitly; rules are always available.

**Two tiers of skill**:

- **Workflow skills** orchestrate a multi-step process and compose operation
  skills. `fix-issue` is the super-workflow example -- it drives a triaged issue
  to a PR-ready branch. The autonomous issue -> PR -> merge pipeline lives here.
- **Operation skills** are atomic building blocks: a single check or action
  (`lint-code`, `verify-build`, `log-changes`) that workflows invoke or that you
  run directly.

## Available Skills

**Workflow skills (orchestrators)** -- multi-step, compose operation skills:

- `/fix-issue <issue>` - **Super-workflow**: triage and implement an issue to a PR-ready branch (composes triage-issue, the operation skills, and audit-pr)
- `/triage-issue <issue>` - Issue governance: audit, rewrite, or split into child issues (emits a readiness verdict)
- `/triage-pr <PR>` - Disposition router for draft/stalled PRs (advance/continue/defer/close/escalate)
- `/audit-pr <PR>` - Comprehensive PR review (style + scientific + testing + docs + build)
- `/split-pr <PR>` - Carve an oversized PR into a stacked series of small PRs
- `/queue-pr` - Order ready PRs and run merge-queue preflight
- `/prep-release` - Release pre-flight checks and tag generation
- `/republish-docs` - Republish/revise docs for an already-released version (move tag to a clean anchor)

**Operation skills (building blocks)** -- single-purpose, invoked by workflows or directly:

- `/lint-code` - Check code style against conventions
- `/verify-build` - Verify build configuration (meson / pyproject / CI)
- `/sync-docs` - Check doc-code content consistency
- `/audit-docs` - Docs sanity check: non-ASCII fixes + bib topic-tags (backed by doc-change hooks)
- `/log-changes` - Update CHANGELOG

The user-facing `/suews` skill (run, validate, analyse simulations) is published
as the `suews` plugin; see Plugin Packaging below.

## Plugin Packaging

Only the user-facing `suews` plugin (the `suews` skill plus the `suews-mcp`
server) is published to the AI-agent marketplaces (`.claude-plugin/marketplace.json`,
`.codex-plugin/`, `.agents/`). The contributor/maintainer skills listed above
live in this repository under `.claude/skills/` and are available automatically
on checkout. They are deliberately **not** shipped as a separate marketplace
plugin: anyone who would use them already has the repository cloned, so a
published `suews-dev` plugin added maintenance (a third manifest to keep in sync)
without adding reach.

## Rules vs Skills

- **Loading**: Rules are automatic; skills are on-demand
- **Purpose**: Rules define conventions/guidelines; skills perform workflows/actions
- **Location**: Rules in `.claude/rules/`; skills in `.claude/skills/`
- **Invocation**: Rules need none; skills via `/skill-name`
- **Path-conditional**: Rules yes (`paths:` frontmatter); skills no

## Skill & Hook Relationships

```
fix-issue (super-workflow: issue -> PR-ready)
  ├── triage-issue           (readiness gate / governance)
  ├── lint-code / sync-docs / verify-build   (quality operations)
  ├── log-changes            (CHANGELOG)
  └── audit-pr               (PR review loop)
        └── split-pr (if oversized) ── queue-pr (merge-queue preflight)

prep-release (release workflow)
  ├── verify-build / sync-docs / lint-code   (pre-flight operations)
  └── log-changes            (CHANGELOG)

republish-docs (docs workflow)
  └── docs-sync + release-docs-anchor (move tag to a clean anchor)

standalone operations: audit-docs (hook-backed: non-ASCII + bib tags)

pre-commit hook ─────┬── ruff (Python)
  (auto on            ├── fprettify (Fortran)
   git commit)        ├── meson.build check (new files)
                      ├── RST heading check (docs)
                      ├── CHANGELOG format check
                      └── make test-smoke (Python/Fortran only)
```

## Quick Navigation

- **"Fix a GitHub issue (end to end)"** -> `/fix-issue <issue>` (super-workflow)
- **"Triage or clean up an issue"** -> `/triage-issue <issue>`
- **"How do I set up my environment?"** -> Quick Start (`uv venv && source .venv/bin/activate && make dev`)
- **"Check my code style"** -> `/lint-code`
- **"Update the CHANGELOG"** -> `/log-changes`
- **"Prepare for release"** -> `/prep-release`
- **"Fortran conventions"** -> `.claude/rules/fortran/`
- **"Python conventions"** -> `.claude/rules/python/`
- **"Test patterns"** -> `.claude/rules/tests/`
- **"CI/GitHub Actions"** -> `.claude/rules/ci/`

## Development Workflows

The issue -> PR path is driven by the **`fix-issue` super-workflow**, which
composes the pipeline-stage workflows (`triage-issue`, `audit-pr`, `split-pr`,
`queue-pr`) and the operation skills. The flows below show both the orchestrated
path and the manual steps for cases `fix-issue` does not cover.

### Create Workflows

**Feature / Bug fix (orchestrated)** -- the `fix-issue` super-workflow
1. `/triage-issue <N>` -- confirm readiness
2. `/fix-issue <N>` -- branch, build, implement, run the quality operations, and loop with `/audit-pr` to a PR-ready branch
3. `git fetch origin && git rebase origin/master`, then `gh pr create` (use "Fixes #N" to link the issue)
4. Verify closure: issue closed and CI green (`gh issue view <N>`, `gh pr checks`)

**Feature / Bug fix (manual)** -- when not using `fix-issue`
1. `/triage-issue <N>` -- analyse readiness and scope
2. Branch + `make dev` + baseline tests (`uv venv && make dev` if the environment is not ready)
3. Implement (auto-loaded rules guide coding)
4. `git commit` -- pre-commit hook auto-runs quality checks
5. `/sync-docs` + `/log-changes` + `make docs`
6. `git fetch origin && git rebase origin/master`, then `gh pr create`
7. `/audit-pr <N>` -- review
8. Verify closure: issue closed and CI green

**Refactoring**
1. `/triage-issue <N>` -- understand scope
2. `make test` (full baseline)
3. Implement, `make test` (compare)
4. `git commit` -- hook checks
5. `/sync-docs` + `/log-changes` ([change])
6. `/audit-pr` (refactoring mode)

**Documentation Only**
1. Create branch, `make dev`, `make docs` (baseline)
2. Edit docs, `/sync-docs`, `/lint-code`, `/audit-docs` (non-ASCII + bib tags)
3. `/log-changes` ([doc]), PR

### Review Workflows

**Triage Issue**: `/triage-issue <N>` -- audit readiness, rewrite, or split into child issues
**Audit PR**: `/audit-pr <N>` -- comprehensive review (style + scientific + testing + docs + build)
**Check GitHub**: `gh issue list` / `gh pr list` -- assigned issues, review requests, open PRs

### Maintenance Workflows

**Release**
1. `/prep-release` -- assess necessity
2. Pre-flight: `/verify-build` + `/sync-docs` + `/lint-code`
3. `/log-changes` -- finalise CHANGELOG
4. Open PR, CI, merge; create the tag and GitHub release (`gh release create <tag>`; `/prep-release` generates the tag)
5. Monitor PyPI, GitHub Release, Zenodo

**CI/Build**
1. `/verify-build` -- identify issues
2. Fix, `/verify-build` -- confirm
3. `make test-smoke`, `/log-changes` ([maintenance]), PR

**Stacked PRs / merge queue**
1. `/split-pr <PR>` -- carve an oversized PR into a stacked series (when `/audit-pr` flags size)
2. `/queue-pr` -- order ready PRs and run merge-queue preflight

**Checks**: Run `/verify-build` + `/sync-docs` + `/lint-code` together, report summary

## Hook Infrastructure

Claude Code hooks run automatically at specific trigger points:

- **SessionStart** (`session-start.sh`): Installs system deps, creates venv, builds SUEWS on session init
- **PostToolUse on Edit/Write** (`validate-tutorial.sh`): Validates tutorial Python files after editing
- **PreToolUse on Bash** (`pre-commit-check.sh`): Quality gate on `git commit` -- file-type-specific checks:
  - Python: `ruff check` + `ruff format --check`
  - Fortran: `fprettify --diff` + meson.build new-file check
  - RST: heading level validation
  - CHANGELOG: date format + category tag validation
  - `make test-smoke`: runs only when Python or Fortran files are staged

Global hooks (in `~/.claude/settings.json`):
- **PostToolUse on Edit/Write/MultiEdit**: runs `ruff check --fix` + `ruff format` on Python files

## Post-Merge Verification

After a PR is merged, verify:
- Issue closed (if linked with "Fixes #N")
- CI green on master
- Documentation deployed (if docs changed)
- No regression in dev tag CI

## Git Policy

- **Commit**: All directories and files
- **Ignore**: settings.local.json, any temp-* files
