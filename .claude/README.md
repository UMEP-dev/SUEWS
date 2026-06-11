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
├── skills/                # Action-oriented workflows (invoked via /skill-name)
│   ├── audit-pr/          # PR review orchestrator
│   ├── examine-issue/     # Issue analysis
│   ├── lint-code/         # Code style (references rules/)
│   ├── log-changes/       # CHANGELOG management
│   ├── prep-release/      # Release preparation
│   ├── setup-dev/         # Environment setup guide
│   ├── start-work/        # Workflow selector & launcher
│   ├── sync-docs/         # Doc-code consistency
│   ├── republish-docs/    # Republish/revise released docs (clean anchor)
│   ├── verify-build/      # Build config checks
│   └── queue-pr/         # PR coordination and merge queue preflight
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

## Available Skills

- `/start-work` - **Entry point** for all workflows (feature, bugfix, release, docs, refactoring, CI/build)
- `/audit-pr <PR>` - Review a pull request comprehensively
- `/examine-issue <issue>` - Analyse a GitHub issue
- `/lint-code` - Check code style
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare release
- `/setup-dev` - Set up development environment
- `/sync-docs` - Check doc-code consistency
- `/republish-docs` - Republish/revise docs for an already-released version (move tag to a clean anchor)
- `/verify-build` - Verify build configuration
- `/queue-pr` - Coordinate PRs before merge queue

## Rules vs Skills

- **Loading**: Rules are automatic; skills are on-demand
- **Purpose**: Rules define conventions/guidelines; skills perform workflows/actions
- **Location**: Rules in `.claude/rules/`; skills in `.claude/skills/`
- **Invocation**: Rules need none; skills via `/skill-name`
- **Path-conditional**: Rules yes (`paths:` frontmatter); skills no

## Skill & Hook Relationships

```
start-work ──────────┬── examine-issue (analysis)
  (workflow           ├── gh-link (branch)
   selector)          ├── prep-release (release workflow)
                      └── verify-build (CI/build workflow)

prep-release ────────┬── verify-build (pre-flight)
                     ├── sync-docs (pre-flight)
                     ├── lint-code (pre-flight)
                     └── log-changes (CHANGELOG)

audit-pr ────────────┬── lint-code (style review)
                     ├── sync-docs (doc review)
                     └── verify-build (build review)

pre-commit hook ─────┬── ruff (Python)
  (auto on            ├── fprettify (Fortran)
   git commit)        ├── meson.build check (new files)
                      ├── RST heading check (docs)
                      ├── CHANGELOG format check
                      └── make test-smoke (Python/Fortran only)
```

## Quick Navigation

- **"Start a new task"** -> `/start-work`
- **"How do I set up my environment?"** -> `/setup-dev`
- **"Check my code style"** -> `/lint-code`
- **"Update the CHANGELOG"** -> `/log-changes`
- **"Prepare for release"** -> `/prep-release`
- **"Fortran conventions"** -> `.claude/rules/fortran/`
- **"Python conventions"** -> `.claude/rules/python/`
- **"Test patterns"** -> `.claude/rules/tests/`
- **"CI/GitHub Actions"** -> `.claude/rules/ci/`

## Development Workflows

All workflows begin with `/start-work`, which uses a two-step selection:
1. **Category**: Create, Review, or Maintenance
2. **Specific workflow**: depends on category

### Create Workflows

**Feature Development**
1. `/examine-issue <N>` -- analyse and assess complexity
2. `/start-work` setup -- branch, build, baseline tests
3. Implement (auto-loaded rules guide coding)
4. `git commit` -- pre-commit hook auto-runs quality checks
5. `/sync-docs` + `/log-changes` + `make docs`
6. `/gh-sync` + `gh pr create` -- rebase and create PR
7. `/audit-pr <N>` -- review
8. `/gh-debrief` -- verify closure

**Bug Fix**
1. `/examine-issue <N>` -- triage
2. `/start-work` setup -- branch, build, baseline
3. Implement fix + regression test
4. `git commit` -- hook runs checks
5. `/log-changes` ([bugfix]) + `gh pr create` + `/audit-pr`

**Refactoring**
1. `/examine-issue <N>` -- understand scope
2. `make test` (full baseline)
3. Implement, `make test` (compare)
4. `git commit` -- hook checks
5. `/sync-docs` + `/log-changes` ([change])
6. `/audit-pr` (refactoring mode)

**Documentation Only**
1. Create branch, `make dev`, `make docs` (baseline)
2. Edit docs, `/sync-docs`, `/lint-code`
3. `/log-changes` ([doc]), PR

### Review Workflows

**Examine Issue**: `/examine-issue <N>` -- analyse issue, assess complexity, suggest approach
**Audit PR**: `/audit-pr <N>` -- comprehensive review (style + scientific + testing + docs + build)
**Check GitHub**: `/gh-check` -- show assigned issues, review requests, mentions

### Maintenance Workflows

**Release**
1. `/prep-release` -- assess necessity
2. Pre-flight: `/verify-build` + `/sync-docs` + `/lint-code`
3. `/log-changes` -- finalise CHANGELOG
4. PR, CI, merge, tag via `/gh-release`
5. Monitor PyPI, GitHub Release, Zenodo

**CI/Build**
1. `/verify-build` -- identify issues
2. Fix, `/verify-build` -- confirm
3. `make test-smoke`, `/log-changes` ([maintenance]), PR

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

After a PR is merged, verify via `/gh-debrief`:
- Issue closed (if linked with "Fixes #N")
- CI green on master
- Documentation deployed (if docs changed)
- No regression in dev tag CI

## Git Policy

- **Commit**: All directories and files
- **Ignore**: settings.local.json, any temp-* files
