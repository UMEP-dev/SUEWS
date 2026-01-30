---
name: start-work
description: Start a development workflow. Single entry point for feature development, bug fixes, releases, documentation, refactoring, and CI/build maintenance. Use when beginning any new task.
---

# Start Work

Select and launch a development workflow. Two-step selection via AskUserQuestion.

## Triggers

- "Start work", "begin work", "new task"
- "Start feature", "start bugfix", "start release"
- "What should I work on?"
- Direct: `/start-work feature`, `/start-work review`, `/start-work release`

## Step 1: Choose Category

Use AskUserQuestion with these 3 options:

- **Create** -- Write code, build features, fix bugs, refactor, update docs
- **Review** -- Examine issues, review PRs, check what needs attention
- **Maintenance** -- Release, CI/build fixes, repository upkeep

## Step 2: Choose Specific Workflow

Based on the category selected, present a second AskUserQuestion:

### If Create:
- **Feature** -- New functionality (requires issue)
- **Bug Fix** -- Fix a reported problem (requires issue)
- **Refactoring** -- Restructure code, preserve behaviour (requires issue)
- **Documentation** -- Update docs only

### If Review:
- **Examine Issue** -- Analyse a GitHub issue in depth
- **Audit PR** -- Review a pull request comprehensively
- **Check GitHub** -- What needs my attention (assigned, review requests)

### If Maintenance:
- **Release** -- Prepare and publish a new version
- **CI/Build** -- Build system or CI pipeline maintenance
- **Checks** -- Run verify-build + sync-docs + lint-code

## Per-Workflow Actions

After selection, run the initial stages for the chosen workflow:

### Create: Feature / Bug Fix / Refactoring

1. Ask for issue number (GitHub `#N`)
2. Run `/examine-issue <N>` -- analyse and assess complexity
3. Run `/gh-link <N>` -- create branch linked to issue
4. `make dev` -- build the project
5. `make test-smoke` -- baseline (or full `make test` for Refactoring)
6. Report readiness with next-stage guidance

### Create: Documentation

1. Create branch: `git checkout -b docs/<topic>`
2. `make dev` -- build the project
3. `make docs` -- verify baseline documentation builds
4. Report readiness with editing guidance

### Review: Examine Issue

1. Ask for issue number (GitHub `#N`)
2. Run `/examine-issue <N>`

### Review: Audit PR

1. Ask for PR number
2. Run `/audit-pr <N>`

### Review: Check GitHub

1. Run `/gh-check` -- show assigned issues, review requests, mentions

### Maintenance: Release

1. Run `/prep-release` -- assess necessity and select dev tag
2. Run `/gh-check` -- open issues needing attention
3. Run `/verify-build` + `/sync-docs` + `/lint-code` -- pre-flight checks
4. Report readiness with release branch instructions

### Maintenance: CI/Build

1. Run `/verify-build` -- identify current issues
2. Create branch: `git checkout -b ci/<topic>`
3. Report issues found with fix guidance

### Maintenance: Checks

1. Run `/verify-build` -- build config consistency
2. Run `/sync-docs` -- doc-code content consistency
3. Run `/lint-code` -- code style
4. Report summary of all findings

## Subsequent Stages

After initial setup, guide the user to the next stage. Full workflow details in `references/workflow-details.md`.

All Create workflows converge on:
- **Quality gate**: Handled automatically by pre-commit hook on `git commit`
- **CHANGELOG**: `/log-changes` before PR
- **PR creation**: `gh pr create` to open PR, `/gh-sync` to rebase first
- **PR review**: `/audit-pr <N>` for comprehensive review

## References

- `references/workflow-details.md` -- Full stage definitions for all workflows
