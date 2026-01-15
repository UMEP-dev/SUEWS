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
│   └── changelog/
│       └── format.md               # When editing CHANGELOG.md
│
├── skills/                # Action-oriented workflows (invoked via /skill-name)
│   ├── audit-pr/          # PR review orchestrator
│   ├── examine-issue/     # Issue analysis
│   ├── lint-code/         # Code style (references rules/)
│   ├── log-changes/       # CHANGELOG management
│   ├── prep-release/      # Release preparation
│   ├── setup-dev-env/     # Environment setup guide
│   ├── sync-docs/         # Doc-code consistency
│   └── verify-build/      # Build config checks
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

- `/audit-pr <PR>` - Review a pull request comprehensively
- `/examine-issue <issue>` - Analyse a GitHub issue
- `/lint-code` - Check code style
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare release
- `/setup-dev` - Set up development environment
- `/sync-docs` - Check doc-code consistency
- `/verify-build` - Verify build configuration

## Rules vs Skills

- **Loading**: Rules are automatic; skills are on-demand
- **Purpose**: Rules define conventions/guidelines; skills perform workflows/actions
- **Location**: Rules in `.claude/rules/`; skills in `.claude/skills/`
- **Invocation**: Rules need none; skills via `/skill-name`
- **Path-conditional**: Rules yes (`paths:` frontmatter); skills no

## Skill Relationships

```
prep-release ────────┬── verify-build (pre-flight)
                     ├── sync-docs (pre-flight)
                     ├── lint-code (pre-flight)
                     └── log-changes (CHANGELOG)

audit-pr ────────────┬── lint-code (style review)
                     ├── sync-docs (doc review)
                     └── verify-build (build review)
```

## Quick Navigation

- **"How do I set up my environment?"** -> `/setup-dev`
- **"Check my code style"** -> `/lint-code`
- **"Update the CHANGELOG"** -> `/log-changes`
- **"Prepare for release"** -> `/prep-release`
- **"Fortran conventions"** -> `.claude/rules/fortran/`
- **"Python conventions"** -> `.claude/rules/python/`
- **"Test patterns"** -> `.claude/rules/tests/`

## Git Policy

- **Commit**: All directories and files
- **Ignore**: settings.local.json, any temp-* files
