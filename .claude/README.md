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
├── commands/              # Thin wrappers that invoke skills
│   ├── audit-pr.md        # Review a pull request
│   ├── examine-issue.md   # Analyse GitHub issues
│   ├── lint-code.md       # Check code style
│   ├── log-changes.md     # Update CHANGELOG
│   ├── prep-release.md    # Prepare release
│   ├── setup-dev.md       # Set up dev environment
│   ├── sync-docs.md       # Check doc-code consistency
│   └── verify-build.md    # Verify build configuration
│
├── skills/                # Action-oriented workflows (named with -skill suffix)
│   ├── audit-pr-skill/    # PR review orchestrator
│   ├── examine-issue-skill/  # Issue analysis
│   ├── lint-code-skill/   # Code style (references rules/)
│   ├── log-changes-skill/ # CHANGELOG management
│   ├── prep-release-skill/  # Release preparation
│   ├── setup-dev-skill/   # Environment setup guide
│   ├── sync-docs-skill/   # Doc-code consistency
│   └── verify-build-skill/  # Build config checks
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

Skills perform specific workflows when invoked via commands. Each skill has:
- `SKILL.md` - Main content with frontmatter (name, description)
- Optional subdirectories for references

**Key difference from rules**: Skills are invoked explicitly; rules are always available.

### Commands (Entry Points)

Commands are thin wrappers that invoke skills. They provide:
- Short description for the command menu
- Dynamic context (git status, dates, etc.)
- Reference to the skill to invoke

## Available Commands

| Command | Purpose |
|---------|---------|
| `/audit-pr <PR>` | Review a pull request comprehensively |
| `/examine-issue <issue>` | Analyse a GitHub issue |
| `/lint-code` | Check code style |
| `/log-changes` | Update CHANGELOG |
| `/prep-release` | Prepare release |
| `/setup-dev` | Set up development environment |
| `/sync-docs` | Check doc-code consistency |
| `/verify-build` | Verify build configuration |

## Rules vs Skills

| Aspect | Rules | Skills |
|--------|-------|--------|
| Loading | Automatic | On-demand |
| Purpose | Conventions, guidelines | Workflows, actions |
| Location | `.claude/rules/` | `.claude/skills/` |
| Invocation | None needed | Via `/command` |
| Path-conditional | Yes (`paths:` frontmatter) | No |

## Skill Relationships

```
prep-release-skill ──┬── verify-build-skill (pre-flight)
                     ├── sync-docs-skill (pre-flight)
                     ├── lint-code-skill (pre-flight)
                     └── log-changes-skill (CHANGELOG)

audit-pr-skill ──────┬── lint-code-skill (style review)
                     ├── sync-docs-skill (doc review)
                     └── verify-build-skill (build review)
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
