# Claude Code Workspace Directory

This directory contains all Claude Code-specific documentation, plans, and configuration for SUEWS development.

## Directory Structure

```
.claude/
├── commands/              # Thin wrappers that invoke skills
│   ├── setup-dev.md       # Set up development environment
│   ├── lint-code.md       # Check code style
│   ├── log-changes.md     # Analyse code changes, update CHANGELOG
│   ├── prep-release.md    # Prepare release
│   ├── sync-docs.md       # Check doc-code consistency
│   └── verify-build.md    # Verify build configuration
│
├── skills/                # All knowledge + functionality
│   ├── apply-patterns/    # Config patterns, DRY principles
│   ├── check-naming/      # Variant-neutral naming
│   ├── design-tests/      # Test design with FIRST principles
│   ├── lint-code/         # Code style conventions
│   ├── log-changes/       # CHANGELOG management
│   ├── prep-release/      # Release preparation
│   ├── setup-dev/         # Environment setup guide
│   ├── sync-docs/         # Documentation consistency
│   └── verify-build/      # Build configuration checks
│
├── scripts/               # Shared infrastructure
│   ├── validate-claude-md.py
│   ├── pre-commit-hook.sh
│   └── setup-claude-protection.sh
│
└── README.md              # This file
```

## Concepts

### Skills (Single Source of Truth)

Skills contain all the knowledge Claude needs for specific workflows. Each skill has:
- `SKILL.md` - Main content with frontmatter (name, description)
- Optional subdirectories for references, templates, or scripts

### Commands (Thin Wrappers)

Commands are entry points that invoke skills. They provide:
- Short description for the command menu
- Any dynamic context (git status, dates, etc.)
- Reference to the skill to invoke

### Scripts (Infrastructure)

Scripts handle CLAUDE.md protection and Git hooks - shared infrastructure that isn't skill-specific.

## Available Skills

| Skill | Purpose |
|-------|---------|
| `apply-patterns` | Configuration patterns, DRY principles |
| `check-naming` | Variant-neutral naming conventions |
| `design-tests` | Test design with FIRST principles |
| `lint-code` | Code conventions for Fortran and Python |
| `log-changes` | CHANGELOG management and formatting |
| `prep-release` | Release preparation with pre-flight checks |
| `setup-dev` | Environment setup (uv, venv, mamba, compilers) |
| `sync-docs` | Documentation-code consistency |
| `verify-build` | Build configuration consistency |

## Available Commands

| Command | Purpose |
|---------|---------|
| `/setup-dev` | Set up development environment |
| `/lint-code` | Check code style |
| `/log-changes` | Update CHANGELOG |
| `/prep-release` | Prepare release |
| `/sync-docs` | Check doc-code consistency |
| `/verify-build` | Verify build configuration |

## Quick Navigation

**"How do I set up my environment?"** -> `/setup-dev` or `setup-dev` skill
**"Check my code style"** -> `/lint-code` or `lint-code` skill
**"Update the CHANGELOG"** -> `/log-changes`
**"Prepare for release"** -> `/prep-release`

## For Claude Code Sessions

1. Check current branch: `git branch --show-current`
2. Environment setup: Use `setup-dev` skill
3. Before committing: Use `lint-code` skill

## Git Policy

- Commit: All directories and files
- Ignore: settings.local.json, any temp-* files
