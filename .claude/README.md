# Claude Code Workspace Directory

This directory contains all Claude Code-specific documentation, plans, and configuration for SUEWS development.

## Directory Structure

```
.claude/
├── howto/               # Step-by-step guides
│   └── setup-environment.md
├── reference/           # Technical documentation
│   ├── quick-start.md
│   ├── testing-guide.md
│   ├── config-patterns.md
│   ├── maintenance-principles.md
│   └── README.md
├── templates/           # Reusable templates
│   ├── feature-plan.md
│   ├── commit-message.md
│   └── README.md
├── commands/            # Custom slash commands
│   └── log-changes.md
├── skills/              # Claude skills for SUEWS workflows
│   ├── lint-code/
│   ├── verify-build/
│   ├── sync-docs/
│   └── prep-release/
├── scripts/             # Automation scripts
└── agents/              # Custom agent definitions
```

## Directory Purposes

### howto/
**Purpose**: Step-by-step guides for common tasks
- `setup-environment.md` - Python environment setup options and troubleshooting
- **Quick start**: See `reference/quick-start.md`

### reference/
**Purpose**: Technical documentation and analysis
- `quick-start.md` - Canonical setup commands (single source of truth)
- `testing-guide.md` - Testing requirements and benchmark details
- `config-patterns.md` - Configuration design patterns
- `maintenance-principles.md` - Documentation and code principles

### templates/
**Purpose**: Reusable templates for consistency
- Feature plan template
- Commit message format
- Other common documents

### commands/
**Purpose**: Custom slash commands for automation
- `/log-changes` - Analyse recent changes and update docs/CHANGELOG
- Add new commands as .md files in this directory

### skills/
**Purpose**: Claude skills for SUEWS-specific workflows
- `lint-code` - Code conventions (Fortran + Python)
- `verify-build` - Build configuration checks
- `sync-docs` - Documentation-code consistency
- `prep-release` - Release preparation workflow

## Quick Navigation

**"How do I...?"** → Check `howto/`
**"Why does X work this way?"** → Check `reference/`
**"What's the status of feature Y?"** → Check GitHub issues and PRs
**"I need to create a new Z"** → Check `templates/`

## For Claude Code Sessions

1. Check current branch: `git branch --show-current`
2. Check related GitHub issue or PR for context
3. Environment setup: See `.claude/reference/quick-start.md`


## Slash Commands

Custom commands for streamlined workflows:

### /log-changes
Analyses recent code changes and updates documentation:

- Checks commits since last CHANGELOG.md update
- Categorises changes by type ([feature], [bugfix], etc.)
- Updates CHANGELOG.md with new entries
- Identifies and updates affected documentation
- Runs documentation generation scripts as needed

**Usage**: `/log-changes`

## Skills

SUEWS-specific skills in `.claude/skills/` provide specialised knowledge for development workflows:

| Skill | Purpose |
|-------|---------|
| `lint-code` | Code conventions for Fortran and Python |
| `verify-build` | Build configuration consistency |
| `sync-docs` | Documentation-code consistency (science, API, config) |
| `prep-release` | Release preparation with pre-flight checks |

Skills are triggered automatically based on context (e.g., "lint this code" triggers `lint-code`).

## Git Policy
- ✅ Commit: All directories and files (except settings.local.json)
- ❌ Ignore: settings.local.json, any temp-* files