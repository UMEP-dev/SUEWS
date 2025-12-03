# Claude Code Workspace Directory

This directory contains all Claude Code-specific documentation, plans, and configuration for SUEWS development.

## Directory Structure

```
.claude/
├── commands/              # Thin wrappers that invoke skills
│   ├── audit-pr.md        # Review a pull request
│   ├── setup-dev.md       # Set up development environment
│   ├── lint-code.md       # Check code style
│   ├── log-changes.md     # Analyse code changes, update CHANGELOG
│   ├── prep-release.md    # Prepare release
│   ├── sync-docs.md       # Check doc-code consistency
│   └── verify-build.md    # Verify build configuration
│
├── skills/                # Action-oriented workflows
│   ├── audit-pr/          # PR review orchestrator
│   ├── lint-code/         # Code style conventions (includes naming)
│   ├── log-changes/       # CHANGELOG management
│   ├── prep-release/      # Release preparation (composes other skills)
│   ├── setup-dev/         # Environment setup guide
│   ├── sync-docs/         # Documentation content consistency
│   └── verify-build/      # Build configuration checks
│
├── reference/             # Knowledge documents (not actionable)
│   ├── code-patterns.md   # Config patterns, DRY principles
│   ├── test-patterns.md   # Test design with FIRST principles
│   └── templates/         # Reusable templates
│
├── scripts/               # Shared infrastructure
│   ├── validate-claude-md.py
│   ├── pre-commit-hook.sh
│   └── setup-claude-protection.sh
│
└── README.md              # This file
```

## Concepts

### Skills (Action-Oriented)

Skills perform specific workflows. Each skill has:
- `SKILL.md` - Main content with frontmatter (name, description)
- Optional subdirectories for references, templates, or scripts

**Key principle**: Skills DO things. If it's purely reference knowledge, it belongs in `reference/`.

### Reference Documents (Knowledge)

Reference documents contain patterns and guidelines that inform work but don't perform actions:
- `code-patterns.md` - How to write config code, documentation
- `test-patterns.md` - How to design tests with FIRST principles

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
| `audit-pr` | Comprehensive PR review (orchestrates other skills) |
| `lint-code` | Code style + variant-neutral naming (Fortran, Python, RST, MD) |
| `log-changes` | CHANGELOG management and formatting |
| `prep-release` | Release preparation (composes verify-build, log-changes) |
| `setup-dev` | Environment setup (uv, venv, mamba, compilers) |
| `sync-docs` | Documentation-code content consistency |
| `verify-build` | Build configuration consistency |

## Available Commands

| Command | Purpose |
|---------|---------|
| `/audit-pr <PR>` | Review a pull request comprehensively |
| `/setup-dev` | Set up development environment |
| `/lint-code` | Check code style |
| `/log-changes` | Update CHANGELOG |
| `/prep-release` | Prepare release |
| `/sync-docs` | Check doc-code consistency |
| `/verify-build` | Verify build configuration |

## Reference Documents

| Document | Use When |
|----------|----------|
| `reference/code-patterns.md` | Implementing features, writing docs |
| `reference/test-patterns.md` | Writing tests, setting tolerances |
| `reference/templates/` | Creating commits, planning features |

## Skill Relationships

```
prep-release ──┬── verify-build (pre-flight)
               ├── sync-docs (pre-flight)
               ├── lint-code (pre-flight)
               └── log-changes (CHANGELOG)

audit-pr ──────┬── lint-code (style review)
               ├── sync-docs (doc review)
               └── verify-build (build review)

lint-code ─────── Includes variant-neutral naming (was check-naming)
```

## Quick Navigation

**"How do I set up my environment?"** -> `/setup-dev` or `setup-dev` skill
**"Check my code style"** -> `/lint-code` or `lint-code` skill
**"Update the CHANGELOG"** -> `/log-changes`
**"Prepare for release"** -> `/prep-release`
**"Understand code patterns"** -> `reference/code-patterns.md`
**"How to write tests"** -> `reference/test-patterns.md`

## For Claude Code Sessions

1. Check current branch: `git branch --show-current`
2. Environment setup: Use `setup-dev` skill
3. Before committing: Use `lint-code` skill

## Git Policy

- Commit: All directories and files
- Ignore: settings.local.json, any temp-* files
