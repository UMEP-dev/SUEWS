# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

For full setup options, use `/setup-dev` command.

## Style Guidelines

- **Language**: Use British English for documentation, code comments, file names
  - Exception: Scientific computing terms follow numpy/scipy conventions (e.g., "analyze")
- **No emoji-like characters** in print/logging functions - use plain ASCII only

## Testing

- **Default**: Run `make test-smoke` before committing (fast, critical tests ~20s)
- **Full test**: Run `make test` only when changes affect test files, physics, or data models

## Project Structure

| Directory | Purpose | Conventions |
|-----------|---------|-------------|
| `src/suews/src/` | Fortran source | `.claude/rules/fortran/` |
| `src/supy/` | Python wrapper | `.claude/rules/python/` |
| `docs/` | Sphinx documentation | `.claude/rules/docs/` |
| `test/` | pytest tests | `.claude/rules/tests/` |

## Documentation Building

```bash
make docs                    # Build HTML documentation
cd docs && make livehtml     # Live-reload development server
```

**Auto-generated files** (DO NOT edit directly):
- `docs/source/inputs/yaml/config-reference/` - RST from Pydantic models
- To change: modify `docs/generate_datamodel_rst.py`

## Git

- **IMPORTANT**: Always use `origin` as the only remote (`git@github.com:UMEP-dev/SUEWS.git`)

## Development Reminders

- Check for existing `.venv` with editable supy before rebuilding
- Include new source files in `meson.build`:
  - Python files (.py) in `src/supy/`
  - Fortran files (.f90, .f95) in `src/suews/src/`
- After conversation compaction, re-activate: `source .venv/bin/activate`

## Claude Code Workspace

See `.claude/README.md` for available skills and commands:

- `/setup-dev` - Environment setup
- `/lint-code` - Check code style
- `/audit-pr` - Review pull requests
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare releases

## Rules (Auto-Loaded)

Language-specific conventions auto-load based on files being edited:

- `.claude/rules/00-project-essentials.md` - Always loaded
- `.claude/rules/fortran/` - When editing Fortran files
- `.claude/rules/python/` - When editing Python files
- `.claude/rules/docs/` - When editing documentation
- `.claude/rules/tests/` - When editing tests
- `.claude/rules/changelog/` - When editing CHANGELOG.md
