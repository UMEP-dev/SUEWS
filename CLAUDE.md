# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## ⚠️ CLAUDE.md Protection Active

This file is protected against accidental truncation or content loss via pre-commit hook, GitHub Actions, and backup system.

## Style Guidelines

- **Language**: Use British English for all documentation, code comments, file names, and communication
  - **Exception**: Technical terms follow scientific computing conventions (e.g., "analyze" not "analyse", following numpy/scipy/matplotlib standard)
- **No emoji-like characters** in print/logging functions - use only plain ASCII characters

## Common Workflow

**When no .venv detected** (auto-setup pattern):
```bash
uv venv                      # Create environment
source .venv/bin/activate    # Activate
make dev                     # Install SUEWS in editable mode
make test                    # Run tests if needed
```

For detailed setup options, see `.claude/reference/quick-start.md`

## Documentation Structure

- **Developer reference**: `dev-ref/` - Coding guidelines, testing patterns, interfaces
- **Testing patterns**: `dev-ref/testing/` - Test design, error handling, CI tiers
- **User documentation**: `docs/` - Sphinx-generated user-facing documentation
- **Claude Code workspace**: `.claude/` - See `.claude/README.md` for structure

## Documentation Building

**Building documentation**:
```bash
make docs                    # Build HTML documentation (runs Sphinx)
cd docs && make livehtml     # Live-reload development server
```

**Key documentation points**:
- Documentation uses Sphinx with reStructuredText and Markdown
- **Auto-generated files** (DO NOT edit directly):
  - `docs/source/inputs/yaml/config-reference/` - RST files generated from Pydantic data models
  - These are rebuilt automatically when running `make docs`
- **Manually edited files** (safe to modify):
  - All other files in `docs/source/` including main documentation, guides, tutorials
  - To change auto-generation logic, modify `docs/generate_datamodel_rst.py`
- Documentation dependencies included in main environment (`dev` extras)

**When modifying documentation**:
- Edit source files in `docs/source/`
- Images go in `docs/source/images/` with descriptive names
- Use cross-references and proper RST/Markdown syntax
- Run `make docs` locally to check changes before committing

## Git and GitHub

- **IMPORTANT**: Always use `origin` as the only git remote for this repository
- Check remotes: `git remote -v`
- If multiple remotes exist, remove all except `origin` (git@github.com:UMEP-dev/SUEWS.git)

## Testing Requirements

**IMPORTANT**: Before committing, ALWAYS run `make test`

For benchmark test details and debugging guidance, see `.claude/reference/testing-guide.md`

## Development Reminders

- **Check for existing .venv** with editable supy - if so, use it rather than rebuilding
- **Include new files in meson.build** when creating source files:
  - Python files (.py) in `src/supy/`
  - Fortran files (.f90, .f95) in `src/suews/src/`
- **After conversation compaction**, remember to `source .venv/bin/activate` if venv exists
- **Documentation generation**: Only modify `generate_datamodel_rst.py` script, not the generated YAML RST files directly

## Configuration Pattern

When implementing features with configuration objects, follow strict separation of concerns between configuration parsing (high-level) and implementation (low-level).

See `.claude/reference/config-patterns.md` for detailed pattern and examples.

## Documentation Principles

Follow DRY (Don't Repeat Yourself) and "Brief Overview Pattern":
- Main files provide concise overviews
- Details go in `.claude/reference/`
- Guides go in `.claude/howto/`

See `.claude/reference/maintenance-principles.md` for complete principles and CHANGELOG rules.

## Quick Reference

**Environment setup**: `.claude/reference/quick-start.md`
**Testing guide**: `.claude/reference/testing-guide.md`
**Config patterns**: `.claude/reference/config-patterns.md`
**Maintenance principles**: `.claude/reference/maintenance-principles.md`
**Claude workspace**: `.claude/README.md`
