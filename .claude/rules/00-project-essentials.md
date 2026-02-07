# SUEWS Project Essentials

Rules that apply to ALL Claude Code sessions in this repository.

---

## Style

- **British English** for documentation, comments, file names, and communication
  - Exception: Scientific computing terms follow numpy/scipy conventions (e.g., "analyze" not "analyse")
- **ASCII-only output** in print/logging functions, validation reports, and all generated files
  - Use `->` not `→` for arrows
  - Use `[X]` not `✗` for failure indicators
  - Use `[OK]` not `✓` for success indicators
  - Reason: Ensures compatibility across all platforms and encodings

## Variant-Neutral Naming

Avoid British/American spelling variants (-ise/-ize) in identifiers. Use neutral alternatives:

| Avoid | Use Instead |
|-------|-------------|
| initialise/initialize | `init`, `setup`, `prepare` |
| finalise/finalize | `complete`, `finish`, `cleanup` |
| normalise/normalize | `scale`, `adjust`, `rescale` |
| analyse/analyze | `examine`, `process`, `compute` |
| optimise/optimize | `improve`, `tune`, `refine` |

## Git

- **IMPORTANT**: Always use `origin` as the ONLY remote
- Remote: `git@github.com:UMEP-dev/SUEWS.git`
- Check: `git remote -v`
- If multiple remotes exist, remove all except `origin`

## Before Committing

- **Default**: Run `make test-smoke` before committing (fast, critical tests)
- **Full test**: Run `make test` only when changes affect:
  - Test files in `test/`
  - Core physics modules
  - Data model changes
- Include new source files in `meson.build`:
  - Python files (.py) in `src/supy/`
  - Fortran files (.f90, .f95) in `src/suews/src/`

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

For full setup options, use `/setup-dev` command.

## Build System Notes

- `FCFLAGS` env var is NOT forwarded through `make dev` -> meson-python pipeline
  - Compiler flags are hard-coded in `meson.build` (`fast_build` vs full flag sets)
  - To add debug flags like `-fcheck=bounds`, modify `meson.build` directly
- `make clean` removes `build/` but meson-python may cache compiled extensions elsewhere
  - For a truly clean rebuild: `make clean && pip cache purge && make dev`

## Environment Reminders

- After conversation compaction, re-activate: `source .venv/bin/activate`
- Check for existing `.venv` with editable supy before rebuilding
- Documentation generation: Only modify `generate_datamodel_rst.py`, not generated RST files

## Key Directories

| Directory | Purpose |
|-----------|---------|
| `src/suews/src/` | Fortran source (see `rules/fortran/`) |
| `src/supy/` | Python wrapper (see `rules/python/`) |
| `docs/` | Documentation (see `rules/docs/`) |
| `test/` | Tests (see `rules/tests/`) |
| `.github/workflows/` | CI/Actions (see `rules/ci/`) |
