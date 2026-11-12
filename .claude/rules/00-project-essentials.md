# SUEWS Project Essentials

Rules that apply to all coding-agent sessions in this repository.

---

## Style

- **British English** in prose, documentation, comments, docstrings, file names, and log messages (`organise`, `colour`, `behaviour`, `artefact`)
  - Exception: preserve upstream numpy/scipy/pandas/matplotlib API spellings (`analyze`, `normalize`, `color`) where dictated by library conventions
- **ASCII-only output** (`0x00..0x7F`) in print/logging functions, validation reports, rules/docs source, and all generated files
  - Use `->` instead of Unicode arrows (`U+2192`)
  - Use `[X]` instead of Unicode cross marks (`U+2717`)
  - Use `[OK]` instead of Unicode checkmarks (`U+2713`)
  - Reason: ensures compatibility across all platforms and encodings

## Identifier and Parameter Naming

`.claude/rules/naming-convention.md` is the single source of truth for SUEWS variable, parameter, and YAML field naming:

- **Single-convention `snake_case`**: separate every word with `_` across Python identifiers and all user-facing YAML fields (including STEBBS `building_archetype` and `stebbs` blocks), with physical quantity leading (`temperature_wall_internal`, `thickness_wall_outer`).
- **Variant-neutral SUEWS identifiers**: prefer neutral verbs (`init`, `prepare`, `scale`, `compute`, `tune`) over `-ise`/`-ize` spelling pairs in SUEWS-owned names.
- **Protected legacy identifiers**: fused historical names (e.g. `netradiationmethod`, `storageheatmethod`, `soildepth`) require a dedicated PR with a `yaml_upgrade` migration path (`#1256`) -- do not rename them inline. Cross-layer Fortran TYPE and Rust struct field migrations are tracked in `#1324`/`#1325`/`#1326`.

## Git

- **IMPORTANT**: Always use `origin` as the ONLY remote
- Remote: `git@github.com:UMEP-dev/SUEWS.git`
- Check: `git remote -v`
- If multiple remotes exist, remove all except `origin`

## Before Committing

- **Default**: Run `make test-smoke` before committing (fast, critical tests)
- **Everyday suite**: Run `make test` (core, data model, physics, I/O; excludes
  slow tests and peripheral surfaces) when changes affect:
  - Test files in `test/`
  - Core physics modules
  - Data model changes
- **Peripheral surfaces**: `make test` deliberately skips `test/cmd`,
  `test/mcp`, `test/docs`, `test/knowledge`, and `test/umep`. When you touch
  one of those, run its directory directly (e.g. `pytest test/cmd`) or
  `make test-all`
- **Register new source files in build manifests**:
  - Python files (`.py`) under `src/supy/`: add to `src/supy/meson.build` (`py.install_sources`)
  - Fortran kernel files (`.f95`) under `src/suews/src/`: add to `src/suews/Makefile` and `src/supy/meson.build`, then regenerate `src/suews/Makefile.deps` via `python scripts/suews/gen_fortran_deps.py` (`test/core/test_fortran_deps.py` fails when stale)
  - Fortran C-API facade files (`.f95`) under `src/suews_bridge/c_api/`: add to `src/suews_bridge/build.rs` and update `src/suews_bridge/bridge-manifest.json` (`python scripts/suews/check_bridge_manifest.py` checks parity)
- After adding or modifying any Fortran `USE` statement, rerun `python scripts/suews/gen_fortran_deps.py` so `make -j` module edges stay accurate

## Quick Start

See the Quick Start in `AGENTS.md`. For platform-specific setup (macOS,
Linux, Windows), see the `Makefile` recipes and the installation docs.

## Build System Notes

- `FCFLAGS` env var is NOT forwarded through `make dev` -> meson-python pipeline
  - Fortran compiler flags live in `src/suews/Makefile.gfortran` (kernel) and
    `src/suews_bridge/build.rs` (C-API facades), selected by the build profile:
    `SUEWS_BUILD_PROFILE=release` (`-O3`, no runtime checks, the default) or
    `checked` (`-O0 -fcheck=all`; the nightly physics tier builds it too).
    `src/supy/run_make.py` and `src/suews_bridge/build.rs` read the same variable
  - Checked build locally, for debugging physics: `SUEWS_BUILD_PROFILE=checked make dev`
- `make clean` removes `build/` but meson-python may cache compiled extensions elsewhere
  - For a truly clean rebuild: `make clean && pip cache purge && make dev`

## Environment Reminders

- After conversation compaction, re-activate: `source .venv/bin/activate`
- Check for existing `.venv` with editable supy before rebuilding
- Documentation generation: change the generator, never its output (the generated
  files are listed in `.claude/rules/docs/conventions.md`)
