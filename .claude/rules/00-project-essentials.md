# SUEWS Project Essentials

Rules that apply to all coding-agent sessions in this repository. [decision: software maintainer, 2026-11-17]

---

## Style

- **British English** in prose, documentation, comments, docstrings, file names, and log messages (`organise`, `colour`, `behaviour`, `artefact`) [decision: software maintainer, 2026-11-17]
  - Exception: preserve upstream numpy/scipy/pandas/matplotlib API spellings (`analyze`, `normalize`, `color`) where dictated by library conventions [decision: software maintainer, 2026-11-17]
- **ASCII-only output** (`0x00..0x7F`) in print/logging functions, validation reports, rules/docs source, and all generated files [decision: software maintainer, 2026-11-17]
  - Use `->` instead of Unicode arrows (`U+2192`)
  - Use `[X]` instead of Unicode cross marks (`U+2717`)
  - Use `[OK]` instead of Unicode checkmarks (`U+2713`)
  - Reason: ensures compatibility across all platforms and encodings

## Identifier and Parameter Naming

`.claude/rules/naming-convention.md` is the single source of truth for SUEWS variable, parameter, and YAML field naming: [fact: git grep -n "^## Rule [1-6]" .claude/rules/naming-convention.md]

- **Single-convention `snake_case`**: separate every word with `_` across Python identifiers and all user-facing YAML fields (including STEBBS `building_archetype` and `stebbs` blocks), with physical quantity leading (`temperature_wall_internal`, `thickness_wall_outer`). [fact: git grep -n "every meaningful word" .claude/rules/naming-convention.md && git grep -n "physical quantity comes first" .claude/rules/naming-convention.md]
- **Variant-neutral SUEWS identifiers**: prefer neutral verbs (`init`, `prepare`, `scale`, `compute`, `tune`) over `-ise`/`-ize` spelling pairs in SUEWS-owned names. [decision: software maintainer, 2026-11-17]
- **Protected legacy identifiers**: fused historical names (e.g. `netradiationmethod`, `storageheatmethod`, `soildepth`) require a dedicated PR with a `yaml_upgrade` migration path (`#1256`) -- do not rename them inline. Cross-layer Fortran TYPE and Rust struct field migrations are tracked in `#1324`/`#1325`/`#1326`. [decision: software maintainer, 2026-11-17]

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
  - Python files (`.py`) under `src/supy/`: add to `src/supy/meson.build` (`py.install_sources`) [fact: git grep -n py.install_sources src/supy/meson.build]
  - Fortran kernel files (`.f95`) under `src/suews/src/`: add to `src/suews/Makefile` and `src/supy/meson.build`, then regenerate `src/suews/Makefile.deps` via `python scripts/suews/gen_fortran_deps.py` (`test/core/test_fortran_deps.py` fails when stale) [fact: git grep -n suews_phys_ohm src/suews/Makefile src/supy/meson.build]
  - Fortran C-API facade files (`.f95`) under `src/suews_bridge/c_api/`: add to `src/suews_bridge/build.rs` [fact: git grep -n c_api/driver.f95 src/suews_bridge/build.rs]
  - A new `TYPE, PUBLIC` derived type in `src/suews/src/*.f95`: add it to `src/suews_bridge/bridge-manifest.json` (`python scripts/suews/check_bridge_manifest.py` checks coverage) [fact: git grep -n "TYPE_PUBLIC_RE = " scripts/suews/check_bridge_manifest.py]
- After adding or modifying any Fortran `USE` statement, rerun `python scripts/suews/gen_fortran_deps.py` so `make -j` module edges stay accurate [fact: git grep -n Makefile.deps scripts/suews/gen_fortran_deps.py]

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

For platform-specific setup (macOS, Linux, Windows), see the `Makefile` recipes and the installation docs.
The available skills are listed in `.claude/README.md` ("Available Skills").

## Build System Notes

- `FCFLAGS` env var is NOT forwarded through `make dev` -> meson-python pipeline
  - Fortran compiler flags live in `src/suews/Makefile.gfortran` (kernel) and
    `src/suews_bridge/build.rs` (C-API facades), selected by the build profile:
    `SUEWS_BUILD_PROFILE=release` (`-O3`, no runtime checks, the default) or
    `checked` (`-O0 -fcheck=all`; the nightly physics tier builds it too).
    `src/supy/run_make.py` and `src/suews_bridge/build.rs` read the same variable [fact: git grep -n fcheck=all src/suews/Makefile.gfortran src/suews_bridge/build.rs src/supy/run_make.py && git grep -n SUEWS_BUILD_PROFILE src/suews/Makefile.gfortran src/suews_bridge/build.rs src/supy/run_make.py]
  - Checked build locally, for debugging physics: `SUEWS_BUILD_PROFILE=checked make dev`
- `make clean` removes `build/` but meson-python may cache compiled extensions elsewhere
  - For a truly clean rebuild: `make clean && pip cache purge && make dev`

## Environment Reminders

- After conversation compaction, re-activate: `source .venv/bin/activate`
- Check for existing `.venv` with editable supy before rebuilding
- Documentation generation: change the generator, never its output (the generated
  files are listed in `.claude/rules/docs/conventions.md`)
