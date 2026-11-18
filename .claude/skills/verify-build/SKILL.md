---
name: verify-build
description: Check build config consistency across Meson, Fortran Makefiles, Rust bridge, pyproject.toml, mcp, fixtures, plugin bundle, and CI.
---

# Verify Build

Check build configuration and packaging consistency across the SUEWS repository.

## Checks

- Critical
  - Source files registered in `src/supy/meson.build` (`src_f95_all` and `py.install_sources`) and `src/suews/Makefile`
  - Fortran module dependency edges current: `python scripts/suews/gen_fortran_deps.py --check` (`src/suews/Makefile.deps`)
  - Rust bridge (`src/suews_bridge/`) builds cleanly via `cargo` (`src/supy/build_rust_bridge.py` in Meson and `make bridge` standalone) and passes `python scripts/suews/check_bridge_manifest.py`
  - Schema and sample fixture sync: `CURRENT_SCHEMA_VERSION == sample_config.yml schema_version`, and `sample_config_short.yml` matches `python test/fixtures/sample_short/make_sample_short.py`
  - Distributable plugin bundle synced: `make plugin` (`python scripts/build_plugin.py`)
  - Built wheel licence metadata valid: `python scripts/suews/check_dist_license.py <wheel>`
- Warning
  - Python version matrix alignment (`pyproject.toml`, `.github/workflows/build-wheels-reusable.yml`, `.github/workflows/build-publish_to_pypi.yml`)
  - Fortran build profile support: `SUEWS_BUILD_PROFILE=release|checked` (`src/suews/Makefile.gfortran`, `src/supy/run_make.py`, `src/suews_bridge/build.rs`)
  - Companion MCP package consistency (`mcp/pyproject.toml` dynamic version and build)
- Style
  - `.fprettify.rc`, `.ruff.toml`

Details: `references/checks-detail.md`

## Key Files

- `meson.build` and `src/supy/meson.build` -- Meson build definitions (`src_f95_all`, `py.install_sources`, Rust bridge custom target)
- `src/suews/Makefile`, `src/suews/Makefile.deps`, `src/suews/Makefile.gfortran` -- Fortran compilation, module dependency graph, and `SUEWS_BUILD_PROFILE=release|checked` flags
- `src/supy/build_rust_bridge.py` and `src/suews_bridge/Cargo.toml` -- Cargo build orchestration (`cp312-abi3` wheel via PyO3 `abi3-py312`, NumPy-free ABI supporting `numpy>=1.22`)
- `pyproject.toml` and `mcp/pyproject.toml` -- Python packaging for `supy` and `suews-mcp` (dynamic versions via `get_ver_git.py` / `suews_mcp.__version__`)
- `.github/workflows/build-wheels-reusable.yml` and `.github/workflows/build-publish_to_pypi.yml` -- CI wheel build and PyPI publishing
- `src/supy/data_model/configuration/version.py` -- `CURRENT_SCHEMA_VERSION`
- `src/supy/sample_data/sample_config.yml` and `test/fixtures/sample_short/sample_config_short.yml` -- canonical and short sample configurations
- `scripts/build_plugin.py` -- generates `plugins/suews/skills/` from `.claude/skills/` (`make plugin`)

## Single-Wheel ABI Note

Releases publish a single `YYYY.M.D` distribution (`cp312-abi3` stable-ABI wheel on CPython >=3.12). Because the Rust bridge is NumPy-free at the C-ABI boundary, one wheel supports `numpy>=1.22` (covering both modern Python stacks and QGIS/UMEP); the pre-2026-04 `YYYY.M.Drc1` dual-build line is retired.

## Output Format

```text
[check-build] Build Configuration Analysis

=== Source & Dependency Graph ===
  Fortran: N found, M in src/supy/meson.build (src_f95_all) and src/suews/Makefile
  Makefile.deps: [OK] | [X] stale (run python scripts/suews/gen_fortran_deps.py)
  MISSING: <files>

=== Bridge, Schema, Fixtures & Packaging ===
  Bridge manifest: [OK] | [X] <details>
  Schema & sample_short sync: [OK] | [X] <details>
  Plugin bundle & mcp package: [OK] | [X] <details>

Summary: N critical, M warnings
```

## References

- `references/checks-detail.md` - Full check descriptions and commands
