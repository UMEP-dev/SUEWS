# Build Checks Detail

## Critical Checks

### 1. Source Inclusion and Fortran Dependency Graph

- Every `.f95` / `.f90` file in `src/suews/src/` (and compiled `src/suews/ext_lib/` objects) must be listed in `src/supy/meson.build` (`src_f95_all`) and `src/suews/Makefile` (`src/suews/meson.build` does not exist).
- Every shipped `.py` file (and non-Python package resource) under `src/supy/` must be installed via `py.install_sources` in `src/supy/meson.build`.
- After adding a Fortran file or `USE` statement, `src/suews/Makefile.deps` must be up to date so parallel `make -j` builds succeed:

```bash
python scripts/suews/gen_fortran_deps.py --check
```

```text
ISSUE: src/suews/src/suews_phys_new.f95 missing from build lists
-> Add to src_f95_all in src/supy/meson.build and objects in src/suews/Makefile,
   then run: python scripts/suews/gen_fortran_deps.py
```

### 2. Version Plumbing, Rust Bridge, and Bridge Manifest

- Dynamic package versioning: `pyproject.toml` (`supy`) and `mcp/pyproject.toml` (`suews-mcp`) compute versions dynamically via `get_ver_git.py` (with `suews_mcp.__version__` fallback chain). Do **not** compare `src/suews_bridge/Cargo.toml` (`version = "0.1.0"`) against `pyproject.toml` -- the internal Rust crate version is intentionally static.
- Rust bridge (`src/suews_bridge/`): built by `cargo` via `src/supy/build_rust_bridge.py::main` during Meson builds (`--features python-extension,physics,arrow-output`, using PyO3 `abi3-py312` for a single `cp312-abi3` wheel supporting `numpy>=1.22`) and via `make bridge` (`cargo build --release --features physics,arrow-output`) for the standalone `suews-engine` CLI.
- Fortran-to-bridge type coverage must pass:

```bash
python get_ver_git.py
python scripts/suews/check_bridge_manifest.py
```

### 3. Schema Version and Short Sample Fixture Sync

`CURRENT_SCHEMA_VERSION` in `src/supy/data_model/configuration/version.py` must match `schema_version` in `src/supy/sample_data/sample_config.yml`, and `test/fixtures/sample_short/sample_config_short.yml` (+ `Kc_2012_data_60_jan.txt`) must remain byte-synced with `sample_config.yml` via `make_sample_short.py`:

```bash
CODE_VER=$(python -c "from supy.data_model.configuration import CURRENT_SCHEMA_VERSION; print(CURRENT_SCHEMA_VERSION)")
YAML_VER=$(python -c "import yaml; print(yaml.safe_load(open('src/supy/sample_data/sample_config.yml', encoding='utf-8'))['schema_version'])")
[ "$CODE_VER" = "$YAML_VER" ] && echo "[OK] Schema version sync: $CODE_VER" || echo "[X] Schema version mismatch: code=$CODE_VER, sample_config=$YAML_VER"

# Regenerate / verify short sample fixture parity (verified in CI by test_sample_short_fixture.py)
python test/fixtures/sample_short/make_sample_short.py
```

### 4. Wheel Licence Metadata and Plugin Bundle Sync

- Wheel/archive licence metadata (`MPL-2.0 AND Apache-2.0` plus SPARTACUS `LICENSE` and `NOTICE`) is validated on built wheels:
  ```bash
  python scripts/suews/check_dist_license.py dist/*.whl
  ```
- Distributable skill bundle (`plugins/suews/skills/`) is generated from `.claude/skills/` and guarded by `test/mcp/test_packaging_manifests.py`:
  ```bash
  make plugin   # runs python scripts/build_plugin.py
  ```

## Warning Checks

### 5. Python Version Matrix and CI Workflows

- `pyproject.toml` `requires-python` (`>=3.12`) and classifiers match the CI wheel/test matrices in `.github/workflows/build-wheels-reusable.yml` and `.github/workflows/build-publish_to_pypi.yml` (single `YYYY.M.D` release line; the retired `YYYY.M.Drc1` dual-build workflow no longer exists).
- Companion MCP package (`mcp/pyproject.toml`) builds cleanly (`python -m build mcp`) and `build_mcp` job in `.github/workflows/build-publish_to_pypi.yml` stays aligned.

### 6. Makefile and Build Profiles

- Documented `Makefile` targets (`setup`, `dev`, `docs-setup`, `test`, `test-smoke`, `test-all`, `audit-deps`, `docs`, `clean`, `bridge`, `format`, `plugin`, `agent-plugin`) exist and have valid prerequisites.
- Fortran build profiles in `src/suews/Makefile.gfortran` (`SUEWS_BUILD_PROFILE=release` default `-O3`, or `SUEWS_BUILD_PROFILE=checked` with runtime bounds/pointer/NaN checks) are honoured consistently by `src/supy/run_make.py` and `src/suews_bridge/build.rs`.

## Style Checks

### 7. Tool Configuration Alignment

- `.fprettify.rc` matches documented Fortran formatting settings
- `.ruff.toml` covers repository Python directories (`src`, `test`, `scripts`, `mcp`, `docs`)
