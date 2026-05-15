# Build Checks Detail

## Critical Checks

### 1. Source Inclusion in meson.build

- All `.f95` files in `src/suews/src/` must be in meson.build
- All `.py` files in `src/supy/` must be in meson.build

```
ISSUE: src/suews/src/suews_phys_new.f95 not in meson.build
-> Add to sources in src/suews/meson.build
```

### 2. Version Management and Rust Bridge

- `get_ver_git.py` syntax valid
- Rust bridge (`src/suews_bridge/`) builds via maturin/PyO3
- `Cargo.toml` and `pyproject.toml` versions consistent

### 3. Schema Version Sync

`CURRENT_SCHEMA_VERSION` in `src/supy/data_model/schema/version.py` must match the `schema_version` field in `src/supy/sample_data/sample_config.yml`. A mismatch means either:
- The schema was bumped but sample_config wasn't updated, or
- sample_config was edited but the code constant wasn't bumped

```bash
CODE_VER=$(python -c "from supy.data_model.schema import CURRENT_SCHEMA_VERSION; print(CURRENT_SCHEMA_VERSION)")
YAML_VER=$(python -c "import yaml; print(yaml.safe_load(open('src/supy/sample_data/sample_config.yml'))['schema_version'])")
[ "$CODE_VER" = "$YAML_VER" ] && echo "[OK] Schema version sync: $CODE_VER" || echo "[FAIL] Schema version mismatch: code=$CODE_VER, sample_config=$YAML_VER"
```

## Warning Checks

### 3. Python Version Matrix

- pyproject.toml classifiers match CI build matrix
- `.github/workflows/build_wheels.yml` matrix alignment

### 4. Makefile Consistency

- All documented targets exist
- Prerequisites are valid
- Clean target covers all build artifacts

## Style Checks

### 5. Configuration Alignment

- `.fprettify.rc` matches documented settings
- `.ruff.toml` covers all source directories
