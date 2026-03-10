# Build Checks Detail

## Critical Checks

### 1. Source Inclusion in meson.build

- All `.f95` files in `src/suews/src/` must be in meson.build
- All `.py` files in `src/supy/` must be in meson.build

```
ISSUE: src/suews/src/suews_phys_new.f95 not in meson.build
-> Add to sources in src/suews/meson.build
```

### 2. Version Management

- `get_ver_git.py` syntax valid
- f90wrap version pinned (`==0.2.16`) consistently

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
