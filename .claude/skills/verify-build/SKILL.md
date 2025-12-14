---
name: verify-build
description: Check SUEWS build configuration consistency (meson, Makefile, pyproject.toml, CI). Use before releases, when modifying build files, adding new source files, or troubleshooting build issues. Validates source file inclusion in meson.build, dependency sync between pyproject.toml and env.yml, Python version matrix consistency, Makefile targets, and dual-build system (standard + UMEP).
---

# SUEWS Build Configuration Checker

Check build configuration consistency across `meson.build`, `Makefile`, `pyproject.toml`, `env.yml`, and CI workflows.

## Checks to Perform

### Critical

1. **Source inclusion in meson.build**
   - All `.f95` files in `src/suews/src/` must be in meson.build
   - All `.py` files in `src/supy/` must be in meson.build
   
   ```
   ISSUE: src/suews/src/suews_phys_new.f95 not in meson.build
   -> Add to sources in src/suews/meson.build
   ```

2. **Version management**
   - `get_ver_git.py` syntax valid
   - f90wrap version pinned (`==0.2.16`) consistently

### Warnings

3. **Dependency sync**: `pyproject.toml` ↔ `env.yml`
   ```
   ISSUE: lmfit in pyproject.toml but not in env.yml
   ISSUE: pandas>=2.0 vs pandas=1.5.3 version mismatch
   ```

4. **Python version matrix**
   - pyproject.toml classifiers match CI build matrix
   - `.github/workflows/build_wheels.yml` matrix alignment

5. **Makefile consistency**
   - All documented targets exist
   - Prerequisites are valid
   - Clean target covers all build artifacts

### Style

6. **Configuration alignment**
   - `.fprettify.rc` matches documented settings
   - `.ruff.toml` covers all source directories

## Output Format

```
[check-build] Build Configuration Analysis

=== Source Inclusion ===
  Fortran: N found, M in meson.build
    MISSING: <files>
  Python: N found, M in meson.build
    MISSING: <files>

=== Dependencies ===
  Mismatches: <list>
  pyproject.toml only: <list>
  env.yml only: <list>

=== CI Matrix ===
  pyproject.toml: 3.9, 3.10, 3.11, 3.12, 3.13, 3.14
  CI matrix: 3.9, 3.10, 3.11, 3.12, 3.13
  MISSING: 3.14

Summary: N critical, M warnings, O style
```

## Key Files

| File | Purpose |
|------|---------|
| `meson.build` | Build system configuration |
| `pyproject.toml` | Python packaging |
| `env.yml` | Conda environment |
| `Makefile` | Convenience targets |
| `.github/workflows/` | CI configuration |
| `.fprettify.rc` | Fortran formatting |
| `.ruff.toml` | Python formatting |

## Dual-Build System

SUEWS creates two versions from each tag:
- `2025.11.27` (standard, NumPy ≥2.0)
- `2025.11.27rc1` (UMEP, NumPy 1.x)

Verify both `build_wheels` and `build_umep` jobs exist in release workflow.
