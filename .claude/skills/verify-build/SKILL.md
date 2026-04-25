---
name: verify-build
description: Check build config consistency (meson, pyproject.toml, CI).
---

# Verify Build

Check build configuration consistency.

## Checks

- Critical
  - Source files in meson.build
  - Rust bridge (suews_bridge) builds cleanly
  - Schema version sync (CURRENT_SCHEMA_VERSION == sample_config.yml schema_version)
- Warning
  - Python version matrix alignment
- Style
  - .fprettify.rc, .ruff.toml

Details: `references/checks-detail.md`

## Key Files

- `meson.build` — Build system
- `pyproject.toml` — Python packaging
- `.github/workflows/` — CI
- `src/supy/data_model/schema/version.py` — CURRENT_SCHEMA_VERSION
- `src/supy/sample_data/sample_config.yml` — schema_version field

## Output Format

```
[check-build] Build Configuration Analysis

=== Source Inclusion ===
  Fortran: N found, M in meson.build
  MISSING: <files>

=== Dependencies ===
  Mismatches: <list>

Summary: N critical, M warnings
```

## Dual-Build

Each release creates:
- `YYYY.M.D` (NumPy ≥2.0)
- `YYYY.M.Drc1` (UMEP, NumPy 1.x)

## References

- `references/checks-detail.md` - Full check descriptions
