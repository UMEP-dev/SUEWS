---
name: verify-build
description: Check build config consistency (meson, pyproject.toml, CI).
---

# Verify Build

Check build configuration consistency.

## Checks

| Priority | Check |
|----------|-------|
| Critical | Source files in meson.build |
| Critical | f90wrap version pinned (==0.2.16) |
| Warning | pyproject.toml ↔ env.yml sync |
| Warning | Python version matrix alignment |
| Style | .fprettify.rc, .ruff.toml |

Details: `references/checks-detail.md`

## Key Files

| File | Purpose |
|------|---------|
| `meson.build` | Build system |
| `pyproject.toml` | Python packaging |
| `env.yml` | Conda environment |
| `.github/workflows/` | CI |

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
