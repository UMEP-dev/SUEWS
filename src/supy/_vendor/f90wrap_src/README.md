# Vendored f90wrap Runtime Files

This directory contains a minimal subset of [f90wrap](https://github.com/jameskermode/f90wrap) source files required for building the supy Fortran extension.

## Why Vendored?

These files are vendored (copied directly) rather than used as a git submodule to:

1. **Eliminate worktree issues** - Git submodules require explicit initialisation in each worktree
2. **Simplify builds** - No `git submodule update --init` needed before building
3. **Reduce dependencies** - Only runtime files are included, not the full f90wrap package

## Files Included

| File | Purpose |
|------|---------|
| `f90wrap/sizeoffortran.f90` | Fortran subroutine to determine pointer sizes |
| `f90wrap/arraydatamodule.c` | C extension for NumPy array interface |
| `f90wrap/runtime.py` | Python runtime for wrapped Fortran types |
| `f90wrap/fortrantype.py` | Fortran derived type handling |

## Source

- **Repository**: https://github.com/jameskermode/f90wrap
- **Commit**: `1cd22106fcd055d4d796046dabcb444fa82ed2da`
- **Version**: v0.2.16
- **License**: LGPL-3.0 (see LICENSE file)

## Updating

To update these files from upstream:

```bash
# Clone f90wrap
git clone https://github.com/jameskermode/f90wrap.git /tmp/f90wrap
cd /tmp/f90wrap
git checkout <new-version-tag>

# Copy essential files
cp f90wrap/sizeoffortran.f90 <suews>/src/supy/_vendor/f90wrap_src/f90wrap/
cp f90wrap/arraydatamodule.c <suews>/src/supy/_vendor/f90wrap_src/f90wrap/
cp f90wrap/runtime.py <suews>/src/supy/_vendor/f90wrap_src/f90wrap/
cp f90wrap/fortrantype.py <suews>/src/supy/_vendor/f90wrap_src/f90wrap/

# Update commit hash in LICENSE and this README
```

## Note on runtime.py

The `runtime.py` file is patched at build time by `src/supy/_vendor/f90wrap/patch_runtime.py` to use relative imports, enabling it to work as a vendored module.
