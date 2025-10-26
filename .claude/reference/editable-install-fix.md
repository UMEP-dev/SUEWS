# Editable Install Fix

## Problem

After `make clean` then `make dev`, importing `supy` would fail with:

```
FileNotFoundError: [Errno 2] No such file or directory: '.../build/cp313'
```

## Root Cause

When using meson-python's editable install mode:

1. `make clean` removes the `build/` directory
2. `make dev` runs `uv pip install -e .` which creates editable install metadata
3. The metadata references a build directory (`build/cp313`) that doesn't exist yet
4. On import, the editable loader tries to rebuild but can't find the build directory
5. Without `--reinstall`, uv skips the build step if the package is already installed

## Solution

Use `--reinstall` flag in `make dev` to force a complete rebuild every time:

```makefile
# For uv
bash -c 'FC=/opt/homebrew/bin/gfortran uv pip install --reinstall --no-build-isolation -e ".[dev]"'

# For pip
FC=/opt/homebrew/bin/gfortran python -m pip install --force-reinstall --no-build-isolation -e ".[dev]"
```

Note: We use `bash -c` wrapper for uv because zsh doesn't support the `VAR=value command` syntax directly in Makefiles.

## Alternative Solutions (Not Used)

1. Don't remove `build/` in `make clean` - but this leaves stale builds
2. Manually create `build/cp313` and run `meson setup` - too complex
3. Use non-editable install - loses development convenience

## Verification

```bash
source .venv/bin/activate
make clean  # Removes build directory
make dev    # Reinstalls with fresh build
python -c "import supy"  # Should work without errors
```
