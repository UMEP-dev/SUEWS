---
name: setup-dev
description: Set up SUEWS development environment on macOS, Linux, or Windows.
---

# Setup Dev

Set up SUEWS development environment for building and testing.

## Triggers

- "setup dev environment", "install SUEWS dependencies"
- "Windows setup", "macOS setup", "Linux setup"
- "gfortran", "MSYS2", "Fortran compiler"
- Building SUEWS from source

## Capabilities

- Cross-platform setup instructions (macOS, Linux, Windows)
- Automated Windows environment setup via PowerShell script
- Troubleshooting common build issues
- Makefile-based build workflow

## Quick Start

**macOS/Linux:**
```bash
uv venv && source .venv/bin/activate && make dev && make test
```

**Windows (PowerShell):**
```powershell
# Run setup script first (one-time)
.\.claude\skills\setup-dev\scripts\setup-windows-dev.ps1

# Then build SUEWS
uv venv && .venv\Scripts\activate && uv pip install -e .[dev] && make test
```

## Requirements Summary

- **macOS**: `brew install gcc uv`
- **Linux**: `apt install gfortran` + `pip install uv`
- **Windows**: MSYS2 UCRT64 gfortran + `winget install astral-sh.uv`

See `references/platform-setup.md` for detailed instructions.

## Build Commands

```bash
make setup      # Create .venv with uv
make dev        # Install editable mode
make test       # Run tests (~2-3 min)
make test-smoke # Quick smoke tests (~1 min)
make clean      # Smart clean (keeps .venv if active)
```

## Scripts

- `scripts/setup-windows-dev.ps1` - Automated Windows environment setup

## References

- `references/platform-setup.md` - Detailed platform instructions
- `references/troubleshooting.md` - Common issues and fixes
- `Makefile` - All build recipes
- `pyproject.toml` - Python dependencies
