# Platform Setup - Detailed Instructions

## macOS

**Prerequisites:**
```bash
# Xcode command line tools
xcode-select --install

# Homebrew packages
brew install gcc uv
```

**Verify:**
```bash
gfortran --version
uv --version
```

**Build:**
```bash
uv venv && source .venv/bin/activate && make dev && make test
```

---

## Linux (Ubuntu/Debian)

**Prerequisites:**
```bash
sudo apt-get update
sudo apt-get install gfortran build-essential
pip install uv
```

**Verify:**
```bash
gfortran --version
uv --version
```

**Build:**
```bash
uv venv && source .venv/bin/activate && make dev && make test
```

---

## Windows

### Automated Setup (Recommended)

Run the PowerShell script from the SUEWS repo root:
```powershell
.\.claude\skills\setup-dev\scripts\setup-windows-dev.ps1
```

Options:
- `-SkipMSYS2` - Skip MSYS2 installation if already present
- `-SkipQGIS` - Skip QGIS installation prompt

Notes:
- The script uses `winget` for installs where available. If `winget` is missing, it skips those steps and prints manual download links.
- The script attempts to download the latest MSYS2 installer from GitHub releases and falls back to a pinned release if the API is unavailable.

### Manual Setup

Install in this order:

1. **Git**: `winget install Git.Git`
2. **GitHub CLI**: `winget install GitHub.cli`
3. **Python 3.12**: `winget install Python.Python.3.12`
4. **uv**: `winget install astral-sh.uv`
5. **MSYS2 UCRT64** (for gfortran):
   - Download: https://www.msys2.org/ (or https://github.com/msys2/msys2-installer/releases)
   - Install to `C:\msys64`
   - In MSYS2 UCRT64 terminal:
     ```bash
     pacman -S mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-meson mingw-w64-ucrt-x86_64-ninja
     ```
   - Add `C:\msys64\ucrt64\bin` to PATH
6. **VS Code**: `winget install Microsoft.VisualStudioCode`
7. **QGIS LTR** (optional, for UMEP plugin testing):
   - Download OSGeo4W: https://download.osgeo.org/osgeo4w/v2/osgeo4w-setup.exe
   - Express Install → QGIS LTR

**Build:**
```powershell
uv venv
.venv\Scripts\activate
uv pip install -e .[dev]
make test
```

---

## Setup Options Comparison

| Tool | Create Env | Install Deps | Recommendation |
|------|------------|--------------|----------------|
| uv | 1s | 5-10s | **Best choice** |
| pip | 3s | 60-120s | Works everywhere |

### Using uv (fastest)
```bash
uv venv && source .venv/bin/activate && make dev
```

### Using venv
```bash
python -m venv .venv && source .venv/bin/activate && make dev
```

---

## Git Worktrees

Each worktree needs its own virtual environment:

```bash
cd /path/to/worktree
uv venv
source .venv/bin/activate
make dev
```

**Best practice**: One environment per worktree to avoid cross-contamination.
