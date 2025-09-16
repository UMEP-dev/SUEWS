# SUEWS - Surface Urban Energy and Water Balance Scheme

This is the SUEWS urban climate model repository.

## Installation

```bash
pip install supy
```

## Documentation

See the full documentation at: https://suews.readthedocs.io

## Quick Start

For users who want to run SUEWS simulations:

1. **Install from PyPI** (simplest):
   ```bash
   pip install supy
   ```

2. **Run a simulation**:
   ```bash
   suews-run /path/to/config.yml
   ```

For developers, see the [Developer Note](#developer-note) section below.

## Developer Note

### Development Environment

#### Claude Code Integration

For enhanced development productivity, SUEWS includes integration with Claude Code in a containerised environment:

* **Setup Guide**: See [`claude-dev/README.md`](claude-dev/README.md) for complete setup instructions
* **Quick Start**:
  - **Workspace Manager** (recommended): `./claude-dev/claude.sh start myproject`
  - **Direct Setup**: `./claude-dev/setup-claude-dev.sh` from repository root

#### CLAUDE.md Protection System

This repository includes automatic protection for the CLAUDE.md configuration file to prevent accidental content loss:

* **Automatic Features** (no setup required):
  - GitHub Actions validation on all PRs/pushes affecting CLAUDE.md
  - Content reduction detection (alerts if >20% content removed)
  - Automatic snapshots on validation failures

* **Local Protection** (one-time setup):
  ```bash
  # Run once after cloning or pulling this feature
  bash .claude/scripts/setup-claude-protection.sh
  ```
  This enables:
  - Git pre-commit validation
  - Local backup system
  - Manual validation: `python3 .claude/scripts/validate-claude-md.py`
* **Features**: Intelligent code assistance, automated testing, British academic standards, multi-workspace support
* **Benefits**: Isolated environment, reproducible development, AI-powered debugging, parallel project development

#### Traditional Development

For local development without containerisation, follow these steps:

##### Prerequisites

**Essential Tools**:
* **Fortran Compiler**: [gfortran](https://gcc.gnu.org/wiki/GFortran) (≥ 9.3.0) or Intel ifort
  - macOS: `brew install gcc`
  - Ubuntu/Debian: `sudo apt-get install gfortran`
  - Windows: Use WSL or MinGW-w64
* **Version Control**: [git](https://git-scm.com/)
* **Package Manager**: [mamba](https://mamba.readthedocs.io/en/latest/) (faster than conda)
  ```bash
  # Install mambaforge (if not already installed)
  curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
  bash Miniforge3-$(uname)-$(uname -m).sh
  ```

**Recommended Tools**:
* [VS Code](https://code.visualstudio.com/) with extensions:
  - Modern Fortran
  - Python
  - GitHub Copilot (free for academic use)
* [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) (Windows users)

##### Setup Steps

1. **Clone the repository**:
   ```bash
   git clone https://github.com/UMEP-dev/SUEWS.git
   cd SUEWS
   ```

2. **Initialise submodules** (required for SPARTACUS dependency):
   ```bash
   git submodule init
   git submodule update
   ```
   *Note: If permission denied, [configure SSH for GitHub](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh)*

3. **Create development environment**:
   ```bash
   mamba env create -f env.yml
   ```
   This creates `suews-dev` environment with all required packages.

4. **Activate environment**:
   ```bash
   mamba activate suews-dev
   ```

5. **Build SUEWS**:
   ```bash
   # Quick development build (recommended)
   make dev

   # Or full build with tests
   make
   ```

6. **Verify installation**:
   ```bash
   pip show supy
   suews-run --help
   ```

##### Development Workflow

* **Build commands**:
  ```bash
  make dev          # Fast development build
  make              # Full build with tests
  make test         # Run test suite only
  make clean        # Clean build artifacts
  make wheel        # Build distribution wheels
  make docs         # Build documentation
  make livehtml     # Live documentation preview
  ```

* **Environment management**:
  ```bash
  make help         # Show all available commands
  make deactivate   # Show deactivation command
  ```

* **Common issues**:
  - **Build conflicts**: Run `make clean` before rebuilding
  - **Import errors**: Ensure you're in the `suews-dev` environment
  - **Permission errors on Windows**: Right-click project folder → Properties → Security → Edit → Everyone → Allow

##### Project Structure

```
SUEWS/
├── src/
│   ├── suews/          # Fortran physics engine
│   ├── supy/           # Python interface
│   └── supy_driver/    # F2Py wrapper
├── test/               # Test suite
├── docs/               # Documentation source
├── env.yml             # Development environment
└── Makefile            # Build commands
```


## Contributing

### Code Style and Formatting

SUEWS maintains consistent code style through automated formatting:

* **Coding Standards**: See [`CODING_GUIDELINES.md`](dev-ref/CODING_GUIDELINES.md) for detailed standards
* **Automated Formatting**: The master branch is automatically formatted after merge
* **Zero Friction**: Contributors can focus on functionality; formatting is handled by machines
* **Tools Used**:
  - Python: [`ruff`](https://docs.astral.sh/ruff/) (configuration in `.ruff.toml`)
  - Fortran: [`fprettify`](https://github.com/pseewald/fprettify) (configuration in `.fprettify.rc`)

**For Contributors**: Just write working code! Formatting will be applied automatically after merge.

### Testing Development Versions

For developers who need to test pre-release versions from test.pypi.org:

**1. Install uv (one-time setup):**
```bash
# macOS/Linux
curl -LsSf https://astral.sh/uv/install.sh | sh

# Windows (PowerShell)
powershell -c "irm https://astral.sh/uv/install.ps1 | iex"
```

**2. Create isolated environment with uv:**
```bash
uv venv .venv-dev
source .venv-dev/bin/activate  # Linux/macOS
# or: .venv-dev\Scripts\activate  # Windows
# You'll see (.venv-dev) in your terminal prompt when activated
```
Note: `uv venv` is 80x faster than `python -m venv` and manages Python versions automatically.

**3. Check latest version:**
Visit https://test.pypi.org/project/supy/ to find the latest development version (format: `YYYY.M.D.dev0`)

**4. Install development version:**
```bash
# Replace 2025.9.16.dev0 with the latest version from step 3
uv pip install --extra-index-url https://test.pypi.org/simple/ \
              --index-strategy unsafe-best-match \
              supy==2025.9.16.dev0
```

**5. Verify installation:**
```bash
python -c "import supy; print(f'SuPy version: {supy.__version__}')"
# Should show: 2025.9.16.dev0 (or your installed version)
```

**6. Test functionality:**
```bash
python -c "import supy as sp; sp.load_sample_data(); print('✓ Installation successful')"
```

**For future use:** Always activate the environment before working:
```bash
source .venv-dev/bin/activate  # Linux/macOS
# or: .venv-dev\Scripts\activate  # Windows
# Use 'deactivate' to exit the environment
```

**Why uv?** 
- Creates virtual environments 80x faster than `python -m venv`
- Handles test.pypi.org dependencies correctly with `--index-strategy unsafe-best-match`
- Single tool for both environment and package management
- No Python installation required (uv can download Python as needed)