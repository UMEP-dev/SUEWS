# SUEWS

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5721639.svg)](https://doi.org/10.5281/zenodo.5721639)

This is a public repo for SUEWS source code and documentation.

---



## Documentation

* Documentation site: <https://suews.readthedocs.io/>

* Documentation source: `docs` folder in this repo


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

> [!NOTE]
> **the following is deprecated and will be updated**

### Development Environment

#### Claude Code Integration

For enhanced development productivity, SUEWS includes integration with Claude Code in a containerised environment:

* **Setup Guide**: See [`claude-dev/README.md`](claude-dev/README.md) for complete setup instructions
* **Quick Start**:
  - **Workspace Manager** (recommended): `./claude-dev/claude.sh start myproject`
  - **Direct Setup**: `./claude-dev/setup-claude-dev.sh` from repository root
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

**For Local Development** (optional):
```bash
make format  # Format code locally
make lint    # Check code style
```


### Debugging with GDB

GDB is a generic debugging tool used along with gfortran.
Here are some tips to debug SUEWS code:

#### GDB on macOS

Recent macOS (since High Sierra) introduces extra security procedures for system level operations that makes installation GDB more tedious than before.
The best practice, in TS's opinion, to avoid hacking your macOS, is to use Linux docker images with gfortran & gdb installations: e.g., [alpine-gfortran](https://github.com/cmplopes/alpine-gfortran)
(otherwise, [this guide](https://dev.to/jasonelwood/setup-gdb-on-macos-in-2020-489k#generate-cert) might be useful for installation of GDB on macOS; also run `set startup-with-shell off` *inside GDB* before `run` the debuggng process)

Once the docker image is installed, simply run this from the SUEWS root folder for debugging:

```bash
 docker run --rm -it -v $(pwd):/source sunt05/alpine-gfortran /bin/bash

```
 which will mount the current `SUEWS` directory to docker's path `/source` and enter the interactive mode for debugging.


#### debugging with GDB

1. enable the debugging related flags in `Makefile` under `SUEWS-SourceCode` by removing the `#` after the equal sign `=`:

```makefile
FCNOOPT = -O0
FFLAGS = -O3 $(STATIC) $(FCDEBUG) -Wall -Wtabs -fbounds-check -cpp \
					-Wno-unused-dummy-argument -Wno-unused-variable
```

2. fully clean and recompile `SUEWS`:
```
make clean; make
```

3. copy the recompiled `SUEWS` binary into your SUEWS testing folder (e.g., `Test/BaseRun/2019a`) and load it into GDB:

```
gdb SUEWS

run

```
then you should have stack info printed out by GDB if any runtime error occurs.

More detailed GDB tutorial can be found [here](https://github.com/jackrosenthal/gdb-tutorial/blob/master/notes.pdf).



### Questions

* Please [raise issues](https://github.com/UMEP-dev/SUEWS/issues/new) for questions in the development so our progress can be well managed.
