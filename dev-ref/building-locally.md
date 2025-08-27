# Building SUEWS Locally

If you want to build SUEWS from source for local use, this guide covers prerequisites, setup, testing, and the project structure.

## Prerequisites

- Python 3.9+
- `gfortran` compiler
- `pip` or `conda/mamba`

## Basic Setup

```bash
# Clone repository
git clone https://github.com/UMEP-dev/SUEWS.git
cd SUEWS

# Option 1: Install in development mode with pip
pip install -e .

# Option 2: Create a conda/mamba environment
mamba env create -f env.yml
mamba activate suews-dev
make dev
```

## Running Tests

To verify your build:

```bash
# Run test suite via Makefile
make test

# Or using pytest directly
pytest test/
```

## Project Structure

```text
SUEWS/
├── src/
│   ├── suews/          # Fortran physics engine
│   ├── supy/           # Python interface
│   └── supy_driver/    # Python-Fortran bridge
├── test/               # Test suite
├── docs/               # User documentation (Sphinx)
└── dev-ref/            # Developer documentation (Markdown)
```

For a more comprehensive developer overview (workflow, testing strategy, best practices), see the [Onboarding Guide](onboarding-guide.md).

