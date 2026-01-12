# Troubleshooting

## Import Errors

**Symptoms**: `ModuleNotFoundError: No module named 'supy'`

**Solutions**:
1. Ensure environment is activated: `source .venv/bin/activate`
2. Rebuild: `make clean && make dev`

---

## Package Conflicts

**Symptoms**: Version conflicts, failed installations

**Solutions**:
1. Clear uv cache: `uv cache clean`
2. Remove and recreate environment: `rm -rf .venv && make setup`

---

## Compiler Issues

**Symptoms**: `gfortran: command not found`

**Solutions**:

**macOS**:
```bash
xcode-select --install
brew install gcc
```

**Linux**:
```bash
sudo apt-get install gfortran  # Ubuntu/Debian
sudo dnf install gcc-gfortran  # Fedora/RHEL
```

**Verify**:
```bash
which gfortran
gfortran --version
```

---

## Python Version Issues

**Symptoms**: Compatibility errors with Python 3.13

**Solutions**:
1. Check version: `python --version`
2. Use specific version: `uv venv --python 3.12`

---

## Build Failures

**Symptoms**: Meson build errors

**Solutions**:
1. Check Fortran compiler: `which gfortran`
2. Clean and rebuild: `make clean && make dev`
3. Check meson.build for missing files

---

## Test Failures After Setup

**Solutions**:
1. Verify installation: `python -c "import supy; print(supy.__version__)"`
2. Run smoke tests: `make test-smoke`
3. Check for environment activation
