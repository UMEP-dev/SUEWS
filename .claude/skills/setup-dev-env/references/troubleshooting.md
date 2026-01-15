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

---

## Windows: gfortran Not Found

**Symptoms**: `gfortran: command not found` or meson can't find Fortran compiler

**Solutions**:
1. Ensure MSYS2 is installed: `C:\msys64\ucrt64\bin\gfortran.exe`
2. Add to PATH: `C:\msys64\ucrt64\bin`
3. Restart terminal after modifying PATH
4. Verify: `gfortran --version`

---

## Windows: winget Not Recognised

**Symptoms**: `winget: command not recognized`

**Solutions**:
1. Open Microsoft Store and update "App Installer"
2. Or download installers manually from official websites
3. Run PowerShell as Administrator

---

## Windows: MSYS2 Package Issues

**Symptoms**: pacman fails to install packages

**Solutions**:
1. Update MSYS2 first: `pacman -Syu`
2. Close and reopen MSYS2 terminal
3. Run update again: `pacman -Syu`
4. Install packages: `pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`
