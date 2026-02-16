# SUEWS Simplified Makefile - Essential recipes only
.PHONY: help setup submodules dev dev-dts reinstall test test-smoke test-all docs clean format bridge

# Default Python
PYTHON := python

help:
	@echo "SUEWS Development - Essential Commands"
	@echo ""
	@echo "  setup      - Create virtual environment (if using uv)"
	@echo "  dev        - Fast build without DTS type wrappers (traditional interface)"
	@echo "  dev-dts    - Full build with DTS type wrappers (DTS interface enabled)"
	@echo "  test       - Run standard tests (excludes slow, ~2-3 min)"
	@echo "  test-smoke - Run smoke tests only (fast CI validation, ~30-60 sec)"
	@echo "  test-all   - Run ALL tests including slow (~4-5 min)"
	@echo "  docs       - Build documentation"
	@echo "  clean      - Smart clean (keeps .venv if active)"
	@echo "  bridge     - Build the Rust bridge CLI (suews_bridge)"
	@echo "  format     - Format Python and Fortran code"
	@echo ""
	@echo "Quick start:"
	@echo "  With uv:    make setup && source .venv/bin/activate && make dev"
	@echo "  With conda: mamba env create -f env.yml && mamba activate suews-dev && make dev"
	@echo ""
	@echo "Build options:"
	@echo "  make dev      - Fast build (skips 13 type files in f90wrap)"
	@echo "  make dev-dts  - Full build (for DTS interface development)"
	@echo ""
	@echo "Common workflows:"
	@echo "  Fresh start:     make clean && make dev"
	@echo "  Update code:     git pull && make dev"
	@echo "  Test changes:    make dev && make test"
	@echo "  CI validation:   make test-smoke"

# Setup virtual environment (for uv users)
setup:
	@if command -v uv >/dev/null 2>&1; then \
		if [ ! -d ".venv" ]; then \
			echo "Creating uv virtual environment..."; \
			uv venv; \
			echo "✓ Created .venv"; \
			echo "→ Now run: source .venv/bin/activate"; \
		else \
			echo "Virtual environment already exists at .venv"; \
		fi \
	else \
		echo "uv not found. Install with: brew install uv"; \
		echo "Or use conda/mamba instead"; \
		exit 1; \
	fi

# Initialise git submodules (idempotent - safe to run multiple times)
submodules:
	@git submodule update --init --recursive

# Fast build without DTS type wrappers (for most development)
# Skips wrapping 13 type definition files, significantly reducing f90wrap time
# Traditional DataFrame interface works; DTS features will raise clear error
dev:
	@# Check for activated virtual environment
	@if [ -z "$$VIRTUAL_ENV" ]; then \
		echo ""; \
		echo "ERROR: No virtual environment activated."; \
		echo ""; \
		echo "Please activate a virtual environment first:"; \
		echo "  make setup && source .venv/bin/activate && make dev"; \
		echo ""; \
		echo "Or with conda:"; \
		echo "  mamba activate suews-dev && make dev"; \
		echo ""; \
		exit 1; \
	fi
	@$(MAKE) submodules
	@echo "Installing SUEWS in editable mode (fast build - no DTS type wrappers)..."
	@echo "Note: DTS features unavailable. Use 'make dev-dts' for full build."
	@# Get current Python version tag
	@PYVER=$$($(PYTHON) -c "import sys; print(f'cp{sys.version_info.major}{sys.version_info.minor}')") || { \
		echo "ERROR: Failed to get Python version"; \
		exit 1; \
	}; \
	echo "Python version: $$PYVER"; \
	# Stale build detection: check for builds from different Python versions \
	if [ -d "build" ]; then \
		STALE_BUILDS=$$(ls -d build/cp* 2>/dev/null | grep -v "build/$$PYVER" || true); \
		if [ -n "$$STALE_BUILDS" ]; then \
			echo ""; \
			echo "WARNING: Found build directories for different Python versions:"; \
			echo "$$STALE_BUILDS"; \
			echo ""; \
			echo "Cleaning stale builds to avoid import errors..."; \
			for dir in $$STALE_BUILDS; do \
				rm -rf "$$dir"; \
				echo "  Removed: $$dir"; \
			done; \
			echo ""; \
		fi \
	fi; \
	# Uninstall first if build directory is missing (post-clean state) \
	if [ ! -d "build/$$PYVER" ]; then \
		echo "Build directory missing for $$PYVER - performing clean reinstall..."; \
		if command -v uv >/dev/null 2>&1; then \
			uv pip uninstall supy 2>/dev/null || true; \
		else \
			$(PYTHON) -m pip uninstall supy -y 2>/dev/null || true; \
		fi \
	fi
	@# Install build dependencies first (required for --no-build-isolation)
	@if command -v uv >/dev/null 2>&1; then \
		echo "Using uv for fast installation..."; \
		uv pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			echo "Using Homebrew gfortran for macOS compatibility"; \
			bash -c 'FC=/opt/homebrew/bin/gfortran uv pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=false"'; \
		else \
			uv pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=false"; \
		fi \
	else \
		$(PYTHON) -m pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			FC=/opt/homebrew/bin/gfortran $(PYTHON) -m pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=false"; \
		else \
			$(PYTHON) -m pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=false"; \
		fi \
	fi
	@# Ensure meson build directory is initialized (fixes post-clean state)
	@$(MAKE) rebuild-meson
	@echo "Build complete (fast build - DTS features disabled)"

# Full build with DTS type wrappers (for DTS interface development)
# Wraps all type definition files for direct Python access to nested Fortran types
dev-dts:
	@# Check for activated virtual environment
	@if [ -z "$$VIRTUAL_ENV" ]; then \
		echo ""; \
		echo "ERROR: No virtual environment activated."; \
		echo ""; \
		echo "Please activate a virtual environment first:"; \
		echo "  make setup && source .venv/bin/activate && make dev-dts"; \
		echo ""; \
		echo "Or with conda:"; \
		echo "  mamba activate suews-dev && make dev-dts"; \
		echo ""; \
		exit 1; \
	fi
	@$(MAKE) submodules
	@echo "Installing SUEWS in editable mode (full build - with DTS type wrappers)..."
	@# Get current Python version tag
	@PYVER=$$($(PYTHON) -c "import sys; print(f'cp{sys.version_info.major}{sys.version_info.minor}')") || { \
		echo "ERROR: Failed to get Python version"; \
		exit 1; \
	}; \
	echo "Python version: $$PYVER"; \
	# Stale build detection: check for builds from different Python versions \
	if [ -d "build" ]; then \
		STALE_BUILDS=$$(ls -d build/cp* 2>/dev/null | grep -v "build/$$PYVER" || true); \
		if [ -n "$$STALE_BUILDS" ]; then \
			echo ""; \
			echo "WARNING: Found build directories for different Python versions:"; \
			echo "$$STALE_BUILDS"; \
			echo ""; \
			echo "Cleaning stale builds to avoid import errors..."; \
			for dir in $$STALE_BUILDS; do \
				rm -rf "$$dir"; \
				echo "  Removed: $$dir"; \
			done; \
			echo ""; \
		fi \
	fi; \
	# Uninstall first if build directory is missing (post-clean state) \
	if [ ! -d "build/$$PYVER" ]; then \
		echo "Build directory missing for $$PYVER - performing clean reinstall..."; \
		if command -v uv >/dev/null 2>&1; then \
			uv pip uninstall supy 2>/dev/null || true; \
		else \
			$(PYTHON) -m pip uninstall supy -y 2>/dev/null || true; \
		fi \
	fi
	@# Install build dependencies first (required for --no-build-isolation)
	@if command -v uv >/dev/null 2>&1; then \
		echo "Using uv for fast installation..."; \
		uv pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			echo "Using Homebrew gfortran for macOS compatibility"; \
			bash -c 'FC=/opt/homebrew/bin/gfortran uv pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=true"'; \
		else \
			uv pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=true"; \
		fi \
	else \
		$(PYTHON) -m pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			FC=/opt/homebrew/bin/gfortran $(PYTHON) -m pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=true"; \
		else \
			$(PYTHON) -m pip install --no-build-isolation -e ".[dev]" --config-settings=setup-args="-Dwrap_dts_types=true"; \
		fi \
	fi
	@# Ensure meson build directory is initialized (fixes post-clean state)
	@$(MAKE) rebuild-meson
	@echo "Build complete (full build - DTS features enabled)"

# Deprecated: use 'make clean && make dev' instead (kept for backwards compatibility)
reinstall:
	@echo "Note: 'make reinstall' is deprecated. Use 'make clean && make dev' instead."
	@echo "Forcing clean reinstall..."
	@rm -rf build
	@$(MAKE) dev

# Initialize meson build after clean OR rebuild if sources changed
rebuild-meson:
	@# Get Python version for build directory naming
	@PYVER=$$($(PYTHON) -c "import sys; print(f'cp{sys.version_info.major}{sys.version_info.minor}')") || { \
		echo "ERROR: Failed to get Python version. Is Python available?"; \
		exit 1; \
	}; \
	if [ -z "$$PYVER" ]; then \
		echo "ERROR: Could not determine Python version"; \
		exit 1; \
	fi; \
	if [ ! -d "build/$$PYVER" ]; then \
		echo "Initializing meson build directory for $$PYVER..."; \
		mkdir -p "build/$$PYVER"; \
		if cd "build/$$PYVER" && meson setup ../.. --prefix=$$VIRTUAL_ENV; then \
			echo "✓ Build directory initialized"; \
		else \
			echo "ERROR: meson setup failed"; \
			exit 1; \
		fi; \
	else \
		echo "Rebuilding changed Fortran sources..."; \
		if cd "build/$$PYVER" && ninja; then \
			echo "✓ Fortran extension rebuilt"; \
		else \
			echo "ERROR: ninja build failed"; \
			exit 1; \
		fi; \
	fi

# Run tests - Three tiers available:
# - test-smoke: Fast critical tests (~30-60 sec) - used in CI wheel validation
# - test: Standard tests excluding slow (~2-3 min) - default for development
# - test-all: All tests including slow (~4-5 min) - comprehensive validation
test:
	@echo "Running standard tests (excluding slow tests)..."
	@echo "NOTE: Slow tests (e.g., Fortran state persistence ~3-4 min) are skipped."
	@echo "      Run 'make test-all' for comprehensive testing."
	@echo ""
	$(PYTHON) -m pytest test -m "not slow" -v --tb=short --durations=10

# Smoke tests - fast critical path tests for CI
test-smoke:
	@echo "Running smoke tests (critical path only)..."
	@echo "This is the fastest test tier for CI wheel validation."
	@echo ""
	$(PYTHON) -m pytest test -m "smoke" -v --tb=short --durations=10

# All tests including slow tests
test-all:
	@echo "Running ALL tests including slow tests..."
	@echo "This may take 4-5 minutes."
	@echo ""
	$(PYTHON) -m pytest test -v --tb=short --durations=10

# Build documentation
docs:
	$(MAKE) -C docs html

# Smart clean - preserves .venv if you're in it
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf build dist *.egg-info .pytest_cache
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -type d -exec rm -rf {} +
	@$(MAKE) -C src/suews clean 2>/dev/null || true
	@$(MAKE) -C docs clean 2>/dev/null || true
	@if [ -n "$$VIRTUAL_ENV" ] && [ -d ".venv" ]; then \
		echo "✓ Cleaned (keeping .venv - you're using it)"; \
		echo "→ Run 'make dev' to rebuild"; \
	elif [ -d ".venv" ]; then \
		echo "Removing .venv (not active)..."; \
		rm -rf .venv; \
		echo "✓ Cleaned everything including .venv"; \
	else \
		echo "✓ Cleaned"; \
	fi

# Build the Rust bridge CLI binary
bridge:
	@if ! command -v cargo >/dev/null 2>&1; then \
		echo "ERROR: cargo not found. Install Rust: https://rustup.rs"; \
		exit 1; \
	fi
	@echo "Building Rust bridge CLI..."
	cd src/suews_core && cargo build --release
	@echo "Binary at: src/suews_core/target/release/suews"
	@echo "Run: src/suews_core/target/release/suews --help"

# Format code
format:
	ruff format src test
	fprettify --config .fprettify.rc src/suews/src/*.f95 2>/dev/null || true
