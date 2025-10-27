# SUEWS Simplified Makefile - Essential recipes only
.PHONY: help setup dev test docs clean format

# Default Python
PYTHON := python

help:
	@echo "SUEWS Development - Essential Commands"
	@echo ""
	@echo "  setup     - Create virtual environment (if using uv)"
	@echo "  dev       - Install in editable mode"
	@echo "  test      - Run test suite"
	@echo "  docs      - Build documentation"
	@echo "  clean     - Smart clean (keeps .venv if active)"
	@echo "  format    - Format Python and Fortran code"
	@echo ""
	@echo "Quick start:"
	@echo "  With uv:    make setup && source .venv/bin/activate && make dev"
	@echo "  With conda: conda activate suews-dev && make dev"

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

# Install in editable mode
dev:
	@echo "Installing SUEWS in editable mode..."
	@# Check if build directory exists for current Python version
	@NEEDS_REINSTALL=false; \
	PY_TAG=$$(python -c "import sys; print(f'cp{sys.version_info.major}{sys.version_info.minor}')"); \
	if [ -d "build" ] && [ ! -d "build/$$PY_TAG" ]; then \
		echo "Build directory for $$PY_TAG not found (stale build detected)"; \
		echo "Cleaning build directory and uninstalling..."; \
		rm -rf build; \
		NEEDS_REINSTALL=true; \
	elif ! [ -d "build" ]; then \
		if command -v uv >/dev/null 2>&1; then \
			if uv pip list | grep -q "^supy "; then \
				echo "Package installed but build directory missing (stale install detected)"; \
				echo "Uninstalling..."; \
				NEEDS_REINSTALL=true; \
			fi \
		elif python -m pip list | grep -q "^supy "; then \
			echo "Package installed but build directory missing (stale install detected)"; \
			echo "Uninstalling..."; \
			NEEDS_REINSTALL=true; \
		fi \
	fi; \
	if command -v uv >/dev/null 2>&1; then \
		echo "Using uv for fast installation..."; \
		if [ "$$NEEDS_REINSTALL" = "true" ]; then \
			uv pip uninstall supy || true; \
		fi; \
		uv pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if ! [ -d "build/$$PY_TAG" ]; then \
			echo "Initializing build directory for $$PY_TAG..."; \
			python -m mesonbuild.mesonmain setup build/$$PY_TAG --buildtype=release; \
		fi; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			echo "Using Homebrew gfortran for macOS compatibility"; \
			FC=/opt/homebrew/bin/gfortran uv pip install --no-build-isolation -e ".[dev]"; \
		else \
			uv pip install --no-build-isolation -e ".[dev]"; \
		fi \
	else \
		if [ "$$NEEDS_REINSTALL" = "true" ]; then \
			$(PYTHON) -m pip uninstall supy -y || true; \
		fi; \
		$(PYTHON) -m pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if ! [ -d "build/$$PY_TAG" ]; then \
			echo "Initializing build directory for $$PY_TAG..."; \
			python -m mesonbuild.mesonmain setup build/$$PY_TAG --buildtype=release; \
		fi; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			FC=/opt/homebrew/bin/gfortran $(PYTHON) -m pip install --no-build-isolation -e ".[dev]"; \
		else \
			$(PYTHON) -m pip install --no-build-isolation -e ".[dev]"; \
		fi \
	fi
	@echo "✓ Installation complete"

# Run tests
test:
	@echo "Running tests (excluding slow tests)..."
	@echo "NOTE: Slow tests (e.g., Fortran state persistence ~3-4 min) are skipped."
	@echo "      These run automatically in CI. To run manually: pytest test -m slow -v"
	@echo ""
	$(PYTHON) -m pytest test -m "not slow" -v --tb=short --durations=10

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
	elif [ -d ".venv" ]; then \
		echo "Removing .venv (not active)..."; \
		rm -rf .venv; \
		echo "✓ Cleaned everything including .venv"; \
	else \
		echo "✓ Cleaned"; \
	fi

# Format code
format:
	ruff format src test
	fprettify --config .fprettify.rc src/suews/src/*.f95 2>/dev/null || true
