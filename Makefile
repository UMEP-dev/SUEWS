# SUEWS Simplified Makefile - Essential recipes only
.PHONY: help setup dev reinstall test docs clean format

# Default Python
PYTHON := python

help:
	@echo "SUEWS Development - Essential Commands"
	@echo ""
	@echo "  setup     - Create virtual environment (if using uv)"
	@echo "  dev       - Install in editable mode"
	@echo "  reinstall - Fix stale/broken install (clean + reinstall)"
	@echo "  test      - Run test suite"
	@echo "  docs      - Build documentation"
	@echo "  clean     - Smart clean (keeps .venv if active)"
	@echo "  format    - Format Python and Fortran code"
	@echo ""
	@echo "Quick start:"
	@echo "  With uv:    make setup && source .venv/bin/activate && make dev"
	@echo "  With conda: conda activate suews-dev && make dev"
	@echo ""
	@echo "After 'make clean', run 'make reinstall' (not 'make dev')"

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
	@# Install build dependencies first (required for --no-build-isolation)
	@if command -v uv >/dev/null 2>&1; then \
		echo "Using uv for fast installation..."; \
		uv pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			echo "Using Homebrew gfortran for macOS compatibility"; \
			bash -c 'FC=/opt/homebrew/bin/gfortran uv pip install --reinstall --no-build-isolation -e ".[dev]"'; \
		else \
			uv pip install --reinstall --no-build-isolation -e ".[dev]"; \
		fi \
	else \
		$(PYTHON) -m pip install wheel pytest "f90wrap==0.2.16" "numpy>=2.0" "meson-python>=0.12.0"; \
		if [ -x "/opt/homebrew/bin/gfortran" ]; then \
			FC=/opt/homebrew/bin/gfortran $(PYTHON) -m pip install --force-reinstall --no-build-isolation -e ".[dev]"; \
		else \
			$(PYTHON) -m pip install --force-reinstall --no-build-isolation -e ".[dev]"; \
		fi \
	fi
	@echo "✓ Installation complete"

# Fix stale editable install
reinstall:
	@echo "Fixing stale editable install..."
	@if command -v uv >/dev/null 2>&1; then \
		uv pip uninstall supy || true; \
	else \
		$(PYTHON) -m pip uninstall supy -y || true; \
	fi
	@rm -rf build
	@$(MAKE) dev
	@$(MAKE) rebuild-meson

# Initialize meson build after clean (fixes editable install)
rebuild-meson:
	@PYVER=$$($(PYTHON) -c "import sys; print(f'cp{sys.version_info.major}{sys.version_info.minor}')"); \
	if [ ! -d "build/$$PYVER" ]; then \
		echo "Initializing meson build directory for $$PYVER..."; \
		mkdir -p "build/$$PYVER"; \
		cd "build/$$PYVER" && meson setup ../.. --prefix=$$VIRTUAL_ENV; \
		echo "✓ Build directory initialized"; \
	fi

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
		echo "→ Next run 'make dev' to rebuild"; \
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

