# SUEWS MCP Server - Development Makefile

.PHONY: help install install-dev test test-unit test-integration test-e2e test-performance
.PHONY: lint format type-check coverage clean build publish-test publish docs serve-docs
.PHONY: security-check pre-commit-install pre-commit-run all-checks setup-dev

# Default target
help: ## Show this help message
	@echo "SUEWS MCP Server - Development Commands"
	@echo ""
	@echo "Setup Commands:"
	@echo "  make install       Install package in editable mode"
	@echo "  make install-dev   Install development dependencies"
	@echo "  make setup-dev     Complete development setup (install + pre-commit)"
	@echo ""
	@echo "Testing Commands:"
	@echo "  make test          Run all tests with coverage"
	@echo "  make test-unit     Run unit tests only"
	@echo "  make test-integration  Run integration tests only"
	@echo "  make test-e2e      Run end-to-end tests only"
	@echo "  make test-performance  Run performance tests only"
	@echo "  make coverage      Generate detailed coverage report"
	@echo ""
	@echo "Code Quality Commands:"
	@echo "  make lint          Run linting (ruff)"
	@echo "  make format        Format code (black + ruff --fix)"
	@echo "  make type-check    Run type checking (mypy)"
	@echo "  make security-check  Run security checks (bandit + safety)"
	@echo "  make all-checks    Run all quality checks"
	@echo ""
	@echo "Development Commands:"
	@echo "  make clean         Clean build artifacts and cache"
	@echo "  make build         Build distribution packages"
	@echo "  make publish-test  Publish to Test PyPI"
	@echo "  make docs          Build documentation"
	@echo "  make serve-docs    Serve documentation locally"
	@echo ""
	@echo "Pre-commit Commands:"
	@echo "  make pre-commit-install  Install pre-commit hooks"
	@echo "  make pre-commit-run      Run pre-commit on all files"
	@echo ""
	@echo "Use 'make <command>' to run a specific command."

# Setup commands
install: ## Install package in editable mode
	pip install -e .

install-dev: ## Install development dependencies
	pip install -e ".[dev,test,docs]"

setup-dev: install-dev pre-commit-install ## Complete development environment setup
	@echo "Development environment setup complete!"
	@echo "You can now run 'make test' to verify everything works."

# Testing commands
test: ## Run all tests with coverage
	pytest tests/ -v --cov=src/suews_mcp --cov-report=term --cov-report=html --cov-report=xml

test-unit: ## Run unit tests only
	pytest tests/unit/ -v

test-integration: ## Run integration tests only
	pytest tests/integration/ -v

test-e2e: ## Run end-to-end tests only
	pytest tests/e2e/ -v

test-performance: ## Run performance tests only
	pytest tests/performance/ -v

coverage: ## Generate detailed coverage report
	pytest tests/ --cov=src/suews_mcp --cov-report=term --cov-report=html --cov-report=xml
	@echo "Coverage report generated in htmlcov/index.html"

# Code quality commands
lint: ## Run linting with ruff
	ruff check src tests

format: ## Format code with black and fix issues with ruff
	black src tests
	ruff check --fix src tests

type-check: ## Run type checking with mypy
	mypy src tests

security-check: ## Run security checks
	bandit -r src/ -f json -o bandit-report.json || echo "Bandit found issues, check bandit-report.json"
	safety check --json --output safety-report.json || echo "Safety found issues, check safety-report.json"

all-checks: lint type-check security-check ## Run all code quality checks
	@echo "All code quality checks completed!"

# Build and publish commands
clean: ## Clean build artifacts and cache
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info/
	rm -rf .coverage
	rm -rf htmlcov/
	rm -rf coverage.xml
	rm -rf .pytest_cache/
	rm -rf .mypy_cache/
	rm -rf .tox/
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete 2>/dev/null || true

build: clean ## Build distribution packages
	python -m build
	twine check dist/*

publish-test: build ## Publish to Test PyPI
	twine upload --repository testpypi dist/*

publish: build ## Publish to PyPI (use with caution!)
	twine upload dist/*

# Documentation commands
docs: ## Build documentation
	cd docs && python -m sphinx -b html . _build/html

serve-docs: docs ## Build and serve documentation locally
	cd docs/_build/html && python -m http.server 8000
	@echo "Documentation served at http://localhost:8000"

# Pre-commit commands
pre-commit-install: ## Install pre-commit hooks
	pre-commit install
	pre-commit install --hook-type commit-msg

pre-commit-run: ## Run pre-commit hooks on all files
	pre-commit run --all-files

# Development workflow commands
dev-test: lint type-check test ## Quick development test cycle
	@echo "Development checks completed successfully!"

ci-test: all-checks test-integration test-e2e ## Run the full CI test suite locally
	@echo "All CI checks completed successfully!"

# Utility commands
requirements: ## Generate/update requirements files
	pip-compile pyproject.toml --output-file requirements.txt
	pip-compile pyproject.toml --extra dev --output-file requirements-dev.txt

show-deps: ## Show installed dependencies
	pip list

show-outdated: ## Show outdated dependencies
	pip list --outdated

# Server commands
run-server: ## Run the SUEWS MCP server in development mode
	python -m suews_mcp.server --log-level DEBUG

test-server: ## Test the server with sample requests
	python -c "from suews_mcp.server import main; print('Server import successful')"

# Docker commands (if needed in future)
docker-build: ## Build Docker image (when Dockerfile is added)
	@echo "Docker support not yet implemented"

docker-run: ## Run server in Docker (when Dockerfile is added)
	@echo "Docker support not yet implemented"

# Quick shortcuts
t: test ## Shortcut for test
l: lint ## Shortcut for lint
f: format ## Shortcut for format
c: clean ## Shortcut for clean
b: build ## Shortcut for build