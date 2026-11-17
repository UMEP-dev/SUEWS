# SUEWS Testing Guidelines Reference

> **Canonical Rules Notice**
>
> Active, path-scoped testing rules in `.claude/` and CI lints supersede this overview whenever an example drifts. Always read the governing rule files first:
> - [`.claude/rules/tests/patterns.md`](../../.claude/rules/tests/patterns.md) -- test placement taxonomy, nature/cost markers, tolerances, `importlib.resources` packaged-data access, and banned test constructions
> - [`.claude/rules/tests/verification-methodology.md`](../../.claude/rules/tests/verification-methodology.md) -- reading existing loaders (`sample_output_io.load_sample_output`), covering all public API paths, and diagnostic discipline
> - [`.claude/rules/physics-change-evidence.md`](../../.claude/rules/physics-change-evidence.md) -- required evidence, fixtures, and `0-physics:change` CI gates when model outputs move
>
> Note: Companion files [`FORTRAN_TEST_PATTERNS.md`](FORTRAN_TEST_PATTERNS.md) and [`ERROR_HANDLING_PATTERNS.md`](ERROR_HANDLING_PATTERNS.md) contain historical design explorations -- check their top banners before copying snippets.

## 1. Test Design Philosophy

### Level Taxonomy (Test Contracts, Not Implementation Details)
Before writing a test, ask whether it could fail while user-visible or physically meaningful behaviour is unchanged. Place each test at the level that owns the contract (see `.claude/rules/tests/patterns.md` "Test Placement: the Level Taxonomy"):
- **Config / validator contracts**: Test via `SUEWSConfig.from_yaml` / `from_dict` or CLI entry points, not private `_validate_*` helpers on `SimpleNamespace` stubs.
- **Model physics**: Test via `SUEWSSimulation` runs (`test/physics/`, `test/core/`) with physically justified tolerances, plus Rust bridge codec/CLI checks in `src/suews_bridge/`.
- **Adaptor layers** (CLI wrapping `supy`, MCP wrapping CLI): Assert delegation, argument wiring, and error mapping rather than re-testing core physics.
- **Public pure helpers**: Unit-test directly.

### FIRST Principles and AAA Pattern
- **FIRST**: **F**ast, **I**ndependent, **R**epeatable, **S**elf-validating, **T**imely.
- **AAA**: **A**rrange (set up inputs/fixtures), **A**ct (run the public entry point), **A**ssert (verify physical or API contracts).

## 2. Running Tests Locally (`Makefile` Targets)

Use the repository `Makefile` targets (requires an activated virtual environment built with `make dev`):

```bash
# Fast fail-fast smoke suite (~30-60 s) -- run before every commit
make test-smoke

# Everyday development suite (~2 min across up to TEST_JOBS=4 xdist workers)
# Covers test/test_api_surface.py, test/core, test/data_model, test/physics, test/io_tests
# Excludes slow tests and peripheral surfaces (cmd, mcp, docs, knowledge, umep)
make test

# Full test suite including slow regressions and all peripheral directories
make test-all

# UMEP/QGIS compatibility suite (targets Windows + Python 3.12; skips elsewhere)
make test-qgis
```

When editing a peripheral surface skipped by `make test` (`test/cmd/`, `test/mcp/`, `test/docs/`, `test/knowledge/`, or `test/umep/`), run that directory explicitly (for example `pytest test/cmd -v`) or run `make test-all`.

## 3. Test File Organisation

**SuPy runtime and physics tests MUST live under `test/`.** Standard `make test*` targets and `scripts/lint/check_test_markers.py` collect from `test/` (`pyproject.toml` sets `testpaths = ["test"]`); placing a SuPy test alongside `src/` leaves it uncollected. Two dedicated standalone suites live outside `test/` and run in their own workflows: `benchmark/tests/test_*.py` (regression-harness unit tests) and `.claude/scripts/test_validate_agents_md.py` (`unittest` suite for shared agent-governance rules).

```text
test/
|-- core/           # Core API, state persistence, sample output, exact reference gate
|-- data_model/     # Pydantic config validation, schema versioning, YAML converters
|-- physics/        # Scientific validation, conservation checks, option sweeps
|-- io_tests/       # Input/output formats, forcing loaders, resampling
|-- cmd/            # CLI entry points (suews run, convert, validate, schema)
|-- mcp/            # MCP server tools and plugin packaging manifests
|-- docs/           # Documentation generation and RST sanity checks
|-- umep/           # UMEP/QGIS compatibility contracts (auto-marked api + qgis)
`-- fixtures/       # Vendored self-contained test fixtures and reference shards
```

## 4. Composed Pytest Markers and CI Tiers

Markers compose across orthogonal nature, importance, and cost axes declared in `pyproject.toml` (gh#1300, gh#1680):

- **Nature axis (REQUIRED at module level in every `test/**/*.py` file)**:
  ```python
  import pytest

  pytestmark = pytest.mark.api      # Python wrapper, Pydantic, pandas, CLI, I/O
  # or:
  pytestmark = pytest.mark.physics  # Numerical / compiled Fortran + Rust kernel output
  ```
  Enforced by `scripts/lint/check_test_markers.py` and `test/conftest.py`.
- **Importance and cost decorators** (applied per test as needed):
  - `smoke`: Minimal wheel validation (~60 s).
  - `smoke_bridge`: Bridge-loading subset across `cp312..cp3xx` after one `abi3` build.
  - `core`: Essential physics/logic guardrails (included in `standard` even if `slow`).
  - `cfg`: Configuration and schema validation tests.
  - `util`: Non-critical utility tests.
  - `rust`: Rust bridge backend tests requiring `suews_bridge` with `physics`.
  - `medium`: Test body takes 10 to 30 CPU seconds on the Linux `cp312` reference runner.
  - `slow`: Test body takes 30+ CPU seconds on the reference runner, or carries `slow(reason="...")` for a non-CPU reason; audited nightly by `scripts/lint/check_cost_markers.py`.
  - `qgis`: UMEP/QGIS plugin tests in `test/umep/` (Windows + Python 3.12 target).

### Tier -> CI Mapping

| Tier | Approx. duration | Runs on |
|------|------------------|---------|
| `smoke` | ~60 s | Draft PRs (`python`/`util`/`ci`/tests-only changes), ready PRs (`ci`/tests-only) |
| `core` / `cfg` | ~2-3 min | Draft PRs (`fortran`/`rust` -> `core`; build-system -> `cfg`) |
| `standard` | ~5-10 min | Ready PRs, merge queue |
| `physics-full` | physics axis includes `slow`; api axis stays `standard` | Ready PR / merge queue when PR carries `0-physics:change` (gh#1576) |
| `all` | ~15-30 min | Nightly schedule, tag push, manual dispatch (`full`) |

## 5. Session-Scoped Sample Fixtures

Tests that only **read** a completed sample simulation's config, forcing, state, or output should reuse the session-scoped fixtures in `test/conftest.py` instead of running a fresh simulation per test:
- `sample_data_loaded`: Bundled `(df_state_init, df_forcing)` loaded once per session (read-only; `.copy()` before mutating).
- `completed_sample_sim`: Short `SUEWSSimulation` from sample YAML with `.run()` already executed (read-only; do not mutate, `.reset()`, or re-run).
- `sample_run_cached`: Session-scoped factory memoising OOP sample runs by forcing window.
- `sample_reference`: Loads the 12 monthly sample-output reference shards via `sample_output_io.load_sample_output`.

## 6. Locating Packaged Data (`importlib.resources` Required)

**NEVER use `Path(sp.__file__).parent` or any imported `<module>.__file__` in `test/`** to reach packaged `sample_data` or schema files. `scripts/lint/check_packaged_data_paths.py` **fails CI** on any `<module>.__file__` access under `test/`.

### Canonical Pattern: `importlib.resources.files("supy")` or `trv_supy_module`

Prefer the package's own `SUEWSSimulation.from_sample_data()` factory when you just need a configured simulation:
```python
import supy as sp

sim = sp.SUEWSSimulation.from_sample_data()
df_state_init = sim.state_init
df_forcing = sim.forcing.df
```

When a test genuinely needs packaged files from `supy/sample_data`, use `importlib.resources.files("supy") / "sample_data"` (or the shared handle `trv_supy_module` from `supy._env`) and honour the `Traversable` protocol:

```python
from importlib.resources import as_file, files
import yaml
import supy as sp
from supy._env import trv_supy_module

# Method 1: Read directly via Traversable protocol methods (.is_file(), .open(), .read_text())
sample_config = trv_supy_module / "sample_data" / "sample_config.yml"
# Equivalent: sample_config = files("supy") / "sample_data" / "sample_config.yml"
assert sample_config.is_file()  # Use .is_file(), NOT .exists()
with sample_config.open(encoding="utf-8") as f:
    config_data = yaml.safe_load(f)

# Method 2: Materialise the directory with as_file() when sibling files (forcing) are needed
with as_file(trv_supy_module / "sample_data") as path_sample_dir:
    sim = sp.SUEWSSimulation(path_sample_dir / "sample_config.yml")
```

**Key rules for `Traversable` handles** (see `.claude/rules/tests/patterns.md` "Locating packaged data"):
- Never call `.parent` or `.exists()` on a `Traversable`, and never pass a `Traversable` directly to `open()` or `str()` as a filesystem path; wrap with `as_file(...)` whenever a real `os.PathLike` is needed.
- Materialise the `sample_data` **directory** (not `sample_config.yml` alone) when calling `SUEWSSimulation(...)` or forcing loaders so sibling forcing files (`Kc_2012_data_60.txt`) exist alongside the YAML.

## 7. Coverage, Skipping, and Documentation Standards

- **Coverage targets**: >= 80% overall and on new code; 95-100% on critical validation/kernel paths.
- **Skipping tests**: Every `pytest.skip`, `@pytest.mark.skip`, `@pytest.mark.skipif`, and `pytest.importorskip` MUST provide a concrete `reason="..."` explaining what resource/platform is missing or linking a tracking issue (`#NNNN`). Never wrap test bodies in `try ... except Exception: pytest.skip(...)`.
- **Scientific docstrings**: Cite literature equations and state physical tolerance justifications in plain ASCII (for example `+/- 5.0 W m-2`):
  ```python
  def test_urban_canyon_radiation():
      """Validate canyon net radiation against Grimmond & Oke (1999) Eq. 3.2 (+/- 5.0 W m-2)."""
  ```
