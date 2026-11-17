# SUEWS Developer Onboarding Guide

> **Canonical Rules Notice**
>
> Active standing rules, path-scoped conventions, and automated workflows live in [`AGENTS.md`](../AGENTS.md) and [`.claude/README.md`](../.claude/README.md). Read those first -- when an onboarding summary drifts from `.claude/rules/`, `.claude/rules/` takes precedence.

## Overview

This guide provides a structured onboarding walkthrough for developers joining SUEWS, combining environment setup with the repository's issue, pull-request, review, and merge-queue workflows.

## Prerequisites

Before starting development, install:

1. **Essential Build & Runtime Tools**:
   - `git` configured with SSH access (`origin` set to `git@github.com:UMEP-dev/SUEWS.git`)
   - GitHub CLI (`gh`) authenticated with repository access
   - **Python `>= 3.12`** and [`uv`](https://docs.astral.sh/uv/)
   - **`gfortran` compiler** (`>= 9.3.0`)
   - **Rust toolchain** (`rustup` / `cargo` from [https://rustup.rs](https://rustup.rs)) for compiling `src/suews_bridge/`
   - `meson` and `ninja` build tools

2. **Recommended Tools**:
   - Coding agents (Claude Code, Codex, or Antigravity) configured to read `AGENTS.md` and `.claude/`
   - Editor support for Ruff (Python), `fprettify` (Fortran), and `rust-analyzer` / `clippy` (Rust)

## Part 1: User Perspective - Understanding SUEWS

### 1.1 Familiarise Yourself as a User

1. **Use `SUEWSSimulation`**:
   - `SUEWSSimulation` is the primary object-oriented Python interface (`from supy import SUEWSSimulation`).
   - Configurations use validated YAML schemas (`SUEWSConfig` in `src/supy/data_model/`).
   - Review the tutorials on [https://docs.suews.io](https://docs.suews.io) to see standard simulation workflows.

2. **CLI Entry Points**:
   - The unified `suews` CLI provides `suews run`, `suews convert`, `suews validate`, and `suews schema`.
   - Legacy table-based inputs are migrated to YAML using `suews convert` (the older `suews-convert` alias is deprecated).

3. **Community Channels**:
   - Technical bugs and feature tracking: [GitHub Issues](https://github.com/UMEP-dev/SUEWS/issues)
   - User questions and community discussion: [SUEWS Community Forum (Discourse)](https://community.suews.io)

## Part 2: Development Workflow

### 2.1 Issue-Driven Development

All substantive changes start with an issue on `UMEP-dev/SUEWS`:

1. **Create or Claim an Issue**:
   - Check existing issues before creating a new one (`gh issue list --state all --limit 50`).
   - Apply the four-tier label hierarchy (`1-*` type, `2-*` area, `3-*` priority, `4-*` status; see [ISSUE_TRIAGE.md](ISSUE_TRIAGE.md) and `.claude/rules/labels.md`).
   - Keep each issue scoped to one reviewable, revertible concern (see `.claude/rules/work-sizing.md`; `/triage-issue` helps audit readiness).

### 2.2 Local Environment and Feature Branches

```bash
# Clone repository (keep 'origin' as the ONLY git remote)
git clone git@github.com:UMEP-dev/SUEWS.git
cd SUEWS
git checkout -b feat/your-feature-name

# Create .venv and install editable build (Fortran + Rust suews_bridge + supy)
make setup
source .venv/bin/activate
make dev

# Run fast smoke tests (~30-60 s) and everyday dev suite
make test-smoke
make test
```

For parallel work across multiple issues or PRs, prefer `git worktree` rather than switching branches inside a single build tree (see [FEATURE_DEVELOPMENT_WORKFLOW.md](FEATURE_DEVELOPMENT_WORKFLOW.md)).

### 2.3 Core Architecture (`src/`, `test/`, `docs/`)

- **`src/suews/src/` (Fortran physics kernel)**:
  - Contains `suews_{ctrl,phys,type,util}_*.f95`.
  - Never initialise local variables in declarations (implies `SAVE` and breaks multi-grid thread safety; blocked by `scripts/lint/check_saved_fortran_locals.py`).
  - Report errors via `set_supy_error` / `supy_error_flag()` (never `STOP` or `WRITE(*,...)`).
  - After adding a Fortran file or changing `USE` statements, register the file in `meson.build` and run `python scripts/suews/gen_fortran_deps.py` to refresh `src/suews/Makefile.deps`.

- **`src/suews_bridge/` (C-ABI + Rust PyO3 bridge)**:
  - Replaces the retired `supy_driver` / `f90wrap` / `f2py` layer.
  - Exposes the `suews_bridge` Python extension (`cp312-abi3` wheel) and the standalone developer binary `suews-engine` (`make bridge`).
  - Governed by `.claude/rules/rust/conventions.md` and `.claude/rules/rust/bridge-contract.md` (BR-01..BR-10): keep physics in Fortran, defaults/migrations in Python Pydantic models, and transport/validation in the bridge.

- **`src/supy/` (Python interface and Pydantic data model)**:
  - Houses `SUEWSSimulation`, `SUEWSConfig` (`src/supy/data_model/`), YAML upgraders, output definitions, and the `suews` CLI (`src/supy/cmd/`).
  - Structural schema changes require a schema version bump and migration handler per `.claude/rules/python/schema-versioning.md`.

- **`test/` (Pytest suite)**:
  - Every `test/**/*.py` file must live under `test/` and declare `pytestmark = pytest.mark.api` or `pytestmark = pytest.mark.physics`.
  - Access packaged `sample_data` via `importlib.resources.files("supy")` or `trv_supy_module` (never `Path(sp.__file__).parent`).

- **`docs/` (Sphinx user documentation)**:
  - Edit source RST or generator scripts/Pydantic descriptions -- never hand-edit generated parameter tables or `schema.json` (see `.claude/rules/docs/conventions.md` and run `/audit-docs`).

### 2.4 Pre-PR Verification, Review, and Merge Queue

1. **Format and Lint**:
   ```bash
   make format      # Formats Python (ruff) and Fortran (fprettify)
   make test-smoke  # Required before every commit
   make test        # Run everyday suite (or make test-all when touching cmd/mcp/docs/umep)
   ```
   Use `/lint-code`, `/verify-build`, and `/audit-docs` in agent sessions.

2. **Open a Pull Request**:
   - Open as a Draft PR early if you want CI feedback on reduced tiers; mark Ready for review once tests, docs, and `CHANGELOG.md` (if applicable) are complete.
   - If the PR alters model physics or reference fixtures, apply `0-physics:change`, include a `## Scientific evidence` section, refresh moved fixtures in the same PR, and obtain required domain-owner sign-off (for example `@yiqing1021` for STEBBS; see `.claude/rules/physics-change-evidence.md` and [SCIENTIFIC_REVIEWERS.md](SCIENTIFIC_REVIEWERS.md)).

3. **Merge via the Merge Queue (Never Push Directly to `master`)**:
   - Branch protection prohibits direct pushes to `master`.
   - Once CI (`pr-gate`) and required reviews pass, enqueue the PR via GitHub's merge queue (`gh pr merge <PR> --auto --squash` or `/queue-pr`; see [MERGE_QUEUE.md](MERGE_QUEUE.md)).

## Part 3: Onboarding Checklist

### Week 1: Environment Setup
- [ ] Install Python `>= 3.12`, `uv`, `gfortran`, `rustup`/`cargo`, `meson`, and `ninja`
- [ ] Clone `git@github.com:UMEP-dev/SUEWS.git` and verify `git remote -v` shows only `origin`
- [ ] Run `make setup && source .venv/bin/activate && make dev`
- [ ] Run `make test-smoke` and `make test`
- [ ] Read [`AGENTS.md`](../AGENTS.md) and [`.claude/README.md`](../.claude/README.md)

### Week 2: Codebase Familiarisation
- [ ] Run a sample simulation with `SUEWSSimulation.from_sample_data().run()`
- [ ] Explore `src/supy/data_model/`, `src/suews_bridge/`, and `src/suews/src/`
- [ ] Read [CODING_GUIDELINES.md](CODING_GUIDELINES.md) and [testing/TESTING_GUIDELINES.md](testing/TESTING_GUIDELINES.md)
- [ ] Pick a starter `4-ready` issue (`gh issue list --label 4-ready`)

### Weeks 3-4: First Contribution
- [ ] Create a feature branch (or `git worktree`) linked to the issue
- [ ] Implement the change with tests (`pytestmark` nature marker included)
- [ ] Run `make format && make test-smoke && make test`
- [ ] Open a PR, address review feedback (`/audit-pr`), and land via the merge queue
