# SUEWS Developer Documentation

This directory contains internal documentation, architectural rationale, and audit ledgers for SUEWS core developers and coding agents.

## Authority Hierarchy

When guidance in this repository appears to conflict, follow this precedence order from highest to lowest:

1. **Executable CI gates and lint scripts** (`scripts/lint/*`, `scripts/suews/*`, `.claude/scripts/validate-claude-md.py`, `.github/workflows/*`).
2. **[`AGENTS.md`](../AGENTS.md) and `.claude/rules/**/*.md`** (standing rules and coding, documentation, and workflow contracts).
3. **`.claude/skills/**` and `.claude/agents/*.md`** (operational workflows and review subagents).
4. **`dev-ref/*.md`** (architectural rationale, whole-repository audit protocol, and dated subsystem ledgers; when a `dev-ref/` example drifts from `.claude/rules/`, `.claude/rules/` wins).

## Quick Navigation

### Getting Started
- **[Onboarding Guide](onboarding-guide.md)** - Comprehensive guide for new developers
  - Environment setup, workflow overview, first contribution checklist
  - Start here if you are new to the team

- **[Building Locally](building-locally.md)** - Quick guide to build and test SUEWS from source
  - Prerequisites (`gfortran`, Rust toolchain, `meson`, `ninja`, `uv`, Python `>= 3.12`), setup, build profiles, and test commands

### Development Workflows / Quality & Auditing
- **[CODEBASE_AUDIT_PROTOCOL.md](CODEBASE_AUDIT_PROTOCOL.md)** - Canonical protocol for whole-repository and subsystem audits
  - Evidence standards, verification methodology, and subsystem audit tracking

- **[Coding Guidelines](CODING_GUIDELINES.md)** - Code style and conventions overview
  - Python, Fortran, and Rust bridge standards
  - Naming conventions, logging, and formatting

- **[Fortran Naming Conventions](FORTRAN_NAMING_CONVENTIONS.md)** - Fortran naming standards and migration notes
  - File (`suews_<category>_<name>.f95`), module (`module_<category>_<name>`), and derived-type (`dts_<name>`) patterns
  - Migration strategy from legacy code

- **[Feature Development Workflow](FEATURE_DEVELOPMENT_WORKFLOW.md)** - Managing large features with incremental PRs
  - Breaking down features into reviewable chunks
  - Stacked and parallel PR strategies
  - Git worktrees for parallel development

- **[Merge Queue](MERGE_QUEUE.md)** - GitHub merge queue best practices
  - Adding PRs to the queue (`/queue-pr`, `gh pr merge --auto`) and monitoring status
  - CI matrix behaviour and troubleshooting queue failures

- **[Review Process](REVIEW_PROCESS.md)** - Pull request review procedures
  - Code governance, `0-physics:change` evidence gates, and review workflow
  - Manual maintainer routing for governance and process changes

- **[Scientific Reviewers](SCIENTIFIC_REVIEWERS.md)** - Manual scientific reviewer routing
  - Domain-owner gates (for example STEBBS `@yiqing1021`) and advisory module reviewers
  - When and how to request scientific review

- **[Rust Bridge Design](RUST_BRIDGE_DESIGN.md)** - Responsibilities, data flow, and shared audit rules
- **[Rust Bridge Baseline Audit](RUST_BRIDGE_AUDIT.md)** - Checked boundaries, known deviations, and follow-up evidence

- **[Adding a Parameter at the Rust Level](RUST_BRIDGE_ADD_PARAMETER.md)** - Bringing a new parameter through the Rust bridge
  - The eight hand-maintained steps, from the Fortran type to the YAML reader and data model
  - Where a mistake fails silently, and a checklist

- **[ESTM Reactivation](ESTM_REACTIVATION.md)** - Bringing back the disabled ESTM storage-heat scheme
  - What gh#1802 took out of the build and where it lives in git history
  - The blockers to fix first, then the mechanical restore

### Testing
- **[Testing Guidelines](testing/TESTING_GUIDELINES.md)** - Overall testing strategy
  - Composed pytest markers, CI test tiers, and `importlib.resources` package-data access
  - FIRST principles and AAA pattern

- **[Fortran Test Patterns](testing/FORTRAN_TEST_PATTERNS.md)** - Historical Fortran unit-testing exploration (unimplemented; `f90wrap` removed)
  - Retained as historical context; active physics tests run via `SUEWSSimulation` and `suews_bridge`

- **[Error Handling](testing/ERROR_HANDLING_PATTERNS.md)** - Historical error-UX design exploration (unimplemented)
  - Retained as historical context; active error handling follows `.claude/rules/fortran/error-reporting.md` and `.claude/rules/rust/conventions.md`

### Operations
- **[Issue Triage](ISSUE_TRIAGE.md)** - How to manage GitHub issues
  - Four-tier (`1-*`..`4-*`) and `0-*` automation labels
  - Assignment and triage workflow

- **[Release Manual](RELEASE_MANUAL.md)** - Release procedures
  - CalVer (`YYYY.M.D`) single-wheel PyPI releases and TestPyPI `.dev` builds
  - Read the Docs release anchors (`release-docs-anchor.yml`) and Zenodo DOI archiving

## External Links
- [GitHub Repository](https://github.com/UMEP-dev/SUEWS)
- [User Documentation](https://docs.suews.io)
- [GitHub Issues](https://github.com/UMEP-dev/SUEWS/issues)
- [SUEWS Community Forum (Discourse)](https://community.suews.io)

## Note for AI Assistants
Shared coding-agent instructions start in [AGENTS.md](../AGENTS.md), with active rules, skills, and review subagents maintained under `.claude/`.
