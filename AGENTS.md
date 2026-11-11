# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

Style check: `/lint-code` | Docs check: `/audit-docs` | Build check: `/verify-build` | PR review: `/audit-pr`

## Essential Rules

- **British English** (exception: upstream numpy/scipy/pandas API spellings like `analyze`)
- **No emoji** in print/logging - plain ASCII only
- **Test before commit**: `make test-smoke`
- **Git remote**: `origin` only (`git@github.com:UMEP-dev/SUEWS.git`)
- **New source files**: Register Python files in `src/supy/meson.build`, Fortran kernel files in `src/suews/Makefile` and `src/supy/meson.build` (then run `python scripts/suews/gen_fortran_deps.py`), and C-API files in `src/suews_bridge/build.rs` and `src/suews_bridge/bridge-manifest.json`

## Project Structure

| Directory | Purpose | Rules |
|-----------|---------|-------|
| `src/suews/src/` | Fortran physics & control kernel | `.claude/rules/fortran/` |
| `src/suews_bridge/` | Rust PyO3/CLI bridge & Fortran C-ABI | `.claude/rules/rust/` |
| `src/supy/` | Python package & YAML data model | `.claude/rules/python/` |
| `mcp/` | SUEWS MCP server | `.claude/rules/python/`, `.claude/rules/dependency-safety.md` |
| `docs/` | Documentation | `.claude/rules/docs/` |
| `test/` | Tests | `.claude/rules/tests/` |
| `.github/workflows/` | CI/Actions | `.claude/rules/ci/` |

## Skills

- `/lint-code` - Check code style and static contracts
- `/audit-docs` - Docs sanity check (non-ASCII + bib topic-tags)
- `/triage-issue` - Audit, rewrite, or split a GitHub issue (governance)
- `/fix-issue` - Triage and implement a GitHub issue to PR-ready status
- `/republish-docs` - Republish/revise released docs (move tag to clean anchor)
- `/verify-build` - Build configuration
- `/triage-pr` - Triage draft and stalled PRs into a disposition (advance/continue/defer/close/escalate)
- `/audit-pr` - Review pull requests
- `/split-pr` - Carve an oversized PR into a stacked series of small PRs
- `/queue-pr` - Coordinate PRs before merge queue
- `/log-changes` - Update CHANGELOG
- `/prep-release` - Prepare releases

## Rule loading

Claude Code loads `.claude/rules/` automatically according to each rule's scope. Other agents must read the applicable files listed in the shared rule index below.

## References

- `.impeccable.md` - Design direction for SUEWS-facing surfaces (site/, landing, docs)
- `.claude/README.md` - Full workspace documentation and rule-authoring principles
- `.claude/skills/` - Detailed skill workflows
- `.claude/rules/` - Style and governance conventions
- `.claude/agents/` - Review subagents (`docs-reviewer`, `fortran-error-state-reviewer`, `schema-migration-checker`)
- `dev-ref/CODEBASE_AUDIT_PROTOCOL.md` - Whole-repository and subsystem audit protocol

## Shared rule files

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`, `.github/actions/**/*`, `.github/scripts/**/*`, `.github/path-filters.yml`, `.github/ci-metrics-needs.json`.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - `pyproject.toml`, `mcp/pyproject.toml`, `scripts/security/**/*`, `.github/workflows/**/*`, `.github/actions/**/*`.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - `docs/source/assets/refs/*.bib`, `docs/source/*publications*.rst`, `docs/source/_ext/publications_topic_annotate.py`, `docs/source/_static/publications-filter.js`.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - `docs/**/*`.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.F90`, `src/suews_bridge/c_api/**/*.f9*`.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - `src/suews/**/*.f9*`, `src/suews/**/*.F90`, `src/suews_bridge/c_api/**/*.f9*`.
- [.claude/rules/labels.md](.claude/rules/labels.md) - all work.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - `src/supy/**/*.py`, `src/supy/**/*.yml`, `src/supy/**/*.yaml`, `docs/**/*`.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `src/suews/ext_lib/**/*`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`, `test/fixtures/w16_most.csv`.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`, `mcp/**/*.py`, `scripts/**/*.py`, `.github/scripts/**/*.py`, `benchmark/**/*.py`, `docs/**/*.py`.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - `src/supy/data_model/**/*`, `src/supy/util/converter/**/*`, `src/supy/sample_data/**/*`, `test/fixtures/release_configs/**/*`, `test/fixtures/sample_short/**/*`, `docs/source/contributing/schema/schema_versioning.rst`, `docs/source/inputs/transition_guide.rst`, `docs/source/version-history/**/*`.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `src/supy/cmd/rust_bridge.py`, `src/supy/suews_checkpoint.py`, `src/supy/build_rust_bridge.py`, `src/suews/src/suews_type_*.f95`, `src/suews/src/suews_ctrl_type.f95`, `src/suews/src/suews_ctrl_const.f95`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/c_api/**/*.f9*`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/Cargo.lock`, `src/suews_bridge/build.rs`.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `benchmark/tests/**/*.py`, `.claude/scripts/test_*.py`.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`, `benchmark/tests/**/*.py`.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work.
