# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

Style check: `/lint-code` | Docs check: `/audit-docs` | Build check: `/verify-build` | PR review: `/audit-pr`

## Essential Rules

- **British English** (exception: numpy/scipy conventions like "analyze")
- **No emoji** in print/logging - plain ASCII only
- **Test before commit**: `make test-smoke`
- **Git remote**: `origin` only (`git@github.com:UMEP-dev/SUEWS.git`)
- **New source files**: Add to `meson.build`

## Project Structure

| Directory | Purpose | Rules |
|-----------|---------|-------|
| `src/suews/src/` | Fortran | `.claude/rules/fortran/` |
| `src/supy/` | Python | `.claude/rules/python/` |
| `docs/` | Documentation | `.claude/rules/docs/` |
| `test/` | Tests | `.claude/rules/tests/` |
| `.github/workflows/` | CI/Actions | `.claude/rules/ci/` |

## Skills

- `/lint-code` - Check code style
- `/audit-docs` - Docs sanity check (non-ASCII + bib topic-tags)
- `/sync-docs` - Doc-code consistency
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
- `.claude/README.md` - Full workspace documentation
- `.claude/skills/` - Detailed skill workflows
- `.claude/rules/` - Style conventions

## Shared rule files

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - all work.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - all work.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - all work.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.f90`, `src/suews/**/*.f95`.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - all work.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - all work.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - all work.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - all work.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work.
