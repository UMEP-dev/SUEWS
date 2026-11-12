# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Constitution

Five articles govern every instruction in this repository. They are the reason each rule exists; the rules, skills and guides below are how they are carried out. When a situation is covered by no rule, act on the articles directly.

1. **Claims are checkable.** The tree and its executable gates are the truth; instructions describe them and never override them. A path, symbol, command or flag written in an instruction must exist as written. A scientific claim or audit finding carries evidence someone else can reproduce. A rule that states a target the code has not reached says so and cites the tracking issue.
2. **One owner.** Each fact, rule and behaviour lives in exactly one place, and everything else points to it. Put behaviour in the code that owns the operation; change a generator, never its output. Removing text means moving it to its owner or stating that it is retired.
3. **One convention per surface.** A reader meets one language, one naming scheme and one format on each surface (code, YAML, docs, changelog, labels). The convention is chosen once and recorded in a rule; it is not re-decided per file.
4. **Reversible by default.** Work is sized so one person can review and revert it, and every loop has a stopping rule. Anything irreversible or outward-facing (merging, closing, posting onto others' work, schema migrations, scientific sign-off) waits for a human.
5. **Users are never broken silently.** A change users can feel is announced, versioned and given a migration path. Errors reach the user by name through the supported channel; the model never stops the interpreter or substitutes a value without saying so.

### How instructions derive from the articles

- **Authority**, highest first: executable gates (CI, hooks, tests); this file; `.claude/rules/`; `.claude/skills/` and `.claude/agents/`; `dev-ref/`. A lower layer that disagrees with a higher one is wrong and gets fixed, not followed.
- **Every rule cites its articles.** Each entry in the shared rule index below ends with the article(s) it serves. Skills and guides carry out rules; they add procedure, not policy.
- **Review against the articles.** A change to any instruction file is judged by the article it serves: Article 1 changes are checked against the tree, Article 2 changes by confirming every removed line has an owner.

### Changing the rules

- **Start from the articles.** Before adding or changing a rule, name the article it serves. If an existing rule already serves that purpose, change that rule. If no article fits, the rule is not needed, or the constitution is missing something: settle that first.
- **Amending an article** is its own pull request, touches only this section, and needs maintainer approval. It is never bundled with rule changes and never made by an agent on its own initiative.

## Quick Start

```bash
uv venv && source .venv/bin/activate && make dev && make test-smoke
```

Style check: `/lint-code` | Docs check: `/audit-docs` | Build check: `/verify-build` | PR review: `/audit-pr`

## Essential Rules

The everyday subset of the rules indexed below.

- **British English** (exception: numpy/scipy conventions like "analyze")
- **No emoji** in print/logging - plain ASCII only
- **Test before commit**: `make test-smoke`
- **Git remote**: `origin` only (`git@github.com:UMEP-dev/SUEWS.git`)
- **New source files**: Add to `meson.build`; after adding a Fortran file or `USE` statement, regenerate `src/suews/Makefile.deps` (`python scripts/suews/gen_fortran_deps.py`)

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
- `.claude/rules/` - The rules derived from the constitution
- `.claude/agents/` - Review subagents (`docs-reviewer` for doc-code drift and docs house style)

## Shared rule files

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy (Article 2). Each entry ends with the article(s) it serves.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work; Art. 3.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work; Art. 4.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`; Art. 3, 5.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`; Art. 1, 4.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work; Art. 2.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - `pyproject.toml`, `.github/workflows/**/*`; Art. 4.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - `docs/source/assets/refs/*.bib`, `docs/**/*`; Art. 3.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`; Art. 2, 3.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - `docs/**/*`; Art. 1, 5.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.f90`, `src/suews/**/*.f95`; Art. 3.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - `src/suews/**/*.f9*`; Art. 5.
- [.claude/rules/labels.md](.claude/rules/labels.md) - all work; Art. 3.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - `src/supy/**/*.py`, `src/supy/**/*.yml`, `src/supy/**/*.yaml`, `docs/**/*`; Art. 3.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`; Art. 1.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`; Art. 3.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`; Art. 2.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`; Art. 3.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`; Art. 5.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - `src/supy/data_model/**/*`, `src/supy/util/converter/**/*`, `src/supy/sample_data/**/*`, `docs/**/*`; Art. 5.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work; Art. 4.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract; Art. 2.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`; Art. 3.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`; Art. 1, 3.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`; Art. 1.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work; Art. 4.
