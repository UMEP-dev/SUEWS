# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Constitution

Five articles are the reasons behind the rules below. Each protects something that is expensive to lose: the science, the truth of these instructions, the maintainer's attention, the user's work, and trust that cannot be rebuilt. Rules, skills and guides carry the articles out. Where they are silent, the articles guide the choice among actions already permitted; they never widen a gate.

1. **The model is right, and shown to be.** Numerics are explicit, deterministic and the same on every supported platform. A change to model output carries evidence someone else can reproduce, and the sign-off of whoever owns that science.
2. **Instructions tell the truth.** An instruction describes the tree as it is: a path, symbol, command or flag it names exists as written. The tree records what the code does, not what it should do, so a suspected bug is reported, not documented as intended. A rule that states a target the code has not reached says so and cites the issue.
3. **Decide once, state once.** Each fact, rule and behaviour has one owner, and everything else points to it. Each surface (code, YAML, docs, changelog, labels) has one convention, recorded in a rule. A diff does not add a second copy, edit generated output, or remove text without moving it or declaring it retired.
4. **Users are not broken silently.** A change users can feel is announced, versioned where a schema is involved, and given a migration path. A diff does not add a way to stop the interpreter, or a fallback value without a reported error.
5. **Nothing irreversible without a human.** Work is small enough to review and revert, and every loop has a stopping rule. Actions that cannot be undone, or that act on others' work, wait for a human. Untrusted input never receives credentials, nothing private reaches a public surface, and reporters and authors keep their credit. `.claude/rules/autonomous-workflow.md` owns the list of gated actions.

No article overrides a gate under Article 5. Other conflicts between articles are settled by the rules, and by a human where the rules are silent.

### How instructions derive from the articles

- **Authority among instruction files**, highest first: this file; `.claude/rules/`; `.claude/skills/` and `.claude/agents/`; `dev-ref/`. When two disagree, follow the higher one for the task in hand and report the disagreement; fix the lower one only when asked, or in its own pull request.
- **Gates decide what can merge, not what is correct.** A CI check, test or hook believed wrong is escalated, never bypassed or edited to pass.
- **Your user's explicit instruction** outranks these files, but not a gate.
- **The index cites the articles.** Each entry in the shared rule index ends with the article(s) its rules serve. Know-how in a rule file (build notes, debugging tips) needs no article. Skills and agents add procedure; policy found in one belongs in a rule.

### Changing the rules

- **Start from the articles.** Before adding or changing a rule, name the article it serves. If an existing rule already serves that purpose, change that rule. A poor fit is a signal to raise with the maintainer, not grounds to force a citation or to delete the rule.
- **Amending an article** is its own pull request. It changes only this section and the citations that follow from it, and `@sunt05` approves it. An agent that believes an article is wrong says so to its user; it does not amend one on its own initiative. A typo fix that changes no meaning is exempt.

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

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy (Article 3). Each entry ends with the article(s) it serves.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work; Art. 3, 4.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work; Art. 2, 5.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`; Art. 3, 4.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`; Art. 1, 5.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work; Art. 3.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - `pyproject.toml`, `.github/workflows/**/*`; Art. 5.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - `docs/source/assets/refs/*.bib`, `docs/**/*`; Art. 3.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`; Art. 3.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - `docs/**/*`; Art. 2, 4.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.f90`, `src/suews/**/*.f95`; Art. 1, 3.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - `src/suews/**/*.f9*`; Art. 4.
- [.claude/rules/labels.md](.claude/rules/labels.md) - all work; Art. 3.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - `src/supy/**/*.py`, `src/supy/**/*.yml`, `src/supy/**/*.yaml`, `docs/**/*`; Art. 3, 4.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`; Art. 1, 5.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`; Art. 3.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`; Art. 3.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`; Art. 1, 3.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`; Art. 4.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - `src/supy/data_model/**/*`, `src/supy/util/converter/**/*`, `src/supy/sample_data/**/*`, `docs/**/*`; Art. 4.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work; Art. 5.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract; Art. 3, 4.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`; Art. 3.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`; Art. 1, 3.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`; Art. 1.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work; Art. 5.
