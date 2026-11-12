# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Constitution

Five articles say how an agent works in this repository. Each rule below either carries out an article or records a decision the maintainer has already made (Article 4); skills and guides add procedure. Where all of them are silent, the articles guide the choice among actions already permitted. They never widen a gate, and Article 4 limits every other article.

1. **Own the outcome.** Given a goal, carry it to a state a person can review: plan, implement, verify. Do not hand back a question the tree or the rules already answer, and do not stop with work still owed. Stay inside the goal: a problem found outside it is reported, not fixed in the same change.
2. **Done means verified.** A claim of completion names the check that was run and its result; anything unverified is stated as unverified. A check is never skipped, weakened or edited to pass. Verification is proportionate to what a mistake would cost: a check the agent runs itself for what is easily undone, an independent review for physics, rules and anything users will feel.
3. **Say what you see.** A report lists what was removed, skipped or failed, the problems noticed outside the task, and where the agent is unsure. When the user's plan looks wrong, say so with reasons before carrying it out.
4. **Three kinds of decision belong to a person.** (a) Actions that cannot be undone or that act on others' work; `.claude/rules/autonomous-workflow.md` owns the list. (b) Scientific judgement. (c) Design choices a user can see: interfaces, YAML fields, output variables, physics options. For these the agent prepares the decision (options, a recommendation, evidence) and waits. Internal design is the agent's to make and to report. A decision already recorded in a rule is followed, not re-decided; to change it, propose the change.
5. **Deliver what one person can review.** Each change is one concern, small enough to read, judge and revert alone. Work that grows beyond that is split before it is handed over.

### How instructions derive from the articles

- **Two kinds of rule.** A rule either carries out an article or records a decision. Each entry in the shared rule index ends with the article it carries out, or with `decision`.
- **Authority among instruction files**, highest first: this file; `.claude/rules/`; `.claude/skills/` and `.claude/agents/`; `dev-ref/`. When two disagree, follow the higher one for the task in hand and report the disagreement (Article 3).
- **Gates decide what can merge, not what is correct.** A CI check, test or hook believed wrong is escalated, never bypassed (Article 2).
- **Your user's explicit instruction** outranks these files, but not a gate.

### Changing the rules

- **A new or changed rule** is a decision under Article 4: propose it, with the article it carries out or the decision it records, and wait.
- **Amending an article** is its own pull request. It changes only this section and the index marks that follow from it, and `@sunt05` approves it. An agent that believes an article is wrong says so to its user; it does not amend one on its own initiative. A typo fix that changes no meaning is exempt.

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
- `.claude/rules/` - Rules that carry out the constitution or record decisions
- `.claude/agents/` - Review subagents (`docs-reviewer` for doc-code drift and docs house style)

## Shared rule files

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy. Each entry ends with the article the rule carries out, or `decision` when it records a decision already made.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work; decision.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work; Art. 1, 4.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`; decision.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`; decision.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work; decision.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - `pyproject.toml`, `.github/workflows/**/*`; decision.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - `docs/source/assets/refs/*.bib`, `docs/**/*`; decision.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`; decision.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - `docs/**/*`; decision.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.f90`, `src/suews/**/*.f95`; decision.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - `src/suews/**/*.f9*`; decision.
- [.claude/rules/labels.md](.claude/rules/labels.md) - all work; decision.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - `src/supy/**/*.py`, `src/supy/**/*.yml`, `src/supy/**/*.yaml`, `docs/**/*`; decision.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`; Art. 2, 4.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`; decision.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`; decision.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`; decision.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`; decision.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - `src/supy/data_model/**/*`, `src/supy/util/converter/**/*`, `src/supy/sample_data/**/*`, `docs/**/*`; decision.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work; Art. 2.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract; decision.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`; decision.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`; decision.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`; Art. 2.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work; Art. 5.
