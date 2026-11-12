# AGENTS.md

Shared instructions for Claude Code, Codex, and other coding agents. `AGENTS.md` is canonical; `CLAUDE.md` imports it for compatibility. Edit AGENTS.md (and AGENT_GUIDE.md where present), never the CLAUDE.md compatibility loader. Tool-specific commands below apply only when that tool is available; other agents should read the referenced procedure and use their equivalent tools.

Guidance for coding agents in this repository.

## Constitution

Each rule below carries out one or more of these five articles, records a decision the maintainer has already made, or both; skills and guides add procedure. Where all of them are silent, the articles guide the choice. They never permit what a gate withholds, and Article 4 limits every other article.

1. **Own the outcome.** Given a goal, carry it to a state a person can review: plan, implement, verify. Open a pull request only when asked, or when the invoked skill says so. A question the tree or the rules answer is not handed back; one they do not answer is asked if it falls under Article 4, and otherwise decided and reported as an assumption. Stay inside the goal: a problem found outside it is reported, not fixed in the same change. If it blocks the goal, fix it as a separate prior change when that is itself permitted; otherwise stop and report the block. Being blocked on a person is not work owed.
2. **Done means verified.** A claim of completion names the check that was run and its result; anything unverified is stated as unverified. A check that applies to the change is run. A failing one is fixed, re-run once and reported when the failure is unrelated, or escalated. Expected values change only with the evidence the owning rule requires. For physics source, reference fixtures, rule files, and public API, YAML or output changes, completion is claimed only as "awaiting independent review" until a reviewer that did not write the change has passed it; elsewhere the agent's own check is enough.
3. **Say what you see.** A report lists what was removed, skipped or failed, the problems noticed outside the task, and where the agent is unsure. When a person's plan looks wrong, say so with reasons before carrying it out. With no person present, proceed only if the plan is otherwise permitted, and lead the report with the objection.
4. **Three kinds of decision belong to a person.** (a) Actions that cannot be undone or that act on others' work; `.claude/rules/autonomous-workflow.md` lists the standing cases, and the list is not exhaustive. (b) Scientific judgement, including any change to a model output value. (c) Design a user can see: the names, units, defaults and semantics of interfaces, YAML fields and output variables, and runtime dependencies. For these the agent prepares the decision (options, a recommendation, evidence) and stops that item; in a batch it records the item as escalated and continues. A pull request carrying the evidence the owning rule requires is a prepared decision, and the wait is at merge. Internal design within the goal is the agent's to make and to report. A decision already recorded in a rule is followed, not re-decided; to change it, propose the change.
5. **Deliver what one person can review.** Each change is one concern and can be reverted alone. Mechanical or generated diffs go in their own change and do not count against its size. Work that grows beyond one concern is split before it is handed over.

### How instructions derive from the articles

- **Authority among instruction files**, highest first: this file; `.claude/rules/`; `.claude/skills/` and `.claude/agents/`; `dev-ref/`. When two disagree, follow the higher one for the task in hand and report the disagreement (Article 3).
- **A gate** is (i) a required CI check or hook, or (ii) an action reserved under Article 4(a). A person's explicit instruction satisfies (ii) for that action. Nothing but a maintainer changing the check satisfies (i): a check believed wrong is escalated, never bypassed. Gates decide what can merge, not what is correct.
- **A person's explicit instruction** outranks these files, but not a gate of kind (i).

### Changing the rules

- **A new or changed rule** is proposed with its mark (Article 4).
- **Articles change** only by a dedicated pull request approved by `@sunt05`. An agent that believes an article is wrong says so; it does not amend one on its own initiative. A typo fix that changes no meaning is exempt.

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

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy. Each entry ends with its mark: the article(s) the rule carries out, `decision` when it records a decision already made, or both.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work; decision.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work; Art. 1, 3, 4.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`; decision.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`; decision.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work; Art. 2.
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
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work; Art. 2, 3, 4.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract; Art. 2, 3, 4, decision.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`; decision.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`; decision.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`; Art. 2, 3.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work; Art. 5.
