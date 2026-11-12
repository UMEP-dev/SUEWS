# AGENTS.md

The constitution for work in this repository, and the index of the rules that carry it out.
`AGENTS.md` is canonical for every coding agent; `CLAUDE.md` only imports it, so edit this file and never the loader.
Setup, everyday rules and skill procedures live in the files listed under References.

## Constitution

These five articles bind everyone who works in this repository, person or agent.
Authority runs from evidence, to the decisions of named people where evidence runs out, to procedure.

1. **Evidence decides what evidence can decide.** A claim of fact, including "done", gives the commit, the steps and the result, so that anyone can repeat it.
   It is *reported* when the claimant has run it, and *confirmed* when CI, a person, or an agent that did not write the change and was given the steps but not the expected result, reproduces it; who made the claim, and who disagrees, does not matter.
   Reproduction shows that a check repeats, not that the check is right.
2. **Say what you see.** Report what was removed, skipped or failed, what is unconfirmed, what was found outside the task, and what was chosen along the way.
   Object, with reasons, before acting on a plan you think is wrong.
3. **Where evidence runs out, a named person answers for the decision.** When no stated steps can settle a question, or an action cannot be undone, a named person decides: the domain owner where a rule names one, otherwise the maintainer (`@sunt05`).
   The record says who decided, when, what was known and what evidence would overturn it; it lives in the pull request or issue for a single case and in a rule for a standing decision.
   Being wrong is not a violation; a decision nobody owns, or nobody recorded, is.
   An agent reports its choices and does not own them; an unreported choice is a defect in the change, and whoever submits the change answers for it.
4. **Own the outcome.** Carry a goal to a state another party can review and repeat, and stay inside it.
   What lies outside is reported, not absorbed.
5. **One concern per change.** Each change can be reviewed, repeated and reverted alone.
   Work that grows beyond that is not handed over whole: a split is proposed first.

### Authority and amendment

- **Authority among instruction files**, highest first: this file; `.claude/rules/`; `.claude/skills/` and `.claude/agents/`; `dev-ref/`.
  When two disagree, follow the higher one for the task in hand and report the disagreement.
- **Where the rules are silent**, the articles guide the choice.
  They never permit what a rule reserves for a person.
- **Procedure** for the articles is in `.claude/rules/autonomous-workflow.md` ("Carrying out the constitution").
- **Articles change** only by a dedicated pull request approved by `@sunt05`, who answers for them.
  An agent that believes an article is wrong says so; it does not amend one on its own initiative.
  A typo fix that changes no meaning is exempt.

## Rule loading

Claude Code loads `.claude/rules/` automatically according to each rule's scope. Other agents must read the applicable files listed in the shared rule index below.

## References

- `.claude/rules/00-project-essentials.md` - Setup, the rules that apply to all work, and what to run before committing
- `.claude/rules/` - Facts, recorded decisions, and procedure for the articles
- `.claude/README.md` - The workspace: available skills, hooks, and how rules and skills relate
- `.claude/skills/` - Skill procedures
- `.claude/agents/` - Review subagents
- `.impeccable.md` - Design direction for SUEWS-facing surfaces (site/, landing, docs)

## Shared rule files

Read the unscoped rules below before making changes. Read scoped rules before working on a matching path (patterns are relative to the repository root). These Markdown files are shared instructions even though they live under `.claude/`; agents without automatic rule loading must read them explicitly. Keep the rule bodies in those files so there is one maintained copy. An entry that ends with `Art. N` holds procedure for that article.

- [.claude/rules/00-project-essentials.md](.claude/rules/00-project-essentials.md) - all work.
- [.claude/rules/autonomous-workflow.md](.claude/rules/autonomous-workflow.md) - all work; Art. 1, 2, 3, 4.
- [.claude/rules/changelog/format.md](.claude/rules/changelog/format.md) - `CHANGELOG.md`.
- [.claude/rules/ci/conventions.md](.claude/rules/ci/conventions.md) - `.github/workflows/**/*.yml`, `.github/workflows/**/*.yaml`; Art. 1.
- [.claude/rules/code-design.md](.claude/rules/code-design.md) - all work.
- [.claude/rules/dependency-safety.md](.claude/rules/dependency-safety.md) - `pyproject.toml`, `.github/workflows/**/*`.
- [.claude/rules/docs/bib-topic-tags.md](.claude/rules/docs/bib-topic-tags.md) - `docs/source/assets/refs/*.bib`, `docs/**/*`.
- [.claude/rules/docs/conventions.md](.claude/rules/docs/conventions.md) - `docs/**/*`, `site/**/*`, `README.md`.
- [.claude/rules/docs/release-docs-sanity.md](.claude/rules/docs/release-docs-sanity.md) - `docs/**/*`.
- [.claude/rules/fortran/conventions.md](.claude/rules/fortran/conventions.md) - `src/suews/**/*.f9*`, `src/suews/**/*.f90`, `src/suews/**/*.f95`.
- [.claude/rules/fortran/error-reporting.md](.claude/rules/fortran/error-reporting.md) - `src/suews/**/*.f9*`.
- [.claude/rules/labels.md](.claude/rules/labels.md) - all work.
- [.claude/rules/naming-convention.md](.claude/rules/naming-convention.md) - `src/supy/**/*.py`, `src/supy/**/*.yml`, `src/supy/**/*.yaml`, `docs/**/*`.
- [.claude/rules/physics-change-evidence.md](.claude/rules/physics-change-evidence.md) - `src/suews/src/suews_phys_*.f95`, `test/fixtures/data_test/sample_output_2012-*.csv`, `test/fixtures/data_test/provenance.json`, `test/fixtures/data_test/stebbs_test/**`, `test/fixtures/benchmark1/**`; Art. 1, 3.
- [.claude/rules/python/api-approach.md](.claude/rules/python/api-approach.md) - `src/supy/**/*.py`, `docs/source/tutorials/**/*.py`.
- [.claude/rules/python/config-patterns.md](.claude/rules/python/config-patterns.md) - `src/supy/**/*.py`.
- [.claude/rules/python/conventions.md](.claude/rules/python/conventions.md) - `src/supy/**/*.py`.
- [.claude/rules/python/deprecation.md](.claude/rules/python/deprecation.md) - `src/supy/**/*.py`.
- [.claude/rules/python/schema-versioning.md](.claude/rules/python/schema-versioning.md) - `src/supy/data_model/**/*`, `src/supy/util/converter/**/*`, `src/supy/sample_data/**/*`, `docs/**/*`; Art. 3.
- [.claude/rules/review-convergence.md](.claude/rules/review-convergence.md) - all work; Art. 1, 2, 3.
- [.claude/rules/rust/bridge-contract.md](.claude/rules/rust/bridge-contract.md) - `src/suews_bridge/**`, `src/supy/_run_rust.py`, `dev-ref/RUST_BRIDGE*.md`; also load when changing either side of a bridge contract; Art. 1, 3.
- [.claude/rules/rust/conventions.md](.claude/rules/rust/conventions.md) - `src/suews_bridge/**/*.rs`, `src/suews_bridge/Cargo.toml`, `src/suews_bridge/build.rs`.
- [.claude/rules/tests/patterns.md](.claude/rules/tests/patterns.md) - `test/**/*.py`, `tests/**/*.py`.
- [.claude/rules/tests/verification-methodology.md](.claude/rules/tests/verification-methodology.md) - `test/**/*.py`; Art. 1, 2.
- [.claude/rules/work-sizing.md](.claude/rules/work-sizing.md) - all work; Art. 5.
