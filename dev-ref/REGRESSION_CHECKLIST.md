# Regression checklist

Lessons from past failures, each written so that a reviewer can tell from the diff alone whether it applies.
`audit-pr` and `fix-issue` read this file from the base branch, check only the items whose trigger matches the diff, and report each matched item as checked, not applicable, not verified, or failed.
An item is added only when the same failure has occurred in at least two pull requests or issues, or once with a recorded incident; each item names its origin.
An item whose check is now enforced by CI or a hook is removed, with the enforcing check named in the removal commit.

| Trigger (visible in the diff) | Check | Origin |
|---|---|---|
| New `.py` under `src/supy/` or new `.f9*` under `src/suews/src/` | File listed in the matching `meson.build` | `.claude/rules/00-project-essentials.md` |
| A change that is physics-changing under `.claude/rules/physics-change-evidence.md`, "What counts as a physics-changing PR" (physics source, a vendored reference fixture, a physics-affecting default) | `## Scientific evidence` section, and the `0-physics:change` label for a changed physics default (the `Physics-change label` job enforces the label for a changed stored reference, and the label selects the slow physics tier) | gh#1570, gh#1575; `.claude/rules/physics-change-evidence.md` |
| A change under `src/supy/data_model/` that meets a trigger in `.claude/rules/python/schema-versioning.md`, "When to bump" | The per-PR bookkeeping that rule lists and CI does not check: `SCHEMA_VERSIONS` entry, `yaml_upgrade.py` handler, `sample_config.yml` resync (`check_schema_version_bump.py` checks the version bump and the docs sync) | `.claude/rules/python/schema-versioning.md` |
| A fused legacy identifier (e.g. `netradiationmethod`; inventory in gh#1256) renamed | Dedicated PR with a deprecation path, not an inline rename | gh#1256; `.claude/rules/00-project-essentials.md`, "Legacy identifiers" |
| A change to what a public API or a configuration input does | Every public execution path verified separately, as item 3 of `.claude/rules/tests/verification-methodology.md` lists | `.claude/rules/tests/verification-methodology.md` |
| New or changed text in print or logging calls, validation reports, or generated files | ASCII only | `.claude/rules/00-project-essentials.md` |
| A file listed in `.claude/rules/docs/conventions.md`, "Generated files (do not hand-edit)", changed | The change is made in the generator or its source registry, and the output is only regenerated, not hand-edited | `.claude/rules/docs/conventions.md` |

Each row is an index entry: the rule file named in Origin holds the requirement, and the Check column only summarises it; where they differ, the rule file governs.
The rows above were seeded from rules that already exist, so they predate the admission test in this file's header; maintainers add rows from future incidents.
Where Origin names only a rule file, there is no single incident number; a reviewer may add one beside it.
