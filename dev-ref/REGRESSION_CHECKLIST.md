# Regression checklist

Lessons from past failures, each written so that a reviewer can tell from the diff alone whether it applies.
`audit-pr` and `fix-issue` read this file from the base branch, check only the items whose trigger matches the diff, and report each matched item as checked, not applicable, or failed.
An item is added only when the same failure has occurred in at least two pull requests or issues, or once with a recorded incident; each item names its origin.
An item whose check is now enforced by CI or a hook is removed, with the enforcing check named in the removal commit.

| Trigger (visible in the diff) | Check | Origin |
|---|---|---|
| New `.py` under `src/supy/` or new `.f9*` under `src/suews/src/` | File listed in the matching `meson.build` | `.claude/rules/00-project-essentials.md` |
| `suews_phys_*.f95`, a reference fixture, or a physics default changes | `0-physics:change` label, `## Scientific evidence` section, slow physics tier run before merge | gh#1570, gh#1575 |
| Field renamed, removed, retyped or made required in `src/supy/data_model/` | Schema version bump, `SCHEMA_VERSIONS` entry, `yaml_upgrade.py` handler, `sample_config.yml` resync | `.claude/rules/python/schema-versioning.md` |
| A fused legacy identifier (e.g. `netradiationmethod`) renamed | Dedicated PR with a deprecation path | gh#1256 |
| A behavioural contract changes in a public API | Each public path tested separately: YAML path, dict, config object, update API | `.claude/rules/tests/verification-methodology.md` |
| New print, log or report text | ASCII only | `.claude/rules/00-project-essentials.md` |
| A file listed under "Generated files" in `.claude/rules/docs/conventions.md` edited | Generator or its source registry changed instead of the output | `.claude/rules/docs/conventions.md` |

The rows above were seeded from rules that already exist, so they predate the admission test in this file's header; maintainers add rows from future incidents.
Origins marked with a rule file have no single incident number; a reviewer may replace them with one.
