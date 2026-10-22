# Issue and PR Labels

Rules for labelling issues and pull requests in UMEP-dev/SUEWS. They apply to
every session that adds, removes or proposes a label, by hand or through a skill.

---

## Only the tiered labels

Labels follow a four-tier hierarchy. Use no label outside it.

- `1-*` type: exactly one (`1-bug`, `1-feature`, `1-maintenance`, `1-question`).
- `2-*` area: one or more (`2-module:*`, `2-infra:*`, `2-doc:*`, `2-meta:*`).
- `3-*` priority: exactly one on an open issue (`3-P0`, `3-P1`, `3-P2`).
- `4-*` status: exactly one on an open issue (`4-ready`, `4-in-progress`,
  `4-needs-deps`, `4-needs-discussion`, `4-needs-science`, `4-deferred`).

`0-*` labels are automation and CI metadata (`0-physics:change`,
`0-ci:schema-audit-ok`, `0-auto:audited`, `0-pr:*` and similar). Apply one only
where a rule or skill calls for it; they do not count towards the four tiers.

Untiered labels such as `design-sensitive`, `mechanical` and `hard-gate` were
removed from the repository and must not be recreated or applied. Express what
they meant with a tier instead: judgement calls take `4-needs-science` or
`4-needs-discussion`, and a pause point in a wave is recorded in the umbrella's
checklist, not as a label.

## Pull requests

A PR carries one `1-*` type and the `2-*` areas it touches, plus any `0-*` label
its own rules require (for example `0-physics:change` when it moves model output
or a reference fixture). Priority and status labels belong on the issue the PR
addresses, not on the PR.

## Keep status current

- An issue with an open PR addressing it is `4-in-progress`.
- An agenda item for the weekly dev meeting is `4-needs-discussion`; the agenda
  is built from that label, so remove it once the meeting has decided.
- Do not leave two `4-*` labels on one issue.

## Creating labels

Do not create labels. Use the labels that already exist (`gh label list`); if a
new area label seems needed, propose it to a maintainer instead of creating it.

## Detail

Layer meanings and the audit procedure are in
`.claude/skills/triage-issue/references/label-hierarchy.md`.
