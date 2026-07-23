# CI Gate Diagnosis and Remedies

How to turn a red or pending check on a SUEWS PR into a named, actionable remedy
inside the review, instead of leaving the author to work it out.

**Origin**: gh#1642. An audit-pr pass produced good findings, the author addressed
every one of them, and the PR still sat blocked because the review never said what
the remaining red check (`Require schema version bump`) meant or how to clear it.
The author had to stop and ask a maintainer. The remedy was a bypass label, not a
code defect -- exactly the class of blocker a review is meant to name. A review that
audits the diff but ignores the checks hands back a PR that still cannot move.

---

## Step 1 -- read the checks before drafting

```bash
# All checks with their state, bucket, and originating workflow
gh pr checks {pr} --repo UMEP-dev/SUEWS --json name,state,bucket,workflow,link

# Which contexts the branch ruleset actually requires (do not assume; ask)
gh api repos/UMEP-dev/SUEWS/rulesets \
  --jq '.[] | select(.target=="branch") | .id'
gh api repos/UMEP-dev/SUEWS/rulesets/{id} \
  --jq '.rules[] | select(.type=="required_status_checks")
        | .parameters.required_status_checks[].context'

# Failure detail for a specific red check (job id is the tail of the check link)
gh run view --repo UMEP-dev/SUEWS --job {job-id} --log-failed
```

Read the failure output. Every SUEWS lint gate prints its own remediation text
(`scripts/lint/check_schema_version_bump.py`, `check_knowledge_pack_freshness.py`
and siblings all end with a "what to do" paragraph). Quote the operative sentence
into the finding rather than paraphrasing it from memory.

Also read the `<!-- ci-build-plan -->` bot comment on the PR. It states which
platforms, Python versions, and test tier this PR's paths selected, which explains
why a check some other PR ran is absent here.

## Step 2 -- required is not the same as blocking

`gh pr checks` renders every check identically; the ruleset decides which ones
actually gate the merge, and project convention adds more:

- **Ruleset-required** -- listed under `required_status_checks` above. A red one
  blocks the merge unconditionally.
- **Convention-blocking** -- the `0-ci:*` audit family (schema version, knowledge
  pack, encoding, clippy, Rust/Python alias parity, workflow security). These may
  not appear in the ruleset, but the project does not merge over them. Treat a red
  one as blocking and say so.
- **Informational** -- summary/observability jobs, `skipping` entries for release
  paths, and anything `continue-on-error`. Note, do not block.

For the green / near-green / red vocabulary itself, defer to the single canonical
definition in `.claude/skills/triage-pr/references/rubric.md` -> "Part C". Do not
restate or fork it here.

## Step 3 -- classify each non-green check by who can clear it

This classification is the actionable part of the finding. Name it explicitly.

- **Author-fixable** -- the check is right and the diff is wrong or incomplete.
  Remedy: a specific edit, in this PR. Example: a public field was renamed, so bump
  `CURRENT_SCHEMA_VERSION`, add the `SCHEMA_VERSIONS` entry, register the
  `_HANDLERS` migration, and touch the two schema doc pages.
- **Maintainer-gated** -- the check is a false positive for this diff and the
  documented escape is a bypass label. Remedy: state the reason the diff is
  genuinely non-structural or cosmetic, and *ask a maintainer* to apply the label.
  Never apply it yourself as the reviewer (see "Never do these" below).
- **Re-trigger mechanics** -- the code and labels are already correct and the run is
  stale. Remedy: the specific re-trigger that works for that gate (see the payload
  race below). "Re-run the job" is often the one thing that does not work.
- **Infrastructure or flake** -- runner timeout, network failure, upstream outage.
  Remedy: name it as unrelated to the diff so it is not mistaken for a code defect,
  and escalate if it recurs. There is no acknowledged flaky-check list for this
  repository; do not invent one.

## Step 4 -- severity and the draft

Fold CI findings into the same severity vocabulary as the rest of the review:

- Red ruleset-required or convention-blocking check -> `[blocking]`.
- Red informational check, or a green gate whose bypass label was applied for a
  reason the diff does not support -> `[major]`.
- Pending required checks at review time -> note in the summary, not a finding.

Each CI finding states, in one line: the check name, what it is enforcing, why it
is red *for this diff*, the remedy, and who can apply it.

---

## Gate catalogue

Workflow-to-remedy map for the gates that most often stop a SUEWS PR. Check names
are the job display names as they appear in `gh pr checks`.

- **`Require schema version bump`** (`schema-version-audit.yml`)
  - Fires on: `src/supy/data_model/**`, `sample_config.yml`, the two schema doc
    pages, the lint script, the workflow itself.
  - Enforces: a structural YAML-surface change moves `CURRENT_SCHEMA_VERSION`; and,
    if the version did move, that the user-facing schema docs moved with it.
  - Author-fixable when: the diff renames, removes, retypes, or requires a public
    field, or restructures a nested section. Full checklist in
    `.claude/rules/python/schema-versioning.md`.
  - Maintainer-gated when: the `data_model` diff is genuinely non-structural
    (docstrings, comments, validator-rule tightening, internal refactor). Label:
    `0-ci:schema-audit-ok`. Subject to the payload race below.
- **`Require knowledge-pack rebuild`** (`knowledge-pack-audit.yml`)
  - Fires on: `src/supy/data_model/**`, `src/supy/cmd/**`, `src/supy/knowledge/**`.
  - Enforces: the committed knowledge pack still binds to the current tree.
  - Author-fixable when: the pack genuinely needs rebuilding.
  - Maintainer-gated when: the diff cannot affect pack content. Label:
    `0-ci:knowledge-pack-audit-ok`. Subject to the payload race below.
- **`Require explicit encoding on text I/O`** (`encoding-audit.yml`)
  - Fires on: any `**.py`.
  - Enforces: `ruff PLW1514` plus a `read_text`/`write_text` sweep -- no implicit
    locale encoding.
  - Author-fixable: add `encoding="utf-8"`. This is nearly always the right fix.
  - Bypass label `0-ci:encoding-audit-ok` is referenced by the workflow but does not
    currently exist in the repository, so proposing it means asking a maintainer to
    create the label first. Prefer the code fix.
- **`cargo clippy (suews_bridge)`** (`rust-clippy.yml`)
  - Fires on: `src/suews_bridge/**`.
  - Enforces: clippy with warnings denied.
  - Author-fixable: fix the lint, or justify a scoped `#[allow]`.
  - Bypass label `0-ci:clippy-ok` is likewise referenced but not yet created.
- **`Require Rust/Python registry parity`** (`rust-yaml-aliases-audit.yml`)
  - Fires on: `field_renames.py`, `physics_families.py`, `field_renames.rs`.
  - Enforces: the Python and Rust alias registries agree.
  - Author-fixable only -- no bypass. Update whichever side is behind.
- **`Enforce SHA-pinned actions`** (`workflow-security.yml`)
  - Fires on: `.github/workflows/**`, `.github/actions/**`.
  - Enforces: every action pinned to a full commit SHA with a version comment.
  - Author-fixable only -- no bypass. `Advisory workflow audit` (zizmor) in the same
    workflow is informational.
- **`Audit Python dependency intake`** (`dependency-audit.yml`)
  - Fires on: `pyproject.toml`, `Makefile`, `scripts/security/**`, the build
    workflows.
  - Enforces: `pip-audit` advisories and the Python startup-hook sweep.
  - See `.claude/rules/dependency-safety.md`. A startup-hook hit is never a bypass
    case -- stop and escalate.
- **`Validate CLAUDE.md`** (`validate-claude-md.yml`)
  - Fires on: `CLAUDE.md`, `.claude/rules/**`.
  - Enforces: integrity plus a content-reduction guard.
  - Author-fixable: restore removed content, or justify the reduction in the PR.
- **`PR build validation`** and the wheel/test matrix
  (`build-publish_to_pypi.yml`, `test-*-reusable.yml`)
  - The build and test lanes proper. A red one is ordinary review substance: read
    the failing job log and treat it as a code finding, not a gate finding.

Do not hardcode the required-check list or the label inventory into a review --
both change. Query them (Step 1) at review time.

---

## The bypass-label payload race

Two gates read their bypass label from the **static event payload** captured when
the triggering event fired -- `${{ toJSON(github.event.pull_request.labels.*.name) }}`
-- and subscribe only to the default `pull_request` types (`opened`, `synchronize`,
`reopened`):

- `schema-version-audit.yml` (`0-ci:schema-audit-ok`)
- `knowledge-pack-audit.yml` (`0-ci:knowledge-pack-audit-ok`)

So a label applied *after* the run started is invisible to it, and the gate stays
red even though the label is plainly on the PR.

- Preferred: apply the label at creation -- `gh pr create ... --label "0-ci:schema-audit-ok"`.
- Already failed: push a commit (fires `synchronize`) or
  `gh pr close {pr} && gh pr reopen {pr}` (fires `reopened`). Either regenerates a
  payload carrying the current labels.
- **`gh run rerun` does not help** -- it replays the stale payload. Recommending it
  costs the author a cycle and looks like the fix failed.

`encoding-audit.yml` and `rust-clippy.yml` do include `labeled`/`unlabeled` in their
trigger types, so labelling those re-triggers them directly. Check the workflow's
`types:` line before advising a re-trigger.

---

## Never do these

- **Do not apply a bypass label as the reviewer.** It is a repo write on someone
  else's PR and a maintainer decision by policy
  (`.claude/rules/python/schema-versioning.md`: "a contributor should not add the
  label themselves"). Propose it, with the reason, and let a maintainer act. The
  autonomous tier never creates labels either -- if a bypass label does not exist,
  say so rather than creating it.
- **Do not propose a bypass to dodge a real change.** The label is for diffs that
  do not need the bump, never for diffs that need it and would rather not. Verify
  the non-structural claim against the diff yourself before writing the sentence.
- **Do not propose a bypass at all for `0-physics:change`.** Per
  `.claude/rules/physics-change-evidence.md` that gate has no cosmetic bypass; it is
  satisfied by supplying the evidence.
- **Do not re-run, close/reopen, or push to a PR you do not own** to clear a check.
  Those are author or maintainer actions; the review names them, the review does not
  perform them.
- **Do not report "CI is red" without a remedy.** That is the failure mode gh#1642
  exposed. If the cause is not apparent from the diff and the log, say so explicitly
  and escalate -- an honest "unexplained, needs a maintainer" is actionable; a bare
  red status is not.
