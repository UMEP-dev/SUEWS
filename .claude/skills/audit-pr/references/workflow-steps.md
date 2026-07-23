# PR Review Workflow Steps

## PR Context

```bash
gh pr diff {pr} --name-only
gh pr diff {pr}
gh api repos/UMEP-dev/SUEWS/pulls/{pr}/files --jq '.[] | {filename, status, additions, deletions}'
```

---

## Code Style

Apply lint-code checks. See `style-checks.md`.

---

## Scientific Review

**Only for physics changes** (`suews_phys_*.f95`).

### Suggested Reviewers

Reviewer suggestions are advisory metadata in the audit summary. Derive them
from changed files, `dev-ref/SCIENTIFIC_REVIEWERS.md`, and
`dev-ref/REVIEW_PROCESS.md`; do not request reviews or post comments without
explicit approval.

For changed paths:

1. For physics modules, use the maintained panel in
   `dev-ref/SCIENTIFIC_REVIEWERS.md`.
2. If the module is not listed with named reviewers, suggest the default
   scientific review owners documented in that guide.
3. For coding style, linting, naming conventions, issue triage, or review
   process changes, use the code-governance maintainer table in
   `dev-ref/REVIEW_PROCESS.md`.

### Validation Checks

1. **Equations**: Verify against literature
2. **Units**: Dimensional consistency
3. **Boundary conditions**: Edge cases
4. **Conservation**: `Rn = QH + QE + QS + QF`

### AI-Assisted Changes

Flag for extra scrutiny - verify physical reasoning.

---

## Testing Review

| Requirement | Target |
|-------------|--------|
| New code coverage | ≥80% |
| Critical paths | 95-100% |
| Physics validation | Required for physics |

Check: FIRST principles, AAA pattern, tolerance assertions.

---

## Documentation Review

- **CHANGELOG** — entry with correct category
- **PR description** — scientific rationale (if physics)
- **User docs** — updated if user-facing
- **Schema bump trigger** — if `src/supy/data_model/schema/version.py`
  moved `CURRENT_SCHEMA_VERSION`, the PR must also touch
  `docs/source/contributing/schema/schema_versioning.rst` and
  `docs/source/inputs/transition_guide.rst`. See the full trigger-specific
  checklist in `review-checklist.md` → "Schema version bump".

---

## Build and CI Review

```bash
# Verify meson.build includes new Fortran files, __init__.py new Python files

# Checks, with the workflow each belongs to
gh pr checks {pr} --repo UMEP-dev/SUEWS --json name,state,bucket,workflow,link

# Which contexts the ruleset actually requires (query; do not assume)
gh api repos/UMEP-dev/SUEWS/rulesets --jq '.[] | select(.target=="branch") | .id'
gh api repos/UMEP-dev/SUEWS/rulesets/{id} \
  --jq '.rules[] | select(.type=="required_status_checks")
        | .parameters.required_status_checks[].context'

# Failure detail (job id is the tail of the check link)
gh run view --repo UMEP-dev/SUEWS --job {job-id} --log-failed
```

For each non-green check, diagnose and classify the remedy -- author-fixable,
maintainer-gated (bypass label), re-trigger mechanics, or infrastructure -- and
write it into the draft as a CI finding with a severity. A red check reported
without a remedy leaves the PR blocked; that is the gap gh#1642 exposed.

Propose remedies, never perform them: applying a bypass label, re-running a
workflow, closing/reopening, and pushing a fix are author or maintainer actions.

Gate-by-gate catalogue, the required-vs-convention-blocking distinction, and the
bypass-label payload race: `ci-gates.md`.

---

## Draft, Approve, Post

### Draft Comments

Present to human for approval.

### Approval Options

- `approve` - post all as drafted
- `approve with edits` - provide edits
- `skip line comments` - summary only
- `cancel` - don't post

### Post (after approval)

```bash
# Line comment
gh api repos/UMEP-dev/SUEWS/pulls/{pr}/comments \
  -f body="..." -f commit_id="$SHA" -f path="..." -f line=42 -f side="RIGHT"

# Summary
gh api repos/UMEP-dev/SUEWS/issues/{pr}/comments -f body="..."
```
