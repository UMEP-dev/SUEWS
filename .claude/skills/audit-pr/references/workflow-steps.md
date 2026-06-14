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

### Module Reviewers

| File Pattern | Module | Reviewers |
|--------------|--------|-----------|
| `suews_phys_stebbs` | module:stebbs | @yiqing1021, @denisehertwig |
| `suews_phys_rslprof` | module:rslprof | @vitorlavor, @suegrimmond |
| `suews_phys_spartacus` | module:spartacus | @suegrimmond, @yiqing1021 |
| `suews_phys_snow` | module:snow | @havum, @ljarvi |
| `suews_phys_ehc` | module:ehc | @sunt05 |
| `suews_phys_anohm` | module:anohm | @sunt05 |
| General | Overall | @sunt05, @MatthewPaskin |

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

## Build Review

```bash
gh pr checks {pr}
# Verify meson.build includes new files
```

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
