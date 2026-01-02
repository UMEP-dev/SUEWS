# PR Review Workflow Steps

## Step 1: PR Context

```bash
gh pr diff {pr} --name-only
gh pr diff {pr}
gh api repos/UMEP-dev/SUEWS/pulls/{pr}/files --jq '.[] | {filename, status, additions, deletions}'
```

---

## Step 2: Code Style

Apply lint-code-skill checks. See `style-checks.md`.

---

## Step 3: Scientific Review

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

## Step 4: Testing Review

| Requirement | Target |
|-------------|--------|
| New code coverage | ≥80% |
| Critical paths | 95-100% |
| Physics validation | Required for physics |

Check: FIRST principles, AAA pattern, tolerance assertions.

---

## Step 5: Documentation Review

| Check | Requirement |
|-------|-------------|
| CHANGELOG | Entry with correct category |
| PR description | Scientific rationale (if physics) |
| User docs | Updated if user-facing |

---

## Step 6: Build Review

```bash
gh pr checks {pr}
# Verify meson.build includes new files
```

---

## Step 7-9: Draft, Approve, Post

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
