---
paths:
  - CHANGELOG.md
  - changes/*.md
---

# CHANGELOG Format

Conventions for SUEWS CHANGELOG entries.

---

## Fragments, not direct edits

Pull requests do not edit `CHANGELOG.md`. Each PR adds a fragment file `changes/<number>.<category>.md` holding the exact bullet lines its entry would add, with no date heading [decision: software maintainer, 2026-11-22]. A shared `CHANGELOG.md` edit makes every open PR conflict with every other, so PRs had to merge one at a time; fragments let PRs with disjoint files share a merge-queue batch.

- `<number>` is the PR number (the issue number when there is no PR yet); `<category>` is one of the categories below and must match the tag on every top-level bullet in the file
- A PR with entries in two categories adds two files (`2171.feature.md`, `2171.doc.md`)
- Validate with `python scripts/suews/fold_changelog.py --check`
- Before each release, a maintainer folds the fragments into `CHANGELOG.md` with `python scripts/suews/fold_changelog.py` and lands the result through a PR; the fold dates each entry by the commit that added its fragment and only inserts lines
- `changes/README.md` is the contributor-facing summary

Everything below applies to the bullet text in a fragment exactly as it does to `CHANGELOG.md`.

---

## Date Format

```markdown
### DD Mon YYYY
```

- No leading zero on single-digit days
- Three-letter month abbreviation
- Example: `### 26 Nov 2025`

---

## Categories

- `[feature]` - New functionality
- `[bugfix]` - Bug fixes (link issue)
- `[change]` - User-facing/breaking changes
- `[maintenance]` - Internal/dev tooling, AGENTS.md [decision: software maintainer, 2026-11-17]
- `[doc]` - User documentation (not AGENTS.md) [decision: software maintainer, 2026-11-17]

**IMPORTANT**: AGENTS.md updates are `[maintenance]`, not `[doc]` [decision: software maintainer, 2026-11-17]

---

## Status Tags (Governance)

For `[feature]` and `[change]` entries, add a status tag:

- `[experimental]` - Under development, not for public announcement
- `[stable]` - Governance-approved, can appear in public release notes
- `[internal]` - Internal tooling, never announced publicly

**Rules**:
- New features default to `[experimental]` until governance approval
- Only `[stable]` entries are included in public release announcements
- `[bugfix]`, `[maintenance]`, `[doc]` do not need status tags

**Format**: `[category][status] Description (#ref)`

```markdown
- [feature][experimental] Added new radiation scheme (#123)
- [feature][stable] Added OOP interface for output data (#456)
- [change][experimental] Refactored land cover API (#789)
```

---

## Reference Format

- **Issue/PR**: `(#123)` with `#`
- **Commit SHA**: `(abc1234)` without `#`

---

## Multi-Line Entries

```markdown
- [category] Main description (#123)
  - Sub-bullet with 2-space indent
  - Additional details (abc1234)
```

---

## Critical Rules

1. **NEVER** modify the Annual Statistics table
2. **Use actual commit dates**, not today's date (the fold takes them from git; when editing `CHANGELOG.md` by hand, use the commit date)
3. **Group entries by date** under appropriate heading
4. Keep existing entries intact

---

## Examples

```markdown
### 26 Nov 2025

- [feature] Added new validation system (#123)
  - Implemented Phase A checks (abc1234)
- [bugfix] Fixed temperature calculation (#124)
- [maintenance] Updated CI configuration (def5678)

### 25 Nov 2025

- [doc] Added tutorial for new users (#125)
```

---

## Wrong vs Correct

```markdown
.. WRONG
### 26-Nov-2025              (Wrong date format)
- [new-feature] Added X      (Wrong category)
- doc: Updated readme        (Wrong format)

.. CORRECT
### 26 Nov 2025
- [feature] Added new validation system (#123)
- [doc] Updated README with examples (#124)
```
